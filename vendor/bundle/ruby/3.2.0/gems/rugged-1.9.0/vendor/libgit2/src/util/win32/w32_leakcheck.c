/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "w32_leakcheck.h"

#if defined(GIT_WIN32_LEAKCHECK)

#include "Windows.h"
#include "Dbghelp.h"
#include "win32/posix.h"
#include "hash.h"
#include "runtime.h"

/* Stack frames (for stack tracing, below) */

static bool   g_win32_stack_initialized = false;
static HANDLE g_win32_stack_process = INVALID_HANDLE_VALUE;
static git_win32_leakcheck_stack_aux_cb_alloc  g_aux_cb_alloc  = NULL;
static git_win32_leakcheck_stack_aux_cb_lookup g_aux_cb_lookup = NULL;

int git_win32_leakcheck_stack_set_aux_cb(
	git_win32_leakcheck_stack_aux_cb_alloc cb_alloc,
	git_win32_leakcheck_stack_aux_cb_lookup cb_lookup)
{
	g_aux_cb_alloc = cb_alloc;
	g_aux_cb_lookup = cb_lookup;

	return 0;
}

/**
 * Load symbol table data.  This should be done in the primary
 * thread at startup (under a lock if there are other threads
 * active).
 */
void git_win32_leakcheck_stack_init(void)
{
	if (!g_win32_stack_initialized) {
		g_win32_stack_process = GetCurrentProcess();
		SymSetOptions(SYMOPT_LOAD_LINES);
		SymInitialize(g_win32_stack_process, NULL, TRUE);
		g_win32_stack_initialized = true;
	}
}

/**
 * Cleanup symbol table data.  This should be done in the
 * primary thead at shutdown (under a lock if there are other
 * threads active).
 */
void git_win32_leakcheck_stack_cleanup(void)
{
	if (g_win32_stack_initialized) {
		SymCleanup(g_win32_stack_process);
		g_win32_stack_process = INVALID_HANDLE_VALUE;
		g_win32_stack_initialized = false;
	}
}

int git_win32_leakcheck_stack_capture(git_win32_leakcheck_stack_raw_data *pdata, int skip)
{
	if (!g_win32_stack_initialized) {
		git_error_set(GIT_ERROR_INVALID, "git_win32_stack not initialized.");
		return GIT_ERROR;
	}

	memset(pdata, 0, sizeof(*pdata));
	pdata->nr_frames = RtlCaptureStackBackTrace(
		skip+1, GIT_WIN32_LEAKCHECK_STACK_MAX_FRAMES, pdata->frames, NULL);

	/* If an "aux" data provider was registered, ask it to capture
	 * whatever data it needs and give us an "aux_id" to it so that
	 * we can refer to it later when reporting.
	 */
	if (g_aux_cb_alloc)
		(g_aux_cb_alloc)(&pdata->aux_id);

	return 0;
}

int git_win32_leakcheck_stack_compare(
	git_win32_leakcheck_stack_raw_data *d1,
	git_win32_leakcheck_stack_raw_data *d2)
{
	return memcmp(d1, d2, sizeof(*d1));
}

int git_win32_leakcheck_stack_format(
	char *pbuf, size_t buf_len,
	const git_win32_leakcheck_stack_raw_data *pdata,
	const char *prefix, const char *suffix)
{
#define MY_MAX_FILENAME 255

	/* SYMBOL_INFO has char FileName[1] at the end.  The docs say to
	 * to malloc it with extra space for your desired max filename.
	 */
	struct {
		SYMBOL_INFO symbol;
		char extra[MY_MAX_FILENAME + 1];
	} s;

	IMAGEHLP_LINE64 line;
	size_t buf_used = 0;
	unsigned int k;
	char detail[MY_MAX_FILENAME * 2]; /* filename plus space for function name and formatting */
	size_t detail_len;

	if (!g_win32_stack_initialized) {
		git_error_set(GIT_ERROR_INVALID, "git_win32_stack not initialized.");
		return GIT_ERROR;
	}

	if (!prefix)
		prefix = "\t";
	if (!suffix)
		suffix = "\n";

	memset(pbuf, 0, buf_len);

	memset(&s, 0, sizeof(s));
	s.symbol.MaxNameLen = MY_MAX_FILENAME;
	s.symbol.SizeOfStruct = sizeof(SYMBOL_INFO);

	memset(&line, 0, sizeof(line));
	line.SizeOfStruct = sizeof(IMAGEHLP_LINE64);

	for (k=0; k < pdata->nr_frames; k++) {
		DWORD64 frame_k = (DWORD64)pdata->frames[k];
		DWORD dwUnused;

		if (SymFromAddr(g_win32_stack_process, frame_k, 0, &s.symbol) &&
			SymGetLineFromAddr64(g_win32_stack_process, frame_k, &dwUnused, &line)) {
			const char *pslash;
			const char *pfile;

			pslash = strrchr(line.FileName, '\\');
			pfile = ((pslash) ? (pslash+1) : line.FileName);
			p_snprintf(detail, sizeof(detail), "%s%s:%d> %s%s",
					   prefix, pfile, line.LineNumber, s.symbol.Name, suffix);
		} else {
			/* This happens when we cross into another module.
			 * For example, in CLAR tests, this is typically
			 * the CRT startup code.  Just print an unknown
			 * frame and continue.
			 */
			p_snprintf(detail, sizeof(detail), "%s??%s", prefix, suffix);
		}
		detail_len = strlen(detail);

		if (buf_len < (buf_used + detail_len + 1)) {
			/* we don't have room for this frame in the buffer, so just stop. */
			break;
		}

		memcpy(&pbuf[buf_used], detail, detail_len);
		buf_used += detail_len;
	}

	/* "aux_id" 0 is reserved to mean no aux data. This is needed to handle
	 * allocs that occur before the aux callbacks were registered.
	 */
	if (pdata->aux_id > 0) {
		p_snprintf(detail, sizeof(detail), "%saux_id: %d%s",
				   prefix, pdata->aux_id, suffix);
		detail_len = strlen(detail);
		if ((buf_used + detail_len + 1) < buf_len) {
			memcpy(&pbuf[buf_used], detail, detail_len);
			buf_used += detail_len;
		}

		/* If an "aux" data provider is still registered, ask it to append its detailed
		 * data to the end of ours using the "aux_id" it gave us when this de-duped
		 * item was created.
		 */
		if (g_aux_cb_lookup)
			(g_aux_cb_lookup)(pdata->aux_id, &pbuf[buf_used], (buf_len - buf_used - 1));
	}

	return GIT_OK;
}

int git_win32_leakcheck_stack(
	char * pbuf, size_t buf_len,
	int skip,
	const char *prefix, const char *suffix)
{
	git_win32_leakcheck_stack_raw_data data;
	int error;

	if ((error = git_win32_leakcheck_stack_capture(&data, skip)) < 0)
		return error;
	if ((error = git_win32_leakcheck_stack_format(pbuf, buf_len, &data, prefix, suffix)) < 0)
		return error;
	return 0;
}

/* Stack tracing */

#define STACKTRACE_UID_LEN (15)

/**
 * The stacktrace of an allocation can be distilled
 * to a unique id based upon the stackframe pointers
 * and ignoring any size arguments. We will use these
 * UIDs as the (char const*) __FILE__ argument we
 * give to the CRT malloc routines.
 */
typedef struct {
	char uid[STACKTRACE_UID_LEN + 1];
} git_win32_leakcheck_stacktrace_uid;

/**
 * All mallocs with the same stacktrace will be de-duped
 * and aggregated into this row.
 */
typedef struct {
	git_win32_leakcheck_stacktrace_uid uid; /* must be first */
	git_win32_leakcheck_stack_raw_data raw_data;
	unsigned int count_allocs; /* times this alloc signature seen since init */
	unsigned int count_allocs_at_last_checkpoint; /* times since last mark */
	unsigned int transient_count_leaks; /* sum of leaks */
} git_win32_leakcheck_stacktrace_row;

static CRITICAL_SECTION g_crtdbg_stacktrace_cs;

/**
 * CRTDBG memory leak tracking takes a "char const * const file_name"
 * and stores the pointer in the heap data (instead of allocing a copy
 * for itself).  Normally, this is not a problem, since we usually pass
 * in __FILE__.  But I'm going to lie to it and pass in the address of
 * the UID in place of the file_name.  Also, I do not want to alloc the
 * stacktrace data (because we are called from inside our alloc routines).
 * Therefore, I'm creating a very large static pool array to store row
 * data. This also eliminates the temptation to realloc it (and move the
 * UID pointers).
 *
 * And to efficiently look for duplicates we need an index on the rows
 * so we can bsearch it.  Again, without mallocing.
 *
 * If we observe more than MY_ROW_LIMIT unique malloc signatures, we
 * fall through and use the traditional __FILE__ processing and don't
 * try to de-dup them.  If your testing hits this limit, just increase
 * it and try again.
 */

#define MY_ROW_LIMIT (2 * 1024 * 1024)
static git_win32_leakcheck_stacktrace_row  g_cs_rows[MY_ROW_LIMIT];
static git_win32_leakcheck_stacktrace_row *g_cs_index[MY_ROW_LIMIT];

static unsigned int g_cs_end = MY_ROW_LIMIT;
static unsigned int g_cs_ins = 0; /* insertion point == unique allocs seen */
static unsigned int g_count_total_allocs = 0; /* number of allocs seen */
static unsigned int g_transient_count_total_leaks = 0; /* number of total leaks */
static unsigned int g_transient_count_dedup_leaks = 0; /* number of unique leaks */
static bool g_limit_reached = false; /* had allocs after we filled row table */

static unsigned int g_checkpoint_id = 0; /* to better label leak checkpoints */
static bool g_transient_leaks_since_mark = false; /* payload for hook */

/**
 * Compare function for bsearch on g_cs_index table.
 */
static int row_cmp(const void *v1, const void *v2)
{
	git_win32_leakcheck_stack_raw_data *d1 = (git_win32_leakcheck_stack_raw_data*)v1;
	git_win32_leakcheck_stacktrace_row *r2 = (git_win32_leakcheck_stacktrace_row *)v2;

	return (git_win32_leakcheck_stack_compare(d1, &r2->raw_data));
}

/**
 * Unique insert the new data into the row and index tables.
 * We have to sort by the stackframe data itself, not the uid.
 */
static git_win32_leakcheck_stacktrace_row * insert_unique(
	const git_win32_leakcheck_stack_raw_data *pdata)
{
	size_t pos;
	if (git__bsearch(g_cs_index, g_cs_ins, pdata, row_cmp, &pos) < 0) {
		/* Append new unique item to row table. */
		memcpy(&g_cs_rows[g_cs_ins].raw_data, pdata, sizeof(*pdata));
		sprintf(g_cs_rows[g_cs_ins].uid.uid, "##%08lx", g_cs_ins);

		/* Insert pointer to it into the proper place in the index table. */
		if (pos < g_cs_ins)
			memmove(&g_cs_index[pos+1], &g_cs_index[pos], (g_cs_ins - pos)*sizeof(g_cs_index[0]));
		g_cs_index[pos] = &g_cs_rows[g_cs_ins];

		g_cs_ins++;
	}

	g_cs_index[pos]->count_allocs++;

	return g_cs_index[pos];
}

/**
 * Hook function to receive leak data from the CRT. (This includes
 * both "<file_name>:(<line_number>)" data, but also each of the
 * various headers and fields.
 *
 * Scan this for the special "##<pos>" UID forms that we substituted
 * for the "<file_name>".  Map <pos> back to the row data and
 * increment its leak count.
 *
 * See https://msdn.microsoft.com/en-us/library/74kabxyx.aspx
 *
 * We suppress the actual crtdbg output.
 */
static int __cdecl report_hook(int nRptType, char *szMsg, int *retVal)
{
	static int hook_result = TRUE; /* FALSE to get stock dump; TRUE to suppress. */
	unsigned int pos;

	*retVal = 0; /* do not invoke debugger */

	if ((szMsg[0] != '#') || (szMsg[1] != '#'))
		return hook_result;

	if (sscanf(&szMsg[2], "%08lx", &pos) < 1)
		return hook_result;
	if (pos >= g_cs_ins)
		return hook_result;

	if (g_transient_leaks_since_mark) {
		if (g_cs_rows[pos].count_allocs == g_cs_rows[pos].count_allocs_at_last_checkpoint)
			return hook_result;
	}

	g_cs_rows[pos].transient_count_leaks++;

	if (g_cs_rows[pos].transient_count_leaks == 1)
		g_transient_count_dedup_leaks++;

	g_transient_count_total_leaks++;

	return hook_result;
}

/**
 * Write leak data to all of the various places we need.
 * We force the caller to sprintf() the message first
 * because we want to avoid fprintf() because it allocs.
 */
static void my_output(const char *buf)
{
	fwrite(buf, strlen(buf), 1, stderr);
	OutputDebugString(buf);
}

/**
 * For each row with leaks, dump a stacktrace for it.
 */
static void dump_summary(const char *label)
{
	unsigned int k;
	char buf[10 * 1024];

	if (g_transient_count_total_leaks == 0)
		return;

	fflush(stdout);
	fflush(stderr);
	my_output("\n");

	if (g_limit_reached) {
		sprintf(buf,
				"LEAK SUMMARY: de-dup row table[%d] filled. Increase MY_ROW_LIMIT.\n",
				MY_ROW_LIMIT);
		my_output(buf);
	}

	if (!label)
		label = "";

	if (g_transient_leaks_since_mark) {
		sprintf(buf, "LEAK CHECKPOINT %d: leaks %d unique %d: %s\n",
				g_checkpoint_id, g_transient_count_total_leaks, g_transient_count_dedup_leaks, label);
		my_output(buf);
	} else {
		sprintf(buf, "LEAK SUMMARY: TOTAL leaks %d de-duped %d: %s\n",
				g_transient_count_total_leaks, g_transient_count_dedup_leaks, label);
		my_output(buf);
	}
	my_output("\n");

	for (k = 0; k < g_cs_ins; k++) {
		if (g_cs_rows[k].transient_count_leaks > 0) {
			sprintf(buf, "LEAK: %s leaked %d of %d times:\n",
					g_cs_rows[k].uid.uid,
					g_cs_rows[k].transient_count_leaks,
					g_cs_rows[k].count_allocs);
			my_output(buf);

			if (git_win32_leakcheck_stack_format(
					buf, sizeof(buf), &g_cs_rows[k].raw_data,
					NULL, NULL) >= 0) {
				my_output(buf);
			}

			my_output("\n");
		}
	}

	fflush(stderr);
}

/**
 * Initialize our memory leak tracking and de-dup data structures.
 * This should ONLY be called by git_libgit2_init().
 */
void git_win32_leakcheck_stacktrace_init(void)
{
	InitializeCriticalSection(&g_crtdbg_stacktrace_cs);

	EnterCriticalSection(&g_crtdbg_stacktrace_cs);

	_CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);

	_CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_DEBUG | _CRTDBG_MODE_FILE);
	_CrtSetReportMode(_CRT_ERROR,  _CRTDBG_MODE_DEBUG | _CRTDBG_MODE_FILE);
	_CrtSetReportMode(_CRT_WARN,   _CRTDBG_MODE_DEBUG | _CRTDBG_MODE_FILE);

	_CrtSetReportFile(_CRT_ASSERT, _CRTDBG_FILE_STDERR);
	_CrtSetReportFile(_CRT_ERROR,  _CRTDBG_FILE_STDERR);
	_CrtSetReportFile(_CRT_WARN,   _CRTDBG_FILE_STDERR);

	LeaveCriticalSection(&g_crtdbg_stacktrace_cs);
}

int git_win32_leakcheck_stacktrace_dump(
	git_win32_leakcheck_stacktrace_options opt,
	const char *label)
{
	_CRT_REPORT_HOOK old;
	unsigned int k;
	int r = 0;

#define IS_BIT_SET(o,b) (((o) & (b)) != 0)

	bool b_set_mark         = IS_BIT_SET(opt, GIT_WIN32_LEAKCHECK_STACKTRACE_SET_MARK);
	bool b_leaks_since_mark = IS_BIT_SET(opt, GIT_WIN32_LEAKCHECK_STACKTRACE_LEAKS_SINCE_MARK);
	bool b_leaks_total      = IS_BIT_SET(opt, GIT_WIN32_LEAKCHECK_STACKTRACE_LEAKS_TOTAL);
	bool b_quiet            = IS_BIT_SET(opt, GIT_WIN32_LEAKCHECK_STACKTRACE_QUIET);

	if (b_leaks_since_mark && b_leaks_total) {
		git_error_set(GIT_ERROR_INVALID, "cannot combine LEAKS_SINCE_MARK and LEAKS_TOTAL.");
		return GIT_ERROR;
	}
	if (!b_set_mark && !b_leaks_since_mark && !b_leaks_total) {
		git_error_set(GIT_ERROR_INVALID, "nothing to do.");
		return GIT_ERROR;
	}

	EnterCriticalSection(&g_crtdbg_stacktrace_cs);

	if (b_leaks_since_mark || b_leaks_total) {
		/* All variables with "transient" in the name are per-dump counters
		 * and reset before each dump.  This lets us handle checkpoints.
		 */
		g_transient_count_total_leaks = 0;
		g_transient_count_dedup_leaks = 0;
		for (k = 0; k < g_cs_ins; k++) {
			g_cs_rows[k].transient_count_leaks = 0;
		}
	}

	g_transient_leaks_since_mark = b_leaks_since_mark;

	old = _CrtSetReportHook(report_hook);
	_CrtDumpMemoryLeaks();
	_CrtSetReportHook(old);

	if (b_leaks_since_mark || b_leaks_total) {
		r = g_transient_count_dedup_leaks;

		if (!b_quiet)
			dump_summary(label);
	}

	if (b_set_mark) {
		for (k = 0; k < g_cs_ins; k++) {
			g_cs_rows[k].count_allocs_at_last_checkpoint = g_cs_rows[k].count_allocs;
		}

		g_checkpoint_id++;
	}

	LeaveCriticalSection(&g_crtdbg_stacktrace_cs);

	return r;
}

/**
 * Shutdown our memory leak tracking and dump summary data.
 * This should ONLY be called by git_libgit2_shutdown().
 *
 * We explicitly call _CrtDumpMemoryLeaks() during here so
 * that we can compute summary data for the leaks. We print
 * the stacktrace of each unique leak.
 *
 * This cleanup does not happen if the app calls exit()
 * without calling the libgit2 shutdown code.
 *
 * This info we print here is independent of any automatic
 * reporting during exit() caused by _CRTDBG_LEAK_CHECK_DF.
 * Set it in your app if you also want traditional reporting.
 */
void git_win32_leakcheck_stacktrace_cleanup(void)
{
	/* At shutdown/cleanup, dump cumulative leak info
	 * with everything since startup.  This might generate
	 * extra noise if the caller has been doing checkpoint
	 * dumps, but it might also eliminate some false
	 * positives for resources previously reported during
	 * checkpoints.
	 */
	git_win32_leakcheck_stacktrace_dump(
		GIT_WIN32_LEAKCHECK_STACKTRACE_LEAKS_TOTAL,
		"CLEANUP");

	DeleteCriticalSection(&g_crtdbg_stacktrace_cs);
}

const char *git_win32_leakcheck_stacktrace(int skip, const char *file)
{
	git_win32_leakcheck_stack_raw_data new_data;
	git_win32_leakcheck_stacktrace_row *row;
	const char * result = file;

	if (git_win32_leakcheck_stack_capture(&new_data, skip+1) < 0)
		return result;

	EnterCriticalSection(&g_crtdbg_stacktrace_cs);

	if (g_cs_ins < g_cs_end) {
		row = insert_unique(&new_data);
		result = row->uid.uid;
	} else {
		g_limit_reached = true;
	}

	g_count_total_allocs++;

	LeaveCriticalSection(&g_crtdbg_stacktrace_cs);

	return result;
}

static void git_win32_leakcheck_global_shutdown(void)
{
	git_win32_leakcheck_stacktrace_cleanup();
	git_win32_leakcheck_stack_cleanup();
}

bool git_win32_leakcheck_has_leaks(void)
{
	return (g_transient_count_total_leaks > 0);
}

int git_win32_leakcheck_global_init(void)
{
	git_win32_leakcheck_stacktrace_init();
	git_win32_leakcheck_stack_init();

	return git_runtime_shutdown_register(git_win32_leakcheck_global_shutdown);
}

#else

int git_win32_leakcheck_global_init(void)
{
	return 0;
}

#endif
