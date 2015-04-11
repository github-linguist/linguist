/*
 *  mpq.d -- D programming language module for libmpq
 *
 *  Copyright (c) 2008 Georg Lukas <georg@op-co.de>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 *  This module is written to support Phobos. Patches to allow binding to
 *  Tango are welcome.
 */

module mpq;

/* the following pragma does not work on DMD/Linux, generates a warning on
 * GDC/Linux and has not been tested on Windows. Commented out for now. */
// pragma(lib, "libmpq");

import std.string; // for format() and toStringz()
import std.traits; // for ParameterTypeTuple!()

/* XXX: this assumes that libmpq is compiled with Large File Support on */
alias long off_t;

/* libmpq error return values */
const LIBMPQ_ERROR_OPEN			= -1;	/* open error on file. */
const LIBMPQ_ERROR_CLOSE		= -2;	/* close error on file. */
const LIBMPQ_ERROR_SEEK			= -3;	/* lseek error on file. */
const LIBMPQ_ERROR_READ			= -4;	/* read error on file. */
const LIBMPQ_ERROR_WRITE		= -5;	/* write error on file. */
const LIBMPQ_ERROR_MALLOC		= -6;	/* memory allocation error. */
const LIBMPQ_ERROR_FORMAT		= -7;	/* format errror. */
const LIBMPQ_ERROR_NOT_INITIALIZED	= -8;	/* init() wasn't called. */
const LIBMPQ_ERROR_SIZE			= -9;	/* buffer size is to small. */
const LIBMPQ_ERROR_EXIST		= -10;	/* file or block does not exist in archive. */
const LIBMPQ_ERROR_DECRYPT		= -11;	/* we don't know the decryption seed. */
const LIBMPQ_ERROR_UNPACK		= -12;	/* error on unpacking file. */

/** libmpq internal meta-data for an archive */
extern struct mpq_archive_s;

extern(C) {

/* libmpq__generic information about library. */
char *libmpq__version();

/* libmpq__generic mpq archive information. */
int libmpq__archive_open(mpq_archive_s **mpq_archive, char *mpq_filename, off_t archive_offset);
int libmpq__archive_close(mpq_archive_s *mpq_archive);
int libmpq__archive_packed_size(mpq_archive_s *mpq_archive, off_t *packed_size);
int libmpq__archive_unpacked_size(mpq_archive_s *mpq_archive, off_t *unpacked_size);
int libmpq__archive_offset(mpq_archive_s *mpq_archive, off_t *offset);
int libmpq__archive_version(mpq_archive_s *mpq_archive, uint *version_);
int libmpq__archive_files(mpq_archive_s *mpq_archive, uint *files);

/* libmpq__generic file processing functions. */
int libmpq__file_packed_size(mpq_archive_s *mpq_archive, uint file_number, off_t *packed_size);
int libmpq__file_unpacked_size(mpq_archive_s *mpq_archive, uint file_number, off_t *unpacked_size);
int libmpq__file_offset(mpq_archive_s *mpq_archive, uint file_number, off_t *offset);
int libmpq__file_blocks(mpq_archive_s *mpq_archive, uint file_number, uint *blocks);
int libmpq__file_encrypted(mpq_archive_s *mpq_archive, uint file_number, uint *encrypted);
int libmpq__file_compressed(mpq_archive_s *mpq_archive, uint file_number, uint *compressed);
int libmpq__file_imploded(mpq_archive_s *mpq_archive, uint file_number, uint *imploded);
int libmpq__file_number(mpq_archive_s *mpq_archive, char *filename, uint *number);
int libmpq__file_read(mpq_archive_s *mpq_archive, uint file_number, ubyte *out_buf, off_t out_size, off_t *transferred);

/* libmpq__generic block processing functions. */
int libmpq__block_open_offset(mpq_archive_s *mpq_archive, uint file_number);
int libmpq__block_close_offset(mpq_archive_s *mpq_archive, uint file_number);
int libmpq__block_unpacked_size(mpq_archive_s *mpq_archive, uint file_number, uint block_number, off_t *unpacked_size);
int libmpq__block_read(mpq_archive_s *mpq_archive, uint file_number, uint block_number, ubyte *out_buf, off_t out_size, off_t *transferred);

}


/** exception class for failed libmpq calls */
class MPQException : Exception {
	const string[] Errors = [
		"unknown error",
		"open error on file",
		"close error on file",
		"lseek error on file",
		"read error on file",
		"write error on file",
		"memory allocation error",
		"format errror",
		"init() wasn't called",
		"buffer size is to small",
		"file or block does not exist in archive",
		"we don't know the decryption seed",
		"error on unpacking file"];

	public int errno;
	this(char[] fnname = "unknown_function", int errno = 0) {

		this.errno = errno;
		if (-errno >= Errors.length)
			errno = 0;
		super(std.string.format("Error in %s(): %s (%d)",
					fnname, Errors[-errno], errno));
	}
}


/** template to wrap function calls and throw exceptions in case of error
 *
 * thanks for the idea to while(nan) blog,
 * http://while-nan.blogspot.com/2007/06/wrapping-functions-for-fun-and-profit.html
 *
 * use: MPQ_CHECKERR(libmpq__archive_open)(&m, "foo.mpq", -1);
 *   returns the retval of archive_open on success;
 *   throws an MPQException on failure.
 *
 * @param Fn libmpq__function reference
 * @param args libmpq__function parameters
 * @return return value of libmpq__function on success
 * @throw MPQException on error
 */
int MPQ_CHECKERR(alias Fn)(ParameterTypeTuple!(Fn) args)
{
	int result = Fn(args);
	if (result < 0) {
		/* XXX: relying on non-specified stringof() behaviour */
		throw new MPQException((&Fn).stringof[2..$], result);
	}
        return result;
}


/** mixin alias to wrap library functions into MPQ_CHECKERR.
 *
 * alias mpq.func_name(...) to MPQ_CHECKERR(libmpq__func_name)(...)
 * @param func_name name of the function to be wrapped
 */
template MPQ_FUNC(char[] func_name) {
	const char[] MPQ_FUNC = "alias MPQ_CHECKERR!(libmpq__" ~ func_name ~ ") " ~ func_name ~ ";";
}

alias libmpq__version libversion; /* must be direct alias because it returns char*, not error int */
mixin(MPQ_FUNC!("archive_open"));
mixin(MPQ_FUNC!("archive_close"));
mixin(MPQ_FUNC!("archive_packed_size"));
mixin(MPQ_FUNC!("archive_unpacked_size"));
mixin(MPQ_FUNC!("archive_offset"));
mixin(MPQ_FUNC!("archive_version"));
mixin(MPQ_FUNC!("archive_files"));
mixin(MPQ_FUNC!("file_packed_size"));
mixin(MPQ_FUNC!("file_unpacked_size"));
mixin(MPQ_FUNC!("file_offset"));
mixin(MPQ_FUNC!("file_blocks"));
mixin(MPQ_FUNC!("file_encrypted"));
mixin(MPQ_FUNC!("file_compressed"));
mixin(MPQ_FUNC!("file_imploded"));
mixin(MPQ_FUNC!("file_number"));
mixin(MPQ_FUNC!("file_read"));
mixin(MPQ_FUNC!("block_open_offset"));
mixin(MPQ_FUNC!("block_close_offset"));
mixin(MPQ_FUNC!("block_unpacked_size"));
mixin(MPQ_FUNC!("block_read"));

/** getter function named name for returning archive_* single values:
 *
 *   <type> Archive.<name>() { return libmpq__archive_<name>() }
 *
 * @param type return type for the original function reference
 * @param name name of the original function
 * @param name2 name for the prototype (defaults to name, used for "version")
 * @return getter function mixin
 */
template MPQ_A_GET(char[] type, char[] name, char[] name2 = name) {
	const char[] MPQ_A_GET = type ~ " " ~ name2 ~ "() { " ~
			type ~ " ret; " ~
			"archive_" ~ name ~ "(m, &ret); return ret;" ~
		"}";
}

/** wrapper class for an MPQ Archive
 *
 * syntax: auto a = new mpq.Archive("somefile.mpq");
 */
class Archive {
	mpq_archive_s *m;
	File listfile;
	char[][] listfiledata;

	this(char[] archivename, off_t offset = -1) {
		archive_open(&m, toStringz(archivename), offset);
	}

	mixin(MPQ_A_GET!("off_t", "packed_size"));
	mixin(MPQ_A_GET!("off_t", "unpacked_size"));
	mixin(MPQ_A_GET!("off_t", "offset"));
	mixin(MPQ_A_GET!("uint", "version", "version_"));
	mixin(MPQ_A_GET!("uint", "files"));

	~this() {
		archive_close(m);
	}

	mpq_archive_s* archive() {
		return m;
	}

	File opIndex(char[] fname) {
		return new File(this, fname);
	}
	File opIndex(int fno) {
		return new File(this, fno);
	}

	char[][] filelist() {
		try {
			if (!listfile) {
				listfile = this["(listfile)"];
				listfiledata = (cast(char[])listfile.read()).splitlines();
			}
			return listfiledata;
		} catch (MPQException e) {
			return [];
		}
	}

	/+uint filenumber(char[] filename) {
		try {
			if (!listfile) {
				listfile = this["(listfile)"];
				listfiledata = (cast(char[])listfile.read()).splitlines();
			}
			return listfiledata;
		} catch (MPQException e) {
			return [];
		}
	}+/

}


/** getter function named name for returning file_* single values:
 *
 *   <type> File.<name>() { return libmpq__file_<name>() }
 *
 * @param type return type for the original function reference
 * @param name name of the original function
 * @param name2 name for the prototype (defaults to name, used for "version")
 * @return getter function mixin
 */
template MPQ_F_GET(char[] type, char[] name, char[] name2 = name) {
	const char[] MPQ_F_GET = type ~ " " ~ name2 ~ "() { " ~
			type ~ " ret; " ~
			"file_" ~ name ~ "(am, fileno, &ret); " ~
			"return ret;" ~
		"}";
}

/** wrapper class for a single file in an MPQ Archive
 *
 * syntax:
 *    auto a = new mpq.Archive("somefile.mpq");
 *    auto f = a["(listfile)"];
 *    auto f2 = a[0];
 *    auto f3 = new File(a, "(listfile)");
 */
class File {
	Archive a;
	mpq_archive_s* am;
	char[] filename;
	uint fileno;

	this(Archive a, int fileno) {
		this.a = a;
		this.am = a.archive();
		if (fileno >= a.files) {
			throw new MPQException(format("File(%d)", fileno),
				LIBMPQ_ERROR_EXIST);
		}
		this.filename = format("file%04d.xxx", fileno);
		this.fileno = fileno;
	}

	this(Archive a, char[] filename) {
		this.a = a;
		this.am = a.archive();
		this.filename = filename;
		/* this line will throw an exception when the file is not there */
		mpq.file_number(am, toStringz(filename), &this.fileno);
	}

	mixin(MPQ_F_GET!("off_t", "packed_size"));
	mixin(MPQ_F_GET!("off_t", "unpacked_size"));
	mixin(MPQ_F_GET!("off_t", "offset"));
	mixin(MPQ_F_GET!("uint", "blocks"));
	mixin(MPQ_F_GET!("uint", "encrypted"));
	mixin(MPQ_F_GET!("uint", "compressed"));
	mixin(MPQ_F_GET!("uint", "imploded"));

	uint no() {	return fileno; }
	char[] name() {	return filename; }

	ubyte[] read() {
		ubyte[] content;
		content.length = this.unpacked_size();
		off_t trans;
		mpq.file_read(am, fileno, content.ptr, content.length, &trans);
		content.length = trans;
		return content;
	}
}
