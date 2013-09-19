// Copyright © 2012-2013, Jakob Bornecrantz.  All rights reserved.
// See copyright notice in src/volt/license.d (BOOST ver. 1.0).
module main;

import core.stdc.stdio;
import core.stdc.stdlib;

import watt.process;
import watt.path;

import results;
import list;
import cmd;

int main()
{
	auto cmdGroup = new CmdGroup();
	bool printOk = true;
	bool printImprovments = true;
	bool printFailing = true;
	bool printRegressions = true;
	string compiler = getEnv("VOLT");

	if (compiler is null) {
		printf("compiler envar not set\n".ptr);
		return -1;
	}

	/// @todo Scan for files
	auto tests = testList;

	int total;
	int passed;
	int failed;
	int improved;
	int regressed;

	auto rets = new Result[] (tests.length);
	for (size_t i; i < tests.length; i++) {
		rets[i] = new Result();
		rets[i].runTest(cmdGroup, tests[i], compiler);
	}

	cmdGroup.waitAll();

	for (size_t i; i < tests.length; i++) {
		auto ret = rets[i];
		total++;
		if (ret.ok) {
			passed++;
			improved += cast(int)!ret.hasPassed;

			if (!ret.hasPassed && printImprovments) {
				printf("%s: %s, improved!\n".ptr, ret.test.ptr, ret.msg.ptr);
			} else if (printOk) {
				printf("%s: %s\n".ptr, ret.test.ptr, ret.msg.ptr);
			}
		} else {
			failed++;
			regressed += cast(int)ret.hasPassed;


			if (ret.hasPassed && printRegressions) {
				printf("%s: %s, regressed!\n".ptr, ret.test.ptr, ret.msg.ptr);
			} else if (printFailing) {
				printf("%s: %s\n".ptr, ret.test.ptr, ret.msg.ptr);
			}
		}
		fflush(stdout);
	}

	auto xml = fopen("results.xml".ptr, "w+".ptr);
	if (xml !is null) {
		fprintf(xml, "<testsuites errors=\"%u\" failures=\"%u\" tests=\"%u\">\n".ptr,
				regressed, failed - regressed, total);
		for (size_t i; i < rets.length; i++) {
			rets[i].xmlLog(xml);
		}
		fprintf(xml, "</testsuites>\n".ptr);
		fflush(xml);
		fclose(xml);
		xml = null;
	}

	auto rate = cast(float)passed / cast(float)total * 100.f;
	printf("Summary: %i tests, %i pass%s, %i failure%s, %.2f%% pass rate, %i regressions, %i improvements.\n".ptr,
	       total,
	       passed, (passed == 1 ? "".ptr : "es".ptr),
	       failed, (failed == 1 ? "".ptr : "s".ptr),
	       cast(double)rate, regressed, improved);

	return regressed ? -1 : 0;
}
