// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/runtime.c3
// Permalink: https://github.com/c3lang/c3c/blob/e293c43/lib/std/core/runtime.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::runtime;
import libc, std::time, std::io, std::sort;

struct AnyRaw
{
	void* ptr;
	typeid type;
}

struct SliceRaw
{
	void* ptr;
	usz len;
}

def BenchmarkFn = fn void!();

struct BenchmarkUnit
{
	String name;
	BenchmarkFn func;
}

fn BenchmarkUnit[] benchmark_collection_create(Allocator allocator = allocator::heap())
{
	BenchmarkFn[] fns = $$BENCHMARK_FNS;
	String[] names = $$BENCHMARK_NAMES;
	BenchmarkUnit[] benchmarks = allocator::alloc_array(allocator, BenchmarkUnit, names.len);
	foreach (i, benchmark : fns)
	{
		benchmarks[i] = { names[i], fns[i] };
	}
	return benchmarks;
}

const DEFAULT_BENCHMARK_WARMUP_ITERATIONS = 3;
const DEFAULT_BENCHMARK_MAX_ITERATIONS = 10000;

uint benchmark_warmup_iterations @private = DEFAULT_BENCHMARK_WARMUP_ITERATIONS;
uint benchmark_max_iterations @private = DEFAULT_BENCHMARK_MAX_ITERATIONS;

fn void set_benchmark_warmup_iterations(uint value) @builtin
{
    benchmark_warmup_iterations = value;
}

fn void set_benchmark_max_iterations(uint value) @builtin
{
    assert(value > 0);
    benchmark_max_iterations = value;
}

fn bool run_benchmarks(BenchmarkUnit[] benchmarks)
{
	int benchmarks_passed = 0;
	int benchmark_count = benchmarks.len;
	usz max_name;

	foreach (&unit : benchmarks)
	{
		if (max_name < unit.name.len) max_name = unit.name.len;
	}

	usz len = max_name + 9;

	DString name = dstring::temp_with_capacity(64);
	name.append_repeat('-', len / 2);
	name.append(" BENCHMARKS ");
	name.append_repeat('-', len - len / 2);

	io::printn(name);

	name.clear();

	long sys_clock_started;
	long sys_clock_finished;
	long sys_clocks;
	Clock clock;
	anyfault err;

	foreach(unit : benchmarks)
	{
		defer name.clear();
		name.appendf("Benchmarking %s ", unit.name);
		name.append_repeat('.', max_name - unit.name.len + 2);
		io::printf("%s ", name.str_view());

		for (uint i = 0; i < benchmark_warmup_iterations; i++)
		{
			err = @catch(unit.func()) @inline;
			@volatile_load(err);
		}

		clock = std::time::clock::now();
		sys_clock_started = $$sysclock();

		for (uint i = 0; i < benchmark_max_iterations; i++)
		{
			err = @catch(unit.func()) @inline;
			@volatile_load(err);
		}

		sys_clock_finished = $$sysclock();
		NanoDuration nano_seconds = clock.mark();
		sys_clocks = sys_clock_finished - sys_clock_started;

		if (err)
		{
			io::printfn("[failed] Failed due to: %s", err);
			continue;
		}

		io::printfn("[ok] %.2f ns, %.2f CPU's clocks", (float)nano_seconds / benchmark_max_iterations, (float)sys_clocks / benchmark_max_iterations);
		benchmarks_passed++;
	}

	io::printfn("\n%d benchmark%s run.\n", benchmark_count, benchmark_count > 1 ? "s" : "");
	io::printfn("Benchmarks Result: %s. %d passed, %d failed.",
		benchmarks_passed < benchmark_count ? "FAILED" : "ok",
		benchmarks_passed,
		benchmark_count - benchmarks_passed);

	return benchmark_count == benchmarks_passed;
}

fn bool default_benchmark_runner()
{
	@pool()
	{
		return run_benchmarks(benchmark_collection_create(allocator::temp()));
	};
}

def TestFn = fn void!();

struct TestUnit
{
	String name;
	TestFn func;
}

fn TestUnit[] test_collection_create(Allocator allocator = allocator::heap())
{
	TestFn[] fns = $$TEST_FNS;
	String[] names = $$TEST_NAMES;
	TestUnit[] tests = allocator::alloc_array(allocator, TestUnit, names.len);
	foreach (i, test : fns)
	{
		tests[i] = { names[i], fns[i] };
	}
	return tests;
}

struct TestContext
{
	JmpBuf buf;
}

// Sort the tests by their name in ascending order.
fn int cmp_test_unit(TestUnit a, TestUnit b)
{
	usz an = a.name.len;
	usz bn = b.name.len;
	if (an > bn) @swap(a, b);
	foreach (i, ac : a.name)
	{
		char bc = b.name[i];
		if (ac != bc) return an > bn ? bc - ac : ac - bc;
	}
	return (int)(an - bn);
}

TestContext* test_context @private;

fn void test_panic(String message, String file, String function, uint line)
{
	io::printn("[error]");
	io::print("\n Error: ");
	io::print(message);
	io::printn();
	io::printfn("  - in %s %s:%s.\n", function, file, line);
	libc::longjmp(&test_context.buf, 1);
}

fn bool run_tests(TestUnit[] tests)
{
	usz max_name;
	foreach (&unit : tests)
	{
		if (max_name < unit.name.len) max_name = unit.name.len;
	}
	quicksort(tests, &cmp_test_unit);

	TestContext context;
	test_context = &context;

	PanicFn old_panic = builtin::panic;
	defer builtin::panic = old_panic;
	builtin::panic = &test_panic;
	int tests_passed = 0;
	int test_count = tests.len;
	DString name = dstring::temp_with_capacity(64);
	usz len = max_name + 9;
	name.append_repeat('-', len / 2);
	name.append(" TESTS ");
	name.append_repeat('-', len - len / 2);
	io::printn(name);
	name.clear();
	foreach(unit : tests)
	{
		defer name.clear();
		name.appendf("Testing %s ", unit.name);
		name.append_repeat('.', max_name - unit.name.len + 2);
		io::printf("%s ", name.str_view());
		if (libc::setjmp(&context.buf) == 0)
		{
			if (catch err = unit.func())
			{
				io::printfn("[failed] Failed due to: %s", err);
				continue;
			}
			io::printn("[ok]");
			tests_passed++;
		}
	}
	io::printfn("\n%d test%s run.\n", test_count, test_count > 1 ? "s" : "");
	io::printfn("Test Result: %s. %d passed, %d failed.",
		tests_passed < test_count ? "FAILED" : "ok", tests_passed, test_count - tests_passed);
	return test_count == tests_passed;
}

fn bool default_test_runner()
{
	@pool()
	{
		return run_tests(test_collection_create(allocator::temp()));
	};
}

module std::core::runtime @if(WASM_NOLIBC);

extern fn void __wasm_call_ctors();
fn void wasm_initialize() @extern("_initialize") @wasm
{
	// The linker synthesizes this to call constructors.
	__wasm_call_ctors();
}