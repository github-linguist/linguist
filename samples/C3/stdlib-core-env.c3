// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/env.c3
// Permalink: https://github.com/c3lang/c3c/blob/1b54a99/lib/std/core/env.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Copyright (c) 2021-2024 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::env;

enum CompilerOptLevel
{
	O0,
	O1,
	O2,
	O3
}

enum MemoryEnvironment
{
	NORMAL,
	SMALL,
	TINY,
	NONE
}

enum OsType
{
	UNKNOWN,
	NONE,
	ANANAS,
	CLOUD_ABI,
	DRAGON_FLY,
	FREEBSD,
	FUCHSIA,
	IOS,
	KFREEBSD,
	LINUX,
	PS3,
	MACOS,
	NETBSD,
	OPENBSD,
	SOLARIS,
	WIN32,
	HAIKU,
	MINIX,
	RTEMS,
	NACL,       // Native Client
	CNK,        // BG/P Compute-Node Kernel
	AIX,
	CUDA,
	NVOPENCL,
	AMDHSA,
	PS4,
	ELFIAMCU,
	TVOS,
	WATCHOS,
	MESA3D,
	CONTIKI,
	AMDPAL,
	HERMITCORE,
	HURD,
	WASI,
	EMSCRIPTEN,
}

enum ArchType
{
	UNKNOWN,
	ARM,          // ARM (little endian): arm, armv.*, xscale
	ARMB,         // ARM (big endian): armeb
	AARCH64,      // AArch64 (little endian): aarch64
	AARCH64_BE,   // AArch64 (big endian): aarch64_be
	AARCH64_32,   // AArch64 (little endian) ILP32: aarch64_32
	ARC,          // ARC: Synopsys ARC
	AVR,          // AVR: Atmel AVR microcontroller
	BPFEL,        // eBPF or extended BPF or 64-bit BPF (little endian)
	BPFEB,        // eBPF or extended BPF or 64-bit BPF (big endian)
	HEXAGON,      // Hexagon: hexagon
	MIPS,         // MIPS: mips, mipsallegrex, mipsr6
	MIPSEL,       // MIPSEL: mipsel, mipsallegrexe, mipsr6el
	MIPS64,       // MIPS64: mips64, mips64r6, mipsn32, mipsn32r6
	MIPS64EL,     // MIPS64EL: mips64el, mips64r6el, mipsn32el, mipsn32r6el
	MSP430,       // MSP430: msp430
	PPC,          // PPC: powerpc
	PPC64,        // PPC64: powerpc64, ppu
	PPC64LE,      // PPC64LE: powerpc64le
	R600,         // R600: AMD GPUs HD2XXX - HD6XXX
	AMDGCN,       // AMDGCN: AMD GCN GPUs
	RISCV32,      // RISC-V (32-bit): riscv32
	RISCV64,      // RISC-V (64-bit): riscv64
	SPARC,        // Sparc: sparc
	SPARCV9,      // Sparcv9: Sparcv9
	SPARCEL,      // Sparc: (endianness = little). NB: 'Sparcle' is a CPU variant
	SYSTEMZ,      // SystemZ: s390x
	TCE,          // TCE (http://tce.cs.tut.fi/): tce
	TCELE,        // TCE little endian (http://tce.cs.tut.fi/): tcele
	THUMB,        // Thumb (little endian): thumb, thumbv.*
	THUMBEB,      // Thumb (big endian): thumbeb
	X86,          // X86: i[3-9]86
	X86_64,       // X86-64: amd64, x86_64
	XCORE,        // XCore: xcore
	NVPTX,        // NVPTX: 32-bit
	NVPTX64,      // NVPTX: 64-bit
	LE32,         // le32: generic little-endian 32-bit CPU (PNaCl)
	LE64,         // le64: generic little-endian 64-bit CPU (PNaCl)
	AMDIL,        // AMDIL
	AMDIL64,      // AMDIL with 64-bit pointers
	HSAIL,        // AMD HSAIL
	HSAIL64,      // AMD HSAIL with 64-bit pointers
	SPIR,         // SPIR: standard portable IR for OpenCL 32-bit version
	SPIR64,       // SPIR: standard portable IR for OpenCL 64-bit version
	KALIMBA,      // Kalimba: generic kalimba
	SHAVE,        // SHAVE: Movidius vector VLIW processors
	LANAI,        // Lanai: Lanai 32-bit
	WASM32,       // WebAssembly with 32-bit pointers
	WASM64,       // WebAssembly with 64-bit pointers
	RSCRIPT32,    // 32-bit RenderScript
	RSCRIPT64,    // 64-bit RenderScript
	XTENSA,       // Xtensa
}

const OsType OS_TYPE = (OsType)$$OS_TYPE;
const ArchType ARCH_TYPE = (ArchType)$$ARCH_TYPE;
const bool ARCH_32_BIT = $$REGISTER_SIZE == 32;
const bool ARCH_64_BIT = $$REGISTER_SIZE == 64;
const bool LIBC = $$COMPILER_LIBC_AVAILABLE;
const bool NO_LIBC = !$$COMPILER_LIBC_AVAILABLE;
const CompilerOptLevel COMPILER_OPT_LEVEL = (CompilerOptLevel)$$COMPILER_OPT_LEVEL;
const bool BIG_ENDIAN = $$PLATFORM_BIG_ENDIAN;
const bool I128_NATIVE_SUPPORT = $$PLATFORM_I128_SUPPORTED;
const bool F16_SUPPORT = $$PLATFORM_F16_SUPPORTED;
const bool F128_SUPPORT = $$PLATFORM_F128_SUPPORTED;
const REGISTER_SIZE = $$REGISTER_SIZE;
const bool COMPILER_SAFE_MODE = $$COMPILER_SAFE_MODE;
const bool DEBUG_SYMBOLS = $$DEBUG_SYMBOLS;
const bool BACKTRACE = $$BACKTRACE;
const usz LLVM_VERSION = $$LLVM_VERSION;
const bool BENCHMARKING = $$BENCHMARKING;
const bool TESTING = $$TESTING;
const MemoryEnvironment MEMORY_ENV = (MemoryEnvironment)$$MEMORY_ENVIRONMENT;
const bool TRACK_MEMORY = DEBUG_SYMBOLS && (COMPILER_SAFE_MODE || TESTING);
const bool X86_64 = ARCH_TYPE == X86_64;
const bool X86 = ARCH_TYPE == X86;
const bool AARCH64 = ARCH_TYPE == AARCH64;
const bool NATIVE_STACKTRACE = LINUX || DARWIN || WIN32;
const bool LINUX = LIBC && OS_TYPE == LINUX;
const bool DARWIN = LIBC && os_is_darwin();
const bool WIN32 = LIBC && OS_TYPE == WIN32;
const bool POSIX = LIBC && os_is_posix();
const bool OPENBSD = LIBC && OS_TYPE == OPENBSD;
const bool FREEBSD = LIBC && OS_TYPE == FREEBSD;
const bool NETBSD = LIBC && OS_TYPE == NETBSD;
const bool BSD_FAMILY = env::FREEBSD || env::OPENBSD || env::NETBSD;
const bool WASI = LIBC && OS_TYPE == WASI;
const bool WASM_NOLIBC @builtin = !LIBC && ARCH_TYPE == ArchType.WASM32 || ARCH_TYPE == ArchType.WASM64;
const bool ADDRESS_SANITIZER = $$ADDRESS_SANITIZER;
const bool MEMORY_SANITIZER = $$MEMORY_SANITIZER;
const bool THREAD_SANITIZER = $$THREAD_SANITIZER;
const bool ANY_SANITIZER = ADDRESS_SANITIZER || MEMORY_SANITIZER || THREAD_SANITIZER;

macro bool os_is_darwin() @const
{
	$switch (OS_TYPE)
	$case IOS:
	$case MACOS:
	$case TVOS:
	$case WATCHOS:
		return true;
	$default:
		return false;
	$endswitch
}

macro bool os_is_posix() @const
{
	$switch (OS_TYPE)
	$case IOS:
	$case MACOS:
	$case NETBSD:
	$case LINUX:
	$case KFREEBSD:
	$case FREEBSD:
	$case OPENBSD:
	$case SOLARIS:
	$case TVOS:
	$case WATCHOS:
		return true;
	$case WIN32:
	$case WASI:
	$case EMSCRIPTEN:
		return false;
	$default:
		$echo("Assuming non-Posix environment");
		return false;
	$endswitch
}

const BUILTIN_EXPECT_IS_DISABLED = $feature(DISABLE_BUILTIN_EXPECT);
const BUILTIN_PREFETCH_IS_DISABLED = $feature(DISABLE_BUILTIN_PREFETCH);
