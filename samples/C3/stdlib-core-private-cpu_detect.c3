// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/private/cpu_detect.c3
// Permalink: https://github.com/c3lang/c3c/blob/748c737/lib/std/core/private/cpu_detect.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::core::cpudetect @if(env::X86 || env::X86_64);

struct CpuId
{
	uint eax, ebx, ecx, edx;
}
fn CpuId x86_cpuid(uint eax, uint ecx = 0)
{
	int edx;
	int ebx;
	asm
	{
		movl $eax, eax;
		movl $ecx, ecx;
		cpuid;
		movl eax, $eax;
		movl ebx, $ebx;
		movl ecx, $ecx;
		movl edx, $edx;
	}
	return { eax, ebx, ecx, edx };
}

enum X86Feature
{
	ADX,
	AES,
	AMX_BF16,
	AMX_COMPLEX,
	AMX_FP16,
	AMX_INT8,
	AMX_TILE,
	AVX,
	AVX10_1_256,
	AVX10_1_512,
	AVX2,
	AVX5124FMAPS,
	AVX5124VNNIW,
	AVX512BF16,
	AVX512BITALG,
	AVX512BW,
	AVX512CD,
	AVX512DQ,
	AVX512ER,
	AVX512F,
	AVX512FP16,
	AVX512IFMA,
	AVX512PF,
	AVX512VBMI,
	AVX512VBMI2,
	AVX512VL,
	AVX512VNNI,
	AVX512VP2INTERSECT,
	AVX512VPOPCNTDQ,
	AVXIFMA,
	AVXNECONVERT,
	AVXVNNI,
	AVXVNNIINT16,
	AVXVNNIINT8,
	BMI,
	BMI2,
	CLDEMOTE,
	CLFLUSHOPT,
	CLWB,
	CLZERO,
	CMOV,
	CMPCCXADD,
	CMPXCHG16B,
	CX8,
	ENQCMD,
	F16C,
	FMA,
	FMA4,
	FSGSBASE,
	FXSR,
	GFNI,
	HRESET,
	INVPCID,
	KL,
	LWP,
	LZCNT,
	MMX,
	MOVBE,
	MOVDIR64B,
	MOVDIRI,
	MWAITX,
	PCLMUL,
	PCONFIG,
	PKU,
	POPCNT,
	PREFETCHI,
	PREFETCHWT1,
	PRFCHW,
	PTWRITE,
	RAOINT,
	RDPID,
	RDPRU,
	RDRND,
	RDSEED,
	RTM,
	SAHF,
	SERIALIZE,
	SGX,
	SHA,
	SHA512,
	SHSTK,
	SM3,
	SM4,
	SSE,
	SSE2,
	SSE3,
	SSE4_1,
	SSE4_2,
	SSE4_A,
	SSSE3,
	TBM,
	TSXLDTRK,
	UINTR,
	USERMSR,
	VAES,
	VPCLMULQDQ,
	WAITPKG,
	WBNOINVD,
	WIDEKL,
	X87,
	XOP,
	XSAVE,
	XSAVEC,
	XSAVEOPT,
	XSAVES,
}

uint128 x86_features;

fn void add_feature_if_bit(X86Feature feature, uint register, int bit)
{
	if (register & 1U << bit) x86_features |= 1u128 << feature.ordinal;
}

fn void x86_initialize_cpu_features()
{
	uint max_level = x86_cpuid(0).eax;
	CpuId feat = x86_cpuid(1);
	CpuId leaf7 = max_level >= 8 ? x86_cpuid(7) : CpuId {};
	CpuId leaf7s1 = leaf7.eax >= 1 ? x86_cpuid(7, 1) : CpuId {};
	CpuId ext1 = x86_cpuid(0x80000000).eax >= 0x80000001 ? x86_cpuid(0x80000001) : CpuId {};
	CpuId ext8 = x86_cpuid(0x80000000).eax >= 0x80000008 ? x86_cpuid(0x80000008) : CpuId {};
	CpuId leaf_d = max_level >= 0xd ? x86_cpuid(0xd, 0x1) : CpuId {};
	CpuId leaf_14 = max_level >= 0x14 ? x86_cpuid(0x14) : CpuId {};
	CpuId leaf_19 = max_level >= 0x19 ? x86_cpuid(0x19) : CpuId {};
	CpuId leaf_24 = max_level >= 0x24 ? x86_cpuid(0x24) : CpuId {};
	add_feature_if_bit(ADX, leaf7.ebx, 19);
	add_feature_if_bit(AES, feat.ecx, 25);
	add_feature_if_bit(AMX_BF16, leaf7.edx, 22);
	add_feature_if_bit(AMX_COMPLEX, leaf7s1.edx, 8);
	add_feature_if_bit(AMX_FP16, leaf7s1.eax, 21);
	add_feature_if_bit(AMX_INT8, leaf7.edx, 25);
	add_feature_if_bit(AMX_TILE, leaf7.edx, 24);
	add_feature_if_bit(AVX, feat.ecx, 28);
	add_feature_if_bit(AVX10_1_256, leaf7s1.edx, 19);
	add_feature_if_bit(AVX10_1_512, leaf_24.ebx, 18);
	add_feature_if_bit(AVX2, leaf7.ebx, 5);
	add_feature_if_bit(AVX5124FMAPS, leaf7.edx, 3);
	add_feature_if_bit(AVX5124VNNIW, leaf7.edx, 2);
	add_feature_if_bit(AVX512BF16, leaf7s1.eax, 5);
	add_feature_if_bit(AVX512BITALG, leaf7.ecx, 12);
	add_feature_if_bit(AVX512BW, leaf7.ebx, 30);
	add_feature_if_bit(AVX512CD, leaf7.ebx, 28);
	add_feature_if_bit(AVX512DQ, leaf7.ebx, 17);
	add_feature_if_bit(AVX512ER, leaf7.ebx, 27);
	add_feature_if_bit(AVX512F, leaf7.ebx, 16);
	add_feature_if_bit(AVX512FP16, leaf7.edx, 23);
	add_feature_if_bit(AVX512IFMA, leaf7.ebx, 21);
	add_feature_if_bit(AVX512PF, leaf7.ebx, 26);
	add_feature_if_bit(AVX512VBMI, leaf7.ecx, 1);
	add_feature_if_bit(AVX512VBMI2, leaf7.ecx, 6);
	add_feature_if_bit(AVX512VL, leaf7.ebx, 31);
	add_feature_if_bit(AVX512VNNI, leaf7.ecx, 11);
	add_feature_if_bit(AVX512VP2INTERSECT, leaf7.edx, 8);
	add_feature_if_bit(AVX512VPOPCNTDQ, leaf7.ecx, 14);
	add_feature_if_bit(AVXIFMA, leaf7s1.eax, 23);
	add_feature_if_bit(AVXNECONVERT, leaf7s1.edx, 5);
	add_feature_if_bit(AVXVNNI, leaf7s1.eax, 4);
	add_feature_if_bit(AVXVNNIINT16, leaf7s1.edx, 10);
	add_feature_if_bit(AVXVNNIINT8, leaf7s1.edx, 4);
	add_feature_if_bit(BMI, leaf7.ebx, 3);
	add_feature_if_bit(BMI2, leaf7.ebx, 8);
	add_feature_if_bit(CLDEMOTE, leaf7.ecx, 25);
	add_feature_if_bit(CLFLUSHOPT, leaf7.ebx, 23);
	add_feature_if_bit(CLWB, leaf7.ebx, 24);
	add_feature_if_bit(CLZERO, ext8.ecx, 0);
	add_feature_if_bit(CMOV, feat.edx, 15);
	add_feature_if_bit(CMPCCXADD, leaf7s1.eax, 7);
	add_feature_if_bit(CMPXCHG16B, feat.ecx, 12);
	add_feature_if_bit(CX8, feat.edx, 8);
	add_feature_if_bit(ENQCMD, leaf7.ecx, 29);
	add_feature_if_bit(F16C, feat.ecx, 29);
	add_feature_if_bit(FMA, feat.ecx, 12);
	add_feature_if_bit(FMA4, ext1.ecx, 16);
	add_feature_if_bit(FSGSBASE, leaf7.ebx, 0);
	add_feature_if_bit(FXSR, feat.edx, 24);
	add_feature_if_bit(GFNI, leaf7.ecx, 8);
	add_feature_if_bit(HRESET, leaf7s1.eax, 22);
	add_feature_if_bit(INVPCID, leaf7.ebx, 10);
	add_feature_if_bit(KL, leaf7.ecx, 23);
	add_feature_if_bit(LWP, ext1.ecx, 15);
	add_feature_if_bit(LZCNT, ext1.ecx, 5);
	add_feature_if_bit(MMX, feat.edx, 23);
	add_feature_if_bit(MOVBE, feat.ecx, 22);
	add_feature_if_bit(MOVDIR64B, leaf7.ecx, 28);
	add_feature_if_bit(MOVDIRI, leaf7.ecx, 27);
	add_feature_if_bit(MWAITX, ext1.ecx, 29);
	add_feature_if_bit(PCLMUL, feat.ecx, 1);
	add_feature_if_bit(PCONFIG, leaf7.edx, 18);
	add_feature_if_bit(PKU, leaf7.ecx, 4);
	add_feature_if_bit(POPCNT, feat.ecx, 23);
	add_feature_if_bit(PREFETCHI, leaf7s1.edx, 14);
	add_feature_if_bit(PREFETCHWT1, leaf7.ecx, 0);
	add_feature_if_bit(PRFCHW, ext1.ecx, 8);
	add_feature_if_bit(PTWRITE, leaf_14.ebx, 4);
	add_feature_if_bit(RAOINT, leaf7s1.eax, 3);
	add_feature_if_bit(RDPID, leaf7.ecx, 22);
	add_feature_if_bit(RDPRU, ext8.ecx, 4);
	add_feature_if_bit(RDRND, feat.ecx, 30);
	add_feature_if_bit(RDSEED, leaf7.ebx, 18);
	add_feature_if_bit(RTM, leaf7.ebx, 11);
	add_feature_if_bit(SAHF, ext1.ecx, 0);
	add_feature_if_bit(SERIALIZE, leaf7.edx, 14);
	add_feature_if_bit(SGX, leaf7.ebx, 2);
	add_feature_if_bit(SHA, leaf7.ebx, 29);
	add_feature_if_bit(SHA512, leaf7s1.eax, 0);
	add_feature_if_bit(SHSTK, leaf7.ecx, 7);
	add_feature_if_bit(SM3, leaf7s1.eax, 1);
	add_feature_if_bit(SM4, leaf7s1.eax, 2);
	add_feature_if_bit(SSE, feat.edx, 25);
	add_feature_if_bit(SSE2, feat.edx, 26);
	add_feature_if_bit(SSE3, feat.ecx, 0);
	add_feature_if_bit(SSE4_1, feat.ecx, 19);
	add_feature_if_bit(SSE4_2, feat.ecx, 20);
	add_feature_if_bit(SSE4_A, ext1.ecx, 6);
	add_feature_if_bit(SSSE3, feat.ecx, 9);
	add_feature_if_bit(TBM, ext1.ecx, 21);
	add_feature_if_bit(TSXLDTRK, leaf7.edx, 16);
	add_feature_if_bit(UINTR, leaf7.edx, 5);
	add_feature_if_bit(USERMSR, leaf7s1.edx, 15);
	add_feature_if_bit(VAES, leaf7.ecx, 9);
	add_feature_if_bit(VPCLMULQDQ, leaf7.ecx, 10);
	add_feature_if_bit(WAITPKG, leaf7.ecx, 5);
	add_feature_if_bit(WBNOINVD, ext8.ecx, 9);
	add_feature_if_bit(WIDEKL, leaf_19.ebx, 2);
	add_feature_if_bit(X87, feat.edx, 0);
	add_feature_if_bit(XOP, ext1.ecx, 11);
	add_feature_if_bit(XSAVE, feat.ecx, 26);
	add_feature_if_bit(XSAVEC, leaf_d.eax, 1);
	add_feature_if_bit(XSAVEOPT, leaf_d.eax, 0);
	add_feature_if_bit(XSAVES, leaf_d.eax, 3);

}