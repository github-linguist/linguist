/*
 * This file is part of the coreboot project.
 *
 * Copyright (C) 2007-2009 coresystems GmbH
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 2 of
 * the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */


Name(_HID,EISAID("PNP0A08"))	// PCIe
Name(_CID,EISAID("PNP0A03"))	// PCI

Name(_ADR, 0)
Name(_BBN, 0)

Device (MCHC)
{
	Name(_ADR, 0x00000000)	// 0:0.0

	OperationRegion(MCHP, PCI_Config, 0x00, 0x100)
	Field (MCHP, DWordAcc, NoLock, Preserve)
	{
		Offset (0x40),	// EPBAR
		EPEN,	 1,	// Enable
		,	11,	//
		EPBR,	24,	// EPBAR

		Offset (0x48),	// MCHBAR
		MHEN,	 1,	// Enable
		,	13,	//
		MHBR,	22,	// MCHBAR

		Offset (0x60),	// PCIe BAR
		PXEN,	 1,	// Enable
		PXSZ,	 2,	// BAR size
		,	23,	//
		PXBR,	10,	// PCIe BAR

		Offset (0x68),	// DMIBAR
		DMEN,	 1,	// Enable
		,	11,	//
		DMBR,	24,	// DMIBAR

		Offset (0x70),	// ME Base Address
		MEBA,	 64,

		// ...

		Offset (0x80),	// PAM0
		,	 4,
		PM0H,	 2,
		,	 2,
		Offset (0x81),	// PAM1
		PM1L,	 2,
		,	 2,
		PM1H,	 2,
		,	 2,
		Offset (0x82),	// PAM2
		PM2L,	 2,
		,	 2,
		PM2H,	 2,
		,	 2,
		Offset (0x83),	// PAM3
		PM3L,	 2,
		,	 2,
		PM3H,	 2,
		,	 2,
		Offset (0x84),	// PAM4
		PM4L,	 2,
		,	 2,
		PM4H,	 2,
		,	 2,
		Offset (0x85),	// PAM5
		PM5L,	 2,
		,	 2,
		PM5H,	 2,
		,	 2,
		Offset (0x86),	// PAM6
		PM6L,	 2,
		,	 2,
		PM6H,	 2,
		,	 2,

		Offset (0xa0),	// Top of Used Memory
		TOM,	 64,

		Offset (0xbc),	// Top of Low Used Memory
		TLUD,	 32,
	}

	Mutex (CTCM, 1)		/* CTDP Switch Mutex (sync level 1) */
	Name (CTCC, 0)		/* CTDP Current Selection */
	Name (CTCN, 0)		/* CTDP Nominal Select */
	Name (CTCD, 1)		/* CTDP Down Select */
	Name (CTCU, 2)		/* CTDP Up Select */

	OperationRegion (MCHB, SystemMemory, DEFAULT_MCHBAR, 0x8000)
	Field (MCHB, DWordAcc, Lock, Preserve)
	{
		Offset (0x5930),
		CTDN, 15,	/* CTDP Nominal PL1 */
		Offset (0x59a0),
		PL1V, 15,	/* Power Limit 1 Value */
		PL1E, 1,	/* Power Limit 1 Enable */
		PL1C, 1,	/* Power Limit 1 Clamp */
		PL1T, 7,	/* Power Limit 1 Time */
		Offset (0x59a4),
		PL2V, 15,	/* Power Limit 2 Value */
		PL2E, 1,	/* Power Limit 2 Enable */
		PL2C, 1,	/* Power Limit 2 Clamp */
		PL2T, 7,	/* Power Limit 2 Time */
		Offset (0x5f3c),
		TARN, 8,	/* CTDP Nominal Turbo Activation Ratio */
		Offset (0x5f40),
		CTDD, 15,	/* CTDP Down PL1 */
		, 1,
		TARD, 8,	/* CTDP Down Turbo Activation Ratio */
		Offset (0x5f48),
		CTDU, 15,	/* CTDP Up PL1 */
		, 1,
		TARU, 8,	/* CTDP Up Turbo Activation Ratio */
		Offset (0x5f50),
		CTCS, 2,	/* CTDP Select */
		Offset (0x5f54),
		TARS, 8,	/* Turbo Activation Ratio Select */
	}

	/*
	 * Search CPU0 _PSS looking for control=arg0 and then
	 * return previous P-state entry number for new _PPC
	 *
	 * Format of _PSS:
	 *   Name (_PSS, Package () {
	 *     Package (6) { freq, power, tlat, blat, control, status }
	 *   }
	 */
	External (\_PR.CP00._PSS)
	Method (PSSS, 1, NotSerialized)
	{
		Store (One, Local0) /* Start at P1 */
		Store (SizeOf (\_PR.CP00._PSS), Local1)

		While (LLess (Local0, Local1)) {
			/* Store _PSS entry Control value to Local2 */
			ShiftRight (DeRefOf (Index (DeRefOf (Index
			      (\_PR.CP00._PSS, Local0)), 4)), 8, Local2)
			If (LEqual (Local2, Arg0)) {
				Return (Subtract (Local0, 1))
			}
			Increment (Local0)
		}

		Return (0)
	}

	/* Set TDP Down */
	Method (STND, 0, Serialized)
	{
		If (Acquire (CTCM, 100)) {
			Return (0)
		}
		If (LEqual (CTCD, CTCC)) {
			Release (CTCM)
			Return (0)
		}

		Store ("Set TDP Down", Debug)

		/* Set CTC */
		Store (CTCD, CTCS)

		/* Set TAR */
		Store (TARD, TARS)

		/* Set PPC limit and notify OS */
		Store (PSSS (TARD), PPCM)
		PPCN ()

		/* Set PL2 to 1.25 * PL1 */
		Divide (Multiply (CTDD, 125), 100, , PL2V)

		/* Set PL1 */
		Store (CTDD, PL1V)

		/* Store the new TDP Down setting */
		Store (CTCD, CTCC)

		Release (CTCM)
		Return (1)
	}

	/* Set TDP Nominal from Down */
	Method (STDN, 0, Serialized)
	{
		If (Acquire (CTCM, 100)) {
			Return (0)
		}
		If (LEqual (CTCN, CTCC)) {
			Release (CTCM)
			Return (0)
		}

		Store ("Set TDP Nominal", Debug)

		/* Set PL1 */
		Store (CTDN, PL1V)

		/* Set PL2 to 1.25 * PL1 */
		Divide (Multiply (CTDN, 125), 100, , PL2V)

		/* Set PPC limit and notify OS */
		Store (PSSS (TARN), PPCM)
		PPCN ()

		/* Set TAR */
		Store (TARN, TARS)

		/* Set CTC */
		Store (CTCN, CTCS)

		/* Store the new TDP Nominal setting */
		Store (CTCN, CTCC)

		Release (CTCM)
		Return (1)
	}
}

// Current Resource Settings
Name (MCRS, ResourceTemplate()
{
	// Bus Numbers
	WordBusNumber (ResourceProducer, MinFixed, MaxFixed, PosDecode,
			0x0000, 0x0000, 0x00ff, 0x0000, 0x0100,,, PB00)

	// IO Region 0
	DWordIO (ResourceProducer, MinFixed, MaxFixed, PosDecode, EntireRange,
			0x0000, 0x0000, 0x0cf7, 0x0000, 0x0cf8,,, PI00)

	// PCI Config Space
	Io (Decode16, 0x0cf8, 0x0cf8, 0x0001, 0x0008)

	// IO Region 1
	DWordIO (ResourceProducer, MinFixed, MaxFixed, PosDecode, EntireRange,
			0x0000, 0x0d00, 0xffff, 0x0000, 0xf300,,, PI01)

	// VGA memory (0xa0000-0xbffff)
	DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,
			Cacheable, ReadWrite,
			0x00000000, 0x000a0000, 0x000bffff, 0x00000000,
			0x00020000,,, ASEG)

	// OPROM reserved (0xc0000-0xc3fff)
	DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,
			Cacheable, ReadWrite,
			0x00000000, 0x000c0000, 0x000c3fff, 0x00000000,
			0x00004000,,, OPR0)

	// OPROM reserved (0xc4000-0xc7fff)
	DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,
			Cacheable, ReadWrite,
			0x00000000, 0x000c4000, 0x000c7fff, 0x00000000,
			0x00004000,,, OPR1)

	// OPROM reserved (0xc8000-0xcbfff)
	DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,
			Cacheable, ReadWrite,
			0x00000000, 0x000c8000, 0x000cbfff, 0x00000000,
			0x00004000,,, OPR2)

	// OPROM reserved (0xcc000-0xcffff)
	DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,
			Cacheable, ReadWrite,
			0x00000000, 0x000cc000, 0x000cffff, 0x00000000,
			0x00004000,,, OPR3)

	// OPROM reserved (0xd0000-0xd3fff)
	DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,
			Cacheable, ReadWrite,
			0x00000000, 0x000d0000, 0x000d3fff, 0x00000000,
			0x00004000,,, OPR4)

	// OPROM reserved (0xd4000-0xd7fff)
	DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,
			Cacheable, ReadWrite,
			0x00000000, 0x000d4000, 0x000d7fff, 0x00000000,
			0x00004000,,, OPR5)

	// OPROM reserved (0xd8000-0xdbfff)
	DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,
			Cacheable, ReadWrite,
			0x00000000, 0x000d8000, 0x000dbfff, 0x00000000,
			0x00004000,,, OPR6)

	// OPROM reserved (0xdc000-0xdffff)
	DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,
			Cacheable, ReadWrite,
			0x00000000, 0x000dc000, 0x000dffff, 0x00000000,
			0x00004000,,, OPR7)

	// BIOS Extension (0xe0000-0xe3fff)
	DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,
			Cacheable, ReadWrite,
			0x00000000, 0x000e0000, 0x000e3fff, 0x00000000,
			0x00004000,,, ESG0)

	// BIOS Extension (0xe4000-0xe7fff)
	DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,
			Cacheable, ReadWrite,
			0x00000000, 0x000e4000, 0x000e7fff, 0x00000000,
			0x00004000,,, ESG1)

	// BIOS Extension (0xe8000-0xebfff)
	DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,
			Cacheable, ReadWrite,
			0x00000000, 0x000e8000, 0x000ebfff, 0x00000000,
			0x00004000,,, ESG2)

	// BIOS Extension (0xec000-0xeffff)
	DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,
			Cacheable, ReadWrite,
			0x00000000, 0x000ec000, 0x000effff, 0x00000000,
			0x00004000,,, ESG3)

	// System BIOS (0xf0000-0xfffff)
	DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,
			Cacheable, ReadWrite,
			0x00000000, 0x000f0000, 0x000fffff, 0x00000000,
			0x00010000,,, FSEG)

	// PCI Memory Region (Top of memory-CONFIG_MMCONF_BASE_ADDRESS)
	DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,
			Cacheable, ReadWrite,
			0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x00000000,,, PM01)

	// TPM Area (0xfed40000-0xfed44fff)
	DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,
			Cacheable, ReadWrite,
			0x00000000, 0xfed40000, 0xfed44fff, 0x00000000,
			0x00005000,,, TPMR)
})

Method (_CRS, 0, Serialized)
{
	// Find PCI resource area in MCRS
	CreateDwordField(MCRS, ^PM01._MIN, PMIN)
	CreateDwordField(MCRS, ^PM01._MAX, PMAX)
	CreateDwordField(MCRS, ^PM01._LEN, PLEN)

	// Fix up PCI memory region
	// Start with Top of Lower Usable DRAM
	Store (^MCHC.TLUD, Local0)
	Store (^MCHC.MEBA, Local1)

	// Check if ME base is equal
	If (LEqual (Local0, Local1)) {
		// Use Top Of Memory instead
		Store (^MCHC.TOM, Local0)
	}

	Store (Local0, PMIN)
	Store (Subtract(CONFIG_MMCONF_BASE_ADDRESS, 1), PMAX)
	Add(Subtract(PMAX, PMIN), 1, PLEN)

	Return (MCRS)
}
