ACPI_EXTRACT_ALL_CODE ssdp_misc_aml

DefinitionBlock ("ssdt-misc.aml", "SSDT", 0x01, "BXPC", "BXSSDTSUSP", 0x1)
{

/****************************************************************
 * PCI memory ranges
 ****************************************************************/

    Scope(\) {
       ACPI_EXTRACT_NAME_DWORD_CONST acpi_pci32_start
       Name(P0S, 0x12345678)
       ACPI_EXTRACT_NAME_DWORD_CONST acpi_pci32_end
       Name(P0E, 0x12345678)
       ACPI_EXTRACT_NAME_BYTE_CONST acpi_pci64_valid
       Name(P1V, 0x12)
       ACPI_EXTRACT_NAME_BUFFER8 acpi_pci64_start
       Name(P1S, Buffer() { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 })
       ACPI_EXTRACT_NAME_BUFFER8 acpi_pci64_end
       Name(P1E, Buffer() { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 })
       ACPI_EXTRACT_NAME_BUFFER8 acpi_pci64_length
       Name(P1L, Buffer() { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 })
    }


/****************************************************************
 * Suspend
 ****************************************************************/

    Scope(\) {
    /*
     * S3 (suspend-to-ram), S4 (suspend-to-disk) and S5 (power-off) type codes:
     * must match piix4 emulation.
     */

        ACPI_EXTRACT_NAME_STRING acpi_s3_name
        Name(_S3, Package(0x04) {
            One,  /* PM1a_CNT.SLP_TYP */
            One,  /* PM1b_CNT.SLP_TYP */
            Zero,  /* reserved */
            Zero   /* reserved */
        })
        ACPI_EXTRACT_NAME_STRING acpi_s4_name
        ACPI_EXTRACT_PKG_START acpi_s4_pkg
        Name(_S4, Package(0x04) {
            0x2,  /* PM1a_CNT.SLP_TYP */
            0x2,  /* PM1b_CNT.SLP_TYP */
            Zero,  /* reserved */
            Zero   /* reserved */
        })
        Name(_S5, Package(0x04) {
            Zero,  /* PM1a_CNT.SLP_TYP */
            Zero,  /* PM1b_CNT.SLP_TYP */
            Zero,  /* reserved */
            Zero   /* reserved */
        })
    }

    External(\_SB.PCI0, DeviceObj)
    External(\_SB.PCI0.ISA, DeviceObj)

    Scope(\_SB.PCI0.ISA) {
        Device(PEVT) {
            Name(_HID, "QEMU0001")
            /* PEST will be patched to be Zero if no such device */
            ACPI_EXTRACT_NAME_WORD_CONST ssdt_isa_pest
            Name(PEST, 0xFFFF)
            OperationRegion(PEOR, SystemIO, PEST, 0x01)
            Field(PEOR, ByteAcc, NoLock, Preserve) {
                PEPT,   8,
            }

            Method(_STA, 0, NotSerialized) {
                Store(PEST, Local0)
                If (LEqual(Local0, Zero)) {
                    Return (0x00)
                } Else {
                    Return (0x0F)
                }
            }

            Method(RDPT, 0, NotSerialized) {
                Store(PEPT, Local0)
                Return (Local0)
            }

            Method(WRPT, 1, NotSerialized) {
                Store(Arg0, PEPT)
            }

            Name(_CRS, ResourceTemplate() {
                IO(Decode16, 0x00, 0x00, 0x01, 0x01, IO)
            })

            CreateWordField(_CRS, IO._MIN, IOMN)
            CreateWordField(_CRS, IO._MAX, IOMX)

            Method(_INI, 0, NotSerialized) {
                Store(PEST, IOMN)
                Store(PEST, IOMX)
            }
        }
    }
}
