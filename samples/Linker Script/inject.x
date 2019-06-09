/* OUTPUT_FORMAT("elf32-littlearm", "elf32-bigarm", "elf32-littlearm") */
/* OUTPUT_ARCH(arm) */
ENTRY(__adbi$entry)
SECTIONS
{
    . = 0x00000000 + SIZEOF_HEADERS;
   
    .adbi : { 
      *(.rodata) 
      *(.rodata.*) 
      *(.data) *(.data.*)
      *(.bss) *(.bss.*)
      *(.text) 
      *(.text.*)
      *(.adbi)
      *(.adbi.*)
    } = 0
    
}
