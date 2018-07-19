Write-Host Word Size: ((Get-WMIObject Win32_Processor).DataWidth)
Write-Host -NoNewLine "Endianness: "
if ([BitConverter]::IsLittleEndian) {
    Write-Host Little-Endian
} else {
    Write-Host Big-Endian
}
