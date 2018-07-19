for ($i = 1; $i -le 10; $i++) {
    Write-Host -NoNewline $i
    if ($i % 5 -eq 0) {
        Write-Host
        continue
    }
    Write-Host -NoNewline ", "
}
