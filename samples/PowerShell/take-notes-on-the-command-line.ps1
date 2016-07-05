$notes = "notes.txt"
if (($args).length -eq 0) {
    if(Test-Path $notes) {
        Get-Content $notes
    }
} else {
    Get-Date | Add-Content $notes
    "`t" + $args -join " " | Add-Content $notes
}
