function Get-Maximum ($a) {
    return ($a | Measure-Object -Maximum).Maximum
}
