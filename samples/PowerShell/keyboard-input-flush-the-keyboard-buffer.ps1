while ($Host.UI.RawUI.KeyAvailable) {
    $Host.UI.RawUI.ReadKey() | Out-Null
}
