function create_hash ([array] $keys, [array] $values) {
    $h = @{}
    if ($keys.Length -ne $values.Length) {
        Write-Error -Message "Array lengths do not match" `
                    -Category InvalidData `
                    -TargetObject $values
    } else {
        for ($i = 0; $i -lt $keys.Length; $i++) {
            $h[$keys[$i]] = $values[$i]
        }
    }
    return $h
}
