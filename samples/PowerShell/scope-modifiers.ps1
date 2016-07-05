$a = "foo"                        # global scope
function test {
    $a = "bar"                    # local scope
    Write-Host Local: $a          # "bar" - local variable
    Write-Host Global: $global:a  # "foo" - global variable
}
