#
# Weight function.
#
function Update-ZLocation([string]$path)
{
    $now = [datetime]::Now
    if (Test-Path variable:global:__zlocation_current)
    {
        $prev = $global:__zlocation_current
        $weight = $now.Subtract($prev.Time).TotalSeconds
        Add-ZWeight ($prev.Location) $weight 
    }

    $global:__zlocation_current = @{
        Location = $path
        Time = [datetime]::Now
    }

    # populate folder immidiatly after the first cd
    Add-ZWeight $path 0
}

# this approach hurts `cd` performance (0.0008 sec vs 0.025 sec). 
# Consider replace it with OnIdle Event.
(Get-Variable pwd).attributes.Add((new-object ValidateScript { Update-ZLocation $_.Path; return $true }))
#
# End of weight function.
#


#
# Tab complention.
#
if (Test-Path Function:\TabExpansion) {
    Rename-Item Function:\TabExpansion PreZTabExpansion
}

function Get-EscapedPath
{
    param( 
    [Parameter(
        Position=0, 
        Mandatory=$true, 
        ValueFromPipeline=$true,
        ValueFromPipelineByPropertyName=$true)
    ]
    [string]$path
    ) 

    process {
        if ($path.Contains(' '))
        {
            return '"' + $path + '"'
        }
        return $path
    }
}

function global:TabExpansion($line, $lastWord) {
    switch -regex ($line) {
        "^(Set-ZLocation|z) .*" {
            $arguments = $line -split ' ' | Where { $_.length -gt 0 } | select -Skip 1
            Find-Matches (Get-ZLocation) $arguments | Get-EscapedPath
        }
        default {
            if (Test-Path Function:\PreZTabExpansion) {
                PreZTabExpansion $line $lastWord
            }
        }
    }
}
#
# End of tab completion.
#

function Set-ZLocation()
{
    if (-not $args) {
        $args = @()
    }
    $matches = Find-Matches (Get-ZLocation) $args
    if ($matches) {
        Push-Location ($matches | Select-Object -First 1)
    } else {
        Write-Warning "Cannot find matching location"
    }
}


Set-Alias -Name z -Value Set-ZLocation
Export-ModuleMember -Function Set-ZLocation, Get-ZLocation -Alias z