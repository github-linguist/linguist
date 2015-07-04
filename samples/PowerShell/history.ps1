function Save-HistoryAll() {
    $history = Get-History -Count $MaximumHistoryCount
    [array]::Reverse($history)
    $history = $history | Group CommandLine | Foreach {$_.Group[0]}
    [array]::Reverse($history)
    $history | Export-Csv $historyPath
}

function Save-HistoryIncremental() {
#    Get-History -Count $MaximumHistoryCount | Group CommandLine | Foreach {$_.Group[0]} | Export-Csv $historyPath 
    Get-History -Count 1 | Export-Csv -Append $historyPath 
}
 
# hook powershell's exiting event & hide the registration with -supportevent.
#Register-EngineEvent -SourceIdentifier powershell.exiting -SupportEvent -Action { Save-History }

$oldPrompt = Get-Content function:\prompt

if( $oldPrompt -notlike '*Save-HistoryIncremental*' )
{
    $newPrompt = @'
Save-HistoryIncremental

'@
    $newPrompt += $oldPrompt
    $function:prompt = [ScriptBlock]::Create($newPrompt)
}
 
# load previous history, if it exists
if ((Test-Path $historyPath)) {
    $loadTime = 
    (
        Measure-Command {
            Import-Csv $historyPath | Add-History
            Save-HistoryAll
            Clear-History
            Import-Csv $historyPath | ? {$count++;$true} | Add-History
        }
    ).totalseconds
    Write-Host -Fore Green "`nLoaded $count history item(s) in $loadTime seconds.`n"
}


function Search-History()
{
    <#
    .SYNOPSIS
        Retrive and filter history based on query
    .DESCRIPTION
    .PARAMETER Name
    .EXAMPLE
    .LINK
    #>

    param(
        [string[]] $query
    )

    $history = Get-History -Count $MaximumHistoryCount
    foreach ($item in $query){
        $item = $item.ToLower()
        $history = $history | where {$_.CommandLine.ToLower().Contains($item)}    
    }
    $history
}