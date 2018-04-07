#!/usr/bin/env pwsh

# source:  https://github.com/PowerShell/PowerShellStandard/blob/3436bfc162d6804dd11d1d76c4faff486b4b405d/build.ps1

param ( 
    [Parameter(ParameterSetName="Clean")][switch]$Clean,
    [Parameter(ParameterSetName="Test")][switch]$Test
)

import-module $PSScriptRoot/PowerShellStandard.psm1 -force

if ( $Clean ) {
    Start-Clean
    return
}

Start-Build

if ( $Test ) {
    Invoke-Test
}
