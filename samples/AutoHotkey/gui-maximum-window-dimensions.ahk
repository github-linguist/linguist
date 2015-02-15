SysGet, MonitorCount, MonitorCount
SysGet, MonitorPrimary, MonitorPrimary
MsgBox, Monitor Count:`t%MonitorCount%`nPrimary Monitor:`t%MonitorPrimary%
Loop, %MonitorCount%
{
    SysGet, MonitorName, MonitorName, %A_Index%
    SysGet, Monitor, Monitor, %A_Index%
    SysGet, MonitorWorkArea, MonitorWorkArea, %A_Index%
    MsgBox, % "Monitor:`t#" A_Index
            . "`nName:`t" MonitorName
            . "`nLeft:`t" MonitorLeft "(" MonitorWorkAreaLeft " work)"
            . "`nTop:`t" MonitorTop " (" MonitorWorkAreaTop " work)"
            . "`nRight:`t" MonitorRight " (" MonitorWorkAreaRight " work)"
            . "`nBottom:`t" MonitorBottom " (" MonitorWorkAreaBottom " work)"
}
