function GDSDebuffEnd takes nothing returns nothing
    local timer t = GetExpiredTimer()
    local integer tHandle = GetHandleId(t)
    local integer stunType = LoadInteger(udg_GDS_Hashtable, tHandle , 2)
    local unit u = LoadUnitHandle(udg_GDS_Hashtable, tHandle , 0)
    local integer uHandle = LoadInteger(udg_GDS_Hashtable, tHandle , 1)

    if GetUnitTypeId(u) != 0 then  // Stunned unit has been removed from the game
        call UnitRemoveAbility(u, udg_GDS_BUFF[stunType])
    endif

    call FlushChildHashtable(udg_GDS_Hashtable, GetHandleId(t))
    call RemoveSavedHandle(udg_GDS_Hashtable, uHandle , stunType)
    call PauseTimer(t)
    call DestroyTimer(t)

    set u = null
    set t = null
endfunction

function GDSMain takes nothing returns nothing
    local timer t
    local timer timerOld = LoadTimerHandle(udg_GDS_Hashtable, GetHandleId(udg_GDS_Target) , udg_GDS_Type )
    local real duration = udg_GDS_Duration
    local boolean hadTimer = timerOld != null

    
    set udg_GDS_Remaining = TimerGetRemaining(timerOld)
//Check if user wants a permanent debuff
    if udg_GDS_Permanent then
        set udg_GDS_Permanent = false
        if hadTimer then

            call FlushChildHashtable(udg_GDS_Hashtable, GetHandleId(timerOld))
            call PauseTimer(timerOld)
            call DestroyTimer(timerOld)
            call RemoveSavedHandle(udg_GDS_Hashtable, GetHandleId(udg_GDS_Target) , udg_GDS_Type)

            set timerOld = null
        else
            if IssueTargetOrder( udg_GDS_DUMMY[udg_GDS_Type], udg_GDS_ORDERS[udg_GDS_Type], udg_GDS_Target ) or GetUnitAbilityLevel(udg_GDS_Target , udg_GDS_BUFF[udg_GDS_Type]) != 0 then
                set udg_GDS_Remaining = -1
            else
                set udg_GDS_Remaining = 0
            endif
        endif

        return
    endif
//Check if user wants to reduce duration of stun (check for negative duration)
    if duration < -udg_GDS_zMIN and udg_GDS_Remaining > udg_GDS_zMIN then  //Check for reduction
        if -duration > udg_GDS_Remaining then  //Check if reduction goes below 0
            call UnitRemoveAbility(udg_GDS_Target , udg_GDS_BUFF[udg_GDS_Type])
            set duration = 0
        else
            set duration = udg_GDS_Remaining - duration //Change duration so it passes the checks
        endif
    endif 

//Check if the desired duration is higher than remaining time
    if  udg_GDS_Remaining < duration and duration > udg_GDS_zMIN and hadTimer then

        if hadTimer then
            set t = timerOld
        else
            set t = CreateTimer()
            set timerOld = null
        endif
 
    else
        //Check if unit has buff, clean hashtable, destroy timer and set remaining time to 0
        if GetUnitAbilityLevel(udg_GDS_Target, udg_GDS_BUFF[udg_GDS_Type]) == 0 then

            call FlushChildHashtable(udg_GDS_Hashtable, GetHandleId(timerOld))
            call PauseTimer(timerOld)
            call DestroyTimer(timerOld)
            call RemoveSavedHandle(udg_GDS_Hashtable, GetHandleId(udg_GDS_Target) , udg_GDS_Type)

            set timerOld = null

            if duration < udg_GDS_zMIN then 
                set udg_GDS_Remaining = 0
                return
            endif

            set t = CreateTimer()

        else

            if hadTimer then
                set timerOld = null
            else            
                set udg_GDS_Remaining = -1
            endif
            return        
        endif      
    endif


//Check was successful, proceed to attempt to stun
    if not IssueTargetOrder( udg_GDS_DUMMY[udg_GDS_Type], udg_GDS_ORDERS[udg_GDS_Type], udg_GDS_Target ) and GetUnitAbilityLevel(udg_GDS_Target , udg_GDS_BUFF[udg_GDS_Type]) == 0 then
        set timerOld = null
        set udg_GDS_Remaining = 0
        //call BJDebugMsg("No stun")
        return //unable to stun, do nothing
    endif
//Check again for duration reduction (negative duration)    
    if udg_GDS_Duration < 0 then 
        set duration = duration + 2*udg_GDS_Duration //Change duration to reduced duration
    endif
//Save Hashtable values only if a new timer has been created    
    if not hadTimer then
        call SaveUnitHandle(udg_GDS_Hashtable, GetHandleId(t) , 0 , udg_GDS_Target)
        call SaveInteger(udg_GDS_Hashtable , GetHandleId(t) , 1 , GetHandleId(udg_GDS_Target))
        call SaveInteger(udg_GDS_Hashtable , GetHandleId(t) , 2 , udg_GDS_Type)
        call SaveTimerHandle(udg_GDS_Hashtable, GetHandleId(udg_GDS_Target) , udg_GDS_Type , t)
    endif
    

    call TimerStart(t, duration , false, function GDSDebuffEnd)
   
    
    set udg_GDS_Remaining = duration
    
    
    set timerOld = null
    set t = null
    
endfunction



//===========================================================================
function InitTrig_GDS_Main takes nothing returns nothing
    set gg_trg_GDS_Main = CreateTrigger(  )
    call TriggerAddAction( gg_trg_GDS_Main, function GDSMain )
endfunction

