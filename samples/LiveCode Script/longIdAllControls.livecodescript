script "longIdAllControls"


function longIdAllControls pLongId
    local tType, tCards, tControls, tResult

    if there is not a pLongId then  return empty for value
    put word 1 of pLongId into tType

    if tType is "stack" then
        put the cardIDs of pLongId into tCards
        repeat for each line tCard in tCards
            put longIdAllControls("card id"&& tCard && "of" && pLongId) & cr after tResult
        end repeat
        delete char - 1 of tResult
    else if tType is in "group card" then
        put the number of controls of pLongId into tControls
        repeat with x = 1 to tControls
            put the long id of control x of pLongId into line x of tResult
        end repeat
    else
        return the long id of pLongId
    end if

    sort lines of tResult
    return tResult for value
end longIdAllControls