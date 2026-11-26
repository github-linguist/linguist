REM > vtest.bas
REN
REM © A.Thoukydides, 1995, 1996, 1997, 1998, 1999, 2016
REM
REM Virtualise is free software: you can redistribute it and/or modify it
REM under the terms of the GNU General Public License as published by the Free
REM Software Foundation, either version 3 of the License, or (at your option)
REM any later version.
REM
REM Virtualise is distributed in the hope that it will be useful, but WITHOUT
REM ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
REM FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
REM more details.
REM
REM You should have received a copy of the GNU General Public License along
REM with Virtualise. If not, see <http://www.gnu.org/licenses/>.

dynamic_size% = 1000 * 1024
dynamic_area$ = "Example Virtual Memory"
quick% = TRUE

REM Delete any old areas
old_dynamic_areas% = 0
SYS "OS_DynamicArea", 3, -1 TO , dynamic_area%
WHILE dynamic_area% <> -1
    SYS "OS_DynamicArea", 2, dynamic_area% TO ,,,,,,,, area_name$
    IF area_name$ = dynamic_area$ THEN
        PRINT "Existing area "; dynamic_area%;" found - delete? (Y/N)";
        IF INSTR("Yy", GET$) THEN
            PRINT " Yes"
            old_dynamic_areas% += 1
            SYS "XVirtualise_End", dynamic_area%
            SYS "OS_DynamicArea", 1, dynamic_area%
            dynamic_area% = -1
        ELSE
            PRINT " No"
        ENDIF
    ENDIF
    SYS "OS_DynamicArea", 3, dynamic_area% TO , dynamic_area%
ENDWHILE
IF 0 < old_dynamic_areas% THEN
    PRINT old_dynamic_areas%; " dynamic areas removed"
    END
ENDIF

IF quick% THEN
    REM Create a dynamic area of zero size with virtual memory active
    SYS "OS_DynamicArea", 0, -1, dynamic_size%, -1, (1<<7) + (1<<31), dynamic_size% * 2, 0, 0, dynamic_area$ TO , dynamic_area%,, base%
    PRINT "Created dynamic area "; dynamic_area%; " at &"; ~base%; " with virtual memory"
ELSE
    REM Create a dynamic area
    SYS "OS_DynamicArea", 0, -1, dynamic_size%, -1, &80, -1, 0, 0, dynamic_area$ TO , dynamic_area%,, base%
    PRINT "Created dynamic area "; dynamic_area%; " at &"; ~base%
    
    REM Turn the area into virtual memory
    SYS "Virtualise_Start", dynamic_area%, -1, 0
    PRINT "Converted to virtual memory"
ENDIF

REM Test the virtual memory
PRINT "Timed test? (Y/N)";
key$ = GET$
WHILE INSTR("Yy", key$)
    PRINT " Yes"
    PROCTest
    PRINT "Another test? (Y/N)";
    key$=GET$
ENDWHILE
PRINT " No"

IF NOT(quick%) THEN
    REM Return area to proper memory
    SYS "Virtualise_End", dynamic_area%
    PRINT "Converted back to physical memory"
ENDIF

REM Delete the area
SYS "OS_DynamicArea", 1, dynamic_area%
PRINT "Deleted dynamic area"

END

DEFPROCTest
    PRINT "Starting test..."
    rand% = RND(-42)
    start% = TIME
    FOR count% = 0 TO 1000
        base%?RND(dynamic_size%-1) = base%?RND(dynamic_size%-1)
        base%!RND(dynamic_size%-5) = base%!RND(dynamic_size%-5)
    NEXT
    end% = TIME
    PRINT "Time taken for test = "; (end%-start%); " cs"
ENDPROC
