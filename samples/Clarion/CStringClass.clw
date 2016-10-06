      Member()
         omit('***$***',_VER_C55)
_ABCDllMode_  EQUATE(0)
_ABCLinkMode_ EQUATE(1)
         ***$***
      Include('Equates.CLW'),ONCE
      Include('Keycodes.CLW'),ONCE
      Include('Errors.CLW'),ONCE
      Map
      End ! map
      Include('CStringClass.inc'),ONCE
CStringClass.Construct PROCEDURE                       ! Declare Procedure
  CODE
  SELF.bufferSize  = DEFAULT_CS_BUFFER_SIZE
  SELF.CS         &= New(CSTRING(SELF.bufferSize))
CStringClass.Destruct PROCEDURE                        ! Declare Procedure
  CODE
  Dispose(SELF.cs)
CStringClass.Cat PROCEDURE  (STRING pStr) !,*CSTRING,PROC ! Declare Procedure
newLen                       LONG,AUTO
oldCS                        &CSTRING
  CODE
  newLen = Len(pStr)
  IF (newLen+SELF.strLength+2) > SELF.newStrSize
    ! Only grow the internal string if the result of the cat will be larger than the string currently is.
    ! The reason for the "+2" is because this is used in the string slicing outside this IF. Without this matching +2 there is potential for an out of bounds slice which would be bad!

    ! Save a temporary copy of the old string so we can us it in the concatination after we have grown it!
    oldCS &= New(CSTRING(SELF.strLength+1))
    oldCS = SELF.CS
    Dispose(SELF.CS)

    SELF.newStrSize = newLen + SELF.strLength + 1 + SELF.bufferSize
    SELF.CS &= New(CSTRING(SELF.newStrSize))
    SELF.CS = oldCS
    Dispose(oldCS)
  END

  ! Append the new string directly to the end of the old one.
  SELF.CS[SELF.strLength+1 : SELF.strLength+newLen] = pStr
  ! And terminate the CSTRING manually
  SELF.CS[SELF.strLength+newLen+1] = '<0>'

  ! This is the same as doing "SELF.strLength = Len(SELF.CS)" but the Len() is _really_ slow on large strings. This is much faster!
  SELF.strLength += newLen

  ! This is what it used to be:
  ! SELF.Str(SELF.Str() & s)
  ! It is a nice and neat solution but performance, especially on large strings was terrible!

  RETURN SELF.Str()
CStringClass.Str PROCEDURE  (STRING pStr) !,*CSTRING, PROC   ! Declare Procedure
  CODE
  IF Len(pStr) > SELF.newStrSize
    ! Only Dispose/New the internal string if the new one requires it.
    ! This might be slightly innefficient in terms of memory usage when the string gets smaller
    ! But it is _vasty_ better for performance when the string gets added to a lot.
    Dispose(SELF.CS)
    SELF.newStrSize = Len(pStr) + 1 + SELF.bufferSize
    SELF.CS &= New(CSTRING(SELF.newStrSize))
  END

  SELF.CS        = pStr
  SELF.strLength = Len(SELF.CS)

  RETURN SELF.CS
CStringClass.Len PROCEDURE  !,LONG                     ! Declare Procedure
  CODE
  RETURN SELF.strLength
CStringClass.Replace PROCEDURE  (STRING pFind, STRING pReplace) !,*CSTRING,PROC ! Declare Procedure
! FindString , ReplaceWith
locate                       LONG,AUTO
lastLocate                   LONG
  CODE
  LOOP
    locate = InString(Upper(pFind), Upper(SELF.Str()), 1, lastLocate+1)
    IF ~locate
      BREAK
    END

    ! So we dont end up having recursive replacement.
    lastLocate = locate + Len(pReplace)-1

    SELF.Str(Sub(SELF.Str(), 1, locate-1)                  & |
             pReplace                                      & |
             Sub(SELF.Str(), locate+Len(pFind), SELF.Len())    |
             )
  END

  RETURN SELF.Str()
CStringClass.Str PROCEDURE  () !,*CSTRING              ! Declare Procedure 3
  CODE
  RETURN SELF.CS
!------------------------------------------------------------------------------
CStringClass.Contains PROCEDURE  (STRING pFind, BYTE pCaseSensitive=TRUE) !,BYTE ! Declare Procedure
! Returns a value (TRUE) indicating whether the specified String occurs within this string.
! Second parameter defaults to a case sensitive search.
  CODE
  IF pCaseSensitive = TRUE
    IF InString(pFind, SELF.Str(), 1 , 1) > 0
      RETURN TRUE
    END
  ELSE
    IF InString(Lower(pFind), SELF.Lower(), 1 , 1) > 0
      RETURN TRUE
    END
  END

  RETURN FALSE
CStringClass.Lower PROCEDURE  () !,STRING              ! Declare Procedure
! Returns a "Lowered" version of the self.cs doesnt change the self.cs
  CODE
  RETURN Lower(SELF.CS)
CStringClass.SubString PROCEDURE  (LONG pPosition, LONG pLength) !,STRING,PROC ! Declare Procedure
  CODE
  RETURN Sub(SELF.Str(), pPosition, pLength)
CStringClass.ToLower PROCEDURE  () !,*CSTRING,PROC     ! Declare Procedure
! Converts this string to lowercase and returns the converted string

  CODE
  RETURN SELF.Str(SELF.Lower())
CStringClass.ToUpper PROCEDURE  () !,*CSTRING,PROC     ! Declare Procedure
! Converts this string to uppercase and returns the converted string

  CODE
  RETURN SELF.Str(SELF.Upper())
CStringClass.Trim PROCEDURE  () !,*CSTRING,PROC        ! Declare Procedure
  CODE
  SELF.Str(Left(SELF.Str()))
  SELF.Str(Clip(SELF.Str()))
  RETURN SELF.Str()
CStringClass.Upper PROCEDURE  () !,STRING              ! Declare Procedure
  CODE
  RETURN Upper(SELF.Str())
CStringClass.IndexOf PROCEDURE  (STRING pLookIn, BYTE pCaseSensitive=TRUE) !,LONG ! Declare Procedure
! Returns the index of the first parameter (pLookIn) is found within the SELF.CS
! zero if it is not found
  CODE
  IF pCaseSensitive = TRUE
    RETURN InString(SELF.Str(), pLookIn, 1 , 1)
  ELSE
    RETURN InString(SELF.Lower(), Lower(pLookIn), 1 , 1)
  END
CStringClass.FoundIn PROCEDURE  (STRING pLookIn, BYTE pCaseSensitive=TRUE) !,BYTE ! Declare Procedure
! Returns TRUE if the first parameter (pLookIn) is found within the SELF.CS
! FALSE if it is no
  CODE
  IF SELF.IndexOf(pLookIn, pCaseSensitive) > 0
    RETURN TRUE
  ELSE
    RETURN FALSE
  END
CStringClass.SetBuffer PROCEDURE  (LONG pNewBuffer)    ! Declare Procedure
  CODE
  SELF.bufferSize = pNewBuffer
CStringClass.EscapeXml PROCEDURE  (<STRING pStr>) !,STRING ! Declare Procedure
CS CStringClass
  CODE
  IF Omitted(pStr)=FALSE
    CS.Str(pStr)
  ELSE
    ! Make a copy so we don't alter the original
    CS.Str(SELF.Str())
  END
  CS.Replace('&', '&amp;')
  CS.Replace('<', '&lt;')
  CS.Replace('>', '&gt;')
  CS.Replace('"', '&quot;')
  CS.Replace('''', '&apos;')

  RETURN CS.Str()

