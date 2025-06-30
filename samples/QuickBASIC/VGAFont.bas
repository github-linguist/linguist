'-----------------------------------------------------------------------------------------------------------------------
' VGA Font Library
' Copyright (c) 2024 Samuel Gomes
'-----------------------------------------------------------------------------------------------------------------------

$INCLUDEONCE

'$INCLUDE:'VGAFont.bi'

'-----------------------------------------------------------------------------------------------------------------------
' TEST CODE
'-----------------------------------------------------------------------------------------------------------------------
'$RESIZE:SMOOTH
'SCREEN 12

'DIM myFont AS PSF1Type

'DIM i AS LONG: FOR i = 1 TO _WIDTH
'    CIRCLE (RND * _WIDTH, RND * _HEIGHT), RND * _HEIGHT, RND * 16
'NEXT

'IF PSF1_LoadFontFromPath("..\VGA-Font-Editor\fonts\8x16\DIG8X16.psf", myFont) THEN
'    PSF1_DrawString "Hello, world!", 16, 16
'    _PRINTMODE _KEEPBACKGROUND
'    PSF1_DrawString "Hello, world!", 32, 32
'    _PRINTMODE _ONLYBACKGROUND
'    PSF1_DrawString "Hello, world!", 48, 48
'END IF

'END
'-----------------------------------------------------------------------------------------------------------------------

' Draws a single character at x, y using the active font
SUB PSF1_DrawCharacter (cp AS _UNSIGNED _BYTE, x AS LONG, y AS LONG)
    $CHECKING:OFF
    SHARED __CurPSF AS PSF1Type
    DIM AS LONG uy, r, t, p, bc, pm

    r = x + __CurPSF.size.x - 1 ' calculate right just once

    bc = _BACKGROUNDCOLOR
    pm = _PRINTMODE

    ' Go through the scan line one at a time
    FOR uy = 1 TO __CurPSF.size.y
        ' Get the scan line and pepare it
        p = ASC(__CurPSF.bitmap, __CurPSF.size.y * cp + uy)
        p = 256 * (p + (256 * (p > 127)))
        ' Draw the line
        t = y + uy - 1
        SELECT CASE pm
            CASE 1
                LINE (x, t)-(r, t), , , p
            CASE 2
                LINE (x, t)-(r, t), , , NOT p
            CASE ELSE
                LINE (x, t)-(r, t), bc
                LINE (x, t)-(r, t), , , p
        END SELECT
    NEXT
    $CHECKING:ON
END SUB


' Draws a string at x, y using the active font
SUB PSF1_DrawString (text AS STRING, x AS LONG, y AS LONG)
    $CHECKING:OFF
    SHARED __CurPSF AS PSF1Type
    DIM AS LONG uy, l, r, t, p, cidx, bc, pm, cp, sLen

    bc = _BACKGROUNDCOLOR
    pm = _PRINTMODE
    sLen = LEN(text)

    ' We will iterate through the whole text
    FOR cidx = 1 TO sLen
        cp = ASC(text, cidx) ' find the character to draw
        l = x + (cidx - 1) * __CurPSF.size.x ' calculate the starting x position for this character
        r = l + __CurPSF.size.x - 1 ' calculate right
        ' Next go through each scan line and draw those
        FOR uy = 1 TO __CurPSF.size.y
            ' Get the scan line and prepare it
            p = ASC(__CurPSF.bitmap, __CurPSF.size.y * cp + uy)
            p = 256 * (p + (256 * (p > 127)))
            ' Draw the scan line
            t = y + uy - 1
            SELECT CASE pm
                CASE 1
                    LINE (l, t)-(r, t), , , p
                CASE 2
                    LINE (l, t)-(r, t), , , NOT p
                CASE ELSE
                    LINE (l, t)-(r, t), bc
                    LINE (l, t)-(r, t), , , p
            END SELECT
        NEXT
    NEXT
    $CHECKING:ON
END SUB


' Returns the current font width
FUNCTION PSF1_GetFontWidth~%%
    $CHECKING:OFF
    SHARED __CurPSF AS PSF1Type
    PSF1_GetFontWidth = __CurPSF.size.x
    $CHECKING:ON
END FUNCTION


' Returns the current font height
FUNCTION PSF1_GetFontHeight~%%
    $CHECKING:OFF
    SHARED __CurPSF AS PSF1Type
    PSF1_GetFontHeight = __CurPSF.size.y
    $CHECKING:ON
END FUNCTION


' Return the onsreen length of a string in pixels
FUNCTION PSF1_GetDrawStringWidth& (text AS STRING)
    $CHECKING:OFF
    SHARED __CurPSF AS PSF1Type
    PSF1_GetDrawStringWidth = LEN(text) * __CurPSF.size.x
    $CHECKING:ON
END FUNCTION


' Set the active font
SUB PSF1_SetCurrentFont (psf AS PSF1Type)
    $CHECKING:OFF
    SHARED __CurPSF AS PSF1Type
    __CurPSF = psf
    $CHECKING:ON
END SUB


' Loads a PSF v1 font file from memory
' Note that this ignores the mode value
FUNCTION PSF1_LoadFontFromMemory%% (buffer AS STRING, psf AS PSF1Type)
    SHARED __CurPSF AS PSF1Type

    ' Check if we at least have a header
    IF LEN(buffer) < 5 THEN EXIT FUNCTION ' header is 4 bytes

    ' Check font magic id
    IF ASC(buffer, 1) <> __PSF1_MAGIC0 OR ASC(buffer, 2) <> __PSF1_MAGIC1 THEN EXIT FUNCTION

    ' Skip mode and read the height
    DIM i AS LONG: i = ASC(buffer, 4)

    ' Check font height
    IF i = 0 THEN EXIT FUNCTION

    psf.size.x = PSF1_FONT_WIDTH ' the width is always 8 for PSFv1
    psf.size.y = i ' change the font height
    psf.bitmap = MID$(buffer, 5, 256 * i) ' the bitmap data

    ' Set this as the default font if nothing is loaded
    IF __CurPSF.size.x = 0 OR __CurPSF.size.y = 0 OR LEN(__CurPSF.bitmap) = NULL THEN __CurPSF = psf

    PSF1_LoadFontFromMemory = _TRUE
END FUNCTION


' Loads a PSF v1 font file from disk
FUNCTION PSF1_LoadFontFromFile%% (fileName AS STRING, psf AS PSF1Type)
    PSF1_LoadFontFromFile = PSF1_LoadFontFromMemory(File_Load(fileName), psf)
END FUNCTION


' Changes the font height of the active font
' This will wipe out whatever bitmap the font already has
SUB PSF1_SetFontHeight (h AS _UNSIGNED _BYTE)
    SHARED __CurPSF AS PSF1Type
    __CurPSF.size.x = PSF1_FONT_WIDTH ' the width is always 8 for PSFv1
    __CurPSF.size.y = h ' change the font height
    __CurPSF.bitmap = STRING$(256 * __CurPSF.size.y, NULL) ' just allocate enough space for the bitmap

    ' Load default glyphs
    DIM i AS LONG: FOR i = 0 TO 255
        PSF1_SetGlyphDefaultBitmap i
    NEXT
END SUB


' Returns the entire bitmap of a glyph in a string
FUNCTION PSF1_GetGlyphBitmap$ (cp AS _UNSIGNED _BYTE)
    SHARED __CurPSF AS PSF1Type
    PSF1_GetGlyphBitmap = MID$(__CurPSF.bitmap, 1 + __CurPSF.size.y * cp, __CurPSF.size.y)
END FUNCTION


' Returns the entire font as a string
FUNCTION PSF1_GetFont$
    SHARED __CurPSF AS PSF1Type
    PSF1_GetFont = __CurPSF.bitmap
END FUNCTION


' Sets the entire bitmap of a glyph with bmp
SUB PSF1_SetGlyphBitmap (cp AS _UNSIGNED _BYTE, bmp AS STRING)
    SHARED __CurPSF AS PSF1Type
    MID$(__CurPSF.bitmap, 1 + __CurPSF.size.y * cp, __CurPSF.size.y) = bmp
END SUB


' Sets the entire font from a string
FUNCTION PSF1_SetFont%% (buffer AS STRING)
    SHARED __CurPSF AS PSF1Type

    DIM i AS LONG: i = LEN(buffer)

    ' Check some absurd cases
    IF i = 0 OR i MOD 256 <> 0 THEN EXIT FUNCTION

    ' Copy contents from the buffer
    __CurPSF.bitmap = buffer

    ' Adjust the font height
    __CurPSF.size.y = i \ 256
    __CurPSF.size.x = PSF1_FONT_WIDTH ' just in case this was not set

    PSF1_SetFont = _TRUE
END FUNCTION


' Set the glyph's bitmap to QB64's current font glyph
SUB PSF1_SetGlyphDefaultBitmap (cp AS _UNSIGNED _BYTE)
    SHARED __CurPSF AS PSF1Type

    DIM img AS LONG: img = _NEWIMAGE(_FONTWIDTH, _FONTHEIGHT, 256)
    IF img >= -1 THEN EXIT SUB ' leave if we failed to allocate the image

    DIM dst AS LONG: dst = _DEST ' save dest
    _DEST img ' set img as dest

    DIM f AS LONG: f = _FONT ' save the current font

    ' Select the best builtin font to use
    SELECT CASE __CurPSF.size.y
        CASE IS > 15
            _FONT 16

        CASE IS > 13
            _FONT 14

        CASE ELSE
            _FONT 8
    END SELECT

    _PRINTSTRING (0, 0), CHR$(cp) ' render the glyph to our image

    ' Find the starting x, y on the font bitmap where we should start to render
    DIM sx AS LONG: sx = __CurPSF.size.x \ 2 - _FONTWIDTH \ 2
    DIM sy AS LONG: sy = __CurPSF.size.y \ 2 - _FONTHEIGHT \ 2

    DIM src AS LONG: src = _SOURCE ' save the old source
    _SOURCE img ' change source to img

    ' Copy the QB64 glyph
    DIM AS LONG x, y, w, h
    w = _FONTWIDTH
    h = _FONTHEIGHT

    WHILE y < h
        x = 0
        WHILE x < w
            PSF1_SetGlyphPixel cp, sx + x, sy + y, POINT(x, y) <> 0 ' black
            x = x + 1
        WEND
        y = y + 1
    WEND

    _SOURCE src ' restore source
    _FONT f ' restore font
    _DEST dst
    _FREEIMAGE img ' free img
END SUB


' Return true if the pixel-bit at the glyphs x, y is set
FUNCTION PSF1_GetGlyphPixel%% (cp AS _UNSIGNED _BYTE, x AS LONG, y AS LONG)
    SHARED __CurPSF AS PSF1Type

    IF x < 0 OR x >= __CurPSF.size.x OR y < 0 OR y >= __CurPSF.size.y THEN EXIT FUNCTION

    PSF1_GetGlyphPixel = _READBIT(ASC(__CurPSF.bitmap, __CurPSF.size.y * cp + y + 1), __CurPSF.size.x - x - 1)
END FUNCTION


' Sets or unsets pixel at the glyphs x, y
SUB PSF1_SetGlyphPixel (cp AS _UNSIGNED _BYTE, x AS LONG, y AS LONG, b AS _BYTE)
    SHARED __CurPSF AS PSF1Type

    IF x < 0 OR x >= __CurPSF.size.x OR y < 0 OR y >= __CurPSF.size.y THEN EXIT SUB

    IF b THEN
        ASC(__CurPSF.bitmap, __CurPSF.size.y * cp + y + 1) = _SETBIT(ASC(__CurPSF.bitmap, __CurPSF.size.y * cp + y + 1), __CurPSF.size.x - x - 1)
    ELSE
        ASC(__CurPSF.bitmap, __CurPSF.size.y * cp + y + 1) = _RESETBIT(ASC(__CurPSF.bitmap, __CurPSF.size.y * cp + y + 1), __CurPSF.size.x - x - 1)
    END IF
END SUB


' Saves the current font to disk in PSF v1 format
' This does not check if the file exists or whatever and will happily overwrite it
' It is the caller's resposibility to check this stuff
' Note that this will set the mode value to 0
FUNCTION PSF1_SaveFont%% (fileName AS STRING)
    SHARED __CurPSF AS PSF1Type

    IF __CurPSF.size.x > 0 AND __CurPSF.size.y > 0 AND LEN(__CurPSF.bitmap) = 256 * __CurPSF.size.y THEN ' check if the font is valid
        ' Open the file for writing
        DIM hFile AS LONG: hFile = FREEFILE
        OPEN fileName FOR BINARY ACCESS WRITE AS hFile

        ' Write font id
        DIM buffer AS STRING: buffer = CHR$(__PSF1_MAGIC0) + CHR$(__PSF1_MAGIC1)
        PUT hFile, , buffer

        ' Write mode as zero
        buffer = CHR$(NULL)
        PUT hFile, , buffer

        ' Write font height
        buffer = CHR$(__CurPSF.size.y)
        PUT hFile, , buffer

        PUT hFile, , __CurPSF.bitmap ' write the font data

        CLOSE hFile

        PSF1_SaveFont = _TRUE
    END IF
END FUNCTION

'$INCLUDE:'File.bas'
