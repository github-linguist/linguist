Interface
uses  crt,   { GetDir }
      graph; { function GetPixel }

type { integer numbers }
  { from unit bitmaps XPERT software production Tamer Fakhoury }
  _bit     = $00000000..$00000001; {number 1 bit   without sign = (0..1) }
  _byte    = $00000000..$000000FF; {number 1 byte  without sign = (0..255)}
  _word    = $00000000..$0000FFFF; {number 2 bytes without sign = (0..65 535)}
  _dWord   = $00000000..$7FFFFFFF; {number 4 bytes without sign = (0..4 294 967 296)}
  _longInt = $80000000..$7FFFFFFF; {number 4 bytes with sign
                                    = (-2 147 483 648..2 147 483 648}

  TbmpFileHeader =
  record
    ID: _word;             { Must be 'BM' =19778=$424D for windows }
    FileSize: _dWord;      { Size of this file in bytes }
    Reserved: _dWord;      { ??? }
    bmpDataOffset: _dword; { = 54 = $36 from begining of file to begining of bmp data }
  end;

  TbmpInfoHeader =
  record
    InfoHeaderSize: _dword;      { Size of Info header
                                   = 28h = 40 (decimal)
                                   for windows }
    Width,
    Height: _longInt;    { Width and Height of image in pixels }
    Planes,              { number of planes of bitmap }
    BitsPerPixel: _word; { Bits can be 1, 4, 8, 24 or 32 }
    Compression,
    bmpDataSize: _dword;    { in bytes rounded to the next 4 byte boundary }
    XPixPerMeter,           { horizontal resolution in pixels }
    YPixPerMeter: _longInt; { vertical }
    NumbColorsUsed,
    NumbImportantColors: _dword;   {= NumbColorUsed}
  end; { TbmpHeader = Record ... }

  T32Color =
  record { 4 byte = 32 bit }
    Blue:  byte;
    Green: byte;
    Red:   byte;
    Alfa:  byte
  end;

var directory,
    bmpFileName:    string;
    bmpFile:        file; { untyped file }
    bmpFileHeader:  TbmpFileHeader;
    bmpInfoHeader:  TbmpInfoHeader;
    color32:        T32Color;
    RowSizeInBytes: integer;
    BytesPerPixel:  integer;

const defaultBmpFileName = 'test';
      DefaultDirectory   = 'c:\bp\';
      DefaultExtension   = '.bmp';
      bmpFileHeaderSize  = 14;
      { compression specyfication }
      bi_RGB             = 0;  { compression }
      bi_RLE8            = 1;
      bi_RLE4            = 2;
      bi_BITFIELDS       = 3;

      bmp_OK          = 0;
      bmp_NotBMP      = 1;
      bmp_OpenError   = 2;
      bmp_ReadError   = 3;

Procedure CreateBmpFile32(directory: string; FileName: string;
                           iWidth, iHeight: _LongInt);

{************************************************}
Implementation  {-----------------------------}
{************************************************}

Procedure CreateBmpFile32(directory: string; FileName: string;
                           iWidth, iHeight: _LongInt);
  var
    x, y: integer;
  begin
    if directory = '' then
      GetDir(0, directory);
    if FileName = '' then
      FileName: = DefaultBmpFileName;
    { create a new file on a disk in a given directory with given name }
    Assign(bmpFile, directory + FileName + DefaultExtension);
    ReWrite(bmpFile, 1);

    { fill the headers }
    with bmpInfoHeader, bmpFileHeader do
    begin
      ID := 19778;
      InfoheaderSize := 40;
      width := iWidth;
      height := iHeight;
      BitsPerPixel := 32;
      BytesPerPixel := BitsPerPixel div 8;
      reserved := 0;
      bmpDataOffset := InfoHeaderSize + bmpFileHeaderSize;
      planes := 1;
      compression := bi_RGB;
      XPixPerMeter := 0;
      YPixPerMeter := 0;
      NumbColorsUsed := 0;
      NumbImportantColors := 0;

      RowSizeInBytes := (Width * BytesPerPixel); { only for >=8 bits per pixel }
      bmpDataSize := height * RowSizeinBytes;
      FileSize := InfoHeaderSize + bmpFileHeaderSize + bmpDataSize;

      { copy headers to disk file }
      BlockWrite(bmpFile, bmpFileHeader, bmpFileHeaderSize);
      BlockWrite(bmpFile, bmpInfoHeader, infoHeaderSize);

      { fill the pixel data area }
      for y := (height - 1) downto 0  do
      begin
	for x := 0 to (width - 1) do
	begin { Pixel(x,y) }
	  color32.Blue  := 255;
	  color32.Green := 0;
	  color32.Red   := 0;
	  color32.Alfa  := 0;
	  BlockWrite(bmpFile, color32, 4);
	end; { for x ... }
      end; { for y ... }
      Close(bmpFile);
    end; { with bmpInfoHeader, bmpFileHeader }
   end; { procedure }
