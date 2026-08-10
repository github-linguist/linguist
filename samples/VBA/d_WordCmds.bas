Global ICFWD As ICFWRD   'required to have ICF_FN class module as well
Global ICF As ICFXL

Sub Test456()
    Set ICFWD = New ICFWRD    'required to run intialize scripts at beginning of class
  
    'setup
'    ICFWD.WordDocument_SelectActive
    ICFWD.WordDocument_Add "C:\Documents and Settings\16955\Desktop\test", "newdoc", "doc"
    ICFWD.WordCloseFile False
    ICFWD.WordOptionsManual
    ICFWD.WordVisible
    ICFWD.PageMargins 2, 2, 1, 1
    ICFWD.PageLandscape
    
    ICFWD.AddPictureInline "C:\Documents and Settings\16955\My Documents\My Pictures\mo_100107c.jpg", 0.5, True
    ICFWD.AddPictureShape "C:\Documents and Settings\16955\My Documents\My Pictures\mo_100107c.jpg", 0.5, True
    
    'exit
    ICFWD.WordVisible
    ICFWD.SaveFile
    ICFWD.WordOptionsAutomatic
    ICFWD.WordCloseFile False
    
End Sub



'Code used to print something to Word
Private Sub CommandCommands()
      
    'DEBUG W/ OPEN DOCUMENT
    Set appWD = Word.Application
    
    '/----------------------------------\
    '|                                  |
    '|    GENERAL FORMATTING OF DOC     |
    '|                                  |
    '\----------------------------------/
                    
    'STANDARDIZE PARAGRAPH FORMAT (REMOVE EXTRA SPACES, ETC)
    StandardParagraphFormat
    
    '/----------------------------------\
    '|                                  |
    '|   PRINTING AND FORMATTING TEXT   |
    '|                                  |
    '\----------------------------------/
    'Print Word
    WRD_PrintBoldText "Example header: "
    WRD_PrintText ExcelWB.Sheets("Appendix Tables").Range("R24") & ", [ADD STATE]"
    WRD_NextLine
    WRD_PageBreak
            
    'FORMATTING
    appWD.Selection.Style = "Normal"
    appWD.Selection.Font.Size = 12
    appWD.Selection.Font.Bold = True
    appWD.Selection.Font.Italic = True
    appWD.Selection.Font.Name = "Calibri"
    appWD.Selection.ParagraphFormat.Alignment = wdAlignParagraphCenter
    
    '/----------------------------------\
    '|                                  |
    '|      TABLES AND TABLE EDITS      |
    '|                                  |
    '\----------------------------------/
        
    'CENTER TABLE
    appWD.Selection.Tables(1).Rows.Alignment = wdAlignRowCenter
    
    'TABLE FORMATTING
    appWD.Selection.Tables(1).Select
    StandardParagraphFormat
    
    'MERGE CELLS
    appWD.Selection.Tables(1).Rows(1).Select
    appWD.Selection.Cells.Merge
    
    'COL WIDTH
    appWD.Selection.Tables(1).Columns(1).SetWidth ColumnWidth:=Application.InchesToPoints(1.5), rulerstyle:=wdAdjustNone
    appWD.Selection.Tables(1).Cell(2, 1).SetWidth ColumnWidth:=Application.InchesToPoints(0.35), rulerstyle:=wdAdjustNone
    
    'ROW HEIGHT
    appWD.Selection.Tables(1).Cell(2, j).SetHeight RowHeight:=Application.InchesToPoints(1), HeightRule:=wdRowHeightExactly
    
    'VERTICAL CENTER TEXT
    appWD.Selection.Tables(1).Cell(2, j).VerticalAlignment = wdCellAlignVerticalCenter
    
    'HORIZONTAL ALIGN TEXT
    appWD.Selection.Tables(1).Rows.Alignment = wdAlignParagraphLeft
    appWD.Selection.Tables(1).Rows.Alignment = wdAlignRowCenter
    
    'REMOVE TABLE INDENTS
    appWD.Selection.Tables(1).Rows.LeftIndent = Application.InchesToPoints(0)
    
    'SHADE SELECTION
    appWD.Selection.Shading.BackgroundPatternColor = -570392321
    
    'CLEAR BORDERS IN TABLE
    WRD_ClearBorders
    
    'ADD BORDER BACK IN TABLE
    appWD.Selection.Borders(wdBorderBottom).Visible = True
    
    '/----------------------------------\
    '|                                  |
    '|     CHARTS AND FIGURE EDITS      |
    '|                                  |
    '\----------------------------------/
    
    'COPY ONE CHART FROM EXCEL TO WORD
    Set ChartPaste = ExcelWB.Sheets("Graphs").ChartObjects("Math")
    ChartPaste.ChartArea.AutoScaleFont = False 'DON'T RESIZE TEXT AUTOMATICALLY
    ChartPaste.Copy
    appWD.Selection.Paste
    appWD.Selection.ParagraphFormat.Alignment = wdAlignParagraphCenter 'CENTER CHART
    
    ' COPY MULTIPLE CHARTS FROM EXCEL TO WORD
    For Each ChartID In ActiveSheet.ChartObjects
        ChartID.ChartArea.AutoScaleFont = False                     'stop resizing of text in chart
        ChartID.Copy                                                'copy chart
        appWD.Selection.Paste                                       'paste into MS Word
        appWD.Selection.EndKey unit:=wdStory
        WRD_NextLine
    Next
    
    '/----------------------------------------\
    '|                                        |
    '|  PASTE CHART FROM EXCEL, EDIT IN WORD  |
    '|                                        |
    '\----------------------------------------/
    
End Sub