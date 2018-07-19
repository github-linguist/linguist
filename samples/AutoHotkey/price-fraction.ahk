; Submitted by MasterFocus --- http://tiny.cc/iTunis

Loop
{
  InputBox, OutputVar, Price Fraction Example, Insert the value to be rounded.`n* [ 0 < value < 1 ]`n* Press ESC or Cancel to exit, , 200, 150
  If ErrorLevel
    Break
  MsgBox % "Input: " OutputVar "`nResult: " PriceFraction( OutputVar )
}

;-----------------------------------------

PriceFraction( p_Input )
{

  If p_Input is not float ; returns 0 if input is not a float
    Return 0

  If ( ( p_Input <= 0 ) OR ( p_Input >= 1 ) ) ; returns 0 is input is out of range
    Return 0

  ; declaring the table (arbitrary delimiters in use are '§' and '|')
  l_List := "0.06|0.10§0.11|0.18§0.16|0.26§0.21|0.32§0.26|0.38§0.31|0.44§0.36|0.50§0.41|0.54§0.46|0.58§0.51|0.62§0.56|0.66§0.61|0.70§0.66|0.74§0.71|0.78§0.76|0.82§0.81|0.86§0.86|0.90§0.91|0.94§0.96|0.98§1.01|1.00"

  Loop, Parse, l_List, § ; retrieves each field (delimited by '§')
  {
    StringSplit, l_Array, A_LoopField, | ; splits current field (using delimiter '|')
    If ( p_Input <= l_Array1 )
      Return l_Array2 ; returns the second value if input <= first value
  }

  Return 0 ; returns 0, indicating failure (shouldn't be reached though)

}
