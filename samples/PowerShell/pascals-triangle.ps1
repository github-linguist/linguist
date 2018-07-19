$Infinity = 1
$NewNumbers = $null
$Numbers = $null
$Result = $null
$Number = $null
$Power = $args[0]

Write-Host $Power

For(
   $i=0;
   $i -lt $Infinity;
   $i++
   )
   {
    $Numbers = New-Object Object[] 1
    $Numbers[0] = $Power
   For(
      $k=0;
      $k -lt $NewNumbers.Length;
      $k++
      )
      {
       $Numbers = $Numbers + $NewNumbers[$k]
      }
   If(
     $i -eq 0
     )
     {
      $Numbers = $Numbers + $Power
     }
    $NewNumbers = New-Object Object[] 0
   Try
   {
   For(
      $j=0;
      $j -lt $Numbers.Length;
      $j++
      )
      {
       $Result = $Numbers[$j] + $Numbers[$j+1]
       $NewNumbers = $NewNumbers + $Result
      }
   }
   Catch [System.Management.Automation.RuntimeException]
   {
    Write-Warning "Value was too large for a Decimal. Script aborted."
    Break;
   }
   Foreach(
          $Number in $Numbers
          )
          {
          If(
            $Number.ToString() -eq "+unendlich"
            )
            {
             Write-Warning "Value was too large for a Decimal. Script aborted."
             Exit
            }
          }
    Write-Host $Numbers
    $Infinity++
   }
