Function SubSequence ( [Array] $S, [Boolean] $all=$false )
{
   $sc = $S.count
   if( $sc -gt ( 2 - [Int32] $all ) ) {
      [void] $sc--
      0..$sc | ForEach-Object {
         $gap = $_
         "$( $S[ $_ ] )"
         if( $gap -lt $sc )
         {
            SubSequence ( ( $gap + 1 )..$sc | Where-Object { $_ -ne $gap } ) ( ( $gap -ne 0 ) -or $all ) | ForEach-Object {
               [String]::Join( ',', ( ( [String]$_ ).Split(',') | ForEach-Object {
                  $lt = $true
               } {
                  if( $lt -and ( $_ -gt $gap ) )
                  {
                     $S[ $gap ]
                     $lt = $false
                  }
                  $S[ $_ ]
               } {
                  if( $lt )
                  {
                     $S[ $gap ]
                  }
               }
               ) )
            }
         }
      }
      #[String]::Join( ',', $S)
   } else {
      $S | ForEach-Object { [String] $_ }
   }
}

Function NonContinuous-SubSequence ( [Array] $S )
{
   $sc = $S.count
   if( $sc -eq 3 )
   {
      [String]::Join( ',', $S[ ( 0,2 ) ] )
   } elseif ( $sc -gt 3 ) {
      [void] $sc--
      $gaps = @()
      $gaps += ( ( NonContinuous-SubSequence ( 1..$sc ) ) | ForEach-Object {
         $gap1 = ",$_,"
         "0,{0}" -f ( [String]::Join( ',', ( 1..$sc | Where-Object { $gap1 -notmatch "$_," } ) ) )
      } )
      $gaps += 1..( $sc - 1 )
      2..( $sc - 1 ) | ForEach-Object {
         $gap2 = $_ - 1
         $gaps += ( ( SubSequence ( $_..$sc ) ) | ForEach-Object {
            "$gap2,$_"
         } )
      }
      #Write-Host "S $S gaps $gaps"
      $gaps | ForEach-Object {
         $gap3 = ",$_,"
         "$( 0..$sc | Where-Object { $gap3 -notmatch ",$_," } | ForEach-Object {
            $S[$_]
         } )" -replace ' ', ','
      }
   } else {
      $null
   }
}

( NonContinuous-SubSequence 'a','b','c','d','e' ) | Select-Object length, @{Name='value';Expression={ $_ } } | Sort-Object length, value | ForEach-Object { $_.value }
