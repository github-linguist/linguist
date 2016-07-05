$c = New-Object Net.WebClient
$words = -split ($c.DownloadString('http://www.puzzlers.org/pub/wordlists/unixdict.txt'))
$top_anagrams = $words `
    | ForEach-Object {
          $_ | Add-Member -PassThru NoteProperty Characters `
                   (-join (([char[]] $_) | Sort-Object))
      } `
    | Group-Object Characters `
    | Group-Object Count `
    | Sort-Object Count `
    | Select-Object -First 1

$top_anagrams.Group | ForEach-Object { $_.Group -join ', ' }
