function New-Employee ($Name, $ID, $Salary, $Department) {
    New-Object PSObject `
        | Add-Member -PassThru NoteProperty EmployeeName $Name `
        | Add-Member -PassThru NoteProperty EmployeeID $ID `
        | Add-Member -PassThru NoteProperty Salary $Salary `
        | Add-Member -PassThru NoteProperty Department $Department
}

$data = (New-Employee 'Tyler Bennett'    E10297  32000  D101),
        (New-Employee 'John Rappl'       E21437  47000  D050),
        (New-Employee 'George Woltman'   E00127  53500  D101),
        (New-Employee 'Adam Smith'       E63535  18000  D202),
        (New-Employee 'Claire Buckman'   E39876  27800  D202),
        (New-Employee 'David McClellan'  E04242  41500  D101),
        (New-Employee 'Rich Holcomb'     E01234  49500  D202),
        (New-Employee 'Nathan Adams'     E41298  21900  D050),
        (New-Employee 'Richard Potter'   E43128  15900  D101),
        (New-Employee 'David Motsinger'  E27002  19250  D202),
        (New-Employee 'Tim Sampair'      E03033  27000  D101),
        (New-Employee 'Kim Arlich'       E10001  57000  D190),
        (New-Employee 'Timothy Grove'    E16398  29900  D190)

function Get-TopRank ($n) {
    $data `
        | Group-Object Department `
        | ForEach-Object {
              $_.Group `
                  | Sort-Object Salary -Descending `
                  | Select-Object -First $n
          } `
        | Format-Table -GroupBy Department
}
