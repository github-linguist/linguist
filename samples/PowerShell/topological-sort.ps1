#Input Data
$a=@"
des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee
dw01             ieee dw01 dware gtech
dw02             ieee dw02 dware
dw03             std synopsys dware dw03 dw02 dw01 ieee gtech
dw04             dw04 ieee dw01 dware gtech
dw05             dw05 ieee dware
dw06             dw06 ieee dware
dw07             ieee dware
dware            ieee dware
gtech            ieee gtech
ramlib           std ieee
std_cell_lib     ieee std_cell_lib
synopsys
"@
#Convert to Object[]
$c = switch ( $a.split([char] 10) ) {
    $_ {
        $b=$_.split(' ')
        New-Object PSObject -Property @{
            Library = $b[0]
            "Library Dependencies" = @( $( $b[1..($b.length-1)] | Where-Object { $_ -match '\w' } ) )
        }
    }
}
#Add pure dependencies
$c | ForEach-Object {
    $_."Library Dependencies" | Where-Object {
        $d=$_
        $(:andl foreach($i in $c) {
            if($d -match $i.Library) {
                $false
                break andl
            }
        }) -eq $null
    } | ForEach-Object {
        $c+=New-Object PSObject -Property @{
            Library=$_
            "Library Dependencies"=@()
        }
    }
}
#Associate with a dependency value
##Initial Dependency Value
$d = $c | Sort Library | Select-Object Library,"Library Dependencies",@{
    Name="Dep Value"
    Expression={
        1
    }
}
##Modify Dependency Value, perform check for incorrect dependency
##Dep Value is determined by a parent child relationship, if a library is a parent, all libraries dependant on it are children
for( $i=0; $i -lt $d.count; $i++ ) {
    $errmsg=""
    foreach( $j in ( 0..( $d.count - 1 ) | Where-Object { $_ -ne $i } ) ) {
        #Foreach other Child Library where this is a dependency, increase the Dep Value of the Child
        if( $( :orl foreach( $k in $d[$j]."Library Dependencies" ) {
            if( $k -match $d[$i].Library ) {
                foreach( $n in $d[$i]."Library Dependencies" ) {
                    if( $n -match $d[$j].Library ) {
                        $errmsg="Error Cyclic Dependency {0}<->{1}" -f $d[$i].Library, $d[$j].Library
                        break
                    }
                }
                $true
                break orl
            }
        } ) ) {
            #If the child has already been processed, increase the Dep Value of its children
            if( $j -lt $i ) {
                foreach( $l in ( 0..( $d.count - 1 ) | Where-Object { $_ -ne $j } ) ) {
                    if( $( :orl2 foreach( $m in $d[$l]."Library Dependencies" ) {
                        if( $m -match $d[$j].Library ) {
                            $true
                            break orl2
                        }
                    } ) ) {
                        $d[$l]."Dep Value"+=$d[$i]."Dep Value"
                    }
                }
            }
            $d[$j]."Dep Value"+=$d[$i]."Dep Value"
        }
        if( $errmsg -ne "" ) {
            $errmsg
            $d=$null
            break
        }
    }
}
#Sort and Display
if( $d ) {
    $d | Sort "Dep Value",Library | ForEach-Object {
        "{0,-14} LIBRARY DEPENDENCIES`n{1,-14} ====================" -f "LIBRARY", "======="
    } {
        "{0,-14} $($_."Library Dependencies")" -f $_.Library
    }
}
