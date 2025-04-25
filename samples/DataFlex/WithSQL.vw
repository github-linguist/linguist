// the grid object  
Object oCJGrid1 is a cCJGrid
    Set Size to 192 517
    Set Location to 6 10

    Object oCJGridColumn1 is a cCJGridColumn
        Set piWidth to 300
        Set psCaption to "Name"
    End_Object

    Object oCJGridColumn1 is a cCJGridColumn
        Set piWidth to 200
        Set psCaption to "City"
    End_Object

    Object oCJGridColumn1 is a cCJGridColumn
        Set piWidth to 60
        Set psCaption to "Zip"
    End_Object        
End_Object

tDataSourceRow[] aDataSourceRows

// Fetch results into aDataSourceRows
Get SQLExecDirect of ghoSQLExecutor @SQL"SELECT           
    Name,  
    City,
    Zip
    FROM Customer" to aDataSourceRows

If (not(Err)) Begin
    //Initialize the grids i.e. fill
    Send InitializeData of oCJGrid1 aDataSourceRows
End
