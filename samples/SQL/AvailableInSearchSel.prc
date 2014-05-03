IF EXISTS (SELECT * FROM DBO.SYSOBJECTS WHERE ID = OBJECT_ID(N'dbo.AvailableInSearchSel') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
    DROP PROCEDURE dbo.AvailableInSearchSel
GO
CREATE Procedure AvailableInSearchSel
AS

    SELECT    '-1',
            'Select...'
    UNION ALL
    SELECT    '1',
            'Yes'
    UNION ALL
    SELECT    '0',
            'No'
GO
IF DB_NAME() = 'Diebold' BEGIN
    GRANT EXECUTE ON dbo.AvailableInSearchSel TO [rv]
END
GO
