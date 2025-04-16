CREATE PROCEDURE [dbo].[ins_SomeObject]
    @ObjectID INT,
    @Date DATETIME,
    @ObjectName NVARCHAR(20),
    @ObjectValue FLOAT
AS
BEGIN
    INSERT INTO [dbo].[ObjectTable]
        (
        ObjectID,
        [Date],
        ObjectName,
        ObjectValue
        )
    OUTPUT
    INSERTED.IdentityID
    VALUES
        (
            @ObjectID,
            @Date,
            @ObjectName,
            @ObjectValue
        )
END