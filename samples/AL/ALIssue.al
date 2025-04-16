table 70990099 ALIssue
{
	//Sample code from Arend-Jan Kauffmann's AL Samples repo:  https://github.com/ajkauffmann/ALCodeSamples
    fields
    {
        field(1;id;Integer)
        {
            CaptionML=ENU='ID';
        }
        field(2;number;Integer)
        {
            CaptionML=ENU='Number';
        }
        field(3;title;text[250])
        {
            CaptionML=ENU='Title';
        }
        field(5;created_at;DateTime)
        {
            CaptionML=ENU='Created at';
        }
        field(6;user;text[50])
        {
            CaptionML=ENU='User';
        }
        field(7;state;text[30])
        {
            CaptionML=ENU='State';
        }
        field(8;html_url;text[250])
        {
            CaptionML=ENU='URL';
        }
    }

    keys
    {
        key(PK;id)
        {
            Clustered = true;
        }
    }

    procedure RefreshIssues();
    var
        RefreshALIssues :Codeunit RefreshALIssueCode;
    begin
        RefreshALIssues.Refresh();
    end;

}