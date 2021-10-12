page 70990099 ALIssueList
{
	//Sample code from Arend-Jan Kauffmann's AL Samples repo:  https://github.com/ajkauffmann/ALCodeSamples	
	
    PageType = List;
    SourceTable = ALIssue;
    CaptionML=ENU='AL Issues';
    Editable = false;
    SourceTableView=order(descending);

    layout
    {
        area(content)
        {
            repeater(General)
            {
                field(Number;number) {}
                field(Title;title) {}
                field(CreatedAt;created_at) {}
                field(User;user) {}
                field(State;state) {}
                field(URL;html_url) 
                {
                    ExtendedDatatype=URL;
                }
            }
        }
    }

    actions
    {
        area(processing)
        {
            action(RefreshALIssueList)
            {
                CaptionML=ENU='Refresh Issues';
                Promoted=true;
                PromotedCategory=Process;
                Image=RefreshLines;
                trigger OnAction();
                begin
                    RefreshIssues();
                    CurrPage.Update;
                    if FindFirst then;
                end;
            }
        }
    }

    trigger OnOpenPage();
    begin
        //RefreshIssues();
        //if FindFirst then;
    end;
}