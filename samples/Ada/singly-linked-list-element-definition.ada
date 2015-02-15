type Link;
type Link_Access is access Link;
type Link is record
   Next : Link_Access := null;
   Data : Integer;
end record;
