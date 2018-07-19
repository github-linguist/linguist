program TopRankPerGroup(output);

uses
  Classes, Math;

type
  TData = record
            name:   string;
	    ID:     string;
	    salary: longint;
	    dept:   string
	  end;
  PTData = ^TData;

const
  data: array [1..13] of TData =
    ( (name: 'Tyler Bennett';   ID: 'E10297'; salary: 32000; dept: 'D101'),
      (name: 'John Rappl';      ID: 'E21437'; salary: 47000; dept: 'D050'),
      (name: 'George Woltman';  ID: 'E00127'; salary: 53500; dept: 'D101'),
      (name: 'Adam Smith';      ID: 'E63535'; salary: 18000; dept: 'D202'),
      (name: 'Claire Buckman';  ID: 'E39876'; salary: 27800; dept: 'D202'),
      (name: 'David McClellan'; ID: 'E04242'; salary: 41500; dept: 'D101'),
      (name: 'Rich Holcomb';    ID: 'E01234'; salary: 49500; dept: 'D202'),
      (name: 'Nathan Adams';    ID: 'E41298'; salary: 21900; dept: 'D050'),
      (name: 'Richard Potter';  ID: 'E43128'; salary: 15900; dept: 'D101'),
      (name: 'David Motsinger'; ID: 'E27002'; salary: 19250; dept: 'D202'),
      (name: 'Tim Sampair';     ID: 'E03033'; salary: 27000; dept: 'D101'),
      (name: 'Kim Arlich';      ID: 'E10001'; salary: 57000; dept: 'D190'),
      (name: 'Timothy Grove';   ID: 'E16398'; salary: 29900; dept: 'D190')
    );

function CompareSalary(Item1, Item2: PTData): longint;
  begin
    CompareSalary := Item2^.salary - Item1^.salary;
  end;

var
  depts   : TStringList;
  deptList: Tlist;
  number, i, j: integer;

begin
  write ('Enter the number of ranks: ');
  readln (number);
  depts := TStringList.Create;
  depts.Sorted := true;
  depts.Duplicates := dupIgnore;
  for i := low(data) to high(data) do
    depts.Add(data[i].dept);

  for i := 0 to depts.Count - 1 do
  begin
    writeln;
    writeln('Department: ', depts.Strings[i]);
    deptList := TList.Create;
    for j := low(data) to high(data) do
      if data[j].dept = depts.Strings[i] then
        deptList.Add(@data[j]);
    deptList.Sort(TListSortCompare(@CompareSalary));
    for j := 0 to min(deptList.count, number) - 1 do
    begin
      write (PTData(deptList.Items[j])^.name, ', ');
      write ('ID: ', PTData(deptList.Items[j])^.ID, ', ');
      write ('Salary: ', PTData(deptList.Items[j])^.Salary);
      writeln;
    end;
    deptList.Destroy;
  end;
end.
