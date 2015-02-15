type
  PLink = ^TLink;
  TLink = record
    FNext: PLink;
    FData: integer;
  end;
