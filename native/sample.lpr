{.$DEFINE DEBUG}

{$IFDEF DEBUG}program{$ELSE}library{$ENDIF} sample;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, sysutils, exporter, strutils;

exports
  // ios + pc
  dbOpen,
  dbClose,
  dbExecuteSQL,
  dbSelect,
  dbGetSelectResultCount,
  dbGetSelectResult,
  dbGetLastError,

  // android
  Java_com_sqlite_sample_NativeAPI_dbOpen,
  Java_com_sqlite_sample_NativeAPI_dbClose,
  Java_com_sqlite_sample_NativeAPI_dbExecuteSQL,
  Java_com_sqlite_sample_NativeAPI_dbSelect,
  Java_com_sqlite_sample_NativeAPI_dbGetSelectResultCount,
  Java_com_sqlite_sample_NativeAPI_dbGetSelectResult,
  Java_com_sqlite_sample_NativeAPI_dbGetLastError;

{$IFDEF DEBUG}
var
  path: string;
  b: Boolean;
  count: Integer;
  r: TDemoRec;
  i: Integer;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  path := ExtractFilePath(ParamStr(0)) + 'demo.db';
  b := dbOpen(PChar(path));
  WriteLn(Format('Open Database => %s', [IfThen(b, 'TRUE', 'FLASE')]));
  b := dbSelect(PChar(path), 'select * from user');
  WriteLn(Format('Select => %s', [IfThen(b, 'TRUE', 'FLASE')]));

  count:= dbGetSelectResultCount(PChar(path));
  WriteLn(Format('Select Rows => %d', [count]));
  for i := 0 to count - 1 do begin
    r := dbGetSelectResult(PChar(path), i);
    WriteLn(Format('Data %d => {id => %d, name => %s}', [i, r.AId, string(r.AName)]));
  end;

  b := dbClose(PChar(path));
  WriteLn(Format('Close Database => %s', [IfThen(b, 'TRUE', 'FLASE')]));
  {$ENDIF}
end.

