unit sqliteData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, fgl;

type

  { TSQLite }

  TSQLite = class
  private
    FDatabase: TSQLite3Connection;
    FQuery: TSQLQuery;
    FTransaction: TSQLTransaction;
  public
    constructor Create;
    destructor Destroy; override;
    function Open(APath: string; var AError: string): Boolean;
    function Close(APath: string; var AError: string): Boolean;
    function ExecuteSQL(ASQL: string; var AError: string): Boolean; overload;
    function ExecuteSQL(ASQL: string; AParam: array of const; var AError: string): Boolean; overload;
    function Insert(ASQL: string; var AError: string): Boolean; overload;
    function Insert(ASQL: string; AParam: array of const; var AError: string): Boolean; overload;
    function Update(ASQL: string; var AError: string): Boolean; overload;
    function Update(ASQL: string; AParam: array of const; var AError: string): Boolean; overload;
    function Delete(ASQL: string; var AError: string): Boolean; overload;
    function Delete(ASQL: string; AParam: array of const; var AError: string): Boolean; overload;
    function Select(ASQL: string; var AError: string): Boolean; overload;
    function Select(ASQL: string; AParam: array of const; var AError: string): Boolean; overload;
  published
    property Database: TSQLite3Connection read FDatabase write FDatabase;
    property Transaction: TSQLTransaction read FTransaction write FTransaction;
    property Query: TSQLQuery read FQuery write FQuery;
  end;

var
  dbs: TStringList;
  lastError: string = '';

function DatabaseExists(APath: string): Boolean;
function GetDatabase(APath: string): TSQLite;

implementation

const
  ERROR_COMMAND = 'Command error';

function DatabaseExists(APath: string): Boolean;
begin
  Result := dbs.IndexOf(APath) <> -1;
end;

function GetDatabase(APath: string): TSQLite;
var
  idx: Integer;
begin
  Result := nil;
  if (DatabaseExists(APath)) then Result := TSQLite(dbs.Objects[dbs.IndexOf(APath)]);
end;

{ TSQLite }

constructor TSQLite.Create;
begin
  FDatabase := TSQLite3Connection.Create(nil);
  FDatabase.CharSet:= 'utf-8';
  FTransaction := TSQLTransaction.Create(nil);
  FQuery := TSQLQuery.Create(nil);
  FQuery.Options:= [sqoKeepOpenOnCommit, sqoAutoApplyUpdates, sqoAutoCommit];
  FDatabase.Transaction := FTransaction;
  FQuery.Transaction := FTransaction;
  FQuery.DataBase := FDatabase;
end;

destructor TSQLite.Destroy;
begin
  FQuery.Transaction := nil;
  FDatabase.Transaction := nil;
  FTransaction.Free;
  FQuery.Free;
  FDatabase.Free;
  inherited Destroy;
end;

function TSQLite.Open(APath: string; var AError: string): Boolean;
begin
  FDatabase.DatabaseName:= APath;
  try
    FDatabase.Open;
    lastError := '';
    Exit(True);
  except
    on E: Exception do begin
      AError:= e.Message;
      lastError := AError;
      Exit(False);
    end;
  end;
end;

function TSQLite.Close(APath: string; var AError: string): Boolean;
begin
  try
    FDatabase.Close(True);
    lastError := '';
    Exit(True);
  except
    on E: Exception do begin
      AError:= e.Message;
      lastError := AError;
      Exit(False);
    end;
  end;
end;

function TSQLite.ExecuteSQL(ASQL: string; var AError: string): Boolean;
begin
  FQuery.Close;
  FQuery.SQL.Text:= ASQL;
  try
    FQuery.ExecSQL;
    lastError := '';
    Exit(True);
  except
    on E: Exception do begin
      AError:= e.Message;
      lastError := AError;
      Exit(False);
    end;
  end;
end;

function TSQLite.ExecuteSQL(ASQL: string; AParam: array of const;
  var AError: string): Boolean;
begin
  Exit(ExecuteSQL(Format(ASQL, AParam), AError));
end;

function TSQLite.Insert(ASQL: string; var AError: string): Boolean;
begin
  if (not ASQL.ToLower.StartsWith('insert')) then begin
    AError:= ERROR_COMMAND;
    lastError := AError;
    Exit(False);
  end;
  Exit(ExecuteSQL(ASQL, AError));
end;

function TSQLite.Insert(ASQL: string; AParam: array of const; var AError: string
  ): Boolean;
begin
  Exit(Insert(Format(ASQL, AParam), AError));
end;

function TSQLite.Update(ASQL: string; var AError: string): Boolean;
begin
  if (not ASQL.ToLower.StartsWith('update')) then begin
    AError:= ERROR_COMMAND;
    lastError := AError;
    Exit(False);
  end;
  Exit(ExecuteSQL(ASQL, AError));
end;

function TSQLite.Update(ASQL: string; AParam: array of const; var AError: string
  ): Boolean;
begin
  Exit(Update(Format(ASQL, AParam), AError));
end;

function TSQLite.Delete(ASQL: string; var AError: string): Boolean;
begin
  if (not ASQL.ToLower.StartsWith('delete')) then begin
    AError:= ERROR_COMMAND;
    lastError := AError;
    Exit(False);
  end;
  Exit(ExecuteSQL(ASQL, AError));
end;

function TSQLite.Delete(ASQL: string; AParam: array of const; var AError: string
  ): Boolean;
begin
  Exit(Delete(Format(ASQL, AParam), AError))
end;

function TSQLite.Select(ASQL: string; var AError: string): Boolean;
begin
  if (not ASQL.ToLower.StartsWith('select')) then begin
    AError:= ERROR_COMMAND;
    Exit(False);
  end;
  FQuery.Close;
  FQuery.SQL.Text:= ASQL;
  try
    FQuery.Open;
    lastError := '';
    Exit(True);
  Except
    on E: Exception do begin
      AError:= e.Message;
      lastError := AError;
      Exit(False);
    end;
  end;
end;

function TSQLite.Select(ASQL: string; AParam: array of const; var AError: string
  ): Boolean;
begin
  Exit(Select(Format(ASQL, AParam), AError));
end;

initialization
  dbs:= TStringList.Create;

finalization
  dbs.Free;

end.

