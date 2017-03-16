unit exporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JNI2, math;

type
  TDemoRec = record
    AId: Integer;
    AName: PChar;
  end;

// ios + pc
function dbOpen(APath: PChar): Boolean; cdecl;
function dbClose(APath: PChar): Boolean; cdecl;
function dbExecuteSQL(APath: PChar; ASQL: PChar): Boolean; cdecl;
function dbSelect(APath: PChar; ASQL: PChar): Boolean; cdecl;
function dbGetSelectResultCount(APath: PChar): Integer; cdecl;
function dbGetSelectResult(APath: PChar; AIndex: Integer): TDemoRec; cdecl;
function dbGetLastError(): PChar; cdecl;

// android

function Java_com_sqlite_sample_NativeAPI_dbOpen(env: PJNIEnv; obj: jobject; APath: jstring): jboolean; stdcall;
function Java_com_sqlite_sample_NativeAPI_dbClose(env: PJNIEnv; obj: jobject; APath: jstring): jboolean; stdcall;
function Java_com_sqlite_sample_NativeAPI_dbExecuteSQL(env: PJNIEnv; obj: jobject; APath: jstring; ASQL: jstring): jboolean; stdcall;
function Java_com_sqlite_sample_NativeAPI_dbSelect(env: PJNIEnv; obj: jobject; APath: jstring; ASQL: jstring): jboolean; stdcall;
function Java_com_sqlite_sample_NativeAPI_dbGetSelectResultCount(env: PJNIEnv; obj: jobject; APath: jstring): jint; stdcall;
function Java_com_sqlite_sample_NativeAPI_dbGetSelectResult(env: PJNIEnv; obj: jobject; APath: jstring; AIndex: jint): jobject; stdcall;
function Java_com_sqlite_sample_NativeAPI_dbGetLastError(env: PJNIEnv; obj: jobject): jstring; stdcall;

implementation

uses
  sqliteData;

function dbOpen(APath: PChar): Boolean; cdecl;
var
  database: TSQLite;
  AError: string;
begin
  Result := False;
  if (not DatabaseExists(string(APath))) then begin
    database := TSQLite.Create;
    Result := database.Open(string(APath), AError);
    dbs.AddObject(string(APath), database);
  end;
end;

function dbClose(APath: PChar): Boolean; cdecl;
var
  database: TSQLite;
  AError: string;
begin
  Result := False;
  if (DatabaseExists(string(APath))) then begin
    database := GetDatabase(string(APath));
    Result := database.Close(string(APath), AError);
    database.Free;
    dbs.Delete(dbs.IndexOf(string(APath)));
  end;
end;

function dbExecuteSQL(APath: PChar; ASQL: PChar): Boolean; cdecl;
var
  database: TSQLite;
  AError: string;
begin
  Result := False;
  if (DatabaseExists(string(APath))) then begin
    database := GetDatabase(string(APath));
    Result := database.ExecuteSQL(string(ASQL), AError);
  end;
end;

function dbSelect(APath: PChar; ASQL: PChar): Boolean; cdecl;
var
  database: TSQLite;
  AError: string;
begin
  Result := False;
  if (DatabaseExists(string(APath))) then begin
    database := GetDatabase(string(APath));
    Result := database.Select(string(ASQL), AError);
  end;
end;

function dbGetSelectResultCount(APath: PChar): Integer; cdecl;
var
  database: TSQLite;
begin
  Result := -1;
  if (DatabaseExists(string(APath))) then begin
    database := GetDatabase(string(APath));
    Result := database.Query.RecordCount;
  end;
end;

function dbGetSelectResult(APath: PChar; AIndex: Integer): TDemoRec; cdecl;
var
  database: TSQLite;
  tmp: string;
begin
  Inc(AIndex);
  Result.AId:= -1;
  Result.AName:= nil;
  if (DatabaseExists(string(APath))) then begin
    database := GetDatabase(string(APath));
    if (database.Query.Active) then begin
      if (database.Query.RecordCount >= AIndex) then begin
        database.Query.RecNo:= AIndex;
        Result.AId:= database.Query.FieldByName('id').AsInteger;
        tmp := database.Query.FieldByName('name').AsString;
        Result.AName:= StrAlloc(tmp.Length);
        strcopy(Result.AName, PChar(tmp));
      end;
    end;
  end;
end;

function dbGetLastError: PChar; cdecl;
begin
  Result := StrAlloc(lastError.Length);
  strcopy(Result, PChar(lastError));
end;

function Java_com_sqlite_sample_NativeAPI_dbOpen(env: PJNIEnv;
  obj: jobject; APath: jstring): jboolean; stdcall;
var
  ret: Boolean;
begin
  ret := dbOpen(PChar(TJNIEnv.JStringToString(env, APath)));
  Exit(ifthen(ret, JNI_TRUE, JNI_FALSE));
end;

function Java_com_sqlite_sample_NativeAPI_dbClose(env: PJNIEnv;
  obj: jobject; APath: jstring): jboolean; stdcall;
var
  ret: Boolean;
begin
  ret := dbClose(PChar(TJNIEnv.JStringToString(env, APath)));
  Exit(ifthen(ret, JNI_TRUE, JNI_FALSE));
end;

function Java_com_sqlite_sample_NativeAPI_dbExecuteSQL(env: PJNIEnv;
  obj: jobject; APath: jstring; ASQL: jstring): jboolean; stdcall;
var
  ret: Boolean;
begin
  ret := dbExecuteSQL(PChar(TJNIEnv.JStringToString(env, APath)),
    PChar(TJNIEnv.JStringToString(env, ASQL)));
  Exit(ifthen(ret, JNI_TRUE, JNI_FALSE));
end;

function Java_com_sqlite_sample_NativeAPI_dbSelect(env: PJNIEnv;
  obj: jobject; APath: jstring; ASQL: jstring): jboolean; stdcall;
var
  ret: Boolean;
begin
  ret := dbSelect(PChar(TJNIEnv.JStringToString(env, APath)),
    PChar(TJNIEnv.JStringToString(env, ASQL)));
  Exit(ifthen(ret, JNI_TRUE, JNI_FALSE));
end;

function Java_com_sqlite_sample_NativeAPI_dbGetSelectResultCount(
  env: PJNIEnv; obj: jobject; APath: jstring): jint; stdcall;
begin
  Result := dbGetSelectResultCount(PChar(TJNIEnv.JStringToString(env, APath)));
end;

function Java_com_sqlite_sample_NativeAPI_dbGetSelectResult(
  env: PJNIEnv; obj: jobject; APath: jstring; AIndex: jint): jobject; stdcall;
var
  cls: jclass;
  mInit: jmethodID;
  r: TDemoRec;
begin
  r := dbGetSelectResult(PChar(TJNIEnv.JStringToString(env, APath)), AIndex);
  cls := env^^.FindClass(env, 'com/hujiang/sqlite/sample/DemoRec');
  mInit:= env^^.GetMethodID(env, cls, '<init>', '(ILjava/lang/String;)V');
  Result := env^^.NewObjectA(env, cls, mInit, TJNIEnv.ArgsToJValues(env, [r.AId, string(r.AName)]));
end;

function Java_com_sqlite_sample_NativeAPI_dbGetLastError(env: PJNIEnv;
  obj: jobject): jstring; stdcall;
begin
  Result := TJNIEnv.StringToJString(env, lastError);
end;

end.

