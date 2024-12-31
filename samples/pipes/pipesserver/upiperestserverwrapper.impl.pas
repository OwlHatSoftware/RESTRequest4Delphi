unit upiperestserverwrapper.impl;

interface

uses
  System.JSON,
  upiperestserverwrapper.iface;

type
  TPipeRESTServerWrapper = class(TInterfacedObject, IPipeRESTServerWrapper)
  private
    FOnPostJSON: TOnPostJSONEvent;
    FOnGetJSON: TOnGetJSONEvent;
    FServerName: string;
    class function DoGetJSON(const AEndPoint: string): string;
    class function DoPostJSON(const AEndPoint, AString: string): string;
    function GetOnGetJSON: TOnGetJSONEvent;
    procedure SetOnGetJSON(const Value: TOnGetJSONEvent);
    function GetOnPostJSON: TOnPostJSONEvent;
    procedure SetOnPostJSON(const Value: TOnPostJSONEvent);
  public
    constructor Create;
    destructor Destroy; override;
    function Start(const APipeName: string): boolean;
    function Stop: boolean;
    function GetServerName: string;
    procedure BroadCastMessage(const AMessage: string); overload;
    procedure BroadCastMessage(const AJSONObject: TJSONObject); overload;
    property OnGetJSON: TOnGetJSONEvent read GetOnGetJSON write SetOnGetJSON;
    property OnPostJSON: TOnPostJSONEvent read GetOnPostJSON
      write SetOnPostJSON;
  end;

implementation

uses
  System.Classes,
  System.Types,
  System.AnsiStrings,
  System.SysUtils,
  upipetypes, superobject, supertypes, RESTRequest4D.Utils;

function InitPipeServer(PipeName: PAnsiChar; CallBack: TCallBackFunction)
  : PAnsiChar; register; stdcall; external('PipeServer.dll');
function StartPipeServer(): boolean; register; stdcall;
  external('PipeServer.dll');
function StopPipeServer(): boolean; register; stdcall;
  external('PipeServer.dll');
procedure GetConnectedPipeClients; register; stdcall;
  external('PipeServer.dll');
procedure BroadcastPipeServerMessage(Msg: PAnsiChar; Size: integer); register;
  stdcall; external('PipeServer.dll');
procedure PipeServerMessageToClient(Pipe: integer; Msg: PAnsiChar); register;
  stdcall; external('PipeServer.dll');
procedure DonePipeServer(); register; stdcall; external('PipeServer.dll');

procedure ParseDelimited(const sl: TStrings; const Value: string;
  const delimiter: string);
var
  dx: integer;
  ns: string;
  txt: string;
  delta: integer;
begin
  delta := Length(delimiter);
  txt := Value + delimiter;
  sl.BeginUpdate;
  sl.Clear;
  try
    while Length(txt) > 0 do
    begin
      dx := Pos(delimiter, txt);
      ns := Copy(txt, 0, dx - 1);
      sl.Add(ns);
      txt := Copy(txt, dx + delta, MaxInt);
    end;
  finally
    sl.EndUpdate;
  end;
end;

function CallBack(msgType: integer; var pipeID: integer; var answer: PAnsiChar;
  var Param: DWORD): boolean; stdcall;
var
  pid, i, v: integer;
  s, m: AnsiString;
  JSON, ReplyJSON, arrobj: ISuperObject;
  jsonarray: TSuperArray;
  p: DWORD;
  sl: TStringList;
  method: string;
  endpoint: string;
begin
  Result := True;
  m := '';
  pid := pipeID;
  s := System.AnsiStrings.StrPas(answer);
  p := Param;
  case msgType of
    MSG_PIPESENT:
      m := 'MSG_PIPESENT';
    MSG_PIPECONNECT:
      begin
        m := 'MSG_PIPECONNECT';
        ReplyJSON := TSuperObject.Create();
        ReplyJSON.s['method'] := 'GetClientID';
        ReplyJSON.i['ClientID'] := pid;
        PipeServerMessageToClient(pid, PAnsiChar(AnsiString(ReplyJSON.AsJSon())));
      end;
    MSG_PIPEDISCONNECT:
      begin
        m := 'MSG_PIPEDISCONNECT';
      end;
    MSG_PIPEMESSAGE:
      begin
        m := 'MSG_PIPEMESSAGE';
        JSON := TSuperObject.ParseString(PSOChar(WideString(s)), True);
        if not assigned(JSON) then
          raise Exception.Create('Incorrect JSON string!');
        method := JSON.GetS('method');
        endpoint := JSON.GetS('endpoint');
        s := JSON.GetS('message'); // store the received message
        ReplyJSON := TSuperObject.Create();
        ReplyJSON.s['method'] := method;
        ReplyJSON.s['endpoint'] := endpoint;
        if (method = Ord(TMethodRequest.mrGET).ToString) then
        begin
          ReplyJSON.s['message'] := TPipeRESTServerWrapper.DoGetJSON(endpoint);
        end;
        if (method = Ord(TMethodRequest.mrPOST).ToString) then
        begin
          ReplyJSON.s['message'] := TPipeRESTServerWrapper.DoPostJSON(endpoint, s);
        end;
        if (method = Ord(TMethodRequest.mrPUT).ToString) then
        begin

        end;
        if (method = Ord(TMethodRequest.mrPATCH).ToString) then
        begin

        end;
        if (method = Ord(TMethodRequest.mrDELETE).ToString) then
        begin

        end;
        PipeServerMessageToClient(pid, PAnsiChar(AnsiString(ReplyJSON.AsJSon())));
      end;
    MSG_PIPEERROR:
      m := 'MSG_PIPEERROR';
    MSG_GETPIPECLIENTS:
      begin
        m := 'MSG_GETPIPECLIENTS';
        sl := TStringList.Create;
        try
          ParseDelimited(sl, s, ';');
          ReplyJSON := SO();
          ReplyJSON.s['method'] := 'GetConnectedPipeClients';
          arrobj := SA([]);
          for i := 0 to sl.Count - 1 do
            if TryStrToInt(sl[i], v) then
              arrobj.i['ID'] := v;
          ReplyJSON.O['ClientIDs'] := arrobj;
          BroadcastPipeServerMessage(PAnsiChar(AnsiString(ReplyJSON.AsString)),
            Length(ReplyJSON.AsString));
        finally
          FreeAndNil(sl);
        end;
      end;
  else
    Result := False;
  end;
end;

{ TPipeRESTServerWrapper }

procedure TPipeRESTServerWrapper.BroadCastMessage(const AMessage: string);
begin
  BroadcastPipeServerMessage(PAnsiChar(AnsiString(AMessage)), Length(AMessage));
end;

procedure TPipeRESTServerWrapper.BroadCastMessage(
  const AJSONObject: TJSONObject);
begin
  BroadCastMessage(AJSONObject.ToString);
end;

constructor TPipeRESTServerWrapper.Create;
begin
  FServerName := '';
end;

destructor TPipeRESTServerWrapper.Destroy;
begin
  DonePipeServer();
  inherited;
end;

class function TPipeRESTServerWrapper.DoGetJSON(const AEndPoint: string): string;
var
  LJSONObj: TJSONObject;
begin
  Result := '';
  if not assigned(PipeRestServer) then
    Exit;
  LJSONObj := TJSONObject.Create;
  PipeRestServer.OnGetJSON(nil, AEndPoint, LJSONObj);
  try
    LJSONObj.AddPair('message', '200 OK');
    Result := LJSONObj.Format;
  finally
    LJSONObj.Free;
  end;
end;

class function TPipeRESTServerWrapper.DoPostJSON(const AEndPoint, AString: string): string;
var
  LSuccess: boolean;
begin
  Result := '';
  LSuccess := True;
  if not assigned(PipeRestServer) then
    Exit;
  try
    PipeRestServer.OnPostJSON(nil, AEndPoint, AString, LSuccess);
    //
    if LSuccess then
      Result := '{"message":"200 OK"}'
    else
      Result := '{"message":"500 Internal Server Error"}';
  except
    on E: Exception do
      Result := E.Message;
  end;
end;

function TPipeRESTServerWrapper.GetOnGetJSON: TOnGetJSONEvent;
begin
  Result := FOnGetJSON;
end;

function TPipeRESTServerWrapper.GetOnPostJSON: TOnPostJSONEvent;
begin
  Result := FOnPostJSON;
end;

function TPipeRESTServerWrapper.GetServerName: string;
begin
  Result := FServerName;
end;

procedure TPipeRESTServerWrapper.SetOnGetJSON(const Value: TOnGetJSONEvent);
begin
  FOnGetJSON := Value;
end;

procedure TPipeRESTServerWrapper.SetOnPostJSON(const Value: TOnPostJSONEvent);
begin
  FOnPostJSON := Value;
end;

function TPipeRESTServerWrapper.Start(const APipeName: string): boolean;
begin
  FServerName := System.AnsiStrings.StrPas
    (InitPipeServer(PAnsiChar(AnsiString(APipeName)), @CallBack));
  Result := StartPipeServer();
end;

function TPipeRESTServerWrapper.Stop: boolean;
begin
  Result := StopPipeServer();
  if not Result then
    DonePipeServer();
end;

end.
