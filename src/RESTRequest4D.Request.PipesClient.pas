unit RESTRequest4D.Request.PipesClient;

interface

uses
  WinApi.Windows, Classes, SysUtils, Generics.Collections,
  RESTRequest4D.Request.Contract,
  RESTRequest4D.Response.Contract,
  RESTRequest4D.Utils,
  RESTRequest4D.Request.Adapter.Contract,
  RESTRequest4D.Response.PipesClient,
{$IFDEF FPC}
  fpjson, fpjsonrtti, base64;
{$ELSE}
System.Json,
  System.NetEncoding,
  REST.Json;
{$ENDIF}

type
  TFile = class
  private
    FFileStream: TStream;
    FFileName: string;
    FContentType: string;
  public
    constructor Create(const AFileStream: TStream; const AFileName: string;
      const AContentType: string); overload;
    destructor Destroy; override;
  end;

  TRequestPipes = class(TInterfacedObject, IRequest)
  private
    FServerName, FNamedPipe: string;
    FAdapters: TArray<IRequestAdapter>;
    FFiles: TDictionary<string, TFile>;
    FStreamSend: TStream;
    procedure ExecuteRequest(const AMethod: TMethodRequest);
    function Adapters(const AAdapter: IRequestAdapter): IRequest; overload;
    function Adapters(const AAdapters: TArray<IRequestAdapter>)
      : IRequest; overload;
    function Adapters: TArray<IRequestAdapter>; overload;
    function PipeServer(const AServerName, ANamedPipe: string): IRequest;
    function OnBeforeExecute(const AOnBeforeExecute
      : TRR4DCallbackOnBeforeExecute): IRequest;
    function OnAfterExecute(const AOnAfterExecute: TRR4DCallbackOnAfterExecute)
      : IRequest;
    function Get: IResponse;
    function Post: IResponse;
    function Put: IResponse;
    function Delete: IResponse;
    function Patch: IResponse;
    function FullRequestURL(const AIncludeParams: Boolean = True): string;
    function ClearBody: IRequest;
    function AddParam(const AName, AValue: string): IRequest;
    function AddHeader(const AName, AValue: string): IRequest;
    function AddBody(const AContent: string): IRequest; overload;
    function AddBody(const AContent: TJSONObject; const AOwns: Boolean = True)
      : IRequest; overload;
    function AddBody(const AContent: TJSONArray; const AOwns: Boolean = True)
      : IRequest; overload;
    function AddBody(const AContent: TObject; const AOwns: Boolean = True)
      : IRequest; overload;
    function AddBody(const AContent: TStream; const AOwns: Boolean = True)
      : IRequest; overload;
    function AddFile(const AFieldName: string; const AFileName: string;
      const AContentType: string = ''): IRequest; overload;
    function AddFile(const AFieldName: string; const AValue: TStream;
      const AFileName: string = ''; const AContentType: string = '')
      : IRequest; overload;
  protected
    procedure DoAfterExecute(const Sender: TObject;
      const AResponse: IResponse); virtual;
  public
    class var FResponse: IResponse;
    class var FConnected: Boolean;
    class var FID: integer;
    constructor Create;
    class function New: IRequest;
    destructor Destroy; override;
  end;

implementation

uses upipetypes, superobject, supertypes;

function InitPipeClient(PipeName: PAnsiChar; CallBack: TCallBackFunction)
  : PAnsiChar; register; stdcall; external('PipeClient.dll');
function ConnectPipeClient(WaitTime: integer): Boolean; register; stdcall;
  external('PipeClient.dll');
procedure PipeClientMessageToServer(Msg: PAnsiChar); register; stdcall;
  external('PipeClient.dll');

function CallBack(msgType: integer; var pipeID: integer; var answer: PAnsiChar;
  var param: DWORD): Boolean; stdcall;
var
  pid, i: integer;
  s, m: AnsiString;
  method: string;
  p: DWORD;
  Json: ISuperObject;
  jsonarray: TSuperArray;
begin
  Result := True;
  pid := pipeID;
  s := StrPas(answer);
  p := param;
  try
    case msgType of
      MSG_PIPESENT:
        m := 'MSG_PIPESENT';
      MSG_PIPECONNECT:
        m := 'MSG_PIPECONNECT';
      MSG_PIPEDISCONNECT:
        begin
          m := 'MSG_PIPEDISCONNECT';
          with TRequestPipes do
          begin
            FID := -1;
            FConnected := False;
          end;
        end;
      MSG_PIPEMESSAGE:
        begin
          m := 'MSG_PIPEMESSAGE';
          Json := TSuperObject.ParseString(PSOChar(WideString(s)), True);
          if not assigned(Json) then
            raise Exception.Create('Incorrect JSON string!');
          method := Json.GetS('method');
          if method = '' then
          begin
            Result := False;
            Exit;
          end;
          if method = 'GetClientID' then
          begin
            TRequestPipes.FID := Json.GetI('ClientID');
          end
          else if method = 'GetConnectedPipeClients' then
          begin
            jsonarray := Json.GetA('ClientIDs');
            for i := 0 to jsonarray.Length - 1 do
              if jsonarray[i].AsInteger <> 0 then
                if jsonarray[i].AsInteger <> TRequestPipes.FID then
          end
          else
          begin
            TRequestPipes.FResponse := nil;
            case StrToInt(method) of
              Ord(TMethodRequest.mrGET):
                TRequestPipes.FResponse :=
                  TResponsePipes.Create(Json.GetS('message'));
              Ord(TMethodRequest.mrPOST):
                TRequestPipes.FResponse :=
                  TResponsePipes.Create(Json.GetS('message'));
              Ord(TMethodRequest.mrPUT):
                TRequestPipes.FResponse :=
                  TResponsePipes.Create(Json.GetS('message'));
              Ord(TMethodRequest.mrPATCH):
                TRequestPipes.FResponse :=
                  TResponsePipes.Create(Json.GetS('message'));
              Ord(TMethodRequest.mrDELETE):
                TRequestPipes.FResponse :=
                  TResponsePipes.Create(Json.GetS('message'));
            end;
          end;
        end;
      MSG_PIPEERROR:
        m := 'MSG_PIPEERROR';
      MSG_GETPIPECLIENTS:
        m := 'MSG_GETPIPECLIENTS';
    else
      Result := False;
    end;
  except
    on E: Exception do
    begin
      raise;
      Result := False;
    end;
  end;
end;

{ TFile }

constructor TFile.Create(const AFileStream: TStream;
  const AFileName, AContentType: string);
begin
  FFileStream := AFileStream;
  FFileName := AFileName;
  FContentType := AContentType;
end;

destructor TFile.Destroy;
begin

  inherited;
end;

{ TRequestPipes }

function TRequestPipes.Adapters: TArray<IRequestAdapter>;
begin

end;

function TRequestPipes.Adapters(const AAdapters: TArray<IRequestAdapter>)
  : IRequest;
begin
  FAdapters := AAdapters;
  Result := Self;
end;

function TRequestPipes.Adapters(const AAdapter: IRequestAdapter): IRequest;
begin
  Result := Adapters([AAdapter]);
end;

function TRequestPipes.AddBody(const AContent: TObject; const AOwns: Boolean)
  : IRequest;
var
  LJSONObject: TJSONObject;
{$IFDEF FPC}
  LJSONStreamer: TJSONStreamer;
{$ENDIF}
begin
{$IFDEF FPC}
  LJSONStreamer := TJSONStreamer.Create(NIL);
  LJSONObject := LJSONStreamer.ObjectToJSON(AContent);
{$ELSE}
  LJSONObject := TJson.ObjectToJsonObject(AContent);
{$ENDIF}
  try
    Result := Self.AddBody(LJSONObject, False);
  finally
{$IFDEF FPC}
    LJSONStreamer.Free;
{$ENDIF}
    LJSONObject.Free;
    if AOwns then
    begin
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
      AContent.Free;
{$ELSE}
      AContent.DisposeOf;
{$ENDIF}
    end;
  end;
end;

function TRequestPipes.AddBody(const AContent: TStream; const AOwns: Boolean)
  : IRequest;
begin
  Result := Self;
  try
    TstringStream(FStreamSend).CopyFrom(AContent, AContent.Size);
    FStreamSend.Position := 0;
  finally
    if AOwns then
      AContent.Free;
  end;
end;

function TRequestPipes.AddFile(const AFieldName: string; const AValue: TStream;
  const AFileName, AContentType: string): IRequest;
var
  LFile: TFile;
begin
  Result := Self;
  if not assigned(AValue) then
    Exit;
  if (AValue <> Nil) and (AValue.Size > 0) then
  begin
    LFile := TFile.Create(AValue, AFileName, AContentType);
    FFiles.AddOrSetValue(AFieldName, LFile);
  end;
end;

function TRequestPipes.AddHeader(const AName, AValue: string): IRequest;
begin

end;

function TRequestPipes.AddParam(const AName, AValue: string): IRequest;
begin

end;

function TRequestPipes.AddBody(const AContent: string): IRequest;
begin
  Result := Self;
  TstringStream(FStreamSend).Writestring(AContent);
  FStreamSend.Position := 0;
end;

function TRequestPipes.AddBody(const AContent: TJSONArray; const AOwns: Boolean)
  : IRequest;
begin
{$IFDEF FPC}
  Result := Self.AddBody(AContent.AsJSON);
{$ELSE}
  Result := Self.AddBody(AContent.ToJSON);
{$ENDIF}
  if AOwns then
  begin
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
    AContent.Free;
{$ELSE}
    AContent.DisposeOf;
{$ENDIF}
  end;
end;

function TRequestPipes.AddBody(const AContent: TJSONObject;
  const AOwns: Boolean): IRequest;
begin
{$IFDEF FPC}
  Result := Self.AddBody(AContent.AsJSON);
{$ELSE}
  Result := Self.AddBody(AContent.ToJSON);
{$ENDIF}
  if AOwns then
  begin
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
    AContent.Free;
{$ELSE}
    AContent.DisposeOf;
{$ENDIF}
  end;
end;

function TRequestPipes.AddFile(const AFieldName, AFileName,
  AContentType: string): IRequest;
var
  LStream: TMemoryStream;
begin
  Result := Self;
  if not FileExists(AFileName) then
    Exit;
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(AFileName);
    LStream.Position := 0;
    AddFile(AFieldName, LStream, AFileName, AContentType);
  finally
    LStream.Free;
  end;
end;

function TRequestPipes.ClearBody: IRequest;
begin
  Result := Self;
  FStreamSend.Position := 0;
  FStreamSend.Size := 0;
end;

constructor TRequestPipes.Create;
begin
  FID := -1;
  FConnected := False;
  FStreamSend := TstringStream.Create('', TEncoding.UTF8);
  FFiles := TDictionary<string, TFile>.Create;
end;

function TRequestPipes.Delete: IResponse;
begin
  ExecuteRequest(mrDELETE);
  Result := FResponse;
end;

destructor TRequestPipes.Destroy;
begin
  FreeAndNil(FFiles);
  FreeAndNil(FStreamSend);
  inherited;
end;

procedure TRequestPipes.DoAfterExecute(const Sender: TObject;
  const AResponse: IResponse);
begin

end;

procedure TRequestPipes.ExecuteRequest(const AMethod: TMethodRequest);
var
  Json, jsonarray: ISuperObject;
  i, v: integer;
begin
  FConnected := ConnectPipeClient(20000);
  if FConnected then
  begin
    FStreamSend.Position := 0;
    Json := SO();
    Json.s['method'] := Ord(AMethod).ToString;
    Json.s['message'] := FStreamSend.ToJSON().AsString;
  end;
  PipeClientMessageToServer(PAnsiChar(AnsiString(Json.AsJSON())))
end;

function TRequestPipes.FullRequestURL(const AIncludeParams: Boolean): string;
begin

end;

function TRequestPipes.Get: IResponse;
begin
  ExecuteRequest(mrGET);
  Result := FResponse;
end;

class function TRequestPipes.New: IRequest;
begin
  Result := TRequestPipes.Create;
end;

function TRequestPipes.OnAfterExecute(const AOnAfterExecute
  : TRR4DCallbackOnAfterExecute): IRequest;
begin

end;

function TRequestPipes.OnBeforeExecute(const AOnBeforeExecute
  : TRR4DCallbackOnBeforeExecute): IRequest;
begin

end;

function TRequestPipes.Patch: IResponse;
begin
  ExecuteRequest(mrPATCH);
  Result := FResponse;
end;

function TRequestPipes.PipeServer(const AServerName, ANamedPipe: string)
  : IRequest;
begin
  Result := Self;
  FServerName := AServerName;
  FNamedPipe := ANamedPipe;
  InitPipeClient(PAnsiChar(AnsiString(FNamedPipe)), @CallBack);
end;

function TRequestPipes.Post: IResponse;
begin
  ExecuteRequest(mrPOST);
  Result := FResponse;
end;

function TRequestPipes.Put: IResponse;
begin
  ExecuteRequest(mrPUT);
  Result := FResponse;
end;

end.
