unit RESTRequest4D.Request.PipesClient;

interface

uses
  WinApi.Windows, Classes, SysUtils, Generics.Collections,
  RESTRequest4D.Request.Contract,
  RESTRequest4D.Response.Contract,
  RESTRequest4D.Utils,
  RESTRequest4D.Request.Adapter.Contract,
  RESTRequest4D.Response.PipesClient,
  upipetypes,
{$IFDEF FPC}
  fpjson, fpjsonrtti, base64;
{$ELSE}
System.AnsiStrings,
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
    // class var FCallBack: TCallBackFunction;
    FResponse: IResponse;
    FConnected: Boolean;
    FID: integer;
    FEndPoint: string;
    FServerName, FNamedPipe: string;
    FAdapters: TArray<IRequestAdapter>;
    FFiles: TDictionary<string, TFile>;
    FStreamSend: TStream;
    FOnBeforeExecute: TRR4DCallbackOnBeforeExecute;
    FOnAfterExecute: TRR4DCallbackOnAfterExecute;
    function DoCallBack(msgType: integer; // this is the messagetype constant
      var pipeID: Int32; // The clients pipeID
      var answer: PAnsiChar; // an answer composed as a string
      var param: DWORD
      // some parameter as a DWORD in most cases the string length
      ): Boolean;
    procedure ExecuteRequest(const AMethod: TMethodRequest);
    function Adapters(const AAdapter: IRequestAdapter): IRequest; overload;
    function Adapters(const AAdapters: TArray<IRequestAdapter>)
      : IRequest; overload;
    function Adapters: TArray<IRequestAdapter>; overload;
    function Endpoint(const AEndPoint: string): IRequest;
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
    function MakeURL(const AIncludeParams: Boolean): string;
  protected
    procedure DoAfterExecute(const Sender: TObject;
      const AResponse: IResponse); virtual;
    procedure DoBeforeExecute; virtual;
  public

    procedure AfterExcecute;
    constructor Create;
    class function New: IRequest;
    destructor Destroy; override;
    // property OnCallBack: TCallBackFunction read FCallBack write FCallBack;
  end;

  var
    FClassInstance: TRequestPipes;

implementation

uses superobject, supertypes;

function InitPipeClient(PipeName: PAnsiChar; CallBack: TCallBackFunction)
  : PAnsiChar; register; stdcall; external('PipeClient.dll');
function ConnectPipeClient(WaitTime: integer): Boolean; register; stdcall;
  external('PipeClient.dll');
procedure PipeClientMessageToServer(Msg: PAnsiChar); register; stdcall;
  external('PipeClient.dll');

function CallBack(msgType: integer; var pipeID: integer; var answer: PAnsiChar;
  var param: DWORD): Boolean; stdcall;
begin
  Result := False;
  if not Assigned(FClassInstance)  then
    Exit;
  result := FClassInstance.DoCallBack(msgType, pipeID,
    answer, param);
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
  result := FAdapters;
end;

function TRequestPipes.Adapters(const AAdapters: TArray<IRequestAdapter>)
  : IRequest;
begin
  FAdapters := AAdapters;
  result := Self;
end;

function TRequestPipes.Adapters(const AAdapter: IRequestAdapter): IRequest;
begin
  result := Adapters([AAdapter]);
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
    result := Self.AddBody(LJSONObject, False);
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
  result := Self;
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
  result := Self;
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
  result := Self;
end;

function TRequestPipes.AddParam(const AName, AValue: string): IRequest;
begin
  result := Self;
end;

procedure TRequestPipes.AfterExcecute;
begin
  if assigned(FClassInstance) then
    FClassInstance.DoAfterExecute(FClassInstance, FResponse);
end;

function TRequestPipes.AddBody(const AContent: string): IRequest;
begin
  result := Self;
  TstringStream(FStreamSend).Writestring(AContent);
  FStreamSend.Position := 0;
end;

function TRequestPipes.AddBody(const AContent: TJSONArray; const AOwns: Boolean)
  : IRequest;
begin
{$IFDEF FPC}
  result := Self.AddBody(AContent.AsJSON);
{$ELSE}
  result := Self.AddBody(AContent.ToJSON);
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
  result := Self.AddBody(AContent.AsJSON);
{$ELSE}
  result := Self.AddBody(AContent.ToJSON);
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
  result := Self;
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
  result := Self;
  FStreamSend.Position := 0;
  FStreamSend.Size := 0;
end;

constructor TRequestPipes.Create;
begin
  FID := -1;
  FConnected := False;
  FStreamSend := TstringStream.Create('', TEncoding.UTF8);
  FFiles := TDictionary<string, TFile>.Create;
  // FCallBack := DoCallBack;
end;

function TRequestPipes.Delete: IResponse;
begin
  ExecuteRequest(mrDELETE);
  result := FResponse;
end;

destructor TRequestPipes.Destroy;
begin
  FreeAndNil(FFiles);
  FreeAndNil(FStreamSend);
  inherited;
end;

procedure TRequestPipes.DoAfterExecute(const Sender: TObject;
  const AResponse: IResponse);
var
  LAdapter: IRequestAdapter;
begin
  if assigned(FOnAfterExecute) then
    FOnAfterExecute(Self, FResponse);
  for LAdapter in FAdapters do
    LAdapter.Execute(FResponse.Content);
end;

procedure TRequestPipes.DoBeforeExecute;
begin
  if assigned(FOnBeforeExecute) then
    FOnBeforeExecute(Self);
end;

function TRequestPipes.DoCallBack(msgType: integer; var pipeID: Int32;
  var answer: PAnsiChar; var param: DWORD): Boolean;
var
  pid, i: integer;
  s, m: AnsiString;
  method: string;
  p: DWORD;
  Json: ISuperObject;
  jsonarray: TSuperArray;
begin
  result := True;
  pid := pipeID;
  s := System.AnsiStrings.StrPas(answer);
  p := param;
  try
    case msgType of
      MSG_PIPESENT:
        begin
          m := 'MSG_PIPESENT';
        end;
      MSG_PIPECONNECT:
        begin
          m := 'MSG_PIPECONNECT';
        end;
      MSG_PIPEDISCONNECT:
        begin
          m := 'MSG_PIPEDISCONNECT';
          FID := -1;
          FConnected := False;
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
            result := False;
            Exit;
          end;
          if (method = 'GetClientID') then
          begin
            FID := Json.GetI('ClientID');
            FConnected := True;
          end
          else if (method = 'GetConnectedPipeClients') and FConnected then
          begin
            jsonarray := Json.GetA('ClientIDs');
            for i := 0 to jsonarray.Length - 1 do
              if jsonarray[i].AsInteger <> 0 then
                if jsonarray[i].AsInteger <> FID then
          end
          else if FConnected then
          begin
            case StrToInt(method) of
              Ord(TMethodRequest.mrGET):
                FResponse :=
                  TResponsePipes.Create(Json.GetS('message'));
              Ord(TMethodRequest.mrPOST):
                FResponse :=
                  TResponsePipes.Create(Json.GetS('message'));
              Ord(TMethodRequest.mrPUT):
                FResponse :=
                  TResponsePipes.Create(Json.GetS('message'));
              Ord(TMethodRequest.mrPATCH):
                FResponse :=
                  TResponsePipes.Create(Json.GetS('message'));
              Ord(TMethodRequest.mrDELETE):
                FResponse :=
                  TResponsePipes.Create(Json.GetS('message'));
            end;
            AfterExcecute;
          end;
        end;
      MSG_PIPEERROR:
        m := 'MSG_PIPEERROR';
      MSG_GETPIPECLIENTS:
        m := 'MSG_GETPIPECLIENTS';
    else
      result := False;
    end;
  except
    on E: Exception do
    begin
      raise;
    end;
  end;
end;

function TRequestPipes.Endpoint(const AEndPoint: string): IRequest;
begin
  result := Self;
  FEndPoint := AEndPoint;
end;

procedure TRequestPipes.ExecuteRequest(const AMethod: TMethodRequest);
var
  Json, jsonarray: ISuperObject;
  i, v: integer;
begin
  FResponse := nil;
  if FConnected then
  begin
    DoBeforeExecute;
    FStreamSend.Position := 0;

    Json := SO();
    Json.s['method'] := Ord(AMethod).ToString;
    Json.s['endpoint'] := FEndPoint;
    case AMethod of
      mrGET, mrPOST, mrPUT, mrPATCH, mrDELETE:
        Json.s['message'] := TstringStream(FStreamSend).DataString;
    else
      Json.s['message'] := FStreamSend.ToJSON().AsString;
    end;
    PipeClientMessageToServer(PAnsiChar(AnsiString(Json.AsJSON())));
  end;
end;

function TRequestPipes.MakeURL(const AIncludeParams: Boolean): string;
var
  i: integer;
begin
  result := FEndPoint;
  // if not FResource.Trim.IsEmpty then
  // begin
  // if not Result.EndsWith('/') then
  // Result := Result + '/';
  // Result := Result + FResource;
  // end;
  // if not FResourceSuffix.Trim.IsEmpty then
  // begin
  // if not Result.EndsWith('/') then
  // Result := Result + '/';
  // Result := Result + FResourceSuffix;
  // end;
  // if FUrlSegments.Count > 0 then
  // begin
  // for I := 0 to Pred(FUrlSegments.Count) do
  // begin
  // Result := stringReplace(Result, Format('{%s}', [FUrlSegments.Names[I]]), FUrlSegments.ValueFromIndex[I], [rfReplaceAll, rfIgnoreCase]);
  // Result := stringReplace(Result, Format(':%s', [FUrlSegments.Names[I]]), FUrlSegments.ValueFromIndex[I], [rfReplaceAll, rfIgnoreCase]);
  // end;
  // end;
  // if not AIncludeParams then
  // Exit;
  // if FParams.Count > 0 then
  // begin
  // Result := Result + '?';
  // for I := 0 to Pred(FParams.Count) do
  // begin
  // if I > 0 then
  // Result := Result + '&';
  // Result := Result + FParams.strings[I];
  // end;
  // end;
end;

function TRequestPipes.FullRequestURL(const AIncludeParams: Boolean): string;
begin
  result := Self.MakeURL(AIncludeParams);
end;

function TRequestPipes.Get: IResponse;
begin
  ExecuteRequest(mrGET);
  result := FResponse;
end;

class function TRequestPipes.New: IRequest;
begin
  FClassInstance := TRequestPipes.Create;
  result := FClassInstance;
end;

function TRequestPipes.OnAfterExecute(const AOnAfterExecute
  : TRR4DCallbackOnAfterExecute): IRequest;
begin
  result := Self;
  FOnAfterExecute := AOnAfterExecute;
end;

function TRequestPipes.OnBeforeExecute(const AOnBeforeExecute
  : TRR4DCallbackOnBeforeExecute): IRequest;
begin
  result := Self;
  FOnBeforeExecute := AOnBeforeExecute;
end;

function TRequestPipes.Patch: IResponse;
begin
  ExecuteRequest(mrPATCH);
  result := FResponse;
end;

function TRequestPipes.PipeServer(const AServerName, ANamedPipe: string)
  : IRequest;
begin
  result := Self;
  FServerName := AServerName;
  FNamedPipe := ANamedPipe;
  InitPipeClient(PAnsiChar(AnsiString(FNamedPipe)), @CallBack);
  FConnected := ConnectPipeClient(20000);
end;

function TRequestPipes.Post: IResponse;
begin
  ExecuteRequest(mrPOST);
  result := FResponse;
end;

function TRequestPipes.Put: IResponse;
begin
  ExecuteRequest(mrPUT);
  result := FResponse;
end;

end.
