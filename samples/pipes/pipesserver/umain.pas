unit umain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.AnsiStrings, System.JSON, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, DataSet.Serialize;

type
  TfrmPipeServer = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    btnStartServer: TButton;
    btnStopServer: TButton;
    GroupBox1: TGroupBox;
    Memo1: TMemo;
    mtUsers: TFDMemTable;
    mtUsersID: TAggregateField;
    mtUsersLASTNAME: TStringField;
    mtUsersNAME: TStringField;
    mtUsersEMAIL: TStringField;
    procedure btnStartServerClick(Sender: TObject);
    procedure btnStopServerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class function GetJSON: string;
    class function PostJSON(const AString: string): string;
  end;

var
  frmPipeServer: TfrmPipeServer;

implementation

{$R *.dfm}

uses upipetypes, superobject, supertypes, RESTRequest4D.Utils;

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

procedure ParseDelimited(const sl: TStrings; const value: string;
  const delimiter: string);
var
  dx: integer;
  ns: string;
  txt: string;
  delta: integer;
begin
  delta := Length(delimiter);
  txt := value + delimiter;
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
  JSON, arrobj: ISuperObject;
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
        JSON := TSuperObject.Create();
        JSON.s['method'] := 'GetClientID';
        JSON.i['ClientID'] := pid;
        PipeServerMessageToClient(pid, PAnsiChar(AnsiString(JSON.AsJSon())));
      end;
    MSG_PIPEDISCONNECT:
      begin
        m := 'MSG_PIPEDISCONNECT';
      end;
    MSG_PIPEMESSAGE:
      begin
        m := 'MSG_PIPEMESSAGE';
        try
          JSON := TSuperObject.ParseString(PSOChar(WideString(s)), True);
          if not assigned(JSON) then
            raise Exception.Create('Incorrect JSON string!');
          method := JSON.GetS('method');
          endpoint := JSON.GetS('endpoint');
          s := JSON.GetS('message'); // store the received message
          JSON := TSuperObject.Create();
          JSON.s['method'] := method;
          JSON.s['endpoint'] := endpoint;
          if (method = Ord(TMethodRequest.mrGET).ToString) then
          begin
            JSON.s['message'] := TfrmPipeServer.GetJSON;
          end;
          if (method = Ord(TMethodRequest.mrPOST).ToString) then
          begin
            JSON.s['message'] := TfrmPipeServer.PostJSON(s);
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
          PipeServerMessageToClient(pid, PAnsiChar(AnsiString(JSON.AsJSon())));
        except
          on E: Exception do
            ShowMessage(E.Message);
        end;
      end;
    MSG_PIPEERROR:
      m := 'MSG_PIPEERROR';
    MSG_GETPIPECLIENTS:
      begin
        m := 'MSG_GETPIPECLIENTS';
        sl := TStringList.Create;
        try
          ParseDelimited(sl, s, ';');
          JSON := SO();
          JSON.s['method'] := 'GetConnectedPipeClients';
          arrobj := SA([]);
          for i := 0 to sl.Count - 1 do
            if TryStrToInt(sl[i], v) then
              arrobj.i['ID'] := v;
          JSON.O['ClientIDs'] := arrobj;
          BroadcastPipeServerMessage(PAnsiChar(AnsiString(JSON.AsString)),
            Length(JSON.AsString));
        finally
          FreeAndNil(sl);
        end;
      end;
  else
    Result := False;
  end;
end;

procedure TfrmPipeServer.btnStartServerClick(Sender: TObject);
begin
  Edit1.Text := System.AnsiStrings.StrPas
    (InitPipeServer(PAnsiChar(AnsiString(Edit2.Text)), @CallBack));
  if StartPipeServer() then
  begin
    Memo1.Lines.Insert(0, 'PipeServer Started!');
    btnStartServer.Enabled := False;
    btnStopServer.Enabled := True;
  end
  else
    Memo1.Lines.Insert(0, 'Unable to -START- PipeServer!')
end;

procedure TfrmPipeServer.btnStopServerClick(Sender: TObject);
begin
  if not StopPipeServer() then
  begin
    Memo1.Lines.Insert(0, 'PipeServer Stopped!');
    btnStartServer.Enabled := True;
    btnStopServer.Enabled := False;
  end
  else
    Memo1.Lines.Insert(0, 'Unable to -STOP- PipeServer!');
  DonePipeServer();
end;

procedure TfrmPipeServer.FormCreate(Sender: TObject);
begin
  Memo1.Clear;
  Edit2.Text := 'PipeServer';
  btnStartServer.Enabled := True;
  btnStopServer.Enabled := False;

end;

procedure TfrmPipeServer.FormDestroy(Sender: TObject);
begin
  DonePipeServer();
end;

class function TfrmPipeServer.GetJSON: string;
var
  LJSONArray: TJSONArray;
begin
  Result := '';
  if not assigned(frmPipeServer) then
    Exit;
  LJSONArray := frmPipeServer.mtUsers.ToJSONArray();
  try
    Result := LJSONArray.Format;
  finally
    LJSONArray.Free;
  end;
end;

class function TfrmPipeServer.PostJSON(const AString: string): string;
begin
  Result := '';
  if not assigned(frmPipeServer) then
    Exit;
  try
    frmPipeServer.mtUsers.LoadFromJSON(AString);
    Result := 'OK';
  except
    on E: Exception do
      Result := E.Message;
  end;
end;

end.
