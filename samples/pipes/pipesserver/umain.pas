unit umain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.AnsiStrings, System.JSON, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, DataSet.Serialize,
  upiperestserverwrapper.iface;

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
    GroupBox2: TGroupBox;
    btnBroadcastString: TButton;
    Memo2: TMemo;
    btnBroadcastJSON: TButton;
    procedure btnStartServerClick(Sender: TObject);
    procedure btnStopServerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnBroadcastStringClick(Sender: TObject);
    procedure btnBroadcastJSONClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoGetJSON(Sender: TObject; const AEndPoint: string;
      var AJSONObject: TJSONObject);
    procedure DoPostJSON(Sender: TObject; const AEndPoint, AJSON: String;
     var ASuccess: boolean);
  public
    { Public declarations }

  end;

var
  frmPipeServer: TfrmPipeServer;

implementation

{$R *.dfm}

procedure TfrmPipeServer.btnBroadcastJSONClick(Sender: TObject);
var
  JSonVal: TJSONValue;
begin
  JSONVal := TJSONObject.ParseJSONValue(Memo2.Text);
  try
    PipeRESTServer.BroadCastMessage(TJSONObject(JSONVal));
  finally
    JSONVal.Free;
  end;
end;

procedure TfrmPipeServer.btnBroadcastStringClick(Sender: TObject);
begin
  PipeRESTServer.BroadCastMessage(Memo2.Text);
end;

procedure TfrmPipeServer.btnStartServerClick(Sender: TObject);
begin
  if PipeRESTServer.Start(Edit2.Text) then
  begin
    Edit1.Text := PipeRESTServer.GetServerName;
    Memo1.Lines.Insert(0, 'PipeServer Started!');
    btnStartServer.Enabled := False;
    btnStopServer.Enabled := True;
  end
  else
    Memo1.Lines.Insert(0, 'Unable to -START- PipeServer!');
end;

procedure TfrmPipeServer.btnStopServerClick(Sender: TObject);
begin
  if not PipeRESTServer.Stop then
  begin
    Memo1.Lines.Insert(0, 'PipeServer Stopped!');
    btnStartServer.Enabled := True;
    btnStopServer.Enabled := False;
  end
  else
    Memo1.Lines.Insert(0, 'Unable to -STOP- PipeServer!');
end;

procedure TfrmPipeServer.DoGetJSON(Sender: TObject; const AEndPoint: string;
  var AJSONObject: TJSONObject);
begin
  if AEndPoint = 'users' then
    AJSONObject.AddPair('value', mtUsers.ToJSONArray());
end;

procedure TfrmPipeServer.DoPostJSON(Sender: TObject;
  const AEndPoint, AJSON: String; var ASuccess: boolean);
begin
  if AEndPoint = 'users' then
  begin
    mtUsers.LoadFromJSON(AJSON);
  end
  else
    ASuccess := False;
end;

procedure TfrmPipeServer.FormCreate(Sender: TObject);
begin
  Memo1.Clear;
  Memo2.Clear;
  Memo2.Lines.Add('{"method":5,"message": "Hello World"}');
  Edit2.Text := 'PipeServer';
  btnStartServer.Enabled := True;
  btnStopServer.Enabled := False;
  PipeRESTServer.OnGetJSON := DoGetJSON;
  PipeRESTServer.OnPostJSON := DoPostJSON;
end;

end.
