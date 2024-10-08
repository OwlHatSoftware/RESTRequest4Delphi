unit umain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Imaging.pngimage;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Panel8: TPanel;
    lblRESTRequest4DelphiComponent: TLabel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Panel6: TPanel;
    lblStatusCode: TLabel;
    Label3: TLabel;
    PageControl2: TPageControl;
    TabSheet6: TTabSheet;
    mmBody: TMemo;
    Panel4: TPanel;
    Panel5: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    edtPipeServer: TLabeledEdit;
    edtPipeName: TLabeledEdit;
    mmCustomBody: TMemo;
    btnDELETE: TButton;
    btnPUT: TButton;
    btnPOST: TButton;
    btnGET: TButton;
    TabSheet2: TTabSheet;
    Panel7: TPanel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    btnMultipartFormDataPut: TButton;
    Panel9: TPanel;
    btnMultipartFormDataPost: TButton;
    edtMultipartFormDataBaseURL: TLabeledEdit;
    Panel10: TPanel;
    imgMultipartFormDataStream: TImage;
    lblMultipartFormDataFile: TLabel;
    edtMultipartFormDataText: TEdit;
    FDMemTable1: TFDMemTable;
    edtEndpoint: TLabeledEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnGETClick(Sender: TObject);
    procedure btnPOSTClick(Sender: TObject);
    procedure btnPUTClick(Sender: TObject);
    procedure btnDELETEClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses RESTRequest4D;

procedure TForm1.btnDELETEClick(Sender: TObject);
var
  LResponse: IResponse;
begin
  LResponse := TRequest.New.PipeServer(edtPipeName.Text)
    .Endpoint(edtEndpoint.Text).Delete;
  if LResponse = nil then
    Exit;
  mmBody.Lines.Add(LResponse.Content);
  lblStatusCode.Caption := LResponse.StatusCode.ToString;
end;

procedure TForm1.btnGETClick(Sender: TObject);
var
  LResponse: IResponse;
begin
  mmBody.Lines.Clear;
  LResponse := TRequest.New.PipeServer(edtPipeName.Text)
    .Endpoint(edtEndpoint.Text).OnAfterExecute(
    procedure(const Req: IRequest; const Res: IResponse)
    begin
      if Res = nil then
        Exit;
      mmBody.Lines.Add(Res.Content);
      lblStatusCode.Caption := Res.StatusCode.ToString;
    end).Get;
end;

procedure TForm1.btnPOSTClick(Sender: TObject);
var
  LResponse: IResponse;
begin
  mmBody.Lines.Clear;
  LResponse := TRequest.New.PipeServer(edtPipeName.Text)
    .AddBody(mmCustomBody.Text).Endpoint(edtEndpoint.Text).OnAfterExecute(
    procedure(const Req: IRequest; const Res: IResponse)
    begin
      if Res = nil then
        Exit;
      mmBody.Lines.Add(Res.Content);
      lblStatusCode.Caption := Res.StatusCode.ToString;
    end).Post;
end;

procedure TForm1.btnPUTClick(Sender: TObject);
var
  LResponse: IResponse;
begin
  LResponse := TRequest.New.PipeServer(edtPipeName.Text)
    .AddBody(mmCustomBody.Text).Endpoint(edtEndpoint.Text).Put;
  if LResponse = nil then
    Exit;
  mmBody.Lines.Add(LResponse.Content);
  lblStatusCode.Caption := LResponse.StatusCode.ToString;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage := TabSheet1;
  lblMultipartFormDataFile.Caption :=
    (ExtractFilePath(ParamStr(0)) + 'RESTRequest4Delphi.pdf');
end;

end.
