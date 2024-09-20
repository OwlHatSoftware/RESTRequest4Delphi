program pipesserver;

uses
  Vcl.Forms,
  umain in 'umain.pas' {frmPipeServer};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPipeServer, frmPipeServer);
  Application.Run;
end.
