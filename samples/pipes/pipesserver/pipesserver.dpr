program pipesserver;

uses
  Vcl.Forms,
  umain in 'umain.pas' {frmPipeServer},
  upiperestserverwrapper.iface in 'upiperestserverwrapper.iface.pas',
  upiperestserverwrapper.impl in 'upiperestserverwrapper.impl.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPipeServer, frmPipeServer);
  Application.Run;
end.
