program COMPRESSION;

uses
  Forms,
  uMAIN in 'uMAIN.pas' {frmMAIN},
  functions in 'functions.pas',
  uOPTION in 'uOPTION.pas' {frmOPTION},
  uTREEVIEW in 'uTREEVIEW.pas' {frmTREEVIEW},
  uABOUT in 'uABOUT.pas' {frmAbout};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMAIN, frmMAIN);
  Application.CreateForm(TfrmOPTION, frmOPTION);
  Application.CreateForm(TfrmTREEVIEW, frmTREEVIEW);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.Run;
end.
