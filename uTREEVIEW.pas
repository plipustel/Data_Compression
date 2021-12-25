unit uTREEVIEW;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ShellCtrls, uOPTION;

type
  TfrmTREEVIEW = class(TForm)
    ShellTreeView1: TShellTreeView;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTREEVIEW: TfrmTREEVIEW;

implementation

{$R *.dfm}

procedure TfrmTREEVIEW.Button2Click(Sender: TObject);
begin
  Self.Hide;
end;

procedure TfrmTREEVIEW.Button1Click(Sender: TObject);
begin
  if Label1.Caption = '0' then begin
    frmOPTION.TxtSaveCompressed.Text := ShellTreeView1.Path;
  end else begin
    frmOPTION.TxtSaveDecompressed.Text := ShellTreeView1.Path;
  end;
  Self.Close;
end;

end.
