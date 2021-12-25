unit uOPTION;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmOPTION = class(TForm)
    GroupBox1: TGroupBox;
    TxtSaveCompressed: TEdit;
    Button1: TButton;
    TxtSaveDecompressed: TEdit;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure ReadPath();
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
  
const
  COMPRESS_DIR = 'SystemPath.dat';
  
var
  frmOPTION: TfrmOPTION;
  FH: TextFile;
  ArrDir: Array[0..5] of String;

implementation

uses uTREEVIEW;

{$R *.dfm}

procedure TfrmOPTION.ReadPath();
var
  Hold: String;
  I: Byte;
begin
  { if file isn't exist, simply write it back so it always can be read }
  if not FileExists(COMPRESS_DIR) then begin
    AssignFile(FH, COMPRESS_DIR);
    ReWrite(FH); { alternative append, reset }
    WriteLn(FH, '{Not Defined Yet}');
    WriteLn(FH, '{Not Defined Yet}');
    CloseFile(FH);
  end;
  
  I := 0;
  AssignFile(FH, COMPRESS_DIR);
  Reset(FH);
  While not EOF(FH) do begin
    Readln(FH, Hold);
    ArrDir[I] := Hold;
    Inc(I);
  end;
  CloseFile(FH);
end;

procedure TfrmOPTION.Button5Click(Sender: TObject);
begin
  MessageDlg('Define where the compressed or decompressed file will be saved', MtInformation, [MbOk], 0);
end;

{ Cancle }
procedure TfrmOPTION.Button4Click(Sender: TObject);
begin
  ReadPath();
  TxtSaveCompressed.Text   := ArrDir[0];
  TxtSaveDecompressed.Text := ArrDir[1];
  Self.Close;
end;

procedure TfrmOPTION.Button1Click(Sender: TObject);
begin
  frmTREEVIEW.Label1.Caption := '0'; { as an indicator for TREEVIEW, so it doesn't confuse }
  frmTREEVIEW.Show;                  { where it is come from }
end;

procedure TfrmOPTION.Button2Click(Sender: TObject);
begin
  frmTREEVIEW.Label1.Caption := '1';
  frmTREEVIEW.Show;
end;

{ Okay }
procedure TfrmOPTION.Button3Click(Sender: TObject);
begin
  { update a new path }
  AssignFile(FH, COMPRESS_DIR);
  ReWrite(FH);
  WriteLn(FH, TxtSaveCompressed.Text);
  WriteLn(FH, TxtSaveDecompressed.Text);
  CloseFile(FH);
  Self.Close;
end;

procedure TfrmOPTION.FormCreate(Sender: TObject);
begin
 ReadPath();
 TxtSaveCompressed.Text   := ArrDir[0];
 TxtSaveDecompressed.Text := ArrDir[1];
end;

end.
