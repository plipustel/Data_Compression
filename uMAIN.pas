{*******************************************************************************}
{*                                                                             *}
{* ANALYSIS OF COMPARISON LEMPEL ZIV WELCH, ARITHMETIC CODING, AND RUN-LENGTH  *}
{* ENCODING COMPRESSION ALGORITHMS ON TEXT FILE                                *}
{*                                                                             *}
{* Author : Plipus Telaumbanua                                                 *}
{* 061401006                                                                   *}
{* philipstel@gmail.com                                                        *}
{* http://www.pilipstel.web.id                                                 *}
{*                                                                             *}
{* Computer Science                                                            *}
{* University of North Sumatera                                                *}
{* 2010-2011                                                                   *}
{*                                                                             *}
{*******************************************************************************}

unit uMAIN;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ToolWin, ImgList, Grids, ValEdit,
  ExtCtrls, StdCtrls, DBGrids, functions, XPMan, uOPTION, Buttons, uAbout;

type
  TArrayInt = array of Integer;
  TArrayChar = array[chr(0)..chr(255)] of integer;
  TfrmMAIN = class(TForm)
    StatusBar1: TStatusBar;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Customize1: TMenuItem;
    CoolBar2: TCoolBar;
    Open1: TMenuItem;
    SaveAs1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Compression1: TMenuItem;
    Decompression1: TMenuItem;
    N2: TMenuItem;
    Settings1: TMenuItem;
    Option1: TMenuItem;
    N8Bits1: TMenuItem;
    N9Bits1: TMenuItem;
    N10Bits1: TMenuItem;
    N11Bits1: TMenuItem;
    N12Bits1: TMenuItem;
    Help1: TMenuItem;
    HelpTopic1: TMenuItem;
    About1: TMenuItem;
    CoolBar3: TCoolBar;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LblFileName: TLabel;
    LblFileType: TLabel;
    LblFileSize: TLabel;
    LblFileSource: TLabel;
    CompressLZW: TButton;
    CompressAC: TButton;
    CompressRLEBWT: TButton;
    DecompressLZW: TButton;
    DecompressAC: TButton;
    DecompressRLEBWT: TButton;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    XPManifest1: TXPManifest;
    StringGrid1: TStringGrid;
    OpenDialog2: TOpenDialog;
    CoolBar1: TCoolBar;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SaveDialog2: TSaveDialog;
    SpeedButton8: TSpeedButton;
    Loading: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure CompressLZWClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure N8Bits1Click(Sender: TObject);
    procedure N9Bits1Click(Sender: TObject);
    procedure N10Bits1Click(Sender: TObject);
    procedure N11Bits1Click(Sender: TObject);
    procedure N12Bits1Click(Sender: TObject);
    procedure Option1Click(Sender: TObject);
    procedure DecompressLZWClick(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SettingDictionary(BitLength: Integer);
    procedure GridTitle(WhatType: string);
    procedure CompressActive();
    procedure DecompressActive();
    procedure OpenFile();
    procedure SaveAs();
    procedure SaveAutomatically(Res, Properties: String; Compress: Boolean);
    procedure Compression1Click(Sender: TObject);
    procedure Decompression1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure CompressRLEBWTClick(Sender: TObject);
    procedure DecompressRLEBWTClick(Sender: TObject);
    procedure CompressACClick(Sender: TObject);
    procedure DecompressACClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure HelpTopic1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  DEFAULT_ASCII_SIZE      = 255;     { for initial dictionary }
  DEFAULT_BUFFER_SIZE     = 2000000; { buffer size 2 mb ( 2 million bytes) }       
  END_OF_HEADER           = '$';     { stop if this found while reading the header }
  END_OF_FILE             = Chr($FF);
  RLE_MARKER_BYTE         = '#';

var
  frmMAIN: TfrmMAIN;
  { File Handling Declaration }
  IntFileSize, NumRead: Integer;     { file size (num bytes) and indicator  }
  StrFileName: String;               { file name}
  FileHandling: File;                { for untyped file instead of textFile }
  Buffer: Array[0..DEFAULT_BUFFER_SIZE] of Char;

  { LZW Properties }
  SetDictionaryLength, SetBitsLength: Integer; { for dictionary arbitary length }
  Dictionary: Array Of String;

  { AC Properties }
  SymbolLow, SymbolHigh, Frequency: TArrayChar;
  Factor, TotalScaled: Integer;
  ACFileType: String;
  HeadMod, HeadFSize : Integer;
  
  { BWT Properties }
  Buffer2: String; { Buffer2 holds the copy of Buffer for F building }
  
  { Others }
  
  { Simply go to option menu to understand this }
  CompressSavingPath, DecompressSavingPath: String;
  
  EncodedStream, DecodedStream: String;        { used for LZW, AC, and RLE+BWT  }
  CompressGridCounter, DecompressGridCounter, HeaderIndexPosition: Integer;
  OpenDialogType: Boolean; { true for compression's openDialog and false for
                             decompression's OpenDialog }
  
implementation


{$R *.dfm}
{**
 * This function is used to search the string characters in the dictionary using
 * by sequential search.
 * @Param 1 : <string>  Str  : symbol being searched
 * @Param 2 : <array>   Dict : dictionary
 * @Return  : <boolean> return TRUE if it's found, otherwise FALSE.
 * @Effect  : None
 **}
function IsInDictionary(Str: string): Boolean;
var
  I : Integer;
  Found : Boolean;
begin
  Found := False;
  if Length(Str) = 1 then begin
    Found := True;
  end else begin
    for I := 256 to Length(Dictionary) - 1 do begin
      if Str = Dictionary[I] then begin
        Found := True;
        Break; { stop searching if it's found }
      end;
    Found := False;
    end;
  end;
  Result := Found;
end;

{**
 * This function is used to take the 'index' for current symbol from the dictionary
 * @Param 1 : <string> Str : symbol searched
 * @Return  : <integer> return the index for its symbol
 * @Effect  : None
 **}
function GetIndex(Str: string): Integer;
var
  I, Index : Integer;
begin
  Index := 0;
  for I := 0 to Length(Dictionary) - 1 do begin
    if Str = Dictionary[I] then begin
      Index := I;
      Break;
    end;
  end;
  Result := Index;
end;

{**
 * This function is used to change the 'CodeWord' in binary to be 'ASCII' characters
 * that fix in 8 bits for each character. (Used for Compresssion)
 * @Param 1 : <string> CW : CodeWord in binary. Ex: 011011100...
 * @Return  : <string> return the 'CodeWord' to ASCII characters as a final encoding.
 * @Effect  : Make the length of 'CodeWord' MOD 8 is 0
 **}
function CodewordToAscii(CW: String): String;
var
  AfterRestRemoved, ActualRest, ActualCodeword, AsciiOutput: String;
  Rest: Integer;
  I: LongInt;
begin
  ActualCodeword := CW;
  { It turns out that the length of CW mod 8 is not 0. Wow that's bad! we need to pad it }
  if Length(CW) mod 8 <> 0 then begin
    Rest := Length(CW) mod 8;
    AfterRestRemoved := SubStr(CW, 1, Length(CW) - Rest);
    ActualRest := BitPadding(SubStr(CW, (Length(CW) - Rest) + 1, Rest), 8, 'RIGHT');
    ActualCodeword := AfterRestRemoved + ActualRest;  { finally it mod 8 is 0}
  end;

  { It turns out that the length of CW mod 8 is 0. So feel free to change it to ASCII character }
  I := 1;
  while I <= Length(ActualCodeWord) do begin
    AsciiOutput := AsciiOutput + Chr(BinerToDecimal(SubStr(ActualCodeWord, I, 8)));
    Inc(I, 8);
  end;
  Result := AsciiOutput;
end;

{**
 * This function is used for decoding. This function works by changing the ASCII characters
 * (encoded stream) to a sequence of biner. (Used for Decompression)
 * that fix in 8 bits for each character.
 * @Param 1 : <array> Buffer : Buffer (encoded stream) which containts human unreadable characters
 * @Parm 2  : <LongInt) IntFileSize : The file size
 * @Return  : <string> Collection of biners which fix with 'Bits' length.
 * @Effect  : Make ASCII characters to biners and fix with 'Bits' length.
 **}
function AsciiToCodeword(var Buffer: Array of Char; intFSize: LongInt): String;
var
  InBiner, EightBits, RemovePadding: String;
  Rest, Bits: Integer;
  I: LongInt;
begin
  { get the header information }
  Bits := SetBitsLength;

  { read buffer from this position, add one since buffer from 0, while HeaderIndexPosition from 1 }
  I := HeaderIndexPosition + 1;

  { read the ASCII character (encoded stream). Read one byte at a time }
  while I < IntFSize - 1 do begin
    InBiner := DecimalToBiner(Ord(Buffer[I]));  { change to biner per character }
    EightBits := EightBits + BitPadding(InBiner, 8, 'LEFT'); { make every character fix in 8 bits }
    Inc(I);
  end;

  { we need to check it again if it's fix in 'Bits' size }
  if Length(EightBits) mod Bits <> 0 then begin
    Rest := Length(EightBits) mod Bits;
    RemovePadding := SubStr(EightBits, 1, Length(EightBits) - Rest);
    Result := RemovePadding;
  end else begin
    Result := EightBits;
  end;

end;

{**
 * This procedure is used to set the arbitary dictionary length
 * @Param 1 : <integer> BitLength : the bit length being used
 * @Return  : None
 * @Effect  : Change the bit and dictionary being used
 **}
procedure TfrmMAIN.SettingDictionary(BitLength: Integer);
begin
  case (BitLength) of
    8: begin
          N8Bits1.Checked  := True;
          N9Bits1.Checked  := False;
          N10Bits1.Checked := False;
          N11Bits1.Checked := False;
          N12Bits1.Checked := False;
       end;

    9: begin
          N8Bits1.Checked  := False;
          N9Bits1.Checked  := True;
          N10Bits1.Checked := False;
          N11Bits1.Checked := False;
          N12Bits1.Checked := False;
       end;

    10: begin
          N8Bits1.Checked  := False;
          N9Bits1.Checked  := False;
          N10Bits1.Checked := True;
          N11Bits1.Checked := False;
          N12Bits1.Checked := False;
        end;

    11: begin
          N8Bits1.Checked  := False;
          N9Bits1.Checked  := False;
          N10Bits1.Checked := False;
          N11Bits1.Checked := True;
          N12Bits1.Checked := False;
        end;
    12: begin
          N8Bits1.Checked  := False;
          N9Bits1.Checked  := False;
          N10Bits1.Checked := False;
          N11Bits1.Checked := False;
          N12Bits1.Checked := True;
        end;
  end;
  
end;

{** This function is used to get the opposite bits due to the underflow 
  * @param1 : <string> Bit : bit input '0' or '1'
  * @return : <string> '0' to '1', '1' to '0'
  **}
function OppositeBit(Bit: String): String;
begin
  if Bit = '0' then begin
    Result := '1';
  end else begin
    Result := '0';
  end;
end;

{** This function is used to shift out the msb as well as shift in lsb
  * @param1 : <string> Biner   : 110011
  * @param2 : <string> ShiftIn : '0' or '1'
  * @return : ShiftMSB('abcde', '0') would return 'bcde0'
  **}
function ShiftMSB(Biner: String; ShiftIn: String): String;
begin
    Result := Copy(Biner, 2, Length(Biner) - 1) + ShiftIn;
end;

{** this function is used to revomve the 2nd bit and then shift in
  * the 'shiftIn' bit in the most left of 'Biner'
  **}
function ShiftUnderflow(Biner: String; ShiftIn: String): String;
var
  FirstBit, ThirdBit: String;
begin
  FirstBit := Copy(Biner, 1, 1);
  ThirdBit := Copy(Biner, 3, Length(Biner) - 1);
  Result := FirstBit + ThirdBit + ShiftIn;
end;

{** This function is used to build the model for counting the lower and
  * upper bound of symbol.
  **}
procedure BuildProbabilities(var Lower, Upper: TArrayChar);
var
  Temp: Char;
  I: Integer;
begin
 Upper[Temp] := 0;
 for I := 0 to 255 do begin
  Lower[Chr(I)] := Upper[Temp];
  Upper[Chr(I)] := Frequency[Chr(I)] + Upper[Temp];
  Temp := Chr(I);
 end;
end;

{** This padding will divide all bits in 'padding'
  * this is obvious different with bitpadding() function
  **}
function OutputBits(Biner: String; Padding: Integer): String;
var
  N, Modulus, Ins: Integer;
begin
  N := Length(Biner);
  Modulus := N mod Padding;
  if Modulus <> 0 then begin
    Ins := N + (Padding - Modulus);
    Biner := BitPadding(Biner, Ins, 'RIGHT');
  end;
  Result := Biner;
end;

{ read AC header }
procedure ReadACHeader();
var
  I: Integer;
  Dexp : TStringList;
begin
  Dexp := Explode('|', Buffer);
  ACFileType := Dexp[0];
  if ACFileType = 'ac' then begin
  
    TotalScaled := 0;
    for I := 1 to 256 do begin
      Frequency[Chr(I - 1)] := StrToInt(Dexp[I]);
      TotalScaled := TotalScaled + StrToInt(Dexp[I]);
    end;

    HeadMod := StrToInt(Dexp[257]);
    HeadFSize := StrToInt(Dexp[258]);

    // now we have low and high range for each symbol
    BuildProbabilities(SymbolLow, SymbolHigh);

    // Get the buffer's index of end of header character position, so that we can start from there.
    HeaderIndexPosition := 0;
    for I := 0 to IntFileSize - 1 do begin
      if Buffer[I] = END_OF_HEADER then begin
        HeaderIndexPosition := I;
        Break;
      end;
    end;
  end;
end;


function WriteString(Chr: Char; Long: Integer): String;
var
  I: Integer;
  Output: String;
begin
  Output := '';
  for I := 1 to Long do begin
    Output := Output + Chr;
  end;
  Result := Output;
end;


{** This function is used to rotate a string starting from I index
  * to look the effect cuased by it, simply run this Rotate('PHILIPS_TEL', 3) it will
  * give you 'LIPS_TELPHI'
  **}
function Rotate(Str: String; I: Integer): String;
var
  RotatedStrings: String;
  N: Integer;
begin
  N := IntFileSize;
  RotatedStrings := Copy(Str, I + 1, N - (I * 1)) + Copy(Str, 1, (I * 1) );
  Result := RotatedStrings;
end;

{** This function is used to compare a byte with the next byte, comparing will be continue
  * until the difference is found. (Theorically and practically this will be always occurs, since
  * there is an unique EOF (indicator)...hehe..nice!
  * @param : <integer> x, y: indicies to be compared
  * @return :<boolean> (try guess, what is it??? lol....)
  **}
function Compare(X, Y: Integer): Boolean;
begin
  while Buffer[X] = Buffer[Y] do begin
      Inc(X);
      Inc(Y);
  end;

  if Buffer[X] < Buffer[Y] then begin
      Result:= True;
  end else begin
      Result:= False;
  end;

end;

{** This is used to ConcatArray two sorted arrays in to an sorted array as well.
  * arr1(3, 4, 7, 9), arr2(5, 8, 9, 10) after concated: arr3(3, 4, 5, 7, 8, 9, 9, 10)
  * @param1 : <array> List1 : the 1st sorted array
  * @param2 : <array> List2 : the 2nd sorted array
  * @param3 : <array> List3 : the concatend of 1st and 2nd array (in a sorted condition of course)
  * @param4 : <boolean> Transform: TRUE belong to 'transformation' otherwise belong to 'inversing'
  * @return : List3
  * @effect : none
  **}
function ConcatArray(List1, List2, List3: TArrayInt; Transform: Boolean): TArrayInt;
var
  List1Index, List2Index, List3Index, I: Integer;
begin
  List1Index := 0;
  List2Index := 0;
  List3Index := 0;
  while (List1Index < Length(List1)) AND (List2Index < Length(List2)) do begin
    if Transform then begin
      if Compare(List1[List1Index], List2[List2Index]) then begin
        List3[List3Index] := List1[List1Index];
        Inc(List1Index);
      end else begin
        List3[List3Index] := List2[List2Index];
        Inc(List2Index);
      end;
    end else begin
      if Buffer[List1[List1Index]] < Buffer[List2[List2Index]] then begin
        List3[List3Index] := List1[List1Index];
        Inc(List1Index);
      end else begin
        List3[List3Index] := List2[List2Index];
        Inc(List2Index);
      end;
    end;
    Inc(List3Index);
  end;  { end while }
                       
  if List1Index >= Length(List1) then begin
    for I := List2Index to Length(List2) - 1 do begin
      List3[List3Index] := List2[I];
      Inc(List3Index);
    end;
  end;

  if List2Index >= Length(List2) then begin
    for I := List1Index to Length(List1) - 1 do begin
      List3[List3Index] := List1[I];
      Inc(List3Index);
    end;
  end;
  Result := List3;
end;

{**
 * This function is used to slice an array from 'from' to 'max' index (it isn't simliar as substr).
 * Is used for SortString algorithm due to the natural of this algorithm to divide
 * recursively an array to be sorted.
 * @Param 1 : <array> Arr : Array being sliced
 * @Parm 2  : <integer> From, Max: Slice array from 'From' index to 'Max' index
 * @Return  : <array> Sliced Array
 * @Effect  : None
 * arr[0, 2, 4, 6, 2, 9, 4, 5]: SliceArray(Arr, 2, 6) would return 4, 6, 2, 9 (4 wouldn't included
 * since would be used as 'from' in the next Slicing (Upper sliced will be excluded) )
 **}
function SliceArray(Arr: TArrayInt; From, Max: Integer): TArrayInt;
var
  Half, I: LongInt;
  NewArr: TArrayInt;
begin
  Half := Max - From;
  SetLength(NewArr, Half);
  for I := 0 to Half - 1 do begin
    NewArr[I] := Arr[From];
    Inc(From);
  end;               
  Result := NewArr;
end;

{** This is used to sort string not integer.
  * @param1 : <array>   : Unsorted array
  * @param2 : <boolean> : Transform : TRUE for 'transformation' sorting owtherwise for 'inverse' sorting.
  * @return : <array>   : a sorted list
  * @effect : <array>   : Unsorted (parameter) will be in a sorted condition.
  **}
function SortString(Unsorted: TArrayInt; Transform: Boolean): TArrayInt;
var
  Cent: Integer;
  First, Second: TArrayInt;
begin
  if Length(Unsorted) > 1 then begin
    Cent   := Length(Unsorted) div 2;
    First  := SortString(SliceArray(Unsorted, 0, Cent), Transform);
    Second := SortString(SliceArray(Unsorted, Cent, Length(Unsorted)), Transform);
    Result := ConcatArray(First, Second, Unsorted, Transform);
  end else begin
    Result := Unsorted;
  end;
end;

function ArraySearch(FChr: Char): Integer;
var
  I, Found: Integer;
begin
  for I := 1 to IntFileSize do begin
    if FChr = Buffer2[I] then begin
      Found := I - 1;
      Buffer2[I] := Chr($00);
      Break;
    end;
  end;
  Result := Found;
end;

function BWTTransform(): String;
var
  I, N: Integer;
  Ranged:  TArrayInt;
  Last: String;
begin
  N := IntFileSize;
  SetLength(Ranged, N);
  for I := 0 to N - 1 do begin
     Ranged[I] := I;
  end;
  SortString(Ranged, TRUE);  { Ranged now in a sorted array }

  for I := 0 to N - 1  do begin
    Last := Last + Copy(Rotate(Buffer, Ranged[I]), N, 1);
  end;
  Result := Last;
end;

function BWTReversing(): String;
var
  FirstColumn, LastColumn, Vector: TArrayInt;
  I, Index: Integer;
  Last: String;
begin
  SetLength(FirstColumn, IntFileSize);
  SetLength(LastColumn, IntFileSize);
  SetLength(Vector, IntFileSize);

  Buffer2 := Buffer;
  
  for I := 0 to IntFileSize - 1 do begin
    LastColumn[I] := I; { containts indicies 0 - n}
  end;

  { FirstColumn now sorted in lexicographically. Remember its indicies = values.
    Only sort the character not string }
  FirstColumn := SortString(LastColumn, FALSE);

  // Creating vector
  for I := 0 to IntFileSize - 1 do begin
    Vector[I] := ArraySearch(Buffer[FirstColumn[I]]);
  end;

  // END_OF_FILE must be always in the last index since it's chr($ff) in the original text
  Index := FirstColumn[I - 1];

  for I := 0 to IntFileSize - 1 do begin
    Index := Vector[Index];
    Last := Last + Buffer[Index]; {Buffer = LastColumn. LastColumn has been sorted. So, don't use it}
  end;
  Result := Last;
end;

procedure FILE_CORRUPT_ERROR(Method : String);
begin
  MessageDlg('It is not a/an ' + Method + ' file type or file may be corrupt. Decompression terminated!', MtERROR, [MbOK], 0);
end;
{**
 * This procedure is used to set what grid will be using (ex: compress or decomress
 * @Param 1 : <string> WhatType : compression or decompress
 * @Return  : None
 * @Effect  : Grid will looks like a dynamic grid
 **}
procedure TFrmMAIN.GridTitle(WhatType: String);
var
  I, J: Integer;
begin
  { Tab's Title Defenition Cells[Column, Rows] }
  if WhatType = 'compression' then begin
    StringGrid1.Cells[0, 0] := 'Algorithm';
    StringGrid1.Cells[1, 0] := 'File Name';
    StringGrid1.Cells[2, 0] := 'Original Size';
    StringGrid1.Cells[3, 0] := 'After Compressed';
    StringGrid1.Cells[4, 0] := 'Compression Ratio';
    StringGrid1.Cells[5, 0] := 'Time Consuming';
  end else begin
    StringGrid1.ColCount := 5;
    StringGrid1.ColWidths[1] := 250;
    StringGrid1.Cells[0, 0] := 'Algorithm';
    StringGrid1.Cells[1, 0] := 'File Name';
    StringGrid1.Cells[2, 0] := 'Compressed Size';
    StringGrid1.Cells[3, 0] := 'Original Size';
    StringGrid1.Cells[4, 0] := 'Time Consuming';
  end;
  { clear string grid }
  for I := 1 to StringGrid1.RowCount do begin
    for J := 0 to StringGrid1.ColCount  do begin
      StringGrid1.Cells[J, I] := '';
    end;
  end;
end;

procedure TFrmMAIN.CompressActive();
begin
  DecompressGridCounter := 1;
  OpenDialogType := True;          { use openDialog1 }
  Settings1.Enabled := True;       { dictionary change is available }
  GridTitle('compression');        { the grid's column title }
  Decompression1.Checked := False;
  Compression1.Checked := True;

  { compression and decompression buttons state }
  CompressLZW.Enabled := True;
  CompressAC.Enabled := True;
  CompressRLEBWT.Enabled := True;

  DecompressLZW.Enabled := False;
  DecompressAC.Enabled := False;
  DecompressRLEBWT.Enabled := False;
end;

procedure TFrmMAIN.DecompressActive();
begin
  CompressGridCounter := 1;
  OpenDialogType := False;
  Settings1.Enabled := False;
  GridTitle('decompression');
  Decompression1.Checked := True;
  Compression1.Checked := False;

  CompressLZW.Enabled := False;
  CompressAC.Enabled := False;
  CompressRLEBWT.Enabled := False;

  DecompressLZW.Enabled := True;
  DecompressAC.Enabled := True;
  DecompressRLEBWT.Enabled := True;
end;

{**
 * This function is used to identify the file type for decompressing file.
 * And that file types is just known for naming the decoded stream while decoding.
 * It assumes the maximum of extention's length is 4 characters.
 * @Param 1 : <string> Fname: File name taken from buffer
 * @Return  : <string> : the file type's name
 * @Effect  : None
 **}
function KnownFileType(Fname: String): String;
var
  Exp : TStringList;
begin
  Exp := Explode('.', Fname );
  Result := Exp[1];
end;

{** This procedure is used to open a file to be compressed or decompressed
  * accept to 'state', OpenDialogType. True for compression and False for
  * decompression. Is available for LZW, AC, and RLE+BWT. 'BUFFER' is the key
  **}
procedure TFrmMAIN.OpenFile();
var
  FH: TextFile;
  Hold: Char;
  I: Integer;
begin
  { compression opendialog }
  if OpenDialogType then begin
    CompressSavingPath := frmOPTION.TxtSaveCompressed.Text;
    if DirectoryExists(CompressSavingPath) then begin
      if OpenDialog1.Execute then begin
        StrFileName := OpenDialog1.FileName;
        { add EOF for BWT indicator. It's also help for AC }
        AssignFile(FH, StrFileName);
        Reset( FH );
        while not EOF( FH ) do begin
          Read( FH, Hold );
        end;

        if Hold <> END_OF_FILE then begin
          Append(FH);                          
          Write(FH, END_OF_FILE);
        end;
        CloseFile(FH);

        { read to buffer }
        AssignFile(FileHandling, StrFileName);
        Reset(FileHandling, 1);
        IntFileSize := FileSize(FileHandling);
        repeat
          BlockRead(FileHandling, Buffer, SizeOf(Buffer), NumRead);
        until (NumRead = 0);
        CloseFile(FileHandling);

        { START PLAY WITH ARITHMETIC CODING BY CREATING A MODEL }
    
        // Set first frequency to zero
        for I := 0 to 255 do begin
          Frequency[Chr(I)] := 0;
        end;

        // count symbols frequency
        for I := 0 to IntFileSize - 1 do begin
          Frequency[Buffer[I]] := Frequency[Buffer[I]] + 1;
        end;

        // rescale the count. Will cause upper bounds of symbol < the total symbols count
        Factor := Ceil(IntFileSize / Pow(2, 14));

        // If a non-zero count become zero, make it one. Thank you Michael 
        TotalScaled := 0;
        for I := 0 to 255 do begin
          if Frequency[Chr(I)] > Factor then begin
            Frequency[Chr(I)] := Floor( Frequency[Chr(I)]/Factor );
          end else if Frequency[Chr(I)] <> 0 then begin
            Frequency[Chr(I)] := 1;
          end;
          TotalScaled := TotalScaled + Frequency[Chr(I)];
        end;

        // Build model which only contain Low and High
        BuildProbabilities(SymbolLow, SymbolHigh);
        { CREATING MODEL END HERE }
        
        // Write the file information
        LblFileName.Caption := ExtractFileName(StrFileName);
        LblFileType.Caption := KnownFileType(ExtractFileName(StrFileName));
        LblFileSize.Caption := IntToStr(IntFileSize) + ' bytes';
        LblFileSource.Caption := StrFileName;
      end;
    end else begin
      MessageDlg('Define first the compressed/decompressed path target from the option menu.', MtWarning, [MbOK], 0);
    end;
  end else begin  { decompress opendialog }
    DecompressSavingPath := frmOPTION.TxtSaveDecompressed.Text;
    if DirectoryExists(DecompressSavingPath) then begin
      if OpenDialog2.Execute then begin
        StrFileName := OpenDialog2.FileName;
        AssignFile(FileHandling, StrFileName);
        Reset(FileHandling, 1);
        IntFileSize := FileSize(FileHandling);
        repeat
          BlockRead(FileHandling, Buffer, SizeOf(Buffer), NumRead);
        until (NumRead = 0);
        CloseFile(FileHandling);
        
        // write the file information
        LblFileName.Caption := ExtractFileName(StrFileName);
        LblFileType.Caption := KnownFileType(ExtractFileName(StrFileName));
        LblFileSize.Caption := IntToStr(IntFileSize) + ' bytes';
        LblFileSource.Caption := StrFileName;
      end;
    end else begin
      MessageDlg('Define first the compressed/decompressed path target from the option menu.', MtWarning, [MbOK], 0);
    end;
  end;
end;

{** This procedure is used to save the compressed or decompressed file manually
  * accept two properties: 'EncodedStream' and 'DecodedStream'.
  * Is available for LZW, AC, and RLE+BWT.
  **}
procedure TFrmMAIN.SaveAs();
var
  Fout: TextFile;
  I: Integer;
begin
  if OpenDialogType then begin
    if SaveDialog1.Execute then begin
      AssignFile(Fout, SaveDialog1.FileName);
      ReWrite(Fout);
      for I := 1 to Length(EncodedStream) do begin
        Write(Fout, EncodedStream[I]);
      end;
      CloseFile(Fout);
      SaveAs1.Enabled := False;
    end;
  end else begin
    if SaveDialog2.Execute then begin
       AssignFile(Fout, SaveDialog2.FileName + '.txt');
       ReWrite(Fout);
       for I := 1 to Length(DecodedStream) do begin
        Write(Fout, DecodedStream[I]);
       end;
       CloseFile(Fout);
       SaveAs1.Enabled := False;
    end;
  end;
end;

procedure TFrmMain.SaveAutomatically(Res, Properties: String; Compress: Boolean);
var
  Exp: TStringList;
  Fout: TextFile;
  I: Integer;
begin
  CompressSavingPath   := frmOPTION.TxtSaveCompressed.Text;
  DecompressSavingPath := frmOPTION.TxtSaveDecompressed.Text;
  Exp := Explode('.', ExtractFileName(StrFileName));
  
  if DirectoryExists(CompressSavingPath) and DirectoryExists(DecompressSavingPath) then begin
    if Compress then begin
      AssignFile(Fout, CompressSavingPath +  '\' + Exp[0] + '.' + Properties);
      Rewrite(Fout);
      for I := 1 to Length(Res) do begin
        Write(Fout, Res[I]);
      end;
      CloseFile(Fout);
      MessageDlg('Saving the compressed file is successfully in ' + CompressSavingPath, MtInformation, [MbOK], 0);
    end else begin
      AssignFile(Fout, DecompressSavingPath +  '\' + Exp[0] + '.' + Properties + '.' + 'txt');
      Rewrite(Fout);
      for I := 1 to Length(Res) do begin
        Write(Fout, Res[I]);
      end;
      CloseFile(Fout);
      MessageDlg('Saving the Decompressed file is successfully in ' + DecompressSavingPath, MtInformation, [MbOK], 0);
    end;
  end else begin
   MessageDlg('Automatic saving is failed. Save as it manually or set the new saving path from the option menu', MtError, [MbOK], 0);
  end;
end;

{ write ratio in percent (%) }
function WriteRatio(Ratio: double):String;
begin
  if Ratio < 1 then begin
    WriteRatio := '0' + FormatFloat('#.##', Ratio) + '%';
  end else begin
    WriteRatio := FormatFloat('#.##', Ratio) + '%';
  end;
end;

{ # --------------------------- END FOR ALL PROPERTIES  ---------------------- #}


{ # --------------------- START FOR IMPLEMENTING OBJECTS --------------------- #}

procedure TfrmMAIN.FormCreate(Sender: TObject);
begin
  ShowHint := True;
  OpenDialogType := True;
  CompressGridCounter := 1;
  DecompressGridCounter := 1;
  SetDictionaryLength := 2048;
  SetBitsLength := 11;
  GridTitle('compression');
  
  { Disable all components }
  DecompressLZW.Enabled    := False;
  DecompressAC.Enabled     := False;
  DecompressRLEBWT.Enabled := False;

  SaveAs1.Enabled := False;
end;

{ LZW Algorithm Compression }
procedure TfrmMAIN.CompressLZWClick(Sender: TObject);
var
  I, J, K, Bits, OutputIndex, LastDictIndex: Integer;
  CurrentChars, NextChar, ConcatStr, Header, Codeword: String;

  { counting the elapsed time }
  Elapsed: String;
  Start, Stop: TDateTime;

begin
  Loading.Caption := 'Compressing...';
  
 { begin here to estimate the complexity (time complexity and running time) of
    this LZW's encoding algorithm }
  Start := Now;
  { Dictionary can be arbitary length: 8, 9, 10, 11 or 12 bits | 256, 512, 1024, 2048, 4096 }
  SetLength(Dictionary, SetDictionaryLength);

  Bits := SetBitsLength;

  { inisialization a dictionary 0 - 255 }
  for I := 0 to DEFAULT_ASCII_SIZE do begin
      Dictionary[I] := Chr(I);
  end;
  
  LastDictIndex := 256;
  J := 0;
  CurrentChars := SubStr(Buffer, 1, 1);

  while J < IntFileSize do begin
    NextChar  := Buffer[J + 1];
    ConcatStr := CurrentChars + NextChar;

    if IsInDictionary(ConcatStr) then begin
      CurrentChars := ConcatStr;
    end else begin
      OutputIndex := GetIndex(CurrentChars);
      Codeword := Codeword + BitPadding(DecimalToBiner(OutputIndex), Bits, 'LEFT');
      
      if LastDictIndex < Length(Dictionary) then begin
        Dictionary[LastDictIndex] := ConcatStr;
      end;

      CurrentChars := NextChar;
      Inc(LastDictIndex);
    end;

    if (J = IntFileSize - 1) then begin
      OutputIndex := GetIndex(CurrentChars);
      Codeword := Codeword + BitPadding(DecimalToBiner(OutputIndex), Bits, 'LEFT');
    end;
    Inc(J);
  end;

  { write the header }
  Header := 'lzw|' + IntToStr(Bits) + '|' + IntToStr(SetDictionaryLength) + '|' + END_OF_HEADER; { $ sign is EO header indicator }
  EncodedStream := Header + CodewordToAscii(Codeword);
  Stop := Now;
  Elapsed := FormatDateTime('hh:nn:ss:zzz', Stop - Start);
  { Ending here for estimating the complexity of algorithm }
  MessageDlg('LZW Compression success. Required time : ' + Elapsed + ' seconds', MtInformation, [MbOK], 0);
  
  Loading.Caption := '';
  SaveAs1.Enabled := True;
  { Save the compressed file automatically }
  SaveAutomatically(EncodedStream, 'lzw', TRUE);

  { clear the dictionary }
  for K := 0 to SetDictionaryLength - 1 do begin
    Dictionary[K] := ''; 
  end;

  { display the statistic to the grid Cells[Column, Rows]}
  StringGrid1.Cells[0, CompressGridCounter] := 'LZW';
  StringGrid1.Cells[1, CompressGridCounter] := ExtractFileName(StrFileName);
  StringGrid1.Cells[2, CompressGridCounter] := IntToStr(IntFileSize) + ' bytes';
  StringGrid1.Cells[3, CompressGridCounter] := IntToStr(Length(EncodedStream)) + ' bytes';
  StringGrid1.Cells[4, CompressGridCounter] := WriteRatio((Length(EncodedStream) / IntFileSize) * 100);
  StringGrid1.Cells[5, CompressGridCounter] := Elapsed;
  Inc(CompressGridCounter);
  StringGrid1.RowCount := 3 + CompressGridCounter;
end;

procedure TfrmMAIN.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMAIN.N8Bits1Click(Sender: TObject);
begin
  SettingDictionary(8);
  SetDictionaryLength := 256;
  SetBitsLength := 8;
end;

procedure TfrmMAIN.N9Bits1Click(Sender: TObject);
begin
  SettingDictionary(9);
  SetDictionaryLength := 512;
  SetBitsLength := 9;
end;

procedure TfrmMAIN.N10Bits1Click(Sender: TObject);
begin
  SettingDictionary(10);
  SetDictionaryLength := 1024;
  SetBitsLength := 10;
end;

procedure TfrmMAIN.N11Bits1Click(Sender: TObject);
begin
  SettingDictionary(11);
  SetDictionaryLength := 2048;
  SetBitsLength := 11;
end;

procedure TfrmMAIN.N12Bits1Click(Sender: TObject);
begin
  SettingDictionary(12);
  SetDictionaryLength := 4096;
  SetBitsLength := 12;
end;

procedure TfrmMAIN.Option1Click(Sender: TObject);
begin
  frmOPTION.Show;
end;

{ LZW Algorithm Decompression }
procedure TfrmMAIN.DecompressLZWClick(Sender: TObject);
var
  OutputString, FirstChar, Codeword, Header: String;
  I, K, PrevCode, CurrentCode, LastDictIndex: Integer;
  J: LongInt;

  { counting the elapsed time }
  Elapsed: String;
  Start, Stop: TDateTime;

  { saving the encoded stream }
  Exp: TStringList;
  Fout: TextFile;
begin
  Loading.Caption := 'Decompressing...';
  Start := Now;
  HeaderIndexPosition := 0;
  { begin here to estimate the complexity (time complexity and running time) of
    this LZW's encoding algorithm }

  { read header }
  for I := 0 to IntFileSize do begin
    if Buffer[I] = END_OF_HEADER then begin
      HeaderIndexPosition := I;
      Break;
    end
  end;

  for I := 0 to HeaderIndexPosition do begin
    Header := Header + Buffer[I];
  end;
  
  Exp := Explode('|', Header);

  if Exp[0] = 'lzw' then begin
    SetBitsLength := StrToInt(Exp[1]);
    SetDictionaryLength := StrToInt(Exp[2]);
    SetLength(Dictionary, SetDictionaryLength);

    { change the 'ASCII character' to be 'codeword'}
    Codeword := AsciiToCodeword(Buffer, IntFileSize);

    { initialization }
    for I := 0 to DEFAULT_ASCII_SIZE do begin
      Dictionary[I] := chr(I);
    end;
    LastDictIndex := 256;
    PrevCode := BinerToDecimal(SubStr(Codeword, 1, SetBitsLength));
    OutputString := Dictionary[PrevCode];
    FirstChar := SubStr(OutputString, 1, 1);
    DecodedStream := OutputString;

    J := SetBitsLength + 1; { Bits in header end in 11 position starting from 1. So, we take from 12 position.
                    We aren't working with buffer, so string begin from 1 not 0 }
    while J <= Length(Codeword) do begin
      CurrentCode := BinerToDecimal(SubStr(Codeword, J, SetBitsLength));

      { search in dictionary. Will always found if it's < LastDictIndex }
      if CurrentCode < LastDictIndex  then begin
        OutputString := Dictionary[CurrentCode];
      end else begin
        OutputString := Dictionary[PrevCode] + FirstChar;
      end;

      DecodedStream := DecodedStream + OutputString;
      FirstChar := SubStr(OutputString, 1, 1);

      { add to dictionary as long as it's < dictionary length }
      if LastDictIndex < Length(Dictionary) then begin
        Dictionary[LastDictIndex] := Dictionary[PrevCode] + FirstChar;
      end;
      PrevCode := CurrentCode;
      Inc(J, SetBitsLength);
      Inc(LastDictIndex);
    end;

    Stop := Now;
    Elapsed := FormatDateTime('hh:nn:ss:zzz', Stop - Start);
    MessageDlg('LZW Decompression success. Required time : ' + Elapsed + ' seconds', MtInformation, [MbOK], 0);

    Loading.Caption := '';
    SaveAs1.Enabled := True;
    
    { Save the Decompressed file automatically }
    SaveAutomatically(DecodedStream, 'lzw', FALSE);

    { clear the dictionary }
    for K := 0 to SetDictionaryLength - 1 do begin
      Dictionary[K] := '';
    end;
    
    { show the statistic to the grid Cells[Column, Rows]}
    StringGrid1.Cells[0, DecompressGridCounter] := 'LZW';
    StringGrid1.Cells[1, DecompressGridCounter] := ExtractFileName(StrFileName);
    StringGrid1.Cells[2, DecompressGridCounter] := IntToStr(IntFileSize) + ' bytes';
    StringGrid1.Cells[3, DecompressGridCounter] := IntToStr(Length(DecodedStream)) + ' bytes';
    StringGrid1.Cells[4, DecompressGridCounter] := Elapsed + ' seconds';
    Inc(DecompressGridCounter);
    StringGrid1.RowCount := 3 + DecompressGridCounter;
    end else begin
      FILE_CORRUPT_ERROR('LZW');
    end;
  end;

procedure TfrmMAIN.SpeedButton5Click(Sender: TObject);
begin
  frmOPTION.Show;                          
end;

procedure TfrmMAIN.Compression1Click(Sender: TObject);
begin
  CompressActive();
end;

procedure TfrmMAIN.Decompression1Click(Sender: TObject);
begin
  DecompressActive();
end;

procedure TfrmMAIN.SpeedButton3Click(Sender: TObject);
begin
  CompressActive();
end;

procedure TfrmMAIN.SpeedButton4Click(Sender: TObject);
begin
  DecompressActive();
end;

procedure TfrmMAIN.SpeedButton1Click(Sender: TObject);
begin
  OpenFile();
end;

procedure TfrmMAIN.Open1Click(Sender: TObject);
begin
  OpenFile();
end;

procedure TfrmMAIN.SaveAs1Click(Sender: TObject);
begin
  SaveAs();
end;

procedure TfrmMAIN.SpeedButton2Click(Sender: TObject);
begin
  if (EncodedStream <> '') or (DecodedStream <> '') then begin
    SaveAs();
  end;
end;

procedure TfrmMAIN.SpeedButton7Click(Sender: TObject);
begin
  frmAbout.Show;
end;

procedure TfrmMAIN.SpeedButton8Click(Sender: TObject);
begin
  Application.Terminate;
end;

{ RLE Compression. BWT first and then RLE }
procedure TfrmMAIN.CompressRLEBWTClick(Sender: TObject);
var
  I, Counter: Integer;
  Source, Output, Header: String;

  { Counting the elapsed time }
  Elapsed: String;
  Start, Stop: TDateTime;

begin
  Loading.Caption := 'Compressing...';

  Start := Now;
  Source := BWTTransform(); { transform first the buffer }
  Counter := 1;
  I := 1;  
  while I <= Length(Source) do begin
    while (Source[I] <> RLE_MARKER_BYTE ) and not (Source[I] in ['0'..'9']) and (Source[I] = Source[I + 1]) do begin
      Inc(Counter);
      Inc(I);
    end;

    if Counter > 3 then begin
      Output := Output + RLE_MARKER_BYTE + IntToStr(Counter) + Source[i];
    end else if Source[I] = RLE_MARKER_BYTE then begin
      Output := Output + WriteString(RLE_MARKER_BYTE, 2);
    end else begin
      Output := Output + WriteString(Source[I], Counter);
    end;

    Counter := 1;
    Inc(I);
  end;
  Header := 'rlebwt|' + END_OF_HEADER;
  EncodedStream := Header + Output;
  Stop := Now;
  Elapsed := FormatDateTime('hh:nn:ss:zzz', Stop - Start);
  MessageDlg('RLE+BWT Compression success. Required time : ' + Elapsed + ' seconds', MtInformation, [MbOK], 0);

  Loading.Caption := '';
  SaveAs1.Enabled := True;
  SaveAutomatically(EncodedStream, 'rlebwt', TRUE);
  { display the statistic to the grid Cells[Column,Rows]}
  StringGrid1.Cells[0, CompressGridCounter] := 'RLE+BWT';
  StringGrid1.Cells[1, CompressGridCounter] := ExtractFileName(StrFileName);
  StringGrid1.Cells[2, CompressGridCounter] := IntToStr(IntFileSize) + ' bytes';
  StringGrid1.Cells[3, CompressGridCounter] := IntToStr(Length(EncodedStream)) + ' bytes';
  StringGrid1.Cells[4, CompressGridCounter] := WriteRatio((Length(EncodedStream) / IntFileSize) * 100);
  StringGrid1.Cells[5, CompressGridCounter] := Elapsed;
  Inc(CompressGridCounter);
  StringGrid1.RowCount := 3 + CompressGridCounter;
end;

{ RLE Decompression. RLE First and then BWT }
procedure TfrmMAIN.DecompressRLEBWTClick(Sender: TObject);
var
  I, IntFSizeGrid: Integer;
  Output, Source, Counter, Header: String;
  Exp: TStringList;

  { Counting the elapsed time }
  Elapsed: String;
  Start, Stop: TDateTime;
begin
  IntFSizeGrid := IntFileSize;
  Loading.Caption := 'Decompressing...';
  { read header }
  Start := Now;
  HeaderIndexPosition := 0;
  for I := 0 to IntFileSize do begin
    if Buffer[I] = END_OF_HEADER then begin
      HeaderIndexPosition := I;
      Break;
    end
  end;

  for I := 0 to HeaderIndexPosition do begin
    Header := Header + Buffer[I];
  end;
  
  Exp := Explode('|', Header);

  if Exp[0] = 'rlebwt' then begin
    Source := Buffer;
    I := HeaderIndexPosition + 2;
    while I <= IntFileSize do begin
      if (Source[I] = RLE_MARKER_BYTE) and (Source[I + 1] = RLE_MARKER_BYTE) then begin
        Output := Output + RLE_MARKER_BYTE;
        Inc(I);
      end else if Source[I] = RLE_MARKER_BYTE then begin
        while (Source[I + 1] in ['0'..'9']) do begin
          Counter := Counter + Source[I + 1];  { ex. #536a, take 536 }
          Inc(I);
        end;
        Inc(I);
        Output := Output + WriteString(Source[I], StrToInt(Counter)); { compression achieved }
        Counter := '';
      end else begin
        Output := Output + Source[I]; { simply write the string }
      end;
      Inc(I);
    end;

    { Update new buffer and filesize as well, after RLE Decoding. So the effect of
      previous buffer removed }
    for I := 0 to Length(Output) - 1 do begin
      Buffer[I] := Output[I + 1];
    end;
    IntFileSize := Length(Output);
    DecodedStream := BWTReversing();
    Stop := Now;
    Elapsed := FormatDateTime('hh:nn:ss:zzz', Stop - Start);
    MessageDlg('RLE+BWT Decompression success. Required time : ' + Elapsed + ' seconds', MtInformation, [MbOK], 0);

    Loading.Caption := '';
    SaveAs1.Enabled := True;
    SaveAutomatically(DecodedStream, 'rlebwt', FALSE);

    { show the statistic to the grid Cells[Column, Rows]}
    StringGrid1.Cells[0, DecompressGridCounter] := 'RLE+BWT';
    StringGrid1.Cells[1, DecompressGridCounter] := ExtractFileName(StrFileName);
    StringGrid1.Cells[2, DecompressGridCounter] := IntToStr(IntFSizeGrid) + ' bytes';
    StringGrid1.Cells[3, DecompressGridCounter] := IntToStr(Length(DecodedStream)) + ' bytes';
    StringGrid1.Cells[4, DecompressGridCounter] := Elapsed + ' seconds';
    Inc(DecompressGridCounter);
    StringGrid1.RowCount := 3 + DecompressGridCounter;

  end else begin
    FILE_CORRUPT_ERROR('BWT + RLE');
  end;
end;

{ Arithmetic Coding's Encoding }
procedure TfrmMAIN.CompressACClick(Sender: TObject);
var
  I, J, Low, High, Underflow, Range: Integer;
  HighToBiner, LowToBiner, EncodedBits, Header : String;
  
  { Counting the elapsed time }
  Elapsed: String;
  Start, Stop: TDateTime;
begin
  Loading.Caption := 'Compressing...';

  Start := Now; 
  HeadMod := 0;
  Low  := $0000;
  High := $FFFF; 
  Underflow := 0;
  I := 0;
  
  while I < IntFileSize do begin
    Range := (High - Low) + 1;
    High  := Low + Floor(Range * SymbolHigh[Buffer[I]] / TotalScaled) - 1;
    Low   := Low + Floor(Range * SymbolLow[Buffer[I]] / TotalScaled);
    
    LowToBiner  := BitPadding(DecimalToBiner(Low), 16, 'LEFT');
    HighToBiner := BitPadding(DecimalToBiner(High), 16, 'LEFT');

    for J := 1 to 999999 do begin
      if LowToBiner[1] = HighToBiner[1] then begin    { check MSB of Lower and Upper }
        EncodedBits := EncodedBits + LowToBiner[1];
        while Underflow > 0 do begin
          EncodedBits := EncodedBits + OppositeBit(LowToBiner[1]);
          Dec(Underflow);
        end;
        
        { Shift In }
        LowToBiner  := ShiftMSB(LowToBiner, '0');
        HighToBiner := ShiftMSB(HighToBiner, '1');

      end else begin
        {Check for Underflow}
        if (SubStr(LowToBiner, 2, 1) = '1') and (SubStr(HighToBiner, 2, 1) = '0') then begin
          Inc(Underflow);
          { Shift Out Underflow's Bits }
          LowToBiner  := ShiftUnderflow(LowToBiner, '0');
          HighToBiner := ShiftUnderflow(HighToBiner, '1');
        end else begin
          Low  := BinerToDecimal(LowToBiner);
          High := BinerToDecimal(HighToBiner);
          Break; { continue to the next symbol}
        end;
      end;
    end;  { end for }
    
    Inc(I);
  end; { end while }

  { write remaining bits: the lower bound's 2nd MSB and remaining underflow + plus underflow bits }
  EncodedBits := EncodedBits + LowToBiner[2];

  while Underflow >= -16 do begin 
    EncodedBits := EncodedBits + OppositeBit(LowToBiner[2]);
    Dec(Underflow);
  end;

  if Length(EncodedBits) mod 8 <> 0 then begin
    HeadMod := 8 - (Length(EncodedBits) mod 8); // if 2 return 6. 6 zeros are addedd
  end;
  EncodedBits := OutputBits(EncodedBits, 8);

  { write file header }
  Header := 'ac|';
  for I := 0 to 255 do begin
    Header := Header + IntToStr(Frequency[Chr(I)]) + '|';
  end;
  Header := Header + IntToStr(HeadMod) + '|' + IntToStr(IntFileSize) + '|' + END_OF_HEADER;

  { finally write the encoded symbol }
  EncodedStream := Header;
  I := 1;
  while I <= Length(EncodedBits) do begin
    EncodedStream := EncodedStream + Chr(BinerToDecimal(Copy(EncodedBits, I, 8))) ;
    INC(I, 8);
  end;
  Stop := Now; 
  Elapsed := FormatDateTime('hh:nn:ss:zzz', Stop - Start);
  MessageDlg('AC Compression success. Required time : ' + Elapsed + ' seconds', MtInformation, [MbOK], 0);

  Loading.Caption := '';
  SaveAs1.Enabled := True;
  SaveAutomatically(EncodedStream, 'ac', TRUE);
  { display the statistic to the grid Cells[Column,Rows]}
  StringGrid1.Cells[0, CompressGridCounter] := 'AC';
  StringGrid1.Cells[1, CompressGridCounter] := ExtractFileName(StrFileName);
  StringGrid1.Cells[2, CompressGridCounter] := IntToStr(IntFileSize) + ' bytes';
  StringGrid1.Cells[3, CompressGridCounter] := IntToStr(Length(EncodedStream)) + ' bytes';
  StringGrid1.Cells[4, CompressGridCounter] := WriteRatio((Length(EncodedStream) / IntFileSize) * 100);
  StringGrid1.Cells[5, CompressGridCounter] := Elapsed;
  Inc(CompressGridCounter);
  StringGrid1.RowCount := 3 + CompressGridCounter;
end;

{ Arithmetic Coding's Decoding }
procedure TfrmMAIN.DecompressACClick(Sender: TObject);
var
  Code: String;
  Low, High, I, J, Range, Temp, B: Integer;
  HighToBiner, LowToBiner, Opposite, EncodedBits, DecodedSymbols: String;
  CurrentSymbol: Char;

  { Counting the elapsed time }
  Elapsed: String;
  Start, Stop: TDateTime;
begin
  Loading.Caption := 'Decompressing...';
  Start := Now;
  ReadACHeader();
  if 'ac' = ACFileType then begin
    I := HeaderIndexPosition + 1;
    while I < IntFileSize do begin
      EncodedBits := EncodedBits + BitPadding(DecimalToBiner(Ord(Buffer[I])), 8, 'LEFT');
      Inc(I);
    end;
    EncodedBits := Copy(EncodedBits, 1, Length(EncodedBits) - HeadMod); { original bits, after bits padding removed }
    B := 1;
    Code := Copy(EncodedBits, B, 16);
    Low := $0000;
    High := $FFFF;
    J := 0;
    while J < HeadFSize do begin
      Range := (High - Low) + 1;
      Temp := Floor( ((BinerToDecimal(Code) - Low + 1) * TotalScaled) / Range);
      { search symbol }
      for I := 0 to 255 do begin
        if (Temp >= SymbolLow[Chr(I)]) and (Temp < SymbolHigh[Chr(I)]) then begin
          CurrentSymbol := Chr(I);
          DecodedSymbols := DecodedSymbols + CurrentSymbol;
        end;
      end;

      High  := Low + Floor(Range * SymbolHigh[CurrentSymbol] / TotalScaled) - 1;
      Low   := Low + Floor(Range * SymbolLow[CurrentSymbol] / TotalScaled);

      LowToBiner  := BitPadding(DecimalToBiner(Low), 16, 'LEFT');
      HighToBiner := BitPadding(DecimalToBiner(High), 16, 'LEFT');

      for I := 1 to 999999 do begin
        if LowToBiner[1] = HighToBiner[1] then begin
          { Shift In }
          LowToBiner  := ShiftMSB(LowToBiner, '0');
          HighToBiner := ShiftMSB(HighToBiner, '1');
          Inc(B);
          Code := Copy(EncodedBits, B, 16);
        end else begin
          if (SubStr(LowToBiner, 2, 1) = '1') and (SubStr(HighToBiner, 2, 1) = '0') then begin
            { Shift In Underflow}
            LowToBiner  := ShiftUnderflow(LowToBiner, '0');
            HighToBiner := ShiftUnderflow(HighToBiner, '1');
            Inc(B);
            Opposite := OppositeBit(Copy(EncodedBits, B, 1));
            Code := Opposite + Copy(EncodedBits, B + 1, 15);
            // shift
          end else begin
            Low  := BinerToDecimal(LowToBiner);
            High := BinerToDecimal(HighToBiner);
            Break; // continue to next symbol
          end;
        end;
      end;
      Inc(J);
    end; { end while }
    DecodedStream := DecodedSymbols;
    Stop := Now;
    Elapsed := FormatDateTime('hh:nn:ss:zzz', Stop - Start);
    MessageDlg('AC Decompression success. Required time : ' + Elapsed + ' seconds', MtInformation, [MbOK], 0);

    Loading.Caption := '';
    SaveAs1.Enabled := True;
    SaveAutomatically(DecodedStream, 'ac', FALSE);

    { show the statistic to the grid Cells[Column, Rows]}
    StringGrid1.Cells[0, DecompressGridCounter] := 'AC';
    StringGrid1.Cells[1, DecompressGridCounter] := ExtractFileName(StrFileName);
    StringGrid1.Cells[2, DecompressGridCounter] := IntToStr(IntFileSize) + ' bytes';
    StringGrid1.Cells[3, DecompressGridCounter] := IntToStr(Length(DecodedStream)) + ' bytes';
    StringGrid1.Cells[4, DecompressGridCounter] := Elapsed + ' seconds';
    Inc(DecompressGridCounter);
    StringGrid1.RowCount := 3 + DecompressGridCounter;
  end else begin
     FILE_CORRUPT_ERROR('AC');
  end;
end;

procedure TfrmMAIN.About1Click(Sender: TObject);
begin
  frmAbout.Show;
end;

procedure TfrmMAIN.HelpTopic1Click(Sender: TObject);
begin
  showMessage('Not available!'); 
end;

end.

