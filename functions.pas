unit functions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

{**
 * Framework for functions that will be used.
 * Write only param and its returning value
 *
 * @Param of IsInDictionary : Dict is an array with infinite length theorically,
 * but it should be finite. It is also called as a dynamic array. Only available
 * for parameter not for a variable declaration.  // <-- has been mooved
 **}
 
  function Get_File_Size(sFileToExamine: string; bInKBytes: Boolean): string;
  function FormatByteSize(const bytes: Longint): string;

  { estimate the complexity of these funtions }
  function DecimalToBiner( Dec : Integer) : string;
  function BinerToDecimal(Biner: string) : integer;
  function BitPadding(Str: string; LengthPad: byte; PadType: string) : string;
  function SubStr(Str: string; Start: integer; Long: integer):string;
  function Explode(D, S: String): TStringList;
  function Ceil(X: Real): Integer;
  function Floor(X: Real): Integer;
  function Pow(X, Y: Integer): Integer;

const
  { 16-bits }
  MAX = 16;

var
{**
  * 1 - 16 bits respectively for biner to decimal
  * 16 bits mean 2 power of 16 in which there are possible integer from 0 - 65535
  * (the total of its number is 65535, since it begins from 0)
  **}
  Power : array[0..MAX] of Integer = (1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024,
                                      2048, 4096, 8192, 16384, 32768, 65536);
                                      
{**
 * All functions that has been declared in the interface will be
 * implemented here.
 **}
implementation

{**
 * This function get the file size in string format
 * @Param1 : <string> sFileToExamine : the name of file
 * @Param2 : <boolean> bInKBytes : default is true
 * @Return : <string>
 **}
function Get_File_Size(sFileToExamine: string; bInKBytes: Boolean): string;
var
  SearchRec: TSearchRec;
  sgPath: string;
  inRetval, I1: Integer;
begin
  sgPath := ExpandFileName(sFileToExamine);
  try
    inRetval := FindFirst(ExpandFileName(sFileToExamine), faAnyFile, SearchRec);
    if inRetval = 0 then
      I1 := SearchRec.Size
    else
      I1 := -1;
  finally
    SysUtils.FindClose(SearchRec);
  end;

  { 'Result' is a reserve word }
  { we can use 'Result' instead of 'Get_File_Size' to return the value }
  Result := IntToStr(I1);
end;

{ This function reformat the file size }
function FormatByteSize(const bytes: Longint): string;
const
   B = 1;          { byte     }
   KB = 1024 * B;  { kilobyte }
   MB = 1024 * KB; { megabyte }
   GB = 1024 * MB; { gigabyte }
begin
   if bytes > GB then begin
     result := FormatFloat('#.## GB', bytes / GB)
   end else if bytes > MB then begin
       result := FormatFloat('#.## MB', bytes / MB)
   end else if bytes > KB then begin
         result := FormatFloat('#.## KB', bytes / KB)
   end else begin
         result := FormatFloat('#.## bytes', bytes) ;
   end;
end;

{**
 * Convertion decimal number to biner using 16-bits
 * @Param 1  : <integer> Dec : ex : 0000h - FFFFh
 * @Return   : <string> Biner : ex : 010101000
 **}
function DecimalToBiner(Dec : Integer) : string;
var
  Biner : string;
  Modules : integer;
begin
    while( Dec > 0 ) do begin
      Modules := Dec mod 2;
      Dec := Dec div 2;
      Biner := IntToStr(modules) + Biner;
    end;
    
    Result := Biner;
end;

{**
 * Opposite of 'DecimalToBiner' convertion biner to decimal using 16-bits
 * @Param 1  : <string> biner : ex 65535
 * @Return   : <integer> integer : ex FFFFh
 **}
function BinerToDecimal(Biner: string) : integer;
var
  I, Dec, Index : integer;
begin
  Dec := 0;
  Index := 0;
  for I := Length(Biner) downto 1  do begin
     Dec := Dec + (Power[Index] * StrToInt(Biner[i]) );
     Inc(Index);
  end;

  Result := Dec;
end;

{**
 * This function will adding '0' as much as 'LengthPad' to the most left of 'Str'
 * @Param 1  : <string> Str : ex '101011'
 * @Param 2  : <byte> LengthPad : How Length ?
 * @Param 3  : <const> PadType : to LEFT or to RIGHT?
 * @Return   : <string> : Return the padding value.
 **}
function BitPadding(Str: string; LengthPad: byte; PadType: string) : string;
var
  I : Byte;
begin
   for I := Length(Str) to LengthPad - 1 do begin
    if PadType = 'LEFT' then begin
      Str := '0' + Str;
    end else begin
      Str := Str + '0';
    end;
   end;
   Result := Str;
end;

{**
 * This function will take a substring from a string from 'start' as much as 'long' characters
 * @Param 1  : <string> Str : String to be taken
 * @Param 2  : <byte> Start : Start from index
 * @Param 3  : <const> Long : string long
 * @Return   : <string> : a substring starting from 'start' as much as 'long'
 **}
function SubStr(Str: string; Start: integer; Long: integer):string;
var
  Output : string;
  I : integer;
begin
  Output := '';
  for I := Start to ((Start + Long) - 1) do begin
    Output := Output + Str[I];
  end;
  Result := Output;
end;

{**
 * This function break or split string based on the delimeter inside the string
 * to be substrings that identifed by index start from 0
 * @Param 1  : <string> D : Indicator
 * @Param 2  : <string> S : String to be fold
 * @Return   : <TStringList>  : Return array of string
 **}
function Explode(D : String; S : String): TStringList;
var
  C: Integer;
begin
  Result := TStringList.Create;
  C := 0;
  while S <> '' do begin
    if Pos(D, S) > 0 then begin
      Result.Add(Copy(S, 1, Pos(D, S) - 1));
      Delete(S, 1, Length(Result[C]) + Length(D));
    end else begin
      Result.Add(S);
      S := '';
    end;
    Inc(C);
  end;
end;

function Ceil(X: Real): Integer;
var
  Format: Integer;
begin
  if X < 1 then begin
    Result := 1;
  end else begin
    { Round first, ceil if >= 5 othwerwise floor }
    Format := StrToInt(FormatFloat('#', X));
    { means: ceiling occurss or not, so we need let as it }
    if (Format > X) or (Format = X) then begin
      Result := Format;
    end else begin
      Result := Format + 1;
    end;
  end;
end;

function Floor(X: Real): Integer;
var
  Format: Integer;
begin
  if X < 1 then begin
    Result := 0;
  end else begin
    Format := StrToInt(FormatFloat('#', X));
    if (Format > X) then begin
      Result := Format - 1;
    end else begin
      Result := Format;
    end;
  end;
end;

function Pow(X, Y: Integer): Integer;
var
  I, Res: Integer;
begin
  Res := 1;
  for I := 1 to Y do begin
    Res := Res * X;
  end;
  Result := Res;
end;

end.
