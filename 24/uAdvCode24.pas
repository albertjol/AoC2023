unit uAdvCode24;

{$mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls
  , Generics.Collections
  ;

type

  { TStone }

  TStone = class
  private
    FSlope: Double;
    FIntercept: Double;

    function GetSlope: Double;
    function GetIntercept: Double;
  public
    X: Int64;
    Y: Int64;
    Z: Int64;
    VX: Integer;
    VY: Integer;
    VZ: Integer;

    constructor Create(); overload;
    constructor Create(AX, AY, AZ: Int64; AVX, AVY, AVZ: Integer); overload;

    property Slope: Double read GetSlope;
    property Intercept: Double read GetIntercept;
  end;

  { TFrmAdvCode }

  TFrmAdvCode = class(TForm)
    btn_Start: TButton;
    ed_Answer1: TEdit;
    ed_Answer2: TEdit;
    ed_Filename: TFileNameEdit;
    lbl_Answer2: TLabel;
    lbl_Filename: TLabel;
    lbl_Answer1: TLabel;

    procedure btn_StartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure part1(stones: TObjectList<TStone>);
    procedure part2(stones: TObjectList<TStone>);
    function ParseStones(lines: TStringList): TObjectList<TStone>;

  private

  public

  end;

var
  FrmAdvCode: TFrmAdvCode;

implementation

uses
  Math
  ;

{$R *.lfm}

{ TStone }

constructor TStone.Create();
begin
  FSlope := NaN;
  FIntercept := NaN;
end;

constructor TStone.Create(AX, AY, AZ: Int64; AVX, AVY, AVZ: Integer);
begin
  Create();
  X := AX;
  Y := AY;
  Z := AZ;
  VX := AVX;
  VY := AVY;
  VZ := AVZ;
end;

function TStone.GetSlope: Double;
begin
  // y = ax + b
  // We want a.
  if IsNan(FSlope) then
  begin
    if VY = 0 then
       raise Exception.Create('Infinite slope');
    FSlope := VY / VX;
  end;

  Result := FSlope;
end;

function TStone.GetIntercept: Double;
begin
  // y = ax + b
  // We want b.
  // b = y where x = 0
  // b = y - ax
  if IsNan(FIntercept) then
  begin
    FIntercept := Y.ToDouble() - X.ToDouble() * Slope;
  end;
  Result := FIntercept
end;

{ TFrmAdvCode }

procedure TFrmAdvCode.btn_StartClick(Sender: TObject);
var
  lines: TStringList;
  stones: TObjectList<TStone>;
begin
  with TStringList.Create() do
  begin
    try
      Add(ed_Filename.Text);
      SaveToFile('settings.txt');
    finally
      Free();
    end;
  end;
  lines := TStringList.Create();
  try
    lines.LoadFromFile(ed_Filename.Text);
    stones := ParseStones(lines);
    part1(stones);
    part2(stones);
  finally
    FreeAndNil(stones);
    FreeAndNil(lines);
  end;
end;

procedure TFrmAdvCode.FormCreate(Sender: TObject);
begin
  if FileExists('settings.txt') then
  begin
    with TStringList.Create() do
    begin
      try
        LoadFromFile('settings.txt');
        if(Count > 0) then
        begin
          ed_Filename.Text := Strings[0];
        end;
      finally
        Free();
      end;
    end;
  end;
end;

procedure TFrmAdvCode.part1(stones: TObjectList<TStone>);
const
  MIN_XY = 200000000000000; // or 7
  MAX_XY = 400000000000000; // or 27
var
  i,j: Integer;
  x,y: Double;
  s1, s2: TStone;
  count: Integer = 0;
begin
  for i := 0 to stones.Count - 1 do
  begin
    for j := i + 1 to stones.Count - 1 do
    begin
      s1 := stones[i]; s2 := stones[j];
      if s1.Slope = s2.Slope then
      begin
        // Paths are parallel;
        continue;
      end;
      x := (s2.Intercept - s1.Intercept) / (s1.Slope - s2.Slope);
      y := s1.Slope * x + s1.Intercept;

      if((MIN_XY <= x) and (x <= MAX_XY) and (MIN_XY <= y) and (y <= MAX_XY)) then
      begin
        // Inside!
        // Now, is it in the future (for both stones)?
        // x = x_0 + vx*t
        // is t positive for xIntersection?
        // x/vx = x_0/vx + t
        // t = x/vx - x_0/vx
        if(((x/s1.VX) - (s1.X/s1.VX)) >= 0) and (((x/s2.VX) - (s2.X/s2.VX)) >= 0) then
          Inc(count);
      end;
    end;
  end;
  ed_Answer1.Text := IntToStr(count);
end;

procedure TFrmAdvCode.part2(stones: TObjectList<TStone>);
begin
  ed_Answer2.Text := IntToStr(999);
end;

function TFrmAdvCode.ParseStones(lines: TStringList): TObjectList<TStone>;
var
  line: String;
  values: TStringArray;
begin
  Result := TObjectList<TStone>.Create();
  for line in lines do
  begin
    values := line.Split([',', ' ', '@'], TStringSplitOptions.ExcludeEmpty);
    Result.Add(TStone.Create(
      StrToInt64(values[0]), StrToInt64(values[1]), StrToInt64(values[2])
      , StrToInt(values[3]), StrToInt(values[4]),   StrToInt(values[5])
    ));
  end;
end;

end.

