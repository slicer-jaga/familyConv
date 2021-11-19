unit uBudgetBakers;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  uConv;

type
  TBudgetBakersConverter = class(TCustomFamilyConverter)
  protected
    FFmt: TFormatSettings;
    FBills: TObjectDictionary<string, TStringList>;

  const
    C_zqcDate = 0;
    C_zqcCategory = 1;
    C_zqcTag = 2;
    C_zqcComment = 3;
    C_zqcPayer = 4;
    C_zqcMoney = 5;
    C_zqcBill = 6;
    C_zqcID = 7;
    C_zqcTransaction = 8;
    C_zqcInc = 9;

    C_Header: array [0 .. 5] of string = ('����', '���������', '����������',
      '�����', '�������', '�����');
    // �������� ������
    C_SQL = 'select reestr.re_date, re_cat_id, reestr.re_tag, reestr.re_koment, payee.payee_name, reestr.re_money, scheta.sch_name, reestr.re_id, reestr.re_trans_re, reestr.re_incr'
      + ' from reestr left join category on category.cat_id = reestr.re_cat_id left join cat0 on cat0.cat0_id = category.cat_id0'
      + ' left join payee on payee.payee_id = reestr.re_paye_id' +
      ' left join scheta on scheta.sch_id = reestr.re_sch_id';

    procedure DoConvert; override;
    procedure DoInitConverter; override;

    procedure AddLine(const AValues: TArray<string>);

    procedure GetTags(const AValues: TArray<string>;
      out ATags, AComment: string);
    procedure GetPayer(const AValues: TArray<string>; out APayer: string);
    function GetCategory(const AID: string): string;
    function GetBill(const AValues: TArray<string>; out ABill, AMoney: string;
      out AIsTransaction: Boolean): Boolean;

    procedure SaveBills;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.IOUtils,
  uConsts,
  uDB;

{ TBudgetBakersConverter }

procedure TBudgetBakersConverter.AddLine(const AValues: TArray<string>);
var
  lBill, lMoney: string;
  lTags, lComment: string;
  lPayer: string;
  lCategory: string;
  lIsTransaction: Boolean;
  lBillList: TStringList;
begin
  // ������� ������ ������
  if (AValues[C_zqcDate] = '') or (StrToDate(AValues[C_zqcDate], FFamilyFmt)
    > Now) then
    Exit;

  if not GetBill(AValues, lBill, lMoney, lIsTransaction) then
    Exit;

  GetTags(AValues, lTags, lComment);
  GetPayer(AValues, lPayer);

  if not lIsTransaction then
    lCategory := AValues[C_zqcCategory]
  else
    lCategory := 'TRANSFER';

  lCategory := FindCategory(lCategory, FTags.ToStringArray);

  if lBill <> '' then
    AddBill(lBill, StrToCurr(lMoney, FFmt));

  FLine.Clear;
  FLine.Add(DateToStr(StrToDate(AValues[C_zqcDate], FFamilyFmt), FFmt));
  FLine.Add(lCategory);
  FLine.Add(lPayer);
  FLine.Add(lMoney);
  FLine.Add(lComment);
  FLine.Add(lTags);

  if not FBills.TryGetValue(lBill, lBillList) then
  begin
    lBillList := TStringList.Create;
    AddLineDirect(lBillList, [C_Header[0], C_Header[1], C_Header[2],
      C_Header[3], C_Header[4], C_Header[5]]);
    FBills.Add(lBill, lBillList);
  end;

  lBillList.Add(FLine.DelimitedText);
end;

constructor TBudgetBakersConverter.Create;
begin
  inherited;

  FBills := TObjectDictionary<string, TStringList>.Create([doOwnsValues]);

  FTags.Delimiter := '|';

  // ��� ����������� ������� �����
  FFmt := TFormatSettings.Create;
  FFmt.DecimalSeparator := '.';
  FFmt.ShortDateFormat := 'yyyy-mm-dd';
end;

destructor TBudgetBakersConverter.Destroy;
begin
  FreeAndNil(FBills);
  inherited;
end;

procedure TBudgetBakersConverter.DoConvert;
var
  lLine: TArray<string>;
begin
  SetLength(lLine, dmDB.sqlReestr.Fields.Count);
  // ����� ������� ��������
  dmDB.sqlReestr.First;
  while not dmDB.sqlReestr.Eof do
  begin
    ReadLine(lLine, dmDB.sqlReestr.Fields);
    lLine[C_zqcCategory] := GetCategory(lLine[C_zqcCategory]);
    lLine[C_zqcTag] := lLine[C_zqcTag].Replace(',', '|');
    AddLine(lLine);
    dmDB.sqlReestr.Next;
  end;

  if FBills.Count > 0 then
    SaveBills
  else
    raise EConvException.Create('�� ������� ����� ������ Family 12.',
      C_ExitCode_Error);
end;

procedure TBudgetBakersConverter.DoInitConverter;
begin
  dmDB.sqlReestr.SQL.Text := C_SQL;

  // ������ �� ��������� BB
  AddCategory('��� � �������', ['���', '�������']);
  AddCategory('���, ����', ['������']);
  AddCategory('��������');
  AddCategory('��������, ����-���');
  AddCategory('�������', ['������������� �������']);
  AddCategory('������', ['���������']);
  AddCategory('����');
  AddCategory('��� � ���');
  AddCategory('�������� ��������');
  AddCategory('�����������');
  AddCategory('������� � ��������', ['�����', '����������']);
  AddCategory('������ � �����', ['�����']);
  AddCategory('�����');
  AddCategory('�������, ���������', ['�������']);
  AddCategory('���������, ����������', ['�����']);
  AddCategory('�����������, ����������', ['�������', '���������']);
  AddCategory('�����', []);
  AddCategory('������', []);
  AddCategory('�������', []);
  AddCategory('������', []);
  AddCategory('����������� ���������', []);
  AddCategory('������', []);
  AddCategory('�������������, ������������ ������', []);
  AddCategory('���������', []);
  AddCategory('������� �������', []);
  AddCategory('������� �������', []);
  AddCategory('������������ ���������', ['�����']);
  AddCategory('�����', []);
  AddCategory('������', []);
  AddCategory('������', []);
  AddCategory('��������', []);
  AddCategory('����������� ����������', []);
  AddCategory('���������������', []);
  AddCategory('�������', []);
  AddCategory('����������� � �����', ['�����������']);
  AddCategory('��������, �����', []);
  AddCategory('�����, �����, ��������', []);
  AddCategory('�������� � �����', []);
  AddCategory('�������', ['������������', '��������', '�����', '����', '��������']);
  AddCategory('�������, �������� ����', []);
  AddCategory('�����������, ��������', []);
  AddCategory('��������������� ���������, �������', ['�������', '������']);
  AddCategory('������, �������, �����', []);
  AddCategory('������� � �������������������', ['������� (����.)']);
  AddCategory('���������', ['��������']);
  AddCategory('�����, ������', []);
  AddCategory('�� � �������', ['��']);
  AddCategory('�����', []);
  AddCategory('�����, ��', ['PC']);
  AddCategory('��������', []);
  AddCategory('�������� ������', []);
  AddCategory('���������, ����', ['����', 'Steam']);
  AddCategory('�������', ['���������']);
  AddCategory('���������� �������', ['�����']);
  AddCategory('��������', []);
  AddCategory('�����, ��������', []);
  AddCategory('������������', []);
  AddCategory('������', ['�����']);
  AddCategory('�����, �����', ['����']);
  AddCategory('�����������', []);
  AddCategory('������', []);
  AddCategory('����������', ['������']);
  AddCategory('���������', []);
  AddCategory('������������', []);
  AddCategory('����������', []);
  AddCategory('������������ ��������, �������� ���������', []);
  AddCategory('�����', []);
  AddCategory('��������', []);
  AddCategory('������ � ������', []);
  AddCategory('������� ����� (�����, �������)', ['�������']);
  AddCategory('����� �� ������', []);
  AddCategory('��������, �����-�������', ['��������', '���������']);
  AddCategory('������, ������', []);
  AddCategory('�������, �������� ����', []);
  AddCategory('�������', ['�������']);
  AddCategory('�������', []);
  AddCategory('��������, ���������', ['�������', '�������� �� �������', '�����']);
  AddCategory('����, ������', []);
  AddCategory('������', []);
  AddCategory('�����������', []);
end;

function TBudgetBakersConverter.GetBill(const AValues: TArray<string>;
  out ABill, AMoney: string; out AIsTransaction: Boolean): Boolean;
begin
  Result := True;
  ABill := AValues[C_zqcBill];
  AMoney := CurrToStr(StrToCurr(AValues[C_zqcMoney], FFamilyFmt), FFmt);
  AIsTransaction := ((AValues[C_zqcCategory] = '������� �����') or
    (AValues[C_zqcCategory] = '(��������� �������)')) and
    (AValues[C_zqcPayer] = '<���������>');
end;

function TBudgetBakersConverter.GetCategory(const AID: string): string;
var
  lRes: TArray<string>;
begin
  lRes := GetCategories(AID);
  if Length(lRes) > 0 then
    Result := lRes[High(lRes)]
  else
    Result := '';
end;

procedure TBudgetBakersConverter.GetPayer(const AValues: TArray<string>;
  out APayer: string);
begin
  APayer := AValues[C_zqcPayer];
  if (APayer = '') or (APayer = '<���������>') then
    APayer := '';
end;

procedure TBudgetBakersConverter.GetTags(const AValues: TArray<string>;
  out ATags, AComment: string);
var
  lCategory: string;
begin
  lCategory := AValues[C_zqcCategory];
  ATags := AValues[C_zqcTag];
  AComment := AValues[C_zqcComment];
  ConvertCategoriesToTags(lCategory, ATags, AComment);
end;

procedure TBudgetBakersConverter.SaveBills;
var
  lPair: TPair<string, TStringList>;
begin
  for lPair in FBills do
  begin
    if lPair.Value.Count > 0 then
      lPair.Value.SaveToFile(FOptions.Directory +
        TPath.GetFileNameWithoutExtension(FOptions.Database) + '_' +
        GetBillFilename(lPair.Key) + '.csv', TEncoding.UTF8)
  end;
end;

end.
