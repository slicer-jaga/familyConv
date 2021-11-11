unit uZenmoney;

interface

uses
  System.SysUtils, System.Classes,
  Data.DB,
  uConv;

type
  TZenMoneyConverter = class(TCustomFamilyConverter)
  protected
    FList: TStringList;
    FFmt: TFormatSettings;
    procedure DoInitConverter; override;

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

    // Основной запрос
    C_SQL = 'select reestr.re_date, cat0.cat0_name, reestr.re_tag, reestr.re_koment, payee.payee_name, reestr.re_money, scheta.sch_name, reestr.re_id, reestr.re_trans_re, reestr.re_incr'
      + ' from reestr left join category on category.cat_id = reestr.re_cat_id left join cat0 on cat0.cat0_id = category.cat_id0'
      + ' left join payee on payee.payee_id = reestr.re_paye_id' +
      ' left join scheta on scheta.sch_id = reestr.re_sch_id';
    // Запрос парной транзакции
    C_SQLTransaction =
      'select reestr.re_date, cat0.cat0_name, reestr.re_tag, reestr.re_koment, payee.payee_name, reestr.re_money, scheta.sch_name, reestr.re_id, reestr.re_trans_re, reestr.re_incr'
      + ' from reestr left join category on category.cat_id = reestr.re_cat_id left join cat0 on cat0.cat0_id = category.cat_id0'
      + ' left join payee on payee.payee_id = reestr.re_paye_id' +
      ' left join scheta on scheta.sch_id = reestr.re_sch_id where reestr.re_id = %s';

    procedure DoConvert; override;
    procedure AddLine(const AValues: TArray<string>);
    procedure ReadLine(const ALine: TArray<string>; const AFields: TFields);

    procedure GetTags(const AValues: TArray<string>;
      out ATags, AComment: string);
    procedure GetPayer(const AValues: TArray<string>; out APayer: string);
    function GetBill(const AValues: TArray<string>;
      out ASrcBill, ADec, ADstBill, AInc: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.IOUtils,
  uConsts,
  uDB;

{ TZenMoneyConverter }

procedure TZenMoneyConverter.AddLine(const AValues: TArray<string>);
var
  lPayer, lTags, lSrcBill, lDstBill, lDec, lInc, lComment: string;
begin
  // Пропуск пустой строки
  if (AValues[C_zqcDate] = '') or (StrToDate(AValues[C_zqcDate]) > Now) then
    Exit;

  if not GetBill(AValues, lSrcBill, lDec, lDstBill, lInc) then
    Exit;
  GetTags(AValues, lTags, lComment);
  GetPayer(AValues, lPayer);

  // Добавляем операции в соответствующие счета
  if lSrcBill <> '' then
    AddBill(lSrcBill, -StrToCurr(lDec, FFmt));
  if (lDstBill <> '') and (lDstBill <> lSrcBill) then
    AddBill(lDstBill, StrToCurr(lInc, FFmt));

  FLine.Clear;
  FLine.Add(AValues[C_zqcDate]);
  FLine.Add(lTags);
  FLine.Add(lPayer);
  FLine.Add(lSrcBill);
  FLine.Add(lDec);
  FLine.Add(lDstBill);
  FLine.Add(lInc);
  FLine.Add(lComment);

  FList.Add(FLine.DelimitedText);
end;

constructor TZenMoneyConverter.Create;
begin
  inherited;

  FList := TStringList.Create;

  // Для конвертации дробных чисел
  FFmt := TFormatSettings.Create;
  FFmt.DecimalSeparator := ',';
end;

destructor TZenMoneyConverter.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TZenMoneyConverter.DoConvert;
var
  lLine: TArray<string>;
begin
  SetLength(lLine, dmDB.sqlReestr.Fields.Count);
  // Обход таблицы операций
  dmDB.sqlReestr.First;
  while not dmDB.sqlReestr.Eof do
  begin
    ReadLine(lLine, dmDB.sqlReestr.Fields);
    AddLine(lLine);
    dmDB.sqlReestr.Next;
  end;

  if FList.Count > 0 then
    FList.SaveToFile(FOptions.Directory + TPath.GetFileNameWithoutExtension
      (FOptions.Database) + '.csv', TEncoding.GetEncoding(1251))
  else
    raise EConvException.Create('Не удалось найти данные Family 12.',
      C_ExitCode_Error);
end;

procedure TZenMoneyConverter.DoInitConverter;
begin
  dmDB.sqlReestr.SQL.Text := C_SQL;
end;

function TZenMoneyConverter.GetBill(const AValues: TArray<string>;
  out ASrcBill, ADec, ADstBill, AInc: string): Boolean;
var
  lMoney: Currency;
  lNewMoney: Currency;
  lIdx: Integer;
  lSrc, lDst, lTrans: TArray<string>;
begin
  Result := True;
  ASrcBill := '';
  ADec := '';
  ADstBill := '';
  AInc := '';

  // Пропускаем обработанные транзакции
  if FTransactions.Find(AValues[C_zqcID], lIdx) then
    Exit(False);

  // Не удалось сконвертировать деньги - такого быть не должно
  if not TryStrToCurr(AValues[C_zqcMoney], lMoney, FFmt) then
  begin
    WriteLn('Не удалось конвертировать сумму: ' + AValues[C_zqcMoney]);
    Exit(False);
  end;

  // Перевод между счетами
  if ((AValues[C_zqcCategory] = 'Перевод денег') or
    (AValues[C_zqcCategory] = '(Несколько записей)')) and
    (AValues[C_zqcPayer] = '<Служебный>') then
  begin
    // Делаем запрос парной операции
    dmDB.sqlCustom.SQL.Text := Format(C_SQLTransaction,
      [AValues[C_zqcTransaction]]);
    dmDB.sqlCustom.Active := True;

    dmDB.sqlCustom.First;
    if dmDB.sqlCustom.Eof then
    begin
      // По идее это не должно быть серьезной ошибкой...
      WriteLn('Не удалось получить парную операцию: ' + AValues[C_zqcID] + '->'
        + AValues[C_zqcTransaction]);
      Exit(False);
    end;

    SetLength(lTrans, Length(AValues));
    ReadLine(lTrans, dmDB.sqlCustom.Fields);

    // Определяем счета (исходный\конечный)
    if lMoney < 0 then
    begin
      lSrc := AValues;
      lDst := lTrans;
    end
    else
    begin
      lSrc := lTrans;
      lDst := AValues;
    end;

    FTransactions.Add(lTrans[C_zqcID]);

    lMoney := StrToCurr(lSrc[C_zqcMoney], FFmt);
    lNewMoney := StrToCurr(lDst[C_zqcMoney], FFmt);

    ASrcBill := lSrc[C_zqcBill];
    ADstBill := lDst[C_zqcBill];

    ADec := CurrToStr(Abs(lMoney), FFmt);
    AInc := CurrToStr(Abs(lNewMoney), FFmt);
  end
  else
  begin
    // Обычный перевод
    if lMoney < 0 then
    begin
      // Расход
      ASrcBill := AValues[C_zqcBill];
      ADstBill := '';
      AInc := '';
      ADec := CurrToStr(Abs(lMoney), FFmt);
    end
    else
    begin
      // Доход
      ASrcBill := '';
      ADstBill := AValues[C_zqcBill];
      AInc := CurrToStr(Abs(lMoney), FFmt);
      ADec := '';
    end;
  end;

  FTransactions.Add(AValues[C_zqcID]);
end;

procedure TZenMoneyConverter.GetPayer(const AValues: TArray<string>;
  out APayer: string);
begin
  APayer := AValues[C_zqcPayer];
  if (APayer = '') or (APayer = '<Служебный>') then
    APayer := '';
end;

procedure TZenMoneyConverter.GetTags(const AValues: TArray<string>;
  out ATags, AComment: string);
var
  lCategory: string;
begin
  lCategory := AValues[C_zqcCategory];
  ATags := AValues[C_zqcTag];
  AComment := AValues[C_zqcComment];
  ConvertCategoriesToTags(lCategory, ATags, AComment);
end;

procedure TZenMoneyConverter.ReadLine(const ALine: TArray<string>;
  const AFields: TFields);
var
  i: Integer;
begin
  for i := 0 to AFields.Count - 1 do
    ALine[i] := AFields[i].Text;
end;

end.
