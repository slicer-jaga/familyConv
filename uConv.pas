unit uConv;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Data.DB;

type
  TFamilyConvertOptions = record
    Database: string;
    User: string;
    Password: string;
    Directory: string;
    Converter: string;

    SaveBills: Boolean;
  end;

  IFamilyConverter = interface
    ['{198E6DDD-3504-4010-9E27-17663B8B486B}']
    function Convert(const AOptions: TFamilyConvertOptions): Boolean;
  end;

  TCustomFamilyConverter = class(TInterfacedObject, IFamilyConverter)
  protected
    FOptions: TFamilyConvertOptions;
    // Категории
    FDefCategories: TStringList;
    // Удаляемые или заменяемые тэги
    FRemovedTags: TDictionary<string, string>;
    // Для работы с тэгами
    FTags: TStringList;
    // Перечень счетов и их остатков
    FBills: TDictionary<string, Currency>;
    // Текущая строка
    FLine: TStringList;
    // Обработанные операции
    FTransactions: TStringList;
    FFamilyFmt: TFormatSettings;
    // Исходные категории
    FConverterCategories: TStringList;
    // Заменяемые категории
    FReplacedCategories: TStringList;

  const
    C_SQLCategory =
      'select t1.cat0_name, t2.cat0_name, t3.cat0_name from category left join cat0 t1 on t1.cat0_id = category.cat_id0 left join cat0 t2 on t2.cat0_id = category.cat_id1 left join cat0 t3 on t3.cat0_id = category.cat_id2 where category.cat_id = %s';

    procedure InitRemovedTags;

    // Добавление денег на счет
    procedure AddBill(const ABill: string; const AMoney: Currency);

    procedure DoInitConverter; virtual;
    procedure DoConvert; virtual; abstract;
    procedure DoReadCategories;

    procedure SaveBills;

    function CopyLine(const ALine: TArray<string>): TArray<string>;
    procedure AddLineDirect(const AList: TStrings; const ALine: TArray<string>);

    // Заменяет категории AReplacements на ACategory
    procedure AddCategory(const ACategory: string; const AReplacements: TArray<string> = nil);

    function GetBillFilename(const ABill: string): string;

    function MergeLines(const ALines: TArray<string>;
      const ADelimiter: string): string;

    function GetCategories(const AID: string): TArray<string>;

    // Возвращает новую категорию по старой категории и тегам
    function FindCategory(const ACategory: string; const ATags: TArray<string>): string;

    // Преобразует категории в тэги, устраняет дублирование
    procedure ConvertCategoriesToTags(var ACategory, ATags, AComment: string);
    // Заменяет или удаляет лишние тэги
    procedure RemoveTags(const ATags: TStringList);

    procedure ReadLine(const ALine: TArray<string>; const AFields: TFields);
  public
    function Convert(const AOptions: TFamilyConvertOptions): Boolean;

    constructor Create;
    destructor Destroy; override;
  end;

  TFamilyConverterFactory = class
  public
    class function GetConverterInstance(const AID: string): IFamilyConverter;
    class function GetOptions(var AOptions: TFamilyConvertOptions): Boolean;
  end;

  EConvException = class(Exception)
  public
    ExitCode: Integer;

    constructor Create(const AMsg: string; const AExitCode: Integer);
  end;

implementation

uses
  System.IOUtils,
  uConsts,
  uDB,
  uZenmoney,
  uBudgetBakers;

{ TFamilyConverterFactory }

class function TFamilyConverterFactory.GetConverterInstance(const AID: string)
  : IFamilyConverter;
begin
  if (AID = '') or (AID = 'zm') then
    Result := TZenMoneyConverter.Create
  else if AID = 'bb' then
    Result := TBudgetBakersConverter.Create
  else
    Result := nil;

  if not Assigned(Result) then
    raise EConvException.Create('Не удалось создать конвертер',
      C_ExitCode_CantCreateConverter);
end;

class function TFamilyConverterFactory.GetOptions(var AOptions
  : TFamilyConvertOptions): Boolean;
var
  lDB: string;
  lDir: string;
  lConv: string;
begin
  FillChar(AOptions, SizeOf(AOptions), 0);

  if (ParamCount = 0) or FindCmdLineSwitch('?') or FindCmdLineSwitch('help')
  then
  begin
    WriteLn('Выгрузка данных программы Family 12');
    WriteLn;
    WriteLn('Использование: familyConv "<Файл_базы_данных>.fdb" [/c "<Тип_выгрузки>"] [/d "<Каталог_выгрузки>"]');
    WriteLn;
    WriteLn('  '#9#9'Файл базы данных (".fdb").');
    WriteLn('  /d'#9#9'Каталог выгружаемых файлов.');
    WriteLn('  /c'#9#9'Тип выгружаемого файла:');
    WriteLn('  '#9#9#9'"zm" - https://zenmoney.ru/');
    WriteLn('  '#9#9#9'"bb" - https://budgetbakers.com/');

    Exit(False);
  end;

  if ParamCount > 0 then
    lDB := ParamStr(1)
  else
    lDB := '';
  if not FileExists(lDB) then
    raise EConvException.Create('Необходимо задать базу Family 12',
      C_ExitCode_NoDB);

  AOptions.Database := lDB;
  AOptions.User := 'sysdba';
  AOptions.Password := 'masterkey';

  if FindCmdLineSwitch('c', lConv) then
    AOptions.Converter := lConv;

  if FindCmdLineSwitch('d', lDir) then
    AOptions.Directory := IncludeTrailingPathDelimiter(lDir)
  else
    AOptions.Directory := ExtractFilePath(ParamStr(0));
  ForceDirectories(AOptions.Directory);

  AOptions.SaveBills := True;

  Result := True;
end;

{ TCustomFamilyConverter }

procedure TCustomFamilyConverter.AddBill(const ABill: string;
  const AMoney: Currency);
var
  lMoney: Currency;
begin
  if not FBills.TryGetValue(ABill, lMoney) then
    lMoney := 0;
  lMoney := lMoney + AMoney;
  FBills.AddOrSetValue(ABill, lMoney);
end;

procedure TCustomFamilyConverter.AddCategory(const ACategory: string;
  const AReplacements: TArray<string>);
var
  lIdx: NativeInt;
  lRep: string;
begin
  lIdx := FConverterCategories.Add(ACategory);
  for lRep in AReplacements do
    FReplacedCategories.AddObject(lRep, TObject(lIdx));
end;

procedure TCustomFamilyConverter.AddLineDirect(const AList: TStrings;
  const ALine: TArray<string>);
var
  lTmp: TStringList;
begin
  lTmp := TStringList.Create;
  try
    lTmp.Delimiter := ';';
    lTmp.AddStrings(ALine);
    AList.Add(lTmp.DelimitedText);
  finally
    lTmp.Free;
  end;
end;

function TCustomFamilyConverter.Convert(const AOptions
  : TFamilyConvertOptions): Boolean;
begin
  Result := True;
  FOptions := AOptions;

  dmDB := TdmDB.Create(nil);

  dmDB.FDPhysFBDriverLink.VendorLib := ExtractFilePath(ParamStr(0)) +
    'fbclientd20.dll';
  dmDB.dbConnection.Params.DriverID := 'FB';
  dmDB.dbConnection.Params.Database := FOptions.Database;
  dmDB.dbConnection.Params.UserName := FOptions.User;
  dmDB.dbConnection.Params.Password := FOptions.Password;

  DoInitConverter;

  dmDB.dbConnection.Connected := True;
  dmDB.sqlReestr.Active := True;
  dmDB.sqlCategories.Active := True;

  if not dmDB.dbConnection.Connected or not dmDB.sqlReestr.Active then
    raise EConvException.Create('Не удалось подключиться к базе',
      C_ExitCode_CantConnect);

  DoReadCategories;

  WriteLn(Format('Конвертация базы "%s"...', [FOptions.Database]));
  DoConvert;

  if FOptions.SaveBills then
    SaveBills;
end;

procedure TCustomFamilyConverter.ConvertCategoriesToTags(var ACategory, ATags,
  AComment: string);
var
  lIdx: Integer;
  lCommentTag: string;
  lCommentTags: TArray<string>;
  lInsert: Integer;
begin
  FTags.Clear;

  if ATags <> '' then
    FTags.DelimitedText := ATags;

  // Если комментарий содерит категорию, то переносит его в тэги
  if FDefCategories.Find(AComment, lIdx) then
  begin
    lCommentTags := AComment.Split([': ']);
    lInsert := 0;
    for lCommentTag in lCommentTags do
      if (lCommentTag <> '') and (FTags.IndexOf(lCommentTag) = -1) then
      begin
        FTags.Insert(lInsert, lCommentTag);
        Inc(lInsert);
      end;
    AComment := '';
  end;

  // Добавляет категорию в тэги
  if ACategory <> '' then
  begin
    lCommentTags := ACategory.Split([': ']);
    lInsert := 0;
    for lCommentTag in lCommentTags do
      if (lCommentTag <> '') and (FTags.IndexOf(lCommentTag) = -1) then
      begin
        FTags.Insert(lInsert, lCommentTag);
        Inc(lInsert);
      end;
  end;

  // Убирает дублирование комментария
  if (AComment <> '') and (FTags.IndexOf(AComment) <> -1) then
    AComment := '';

  RemoveTags(FTags);

  ATags := MergeLines(FTags.ToStringArray, FTags.Delimiter);
end;

function TCustomFamilyConverter.CopyLine(const ALine: TArray<string>)
  : TArray<string>;
var
  i: Integer;
begin
  SetLength(Result, Length(ALine));
  for i := Low(ALine) to High(ALine) do
    Result[i] := ALine[i];
end;

constructor TCustomFamilyConverter.Create;
begin
  inherited;

  FBills := TDictionary<string, Currency>.Create;

  FConverterCategories := TStringList.Create;

  FReplacedCategories := TStringList.Create;
  FReplacedCategories.Sorted := True;

  FFamilyFmt := TFormatSettings.Create;
  FFamilyFmt.ShortDateFormat := 'dd.mm.yyyy';
  FFamilyFmt.DecimalSeparator := ',';

  FDefCategories := TStringList.Create;
  FDefCategories.Sorted := True;
  FDefCategories.Duplicates := dupIgnore;

  FTransactions := TStringList.Create;
  FTransactions.Sorted := True;

  FLine := TStringList.Create;
  FLine.Delimiter := ';';

  FTags := TStringList.Create;
  FTags.Delimiter := ',';

  FRemovedTags := TDictionary<string, string>.Create;
  InitRemovedTags;
end;

destructor TCustomFamilyConverter.Destroy;
begin
  FreeAndNil(FDefCategories);
  FreeAndNil(FTags);
  FreeAndNil(FRemovedTags);
  FreeAndNil(FBills);
  FreeAndNil(FLine);
  FreeAndNil(FTransactions);
  FreeAndNil(FConverterCategories);
  FreeAndNil(FReplacedCategories);
  FreeAndNil(dmDB);
  inherited;
end;

procedure TCustomFamilyConverter.DoInitConverter;
begin
  //
end;

procedure TCustomFamilyConverter.DoReadCategories;
var
  i: Integer;
  lLine: TArray<string>;
begin
  // Читаем категории
  SetLength(lLine, dmDB.sqlCategories.Fields.Count - 1);

  dmDB.sqlCategories.First;
  while not dmDB.sqlCategories.Eof do
  begin
    for i := 0 to dmDB.sqlCategories.Fields.Count - 1 do
      lLine[i] := dmDB.sqlCategories.Fields[i].Text;

    if lLine[0] <> '' then
    begin
      FDefCategories.Add(lLine[0]);
      if lLine[1] <> '' then
      begin
        // Вложенные категории
        FDefCategories.Add(lLine[0] + ': ' + lLine[1]);
        if lLine[2] <> '' then
          FDefCategories.Add(lLine[0] + ': ' + lLine[1] + ': ' + lLine[2]);
      end;
    end;

    dmDB.sqlCategories.Next;
  end;
end;

function TCustomFamilyConverter.GetBillFilename(const ABill: string): string;
begin
  Result := StringReplace(ABill, '\', '', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '', [rfReplaceAll]);
  Result := StringReplace(Result, ':', '', [rfReplaceAll]);
  Result := StringReplace(Result, '*', '', [rfReplaceAll]);
  Result := StringReplace(Result, '?', '', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '', [rfReplaceAll]);
  Result := StringReplace(Result, '|', '', [rfReplaceAll]);
end;

function TCustomFamilyConverter.GetCategories(const AID: string)
  : TArray<string>;
var
  i: Integer;
  lNewText, lText: string;
begin
  dmDB.sqlCategories.SQL.Text := Format(C_SQLCategory, [AID]);
  dmDB.sqlCategories.Active := True;
  if not dmDB.sqlCategories.Eof then
  begin
    Result := nil;
    for i := 0 to dmDB.sqlCategories.Fields.Count - 1 do
    begin
      lText := dmDB.sqlCategories.Fields[i].Text;
      if FRemovedTags.TryGetValue(lText, lNewText) then
        lText := lNewText;

      if lText = '' then
        Break;

      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := lText;

      if (lText = 'Перевод') or (lText = 'Процент') then
        Break;
    end;
  end
  else
    Result := [AID];
end;

procedure TCustomFamilyConverter.InitRemovedTags;
begin
  FRemovedTags.Add('Развлечение', 'Развлечения');
  FRemovedTags.Add('Банковское обслуживание', 'Банк');
  FRemovedTags.Add('(Несколько записей)', '');
  FRemovedTags.Add('Перевод денег', 'Перевод');
  FRemovedTags.Add('Стоматолог', 'Стоматология');
  FRemovedTags.Add('Семейные расходы', 'Семья');
end;

function TCustomFamilyConverter.MergeLines(const ALines: TArray<string>;
  const ADelimiter: string): string;
var
  lLine: string;
begin
  Result := '';
  for lLine in ALines do
  begin
    if Result <> '' then
      Result := Result + ADelimiter;
    Result := Result + lLine;
  end;
end;

procedure TCustomFamilyConverter.ReadLine(const ALine: TArray<string>;
  const AFields: TFields);
var
  i: Integer;
begin
  for i := 0 to AFields.Count - 1 do
    ALine[i] := AFields[i].Text;
end;

procedure TCustomFamilyConverter.RemoveTags(const ATags: TStringList);
var
  i: Integer;
  lNewTag: string;
begin
  i := 0;
  while i < ATags.Count do
  begin
    if FRemovedTags.TryGetValue(ATags[i], lNewTag) then
    begin
      if (lNewTag <> '') and (ATags.IndexOf(lNewTag) = -1) then
        ATags[i] := lNewTag
      else
      begin
        ATags.Delete(i);
        Dec(i);
      end;
    end;
    Inc(i);
  end;
end;

function TCustomFamilyConverter.FindCategory(
  const ACategory: string; const ATags: TArray<string>): string;
var
  lIdx: Integer;
  lFinded: Integer;
  lTag: string;
begin
  Result := ACategory;
  if FReplacedCategories.Count = 0 then
    Exit;

  lFinded := MaxInt;
  if FReplacedCategories.Find(ACategory, lIdx) and (lIdx < lFinded) then
    lFinded := lIdx;
  for lTag in ATags do
    if FReplacedCategories.Find(lTag, lIdx) and (lIdx < lFinded) then
      lFinded := lIdx;

  if lFinded <> MaxInt then
    Result := FConverterCategories[NativeInt(FReplacedCategories.Objects[lFinded])];
end;

procedure TCustomFamilyConverter.SaveBills;
var
  lPair: TPair<string, Currency>;
  lBillList: TStringList;
begin
  lBillList := TStringList.Create;
  try
    for lPair in FBills do
    begin
      FLine.Clear;
      FLine.Add(lPair.Key);
      FLine.Add(CurrToStr(lPair.Value));
      lBillList.Add(FLine.DelimitedText);
    end;

    if lBillList.Count > 0 then
      lBillList.SaveToFile(FOptions.Directory +
        TPath.GetFileNameWithoutExtension(FOptions.Database) + '_bills.csv',
        TEncoding.GetEncoding(1251))
    else
      WriteLn('Не удалось найти данные о счетах.');

  finally
    FreeAndNil(lBillList);
  end;
end;

{ EConvException }

constructor EConvException.Create(const AMsg: string; const AExitCode: Integer);
begin
  ExitCode := AExitCode;
  inherited Create(AMsg);
end;

end.
