unit uConv;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

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

    procedure InitRemovedTags;

    // Добавление денег на счет
    procedure AddBill(const ABill: string; const AMoney: Currency);

    procedure DoInitConverter; virtual;
    procedure DoConvert; virtual; abstract;
    procedure DoReadCategories;

    procedure SaveBills;

    function CopyLine(const ALine: TArray<string>): TArray<string>;

    // Преобразует категории в тэги, устраняет дублирование
    procedure ConvertCategoriesToTags(var ACategory, ATags, AComment: string);
    // Заменяет или удаляет лишние тэги
    procedure RemoveTags(const ATags: TStringList);
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
  uZenmoney;

{ TFamilyConverterFactory }

class function TFamilyConverterFactory.GetConverterInstance(const AID: string)
  : IFamilyConverter;
begin
  if (AID = '') or (AID = 'zm') then
    Result := TZenMoneyConverter.Create
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
  lTag: string;
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
  if (ACategory <> '') and (FTags.IndexOf(ACategory) = -1) then
    FTags.Insert(0, ACategory);

  // Убирает дублирование комментария
  if (AComment <> '') and (FTags.IndexOf(AComment) <> -1) then
    AComment := '';

  RemoveTags(FTags);

  ATags := '';
  for lTag in FTags do
  begin
    if ATags <> '' then
      ATags := ATags + ', ';
    ATags := ATags + lTag;
  end;
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

procedure TCustomFamilyConverter.InitRemovedTags;
begin
  FRemovedTags.Add('Развлечение', 'Развлечения');
  FRemovedTags.Add('Банковское обслуживание', 'Банк');
  FRemovedTags.Add('(Несколько записей)', '');
  FRemovedTags.Add('Перевод денег', 'Перевод');
  FRemovedTags.Add('Стоматолог', 'Стоматология');
  FRemovedTags.Add('Семейные расходы', 'Семья');
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
