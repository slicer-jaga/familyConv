program familyConv;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  uDB in 'uDB.pas' {dmDB: TDataModule},
  uZenmoney in 'uZenmoney.pas',
  uConv in 'uConv.pas',
  uConsts in 'uConsts.pas';

var
  lConv: IFamilyConverter;
  lOptions: TFamilyConvertOptions;

begin
  try
    if not TFamilyConverterFactory.GetOptions(lOptions) then
      Exit;

    lConv := TFamilyConverterFactory.GetConverterInstance(lOptions.Converter);
    if not Assigned(lConv) then
      Exit;

    if not lConv.Convert(lOptions) then
      Exit;

    WriteLn('Успешно завершено');
    ExitCode := C_ExitCode_OK;

  except
    on E: EConvException do
    begin
      WriteLn(E.Message);
      ExitCode := E.ExitCode;
    end;
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      ExitCode := C_ExitCode_Error;
    end;
  end;

end.
