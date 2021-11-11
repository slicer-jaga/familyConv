unit uDB;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB,
  FireDAC.Phys.FBDef, FireDAC.ConsoleUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.Phys.IBBase;

type
  TdmDB = class(TDataModule)
    dbConnection: TFDConnection;
    sqlReestr: TFDQuery;
    sqlCategories: TFDQuery;
    sqlCustom: TFDQuery;
    FDPhysFBDriverLink: TFDPhysFBDriverLink;
  private
  public
  end;

var
  dmDB: TdmDB;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}
{$R *.dfm}

end.
