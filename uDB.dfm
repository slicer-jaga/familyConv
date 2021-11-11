object dmDB: TdmDB
  OldCreateOrder = False
  Height = 248
  Width = 304
  object dbConnection: TFDConnection
    Params.Strings = (
      'Database='
      'User_Name='
      'Password='
      'DriverID=FB')
    ConnectedStoredUsage = [auRunTime]
    LoginPrompt = False
    Left = 26
    Top = 12
  end
  object sqlReestr: TFDQuery
    Connection = dbConnection
    SQL.Strings = (
      'select * from reestr')
    Left = 26
    Top = 68
  end
  object sqlCategories: TFDQuery
    Connection = dbConnection
    SQL.Strings = (
      
        'select t1.cat0_name, t2.cat0_name, t3.cat0_name from category le' +
        'ft join cat0 t1 on t1.cat0_id = category.cat_id0 left join cat0 ' +
        't2 on t2.cat0_id = category.cat_id1 left join cat0 t3 on t3.cat0' +
        '_id = category.cat_id2')
    Left = 80
    Top = 72
  end
  object sqlCustom: TFDQuery
    Connection = dbConnection
    Left = 136
    Top = 72
  end
  object FDPhysFBDriverLink: TFDPhysFBDriverLink
    Left = 104
    Top = 16
  end
end
