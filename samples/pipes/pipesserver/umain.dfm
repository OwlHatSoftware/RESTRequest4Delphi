object frmPipeServer: TfrmPipeServer
  Left = 0
  Top = 0
  Caption = 'frmPipeServer'
  ClientHeight = 288
  ClientWidth = 394
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 388
    Height = 74
    Align = alTop
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 11
      Width = 64
      Height = 15
      Caption = 'ServerName'
    end
    object Label2: TLabel
      Left = 16
      Top = 38
      Width = 55
      Height = 15
      Caption = 'PipeName'
    end
    object Edit1: TEdit
      Left = 86
      Top = 11
      Width = 184
      Height = 23
      TabOrder = 0
      Text = 'Edit1'
    end
    object Edit2: TEdit
      Left = 86
      Top = 38
      Width = 184
      Height = 23
      TabOrder = 1
      Text = 'Edit2'
    end
    object btnStartServer: TButton
      Left = 304
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 2
      OnClick = btnStartServerClick
    end
    object btnStopServer: TButton
      Left = 304
      Top = 40
      Width = 75
      Height = 25
      Caption = 'Stop'
      TabOrder = 3
      OnClick = btnStopServerClick
    end
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 80
    Width = 394
    Height = 208
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Caption = 'Messages'
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 1
    object Memo1: TMemo
      Left = 7
      Top = 22
      Width = 380
      Height = 179
      Align = alClient
      Lines.Strings = (
        'Memo1')
      TabOrder = 0
    end
  end
  object mtUsers: TFDMemTable
    FieldDefs = <>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 216
    Top = 144
    object mtUsersID: TAggregateField
      AutoGenerateValue = arAutoInc
      FieldKind = fkData
      FieldName = 'ID'
      DisplayName = ''
    end
    object mtUsersNAME: TStringField
      FieldName = 'NAME'
      Size = 255
    end
    object mtUsersLASTNAME: TStringField
      FieldName = 'LASTNAME'
      Size = 255
    end
    object mtUsersEMAIL: TStringField
      FieldName = 'EMAIL'
      Size = 255
    end
  end
end
