object frmPipeServer: TfrmPipeServer
  Left = 0
  Top = 0
  Caption = 'frmPipeServer'
  ClientHeight = 498
  ClientWidth = 749
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    749
    498)
  TextHeight = 15
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 743
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
    Width = 746
    Height = 234
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Server Messages'
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 1
    object Memo1: TMemo
      Left = 7
      Top = 22
      Width = 732
      Height = 205
      Align = alClient
      Lines.Strings = (
        'Memo1')
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 322
    Width = 746
    Height = 165
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Broadcast Message to Clients'
    Constraints.MinHeight = 148
    TabOrder = 2
    DesignSize = (
      746
      165)
    object btnBroadcastString: TButton
      Left = 632
      Top = 131
      Width = 107
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Broadcast String'
      TabOrder = 0
      OnClick = btnBroadcastStringClick
    end
    object Memo2: TMemo
      Left = 7
      Top = 24
      Width = 732
      Height = 100
      Anchors = [akLeft, akTop, akBottom]
      Constraints.MinHeight = 83
      Lines.Strings = (
        'Memo2')
      TabOrder = 1
    end
    object btnBroadcastJSON: TButton
      Left = 511
      Top = 131
      Width = 107
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Broadcast JSON'
      TabOrder = 2
      OnClick = btnBroadcastJSONClick
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
