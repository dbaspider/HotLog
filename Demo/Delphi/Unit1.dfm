object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 488
  ClientWidth = 809
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 665
    Height = 472
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object btnTest: TButton
    Left = 704
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Test Log'
    TabOrder = 1
    OnClick = btnTestClick
  end
end
