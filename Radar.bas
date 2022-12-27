#Include Radar.def

#Pragma ATR (HB = ""RadarLive")
Declare ApplicationID = "RadarLive Card"

Public AccessAuthorised As Byte = 0

EEPROM PAN As String*16 = ""
'EEPROM PAN As String*16 = "6035785199995533"
EEPROM IsPANSet As Byte = 0

EEPROM CurrentBalance As String*8 = "00000000"
EEPROM SequenceNumber As String*4 = "0000"
EEPROM ActiveFlag As String*1 = "0"
EEPROM GiftCondition As String*1 = "0"
EEPROM UpdateDateTime As String*10 = "0000000000" 'DDMMYYHHMM
EEPROM LoadAwardType As String*1 = "0" ' "1" Nakit "2" KrediKart "3" DebitKart "4" ™nDolum "5" HediyeDolum
EEPROM LoadCardNo As String*4 = "0000"
EEPROM LoadTermID As String*8 = "00000000"
EEPROM LoadDateTime As String*10 = "0000000000" 'DDMMYYHHMM


EEPROM LastTransTermID(1 To 10) As String*8
EEPROM LastTransMerchantName(1 To 10) As String*10
EEPROM LastTransDateTime(1 To 10) As String*10
EEPROM LastTransAmount(1 To 10) As String*8
EEPROM LastTransSeqNo(1 To 10) As String*4
EEPROM LastTransTXType(1 To 10) As String*1


Command &H10 &H10 Format()
   Dim I As Integer
   If AccessAuthorised <> 1 Then
      SW1SW2 = swNotAuthorised
      Exit Command
   End If
   If P1<>&HFE Then
      SW1SW2 = swNotAuthorised
      Exit Command
   End If
   If P2<>&HEF Then
      SW1SW2 = swNotAuthorised
      Exit Command
   End If
   PAN = ""
   IsPANSet = 0
   CurrentBalance = "00000000"
   SequenceNumber = "0000"
   ActiveFlag = "0"
   GiftCondition = "0"
   UpdateDateTime = "0000000000"
   LoadAwardType = "0"
   LoadCardNo = "0000"
   LoadTermID = "00000000"
   LoadDateTime = "0000000000"
   For I=1 To I=10
	LastTransTermID(I) = ""
	LastTransMerchantName(I) = ""
	LastTransDateTime(I) = ""
	LastTransAmount(I) = ""
	LastTransSeqNo(I) = ""
	LastTransTXType(I) = ""
   Next
End Command


Command &H20 &H10 Reset()
   Dim I As Integer
   If AccessAuthorised <> 1 Then
      SW1SW2 = swNotAuthorised
      Exit Command
   End If
   CurrentBalance = "00000000"
   SequenceNumber = "0000"
   ActiveFlag = "0"
   GiftCondition = "0"
   UpdateDateTime = "0000000000"
   LoadAwardType = "0"
   LoadCardNo = "0000"
   LoadTermID = "00000000"
   LoadDateTime = "0000000000" 
   For I=1 To I=10
	LastTransTermID(I) = ""
	LastTransMerchantName(I) = ""
	LastTransDateTime(I) = ""
	LastTransAmount(I) = ""
	LastTransSeqNo(I) = ""
	LastTransTXType(I) = ""
   Next
End Command


Command &H30 &H10 ExternalAuth(Challenge As String*8, AccessKey As String*8)
   Dim TmpData As String*8
   TmpData=DES(+3, InitialKey, Challenge )
   If DES(+3, A1KEY, TmpData) = AccessKey Then
   	AccessAuthorised=1
   ElseIf DES(+3, A2KEY, TmpData) = AccessKey Then
   	AccessAuthorised=2
   End If
End Command


Command &H30 &H20 SetPAN(CardNo As String*16)
   If IsPANSet = 1 Then
      SW1SW2 = swCardNoExists
      Exit Command
   End If
   PAN  = CardNo
   IsPANSet = 1
End Command


Command &H30 &H30 UpdateCardData(Balance As String*8, SeqNo As String*4, ActiveFl As String*1, GiftGiven As String*1, DateTime As String*10, LAwardType As String*1, LCardNo As String*4, LTermID As String*8, LDateTime As String*10)
   If AccessAuthorised <> 2 Then
      SW1SW2 = swNotAuthorised
      Exit Command
   End If
   CurrentBalance = Balance
   SequenceNumber = SeqNo
   ActiveFlag = ActiveFl
   GiftCondition = GiftGiven
   UpdateDateTime = DateTime
   LoadAwardType = LAwardType
   LoadCardNo = LCardNo
   LoadTermID = LTermID
   LoadDateTime = LDateTime
End Command


Command &H30 &H40 ReadCardData(CardNo As String*16, Balance As String*8, SeqNo As String*4, ActiveFl As String*1, GiftGiven As String*1, DateTime As String*10, LAwardType As String*1, LCardNo As String*4, LTermID As String*8, LDateTime As String*10)
   If IsPANSet = 0 Then
      SW1SW2 = swCardNoMissing
      Exit Command
   End If
   
   CardNo = PAN
   
   Balance = CurrentBalance
   SeqNo = SequenceNumber
   ActiveFl = ActiveFlag
   GiftGiven = GiftCondition
   DateTime = UpdateDateTime
   LAwardType = LoadAwardType
   LCardNo = LoadCardNo
   LTermID = LoadTermID
   LDateTime = LoadDateTime
End Command


Command &H30 &H50 InsertHistory( ItemNo As Byte, TransTermID As String*8, TransMerchantName As String*10, TransDateTime As String*10, TransAmount As String*8, TransSeqNo As String*4, TransTXType As String*1)
   'If AccessAuthorised <> 2 Then
      'SW1SW2 = swNotAuthorised
      'Exit Command
   'End If
   If IsPANSet = 0 Then
      SW1SW2 = swCardNoMissing
      Exit Command
   End If
   
   If ItemNo<1 And ItemNo>10 Then
      SW1SW2 = swParamError
      Exit Command
   End If
   
   LastTransTermID(ItemNo) = TransTermID
   LastTransMerchantName(ItemNo) = TransMerchantName
   LastTransDateTime(ItemNo) = TransDateTime
   LastTransAmount(ItemNo) = TransAmount
   LastTransSeqNo(ItemNo) = TransSeqNo
   LastTransTXType(ItemNo) = TransTXType
   
End Command


Command &H30 &H60 ReadHistory( ItemNo As Byte, TransTermID As String*8, TransMerchantName As String*10, TransDateTime As String*10, TransAmount As String*8, TransSeqNo As String*4, TransTXType As String*1)
   If IsPANSet = 0 Then
      SW1SW2 = swCardNoMissing
      Exit Command
   End If
   
   If ItemNo<1 And ItemNo>10 Then
      SW1SW2 = swParamError
      Exit Command
   End If
   
   TransTermID = LastTransTermID(ItemNo)
   TransMerchantName = LastTransMerchantName(ItemNo)
   TransDateTime = LastTransDateTime(ItemNo)
   TransAmount = LastTransAmount(ItemNo)
   TransSeqNo = LastTransSeqNo(ItemNo)
   TransTXType = LastTransTXType(ItemNo)
End Command


