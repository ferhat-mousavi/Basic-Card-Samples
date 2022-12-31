#Include SAM.def

Declare ATR = "SAMCard"
Declare ApplicationID = "SAMCard"

EEPROM KeysInserted As Byte = 0
EEPROM InternalExists As Byte = 0
EEPROM LMK As String*16
EEPROM CommKey As String*16
EEPROM SingleCommKey As String*8
EEPROM InternalKey As String*16
Public CommKeyExists As Byte = 0

Function GetClearKey(KeyUnderLMK As String*16) As String
   Private ClearKey As String*16 
   ClearKey = DES(-3, LMK, Mid$(KeyUnderLMK,1,8))
   Mid$(ClearKey,9,8) = DES(-3, LMK, Mid$(KeyUnderLMK,9,8))
   GetClearKey = ClearKey
End Function

Function DecryptComm (EncCommData$) As String
   Dim DecCommData$, EncPart$ As String
   Dim i As Byte
   DecCommData$ = Space$(Len(EncCommData$))
   For i = 1 To Len(EncCommData$) Step 8
      EncPart$ = Mid$(EncCommData$, i, 8)
      If Len(EncPart$) < 8 Then
         EncPart$ = EncPart$ + Space$(8 - Len(EncPart$))
      End If
      Mid$(DecCommData$,i,8) = DES(-3, CommKey, EncPart$)
   Next i
   DecryptComm = DecCommData$
End Function

Function EncryptComm (DecCommData$) As String
   Dim EncCommData$, DecPart$ As String
   Dim i As Byte
   EncCommData$ = Space$(Len(DecCommData$))
   For i = 1 To Len(DecCommData$) Step 8
      DecPart$ = Mid$(DecCommData$, i, 8)
      If Len(DecPart$) < 8 Then
         DecPart$ = DecPart$ + Space$(8 - Len(DecPart$))
      End If
      Mid$(EncCommData$,i,8) = DES(3, CommKey, DecPart$)
   Next i
   EncryptComm = EncCommData$
End Function

Command &H20 &H01 SetLMK (LMKStr As String*16)
   If CommKeyExists = 0 Then
      SW1SW2 = swCommKeyMissing
      Exit Command
   End If
   If KeysInserted = 1 Then
      SW1SW2 = swLoadedAlready
      Exit Command
   End If
   If Len(LMKStr) <> 16 Then
      SW1SW2 = swInvalidKeyLen
      Exit Command
   End If
   LMK = DecryptComm(LMKStr)
   KeysInserted = 1
End Command

Command &H20 &H11 EncryptData3(EncDec As String*8, KeyUnderLMK As String*16)
   Private ClearKey As String*16
   If CommKeyExists = 0 Then
      SW1SW2 = swCommKeyMissing
      Exit Command
   End If
   EncDec = DecryptComm(EncDec)
   If KeysInserted = 0 Then
      SW1SW2 = swKeysMissing
      Exit Command
   End If
   If Len(KeyUnderLMK) <> 16 Then
      SW1SW2 = swInvalidKeyLen
      Exit Command
   End If
   If Len(EncDec) <> 8 Then
      SW1SW2 = swEncDecDataLen
      Exit Command
   End If
   ClearKey = GetClearKey(KeyUnderLMK)
   If Len(ClearKey) <> 16 Then
      SW1SW2 = swInternalKeyErr
      Exit Command
   End If
   EncDec = DES(3, ClearKey, EncDec)
   EncDec = EncryptComm(EncDec)
End Command

Command &H20 &H12 EncryptData1(EncDec As String*8, KeyUnderLMK As String*16)
   Private ClearKey As String*16
   EncDec = DES(-1, SingleCommKey, EncDec)
   If KeysInserted = 0 Then
      SW1SW2 = swKeysMissing
      Exit Command
   End If
   If Len(KeyUnderLMK) <> 16 Then
      SW1SW2 = swInvalidKeyLen
      Exit Command
   End If
   If Len(EncDec) <> 8 Then
      SW1SW2 = swEncDecDataLen
      Exit Command
   End If
   If CommKeyExists = 0 Then
      SW1SW2 = swCommKeyMissing
      Exit Command
   End If
   ClearKey = GetClearKey(KeyUnderLMK)
   If Len(ClearKey) <> 16 Then
      SW1SW2 = swInternalKeyErr
      Exit Command
   End If
   EncDec = DES(+3, ClearKey, EncDec)
   EncDec = DES(+1, SingleCommKey, EncDec)
End Command

Command &H20 &H13 EncryptData0(EncDec As String*8, KeyUnderLMK As String*16)
   Private ClearKey As String*16
   If KeysInserted = 0 Then
      SW1SW2 = swKeysMissing
      Exit Command
   End If
   If Len(KeyUnderLMK) <> 16 Then
      SW1SW2 = swInvalidKeyLen
      Exit Command
   End If
   If Len(EncDec) <> 8 Then
      SW1SW2 = swEncDecDataLen
      Exit Command
   End If
   ClearKey = GetClearKey(KeyUnderLMK)
   If Len(ClearKey) <> 16 Then
      SW1SW2 = swInternalKeyErr
      Exit Command
   End If
   EncDec = DES(+3, ClearKey, EncDec)
End Command

Command &H20 &H21 SetCommKey(EncCommKey As String*19)
   Private Zero As String*8 = 0,0,0,0,0,0,0,0
   CommKey = DES(-3 , InitialCommKey, Mid$(EncCommKey,1,8))
   Mid$(CommKey,9,8) = DES(-3 , InitialCommKey, Mid$(EncCommKey,9,8))
   If Mid$(DES(+3, CommKey, Zero),1,3) <> Mid$(EncCommKey,17,3) Then
      SW1SW2 = swInvCheckValue
      Exit Command
   End If
   SingleCommKey = DES(+3, CommKey, Zero)
   CommKeyExists = 1
End Command

Command &H20 &H06 CheckLMK (CheckValue As String*8)
   Private Zero As String*8 = 0,0,0,0,0,0,0,0
   If CommKeyExists = 0 Then
      SW1SW2 = swCommKeyMissing
      Exit Command
   End If
   If KeysInserted = 0 Then
      SW1SW2 = swKeysMissing
      Exit Command
   End If
   CheckValue = DES(3, LMK, Zero)
   Mid$(CheckValue, 5, 4) = Mid$(Zero, 1, 4)
   CheckValue = EncryptComm(CheckValue)
End Command

Command &H20 &H51 SetInternalKey(IntKey As String*16)
   If CommKeyExists = 0 Then
      SW1SW2 = swCommKeyMissing
      Exit Command
   End If
   If InternalExists = 1 Then
      SW1SW2 = swLoadedAlready
      Exit Command
   End If
   If Len(IntKey) <> 16 Then
      SW1SW2 = swInvalidKeyLen
      Exit Command
   End If
   InternalKey = DecryptComm(IntKey)
   InternalExists = 1
End Command

Command &H20 &H52 CheckInternalKey(CheckValue As String*8)
   Private Zero As String*8 = 0,0,0,0,0,0,0,0
   If CommKeyExists = 0 Then
      SW1SW2 = swCommKeyMissing
      Exit Command
   End If
   If InternalExists = 0 Then
      SW1SW2 = swKeysMissing
      Exit Command
   End If
   CheckValue = DES(3, InternalKey, Zero)
   Mid$(CheckValue, 5, 4) = Mid$(Zero, 1, 4)
   CheckValue = EncryptComm(CheckValue)
End Command

Command &H20 &H54 VerifyInternalData(DecData As String*8, EncData As String*8)
   Private CompData As String*8 
   If CommKeyExists = 0 Then
      SW1SW2 = swCommKeyMissing
      Exit Command
   End If
   If InternalExists = 0 Then
      SW1SW2 = swKeysMissing
      Exit Command
   End If
   'CompData = 
   If DES(3, InternalKey, DecData) <> EncData Then
      SW1SW2 = swInternalKeyErr
      Exit Command
   End If
End Command
