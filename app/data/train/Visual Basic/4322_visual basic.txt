Option Strict Off
Option Explicit On
Module PrM
	
	Structure SYSTEMTIME
		Dim wYear As Short
		Dim wMonth As Short
		Dim wDayOfWeek As Short
		Dim wDay As Short
		Dim wHour As Short
		Dim wMinute As Short
		Dim wSecond As Short
		Dim wMilliseconds As Short
	End Structure
	
	Public Declare Function Inp Lib "inpout32.dll"  Alias "Inp32"(ByVal PortAddress As Short) As Short
	Public Declare Sub Out Lib "inpout32.dll"  Alias "Out32"(ByVal PortAddress As Short, ByVal value As Short)
	'UPGRADE_WARNING: Structure SYSTEMTIME may require marshalling attributes to be passed as an argument in this Declare statement. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="C429C3A5-5D47-4CD9-8F51-74A1616405DC"'
	Declare Sub GetSystemTime Lib "kernel32.dll" (ByRef lpSystemTime As SYSTEMTIME)


    Public iiRun, IRate, iiFile As Integer
    Public DataPrefFilePath, DataFileName As String
    Public xRate As Integer
	Public IAtom As Short
	Public xIncr As Double
	Public XTrig As Integer
    Public LiquidWait, iRunning As Short
    Public numberOfCapturesSignal, numberOfCapturesNoise As Integer
    Public recordsPerCaptureSignal As Integer
    Public recordsPerCaptureNoise As Integer
    Public captureCountSignal As Integer
    Public captureCountNoise As Integer
    Public SignalData(7, 100000) As Single '0-3 are raw, 4-7 are smoothed
    Public NoiseData(7, 100000) As Single
    Public SignalDataSmoothed(7, 100000) As Single '0-3 are raw, 4-7 are smoothed
    Public NoiseDataSmoothed(7, 100000) As Single
    Public IDPoints, ISmooth, IOk As Short
	Public Ichan2, Ichan1, Ichan3 As Short
	Public RMSCut As Single
    Public OneTrueLiquid As Short
    Public KeyHit, KeyTimer As Short
	Public LogPath, DataFileNameHist As String
    Public AllTracesPath, AllTracesFileNameNoise, AllTracesFileNameSignal As String
    Public isubtrch1 As Short
    Public IPrM, NPrM As Short
    Public DoneTakingData As Boolean 'Status of whether we're done taking data for a specific purity monitor
    Public ScopeWaitTakingData As Boolean 'Status of whether or not ScopeWait function is taking signal data or not
    Public HVWaitTakingData As Boolean 'Status of whether or not HVWait function is taking signal data or not



    Sub Anal_Data()
		Dim fAnoTrue As Double
		Dim fCatTrue As Double
		Dim fCatPeak As Double
		fCatPeak = 1000
		Dim fCatBase As Double
		fCatBase = 0
		Dim fCatSq As Double
		fCatSq = 0
		Dim fCatRMS As Double
		fCatRMS = 0
		Dim fAnoPeak As Double
		fAnoPeak = -1000
		Dim fAnoBase As Double
		fAnoBase = 0
		Dim fAnoSq As Double
		fAnoSq = 0
		Dim fAnoRMS As Double
		fAnoRMS = 0
		Dim fCatTime As Double
		fCatTime = -1
		Dim fAnoRise As Double
		Dim fAnoTime As Double
		fAnoTime = -1
		Dim AnoTimeIndex As Short
		AnoTimeIndex = -1
		Dim CatTimeIndex As Short
		CatTimeIndex = -1
		Dim TriggerTimeIndex As Short
		TriggerTimeIndex = 500
		
		
		On Error GoTo 0
		On Error Resume Next
        FileOpen(7, DataPrefFilePath & "AnalyzedData.txt", OpenMode.Append)
        FileOpen(77, DataPrefFilePath & "CondensedData.txt", OpenMode.Append)
        PrMF.List1.Items.Clear()

        PrMF.List2.Items.Clear()
        PrintLine(7, Today & " " & TimeOfDay & "," & iiRun & "," & IPrM)
        Print(77, Today & " " & TimeOfDay & "," & iiRun & ",", TAB)
		PrMF.List1.Items.Add(Today & " " & TimeOfDay)
        PrMF.List1.Items.Add(" Run = " & iiRun & " PrM = " & IPrM)

        Dim preTriggerSamples As Integer
        Dim postTriggerSamples As Integer
        ' TODO: Select the number of pre-trigger samples per record
        preTriggerSamples = 500
        ' TODO: Select the number of post-trigger samples per record
        postTriggerSamples = 4508

        Dim secPerSample As Double
		If IPrM = 1 Or IPrM = 2 Then
			secPerSample = 1 / 1000000#
		Else
			secPerSample = 1 / 2000000#
		End If
		
		
		' Start with cathode
		Dim i As Short
		For i = 0 To 1000
            If SignalDataSmoothed(Ichan2, i) - NoiseDataSmoothed(Ichan2, i) < fCatPeak Then
                fCatPeak = SignalDataSmoothed(Ichan2, i) - NoiseDataSmoothed(Ichan2, i)
                CatTimeIndex = i
            End If
        Next i
		fCatTime = secPerSample * (-TriggerTimeIndex + CatTimeIndex)
		
		
		
		For i = TriggerTimeIndex / 2 - 25 To TriggerTimeIndex / 2 + 24
            fCatBase = fCatBase + SignalDataSmoothed(Ichan2, i) - NoiseDataSmoothed(Ichan2, i)
        Next i
		fCatBase = fCatBase / 50#
		
		
		For i = TriggerTimeIndex / 2 - 25 To TriggerTimeIndex / 2 + 24
            fCatSq = fCatSq + (fCatBase - SignalDataSmoothed(Ichan2, i) + NoiseDataSmoothed(Ichan2, i)) ^ 2
        Next i
		fCatRMS = System.Math.Sqrt(fCatSq / 50#)
		
		PrintLine(7, "Cathode Peak,Time,Baseline," & VB6.Format(fCatPeak, "0.000e-00") & "," & VB6.Format(fCatTime, "0.000e-00") & "," & VB6.Format(fCatBase, "0.000e-00"))
		PrMF.List1.Items.Add("Cathode Peak = " & VB6.Format(fCatPeak, "0.000e-00"))
		PrMF.List1.Items.Add("Cathode Time = " & VB6.Format(fCatTime, "0.000e-00"))
		PrMF.List1.Items.Add("Cathode Baseline = " & VB6.Format(fCatBase, "0.000e-00"))
		If System.Math.Abs(fCatPeak - fCatBase) < RMSCut * fCatRMS Then fCatPeak = fCatBase
		
		
		' Continue on with anode
		fAnoPeak = -1000
        For i = 1000 To preTriggerSamples + postTriggerSamples - 1
            If SignalDataSmoothed(Ichan1, i) - NoiseDataSmoothed(Ichan1, i) > fAnoPeak Then
                fAnoPeak = SignalDataSmoothed(Ichan1, i) - NoiseDataSmoothed(Ichan1, i)
                AnoTimeIndex = i
            End If
        Next i
        fAnoTime = secPerSample * (-TriggerTimeIndex + AnoTimeIndex)

        ' Find the anode base
        Dim AnoBaseIndex As Short
        AnoBaseIndex = AnoTimeIndex - CShort(0.000083 * (1 / secPerSample))
        fAnoBase = SignalDataSmoothed(Ichan1, AnoBaseIndex) - NoiseDataSmoothed(Ichan1, AnoBaseIndex)



        Dim fAnoNoiseBase As Double
        fAnoNoiseBase = 0
        Dim AnoBaselineIndexLow As Short
		Dim AnoBaselineIndexHigh As Short
		AnoBaselineIndexLow = CShort(CDbl(TriggerTimeIndex) + 0.666666 * (CDbl(AnoTimeIndex) - CDbl(CatTimeIndex)))
		AnoBaselineIndexHigh = CShort(CDbl(AnoTimeIndex) + 0.333333 * (CDbl(AnoTimeIndex) - CDbl(CatTimeIndex)))
        '    Debug.Print AnoTimeIndex
        '    Debug.Print CatTimeIndex
        '    Debug.Print AnoBaselineIndexLow
        '    Debug.Print AnoBaselineIndexHigh
        For i = AnoBaselineIndexLow - 200 To AnoBaselineIndexLow + 199
            'Debug.Print i
            'Debug.Print fAnoBase
            fAnoNoiseBase = fAnoNoiseBase + SignalDataSmoothed(Ichan1, i) - NoiseDataSmoothed(Ichan1, i)
        Next i
        fAnoNoiseBase = fAnoNoiseBase / 400

        For i = AnoBaselineIndexLow - 200 To AnoBaselineIndexLow + 199
            fAnoSq = fAnoSq + (fAnoNoiseBase - SignalDataSmoothed(Ichan1, i) + NoiseDataSmoothed(Ichan1, i)) ^ 2
        Next i
        fAnoRMS = System.Math.Sqrt(fAnoSq / 400)





        Dim istop As Short
		istop = 0
		Dim a1 As Double
		Dim a2 As Double
		Dim da1 As Double
		da1 = 10000#
		Dim da2 As Double
		da2 = 10000#
		Dim ta1 As Double
		Dim ta2 As Double
		Dim va1 As Double
		Dim va2 As Double
        Dim AnoRiseStartIndex As Short
        AnoRiseStartIndex = TriggerTimeIndex + CShort(0.75 * (AnoTimeIndex - CatTimeIndex))
        If istop = 0 Then
			a1 = fAnoBase + 0.25 * (fAnoPeak - fAnoBase)
			a2 = fAnoBase + 0.75 * (fAnoPeak - fAnoBase)
			For i = AnoRiseStartIndex To AnoTimeIndex
                If System.Math.Abs(SignalDataSmoothed(Ichan1, i) - NoiseDataSmoothed(Ichan1, i) - a1) < da1 Then
                    da1 = System.Math.Abs(SignalDataSmoothed(Ichan1, i) - NoiseDataSmoothed(Ichan1, i) - a1)
                    ta1 = -TriggerTimeIndex * secPerSample + i * secPerSample
                    va1 = SignalDataSmoothed(Ichan1, i) - NoiseDataSmoothed(Ichan1, i)
                End If
                If System.Math.Abs(SignalDataSmoothed(Ichan1, i) - NoiseDataSmoothed(Ichan1, i) - a2) < da2 Then
                    da2 = System.Math.Abs(SignalDataSmoothed(Ichan1, i) - NoiseDataSmoothed(Ichan1, i) - a2)
                    ta2 = -TriggerTimeIndex * secPerSample + i * secPerSample
                    va2 = SignalDataSmoothed(Ichan1, i) - NoiseDataSmoothed(Ichan1, i)
                End If
            Next i
			fAnoRise = (ta2 - ta1) * (fAnoPeak - fAnoBase) / (va2 - va1)
		End If
		
		PrintLine(7, "Anode Peak,Time,Baseline,Rise," & VB6.Format(fAnoPeak, "0.000e-00") & "," & VB6.Format(fAnoTime, "0.000e-00") & "," & VB6.Format(fAnoBase, "0.000e-00") & "," & VB6.Format(fAnoRise, "0.000e-00"))
		PrMF.List2.Items.Add("Anode Peak = " & VB6.Format(fAnoPeak, "0.000e-00"))
		PrMF.List2.Items.Add("Anode Time = " & VB6.Format(fAnoTime, "0.000e-00"))
		PrMF.List2.Items.Add("Anode Baseline = " & VB6.Format(fAnoBase, "0.000e-00"))
		PrMF.List2.Items.Add("Anode Rise = " & VB6.Format(fAnoRise, "0.000e-00"))
		
		Dim RC As Double
		RC = 0.000119
		
		Dim CathF As Double
		CathF = (fCatTime + 0.000006) / (RC * (1# - System.Math.Exp(-(fCatTime + 0.000006) / RC)))
		Dim AnoF As Double
		AnoF = (fAnoRise + 0.000005) / (RC * (1# - System.Math.Exp(-(fAnoRise + 0.000005) / RC)))
		fAnoTrue = (fAnoPeak - fAnoBase) * AnoF
		fCatTrue = System.Math.Abs((fCatPeak - fCatBase) * CathF)
		Dim fLifeTime As Double
		If fAnoTrue > 0 Then
			If (fCatTrue / fAnoTrue) > 0 Then
				fLifeTime = fAnoTime / (System.Math.Log(fCatTrue / fAnoTrue))
			Else
				fLifeTime = 0
			End If
		End If
		
		'11/29/11 Set lifetime to 100 ms if it is greater than 100 ms or is negative. T.Yang
		If fLifeTime > 0.1 Or fLifeTime < 0 Then
			fLifeTime = 0.1
		End If
		
		PrintLine(7, "Cath Factor,Anode Factor,Anode True, Cathode True,LifeTime," & VB6.Format(CathF, "0.000e-00") & "," & VB6.Format(AnoF, "0.000e-00") & "," & VB6.Format(fAnoTrue, "0.000e-00") & "," & VB6.Format(fCatTrue, "0.000e-00") & "," & VB6.Format(fLifeTime, "0.000e-00"))
		PrintLine(77, "Cath Factor,Anode Factor,Anode True, Cathode True,LifeTime" & VB6.Format(CathF, "0.000e-00") & "," & VB6.Format(AnoF, "0.000e-00") & "," & VB6.Format(fAnoTrue, "0.000e-00") & "," & VB6.Format(fCatTrue, "0.000e-00") & "," & VB6.Format(fLifeTime, "0.000e-00"))
		PrMF.List2.Items.Add("Cath Factor = " & VB6.Format(CathF, "0.000e-00"))
		PrMF.List2.Items.Add("Anode Factor = " & VB6.Format(AnoF, "0.000e-00"))
		PrMF.List2.Items.Add("Anode True = " & VB6.Format(fAnoTrue, "0.000e-00"))
		PrMF.List2.Items.Add("Cathode True = " & VB6.Format(fCatTrue, "0.000e-00"))
		PrMF.List2.Items.Add("LifeTime = " & VB6.Format(fLifeTime, "0.000e-00"))
		On Error GoTo 0
		FileClose(7)
		FileClose(77)
		
		
        FileOpen(7, LogPath & "Run" & VB6.Format(iiRun, "000000") & "." & IPrM & "." & iiFile & ".LogData.csv", OpenMode.Output)
		On Error GoTo 0
		PrintLine(7, "[Data]")
		PrintLine(7, "Tagname,TimeStamp,Value")
		Dim zztime As String
		zztime = VB6.Format(TimeOfDay, "hh:mm:ss")
		' Get SYSTEMTIME
		Dim sysTime As SYSTEMTIME
		GetSystemTime(sysTime)
		Dim sysDateTime As Date
		sysDateTime = CDate(sysTime.wMonth & "/" & sysTime.wDay & "/" & sysTime.wYear & " " & sysTime.wHour & ":" & sysTime.wMinute & ":" & sysTime.wSecond)
		
		PrintLine(7, "UBOONE.PRM_CATHPEAK_0" & IPrM & ".F_CV," & Today & " " & zztime & "," & VB6.Format(fCatPeak, "0.000e-00"))
		PrintLine(7, "UBOONE.PRM_CATHTIME_0" & IPrM & ".F_CV," & Today & " " & zztime & "," & VB6.Format(fCatTime, "0.000e-00"))
		PrintLine(7, "UBOONE.PRM_CATHBASE_0" & IPrM & ".F_CV," & Today & " " & zztime & "," & VB6.Format(fCatBase, "0.000e-00"))
		PrintLine(7, "UBOONE.PRM_ANODEPEAK_0" & IPrM & ".F_CV," & Today & " " & zztime & "," & VB6.Format(fAnoPeak, "0.000e-00"))
		PrintLine(7, "UBOONE.PRM_ANODETIME_0" & IPrM & ".F_CV," & Today & " " & zztime & "," & VB6.Format(fAnoTime, "0.000e-00"))
		PrintLine(7, "UBOONE.PRM_ANODEBASE_0" & IPrM & ".F_CV," & Today & " " & zztime & "," & VB6.Format(fAnoBase, "0.000e-00"))
		PrintLine(7, "UBOONE.PRM_ANODERISE_0" & IPrM & ".F_CV," & Today & " " & zztime & "," & VB6.Format(fAnoRise, "0.000e-00"))
		
		PrintLine(7, "UBOONE.PRM_CATHFACTOR_0" & IPrM & ".F_CV," & Today & " " & zztime & "," & VB6.Format(CathF, "0.000e-00"))
		PrintLine(7, "UBOONE.PRM_ANODEFACTOR_0" & IPrM & ".F_CV," & Today & " " & zztime & "," & VB6.Format(AnoF, "0.000e-00"))
		PrintLine(7, "UBOONE.PRM_ANODETRUE_0" & IPrM & ".F_CV," & Today & " " & zztime & "," & VB6.Format(fAnoTrue, "0.000e-00"))
		PrintLine(7, "UBOONE.PRM_CATHTRUE_0" & IPrM & ".F_CV," & Today & " " & zztime & "," & VB6.Format(fCatTrue, "0.000e-00"))
		PrintLine(7, "UBOONE.PRM_LIFETIME_0" & IPrM & ".F_CV," & Today & " " & zztime & "," & VB6.Format(fLifeTime, "0.000e-00"))
		If fLifeTime > 0 Then
			PrintLine(7, "UBOONE.PRM_IMPURITIES_0" & IPrM & ".F_CV," & Today & " " & zztime & "," & VB6.Format(0.00015 / fLifeTime, "0.000e-00"))
		Else
			PrintLine(7, "UBOONE.PRM_IMPURITIES_0" & IPrM & ".F_CV," & Today & " " & zztime & "," & VB6.Format(999999, "0.000e-00"))
		End If
		FileClose(7)




        If PrMF.CheckBoxiFix.CheckState = 1 Then

            FileCopy(LogPath & "Run" & VB6.Format(iiRun, "000000") & "." & IPrM & "." & iiFile & ".LogData.csv", "Y:\" & "Run" & VB6.Format(iiRun, "000000") & "." & IPrM & "." & iiFile & ".LogData.csv")
        End If





        If PrMF.CheckBoxDropbox.CheckState = 1 Then
            FileOpen(7, "C:\Users\bcarls\Dropbox\PrM_Logs\lifetimes_0" & IPrM & "_" & VB6.Format(iiRun, "000000") & ".txt", OpenMode.Output)
            PrintLine(7, VB6.Format(iiRun, "000000") & " " & Today.Month & " " & Today.Day & " " & Today.Year & " " & VB6.Format(TimeOfDay, " HH mm ss ") & VB6.Format(fLifeTime, "0.000e-00") & " " & VB6.Format(fCatTrue, "0.000e-00") & " " & VB6.Format(fAnoTrue, "0.000e-00") & " " & VB6.Format(fCatBase, "0.000e-00") & " " & VB6.Format(fAnoBase, "0.000e-00") & " " & VB6.Format(fCatRMS, "0.000e-00") & " " & VB6.Format(fAnoRMS, "0.000e-00") & " " & VB6.Format(CathF, "0.000e-00") & " " & VB6.Format(AnoF, "0.000e-00"))
            FileClose(7)
        End If







        ' Send to slow controls via Winsock
        ' This is a little strange, we need to use a variable, PrMID in this case, to hold the value of IPrM while the While sckConnected
        ' loop is happening, otherwise the program will procede forward and set IPrM back to zero
        Dim PrMID As Short
		PrMID = IPrM
		PrMF.Winsock1.Close()
		PrMF.Winsock1.RemoteHost = PrMF.Text7.Text
		PrMF.Winsock1.RemotePort = CInt(PrMF.Text8.Text)
		PrMF.Winsock1.Connect()
		'UPGRADE_NOTE: State was upgraded to CtlState. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="A9E4979A-37FA-4718-9994-97DD76ED70A7"'
		Do While (PrMF.Winsock1.CtlState <> MSWinsockLib.StateConstants.sckConnected)
			System.Windows.Forms.Application.DoEvents()
		Loop 
		
		
		' Send data for IPrM = 0
		If PrMID = 0 Then
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 1 & "_" & 0 & "/LIFETIME," & fLifeTime & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 1 & "_" & 0 & "/TIMESTAMP," & Today & " " & zztime & vbCrLf)
			If fLifeTime > 0 Then
				PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 1 & "_" & 0 & "/IMPURITIES," & 0.00015 / fLifeTime & vbCrLf)
			Else
				PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 1 & "_" & 0 & "/IMPURITIES," & 999999 & vbCrLf)
			End If
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 1 & "_" & 0 & "/ANODEBASE," & fAnoBase & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 1 & "_" & 0 & "/ANODEFACTOR," & AnoF & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 1 & "_" & 0 & "/ANODEPEAK," & fAnoPeak & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 1 & "_" & 0 & "/ANODERISE," & fAnoRise & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 1 & "_" & 0 & "/ANODETIME," & fAnoTime & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 1 & "_" & 0 & "/ANODETRUE," & fAnoTrue & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 1 & "_" & 0 & "/CATHBASE," & fCatBase & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 1 & "_" & 0 & "/CATHFACTOR," & CathF & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 1 & "_" & 0 & "/CATHPEAK," & fCatPeak & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 1 & "_" & 0 & "/CATHTIME," & fCatTime & vbCrLf)
            PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 1 & "_" & 0 & "/CATHTRUE," & fCatTrue & vbCrLf)
            'UPGRADE_WARNING: DateDiff behavior may be different. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6B38EC3F-686D-4B2E-B5A5-9E8E7A762E32"'
            PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 1 & "_" & 0 & "/UNIXTIME," & DateDiff(Microsoft.VisualBasic.DateInterval.Second, DateSerial(1970, 1, 1), sysDateTime) & vbCrLf)
		End If
		
		' Send data for IPrM = 1
		If PrMID = 1 Then
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 2 & "_" & 1 & "/LIFETIME," & fLifeTime & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 2 & "_" & 1 & "/TIMESTAMP," & Today & " " & zztime & vbCrLf)
			If fLifeTime > 0 Then
				PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 2 & "_" & 1 & "/IMPURITIES," & 0.00015 / fLifeTime & vbCrLf)
			Else
				PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 2 & "_" & 1 & "/IMPURITIES," & 999999 & vbCrLf)
			End If
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 2 & "_" & 1 & "/ANODEBASE," & fAnoBase & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 2 & "_" & 1 & "/ANODEFACTOR," & AnoF & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 2 & "_" & 1 & "/ANODEPEAK," & fAnoPeak & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 2 & "_" & 1 & "/ANODERISE," & fAnoRise & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 2 & "_" & 1 & "/ANODETIME," & fAnoTime & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 2 & "_" & 1 & "/ANODETRUE," & fAnoTrue & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 2 & "_" & 1 & "/CATHBASE," & fCatBase & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 2 & "_" & 1 & "/CATHFACTOR," & CathF & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 2 & "_" & 1 & "/CATHPEAK," & fCatPeak & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 2 & "_" & 1 & "/CATHTIME," & fCatTime & vbCrLf)
            PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 2 & "_" & 1 & "/CATHTRUE," & fCatTrue & vbCrLf)
            'UPGRADE_WARNING: DateDiff behavior may be different. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6B38EC3F-686D-4B2E-B5A5-9E8E7A762E32"'
            PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 2 & "_" & 1 & "/UNIXTIME," & DateDiff(Microsoft.VisualBasic.DateInterval.Second, DateSerial(1970, 1, 1), sysDateTime) & vbCrLf)
		End If
		
		
		
		' Send data for IPrM = 2
		If PrMID = 2 Then
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 3 & "_" & 2 & "/LIFETIME," & fLifeTime & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 3 & "_" & 2 & "/TIMESTAMP," & Today & " " & zztime & vbCrLf)
			If fLifeTime > 0 Then
				PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 3 & "_" & 2 & "/IMPURITIES," & 0.00015 / fLifeTime & vbCrLf)
			Else
				PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 3 & "_" & 2 & "/IMPURITIES," & 999999 & vbCrLf)
			End If
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 3 & "_" & 2 & "/ANODEBASE," & fAnoBase & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 3 & "_" & 2 & "/ANODEFACTOR," & AnoF & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 3 & "_" & 2 & "/ANODEPEAK," & fAnoPeak & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 3 & "_" & 2 & "/ANODERISE," & fAnoRise & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 3 & "_" & 2 & "/ANODETIME," & fAnoTime & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 3 & "_" & 2 & "/ANODETRUE," & fAnoTrue & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 3 & "_" & 2 & "/CATHBASE," & fCatBase & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 3 & "_" & 2 & "/CATHFACTOR," & CathF & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 3 & "_" & 2 & "/CATHPEAK," & fCatPeak & vbCrLf)
			PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 3 & "_" & 2 & "/CATHTIME," & fCatTime & vbCrLf)
            PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 3 & "_" & 2 & "/CATHTRUE," & fCatTrue & vbCrLf)
            'UPGRADE_WARNING: DateDiff behavior may be different. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6B38EC3F-686D-4B2E-B5A5-9E8E7A762E32"'
            PrMF.Winsock1.SendData("uB_ArPurity_PM0" & 3 & "_" & 2 & "/UNIXTIME," & DateDiff(Microsoft.VisualBasic.DateInterval.Second, DateSerial(1970, 1, 1), sysDateTime) & vbCrLf)
		End If
		
		
		
		
		
		
		
		
		
		iiFile = iiFile + 1
		
	End Sub
End Module