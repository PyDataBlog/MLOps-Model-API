Imports System
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Namespace Common
    <TestClass()> _
    Public Class TestLogonDetails
        Private Shared _1stGUID As String = ""
        Private Shared _2ndGUID As String = ""
        Private Shared _bLogonDetailsSet As Boolean

        <TestMethod()> _
        Public Sub LogonDetailsIsInitiallyNothing()
            If _bLogonDetailsSet Then Exit Sub

            Assert.IsNull(Codewell.SysproIntegration.LogonDetails.GetInstance, "The LogonDetails class has been instantiated.")

        End Sub

        <TestMethod()> _
        Public Sub SetLogonDetailsWithWebServices()
            Dim strSysproOperator As String = "DEMO"
            Dim strSysproOperatorPassword As String = ""
            Dim strCompany As String = "0"
            Dim strCompanyPassword As String = ""
            Dim language As Codewell.SysproIntegration.SysproWS_Utilities.Language = Codewell.SysproIntegration.SysproWS_Utilities.Language.ENGLISH
            Dim logDetail As Codewell.SysproIntegration.SysproWS_Utilities.LogDetail = Codewell.SysproIntegration.SysproWS_Utilities.LogDetail.ldNoDebug
            Dim instance As Codewell.SysproIntegration.SysproWS_Utilities.Instance = Codewell.SysproIntegration.SysproWS_Utilities.Instance.EncoreInstance_0
            Dim strXmlIn As String = ""


            'Codewell.SysproIntegration.LogonDetails.SetLogonDetails(strSysproOperator, strSysproOperatorPassword, strCompany, strCompanyPassword)
            Codewell.SysproIntegration.LogonDetails.SetLogonDetails(strSysproOperator, strSysproOperatorPassword, strCompany, strCompanyPassword, _
                language, logDetail, instance, strXmlIn)

            Assert.AreEqual(strSysproOperator, Codewell.SysproIntegration.LogonDetails.GetInstance.SysproOperator)
            Assert.AreEqual(strSysproOperatorPassword, Codewell.SysproIntegration.LogonDetails.GetInstance.SysproOperatorPassword)
            Assert.AreEqual(strCompany, Codewell.SysproIntegration.LogonDetails.GetInstance.Company)
            Assert.AreEqual(strCompanyPassword, Codewell.SysproIntegration.LogonDetails.GetInstance.CompanyPassword)
            Assert.AreEqual(language, Codewell.SysproIntegration.LogonDetails.GetInstance.LanguageCode)
            Assert.AreEqual(logDetail, Codewell.SysproIntegration.LogonDetails.GetInstance.LogDetail)
            Assert.AreEqual(instance, Codewell.SysproIntegration.LogonDetails.GetInstance.SYSPROInstance)
            Assert.AreEqual(strXmlIn, Codewell.SysproIntegration.LogonDetails.GetInstance.XMLIn)

            _bLogonDetailsSet = True
        End Sub

        <TestMethod()> _
        Public Sub LogonWithWebServices()

            Codewell.SysproIntegration.Utilities.Logon(False)

            'If we get here the logon succeeded
            Assert.IsNotNull(Codewell.SysproIntegration.Utilities.SessionID, "SessionID not assigned")

            _1stGUID = Codewell.SysproIntegration.Utilities.SessionID
        End Sub

        <TestMethod()> _
      Public Sub ForceLogonWithWebServices()
            Assert.IsNotNull(Codewell.SysproIntegration.Utilities.SessionID, "There must be an existing SessionID before testing ForceLogon")

            _1stGUID = Codewell.SysproIntegration.Utilities.SessionID

            Codewell.SysproIntegration.Utilities.Logon(True)

            'If we get here the logon succeeded
            Assert.AreNotEqual(Of String)("", Codewell.SysproIntegration.Utilities.SessionID, "SessionID not assigned after forced logon")

            _2ndGUID = Codewell.SysproIntegration.Utilities.SessionID

            Assert.AreNotEqual(Of String)(_1stGUID, _2ndGUID, "The SessionID after the forced logon did not change!")
        End Sub

        <TestMethod()> _
        Public Sub SetLogonDetailsWithWithDCOM()
            Dim strSysproOperator As String = "DEMO"
            Dim strSysproOperatorPassword As String = ""
            Dim strCompany As String = "0"
            Dim strCompanyPassword As String = ""
            Dim language As Codewell.SysproIntegration.SysproWS_Utilities.Language = Codewell.SysproIntegration.SysproWS_Utilities.Language.ENGLISH
            Dim logDetail As Codewell.SysproIntegration.SysproWS_Utilities.LogDetail = Codewell.SysproIntegration.SysproWS_Utilities.LogDetail.ldNoDebug
            Dim instance As Codewell.SysproIntegration.SysproWS_Utilities.Instance = Codewell.SysproIntegration.SysproWS_Utilities.Instance.EncoreInstance_0
            Dim communicationMethod As Codewell.SysproIntegration.LogonDetails.CommunicationMethodEnum = Codewell.SysproIntegration.LogonDetails.CommunicationMethodEnum.COMDCOM
            Dim strXmlIn As String = ""


            'Codewell.SysproIntegration.LogonDetails.SetLogonDetails(strSysproOperator, strSysproOperatorPassword, strCompany, strCompanyPassword)
            Codewell.SysproIntegration.LogonDetails.SetLogonDetails(strSysproOperator, strSysproOperatorPassword, strCompany, strCompanyPassword, _
                language, logDetail, instance, strXmlIn, communicationMethod)

            Assert.AreEqual(strSysproOperator, Codewell.SysproIntegration.LogonDetails.GetInstance.SysproOperator)
            Assert.AreEqual(strSysproOperatorPassword, Codewell.SysproIntegration.LogonDetails.GetInstance.SysproOperatorPassword)
            Assert.AreEqual(strCompany, Codewell.SysproIntegration.LogonDetails.GetInstance.Company)
            Assert.AreEqual(strCompanyPassword, Codewell.SysproIntegration.LogonDetails.GetInstance.CompanyPassword)
            Assert.AreEqual(language, Codewell.SysproIntegration.LogonDetails.GetInstance.LanguageCode)
            Assert.AreEqual(logDetail, Codewell.SysproIntegration.LogonDetails.GetInstance.LogDetail)
            Assert.AreEqual(instance, Codewell.SysproIntegration.LogonDetails.GetInstance.SYSPROInstance)
            Assert.AreEqual(strXmlIn, Codewell.SysproIntegration.LogonDetails.GetInstance.XMLIn)
            Assert.AreEqual(communicationMethod, Codewell.SysproIntegration.LogonDetails.GetInstance.CommunicationMethod)

            _bLogonDetailsSet = True
        End Sub

        <TestMethod()> _
        Public Sub LogonWithDCOM()


            Codewell.SysproIntegration.Utilities.Logon(False)

            'If we get here the logon succeeded
            Assert.AreNotEqual(Of String)("", Codewell.SysproIntegration.Utilities.SessionID, "SessionID not assigned")

            _2ndGUID = Codewell.SysproIntegration.Utilities.SessionID
            Assert.AreNotEqual(Of String)(_1stGUID, _2ndGUID, "The SessionID after the second logon did not change!")
        End Sub

        <TestMethod()> _
       Public Sub ForceLogonWithDCOM()
            Assert.IsNotNull(Codewell.SysproIntegration.Utilities.SessionID, "There must be an existing SessionID before testing ForceLogon")

            _1stGUID = Codewell.SysproIntegration.Utilities.SessionID

            Codewell.SysproIntegration.Utilities.Logon(True)

            'If we get here the logon succeeded
            Assert.AreNotEqual(Of String)("", Codewell.SysproIntegration.Utilities.SessionID, "SessionID not assigned after forced logon")

            _2ndGUID = Codewell.SysproIntegration.Utilities.SessionID

            Assert.AreNotEqual(Of String)(_1stGUID, _2ndGUID, "The SessionID after the forced logon did not change!")
        End Sub

        <TestMethod()> _
        Public Sub Logoff()
            Codewell.SysproIntegration.Utilities.Logoff()

            Assert.AreEqual(Of String)("", Codewell.SysproIntegration.Utilities.SessionID, "SessionID not cleared")
        End Sub

        <TestMethod()> _
        Public Sub LogonWithWCF()
            Common.CommonTestMethods.SetWCFLogonParameters()
            Codewell.SysproIntegration.Utilities.Logon(False)

            'If we get here the logon succeeded
            Assert.IsNotNull(Codewell.SysproIntegration.Utilities.SessionID, "SessionID not assigned")

            _1stGUID = Codewell.SysproIntegration.Utilities.SessionID
        End Sub

        <TestMethod()> _
        Public Sub LogonWithProxy()
            Dim obj As New Codewell.SysproIntegration.SysproWS_Query.queryclass()
            obj.Proxy = System.Net.WebRequest.GetSystemWebProxy()
            obj.Proxy.Credentials = System.Net.CredentialCache.DefaultCredentials

            Assert.Fail()
        End Sub


    End Class
End Namespace
