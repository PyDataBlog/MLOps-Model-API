Imports CrystalDecisions.CrystalReports.Engine
Imports CrystalDecisions.ReportSource
Imports CrystalDecisions.Shared
Imports CrystalDecisions.Windows.Forms
Imports CrystalDecisions.Windows.Forms.CrystalReportViewer
Imports System.Data
Imports System.Data.SqlClient
Imports Basic.Constants
Imports Basic.DAL
Imports Basic.DAL.Utils

Public Class frmRPT_List

    Dim ObjFind As Grid_Help
    Dim strFind As String
    Dim tmp As String
    Public Shared mBrCode As String
    Public Shared mReport As Integer
    Public Shared TableName As String
    Public Fld1 As String
    Public Fld2 As String
    Public SortFld1 As String
    Public SortFld2 As String
    Public milevel As String
    Public RepName As String
    Public StrFilter As String
    Public mCode As String
    Public mECode As String
    Dim StrRepHdr1 As String
    Dim StrRepHdr2 As String
    Dim RepHeader1 As String
    Public Shared Company As Object = ""


    Private Sub btnClose_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnClose.Click
        Me.Close()
    End Sub

    Private Sub frmRPT_List_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        mReport = Constants.ProjConst.pPara
        mOrder = 0
        StrFilter = "1=1"
        txtStart.Focus()
        Select Case mReport

            Case 1

                Me.Text = "Branch List"
                RepName = "Branches.Rpt"
                TableName = "Branch"
                Fld1 = "BrCode"
                Fld2 = "BrName"
                SortFld1 = "BrCode"
                SortFld2 = "BrName"
                GpLevel.Visible = False
                GpBranch.Visible = False
                GpRange.Top = 54
                btnPreview.Top = 180
                btnPrint.Top = 180
                btnClose.Top = 180
                Me.Height = 300
            Case 2
                Me.Text = "Account Codes List"
                RepName = "GLCode.Rpt"
                TableName = "Codes"
                Fld1 = "Code"
                Fld2 = "Description"
                SortFld1 = "Code"
                SortFld2 = "Description"
                cboLevel.Items.Add("1 - Control")
                cboLevel.Items.Add("2 - Subsidary")
                cboLevel.Items.Add("3 - Last")
                GpBranch.Visible = False
                GpRange.Top = 102
                btnPreview.Top = 230
                btnPrint.Top = 230
                btnClose.Top = 230
                Me.Height = 350
            Case 3
                Me.Text = "Budget List"
                RepName = "Budget.Rpt"
                'cboLevel.Items.Add("1 - Control")
                'cboLevel.Items.Add("2 - Subsidary")
                'cboLevel.Items.Add("3 - Last")
                GpCriteria.Visible = False
                GpLevel.Visible = False
                txtBrCode.Focus()
            Case 4
                Me.Text = "Report Definition"
                RepName = "RepDefinition.Rpt"
                'TableName = "Pftloss"
                'Fld1 = "PageNo"
                'Fld2 = "HDR"
                'SortFld1 = "PageNo"
                'SortFld2 = "HDR"
                GpCriteria.Visible = False
                GpLevel.Visible = False
                GpBranch.Visible = False
                GpRange.Top = 3
                btnPreview.Top = 130
                btnPrint.Top = 130
                btnClose.Top = 130
                Me.Height = 250
            Case 28
                Me.Text = "Account Codes List"
                'RepName = "GLCode.Rpt"
                TableName = "Codes"
                Fld1 = "Code"
                Fld2 = "Description"
                SortFld1 = "Code"
                SortFld2 = "Description"
                cboLevel.Items.Add("1 - Control")
                cboLevel.Items.Add("2 - Subsidary")
                cboLevel.Items.Add("3 - Last")
                GpBranch.Visible = False
                GpRange.Top = 102
                btnPreview.Top = 230
                btnPrint.Top = 230
                btnClose.Top = 230
                Me.Height = 350
        End Select

    End Sub

    Private Sub cboLevel_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles cboLevel.KeyDown
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub

    Private Sub cboLevel_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboLevel.LostFocus
        Select Case mReport
            Case 2
                If Mid(cboLevel.Text, 1, 1) = "1" Then
                    StrFilter = " SubString(Code," & aP2P & "," & aP2L + aP3L & ")='000000'"
                    milevel = "1"
                ElseIf Mid(cboLevel.Text, 1, 1) = "2" Then
                    StrFilter = "Right(Code," & aP2L + aP3L & ")<>'000000' And SubString(Code," & aP3P & "," & aP3L & ")='0000'"
                    milevel = "2"
                ElseIf Mid(cboLevel.Text, 1, 1) = "3" Then
                    StrFilter = "1=1"
                    milevel = "3"
                End If
            Case 28
                If Mid(cboLevel.Text, 1, 1) = "1" Then
                    StrFilter = " SubString(Code," & aP2P & "," & aP2L + aP3L & ")='000000'"
                    milevel = "1"
                ElseIf Mid(cboLevel.Text, 1, 1) = "2" Then
                    StrFilter = "Right(Code," & aP2L + aP3L & ")<>'000000' And SubString(Code," & aP3P & "," & aP3L & ")='0000'"
                    milevel = "2"
                ElseIf Mid(cboLevel.Text, 1, 1) = "3" Then
                    StrFilter = "1=1"
                    milevel = "3"
                End If
            Case 3
                If Mid(cboLevel.Text, 1, 1) = "1" Then
                    StrFilter = " SubString(B.Code," & aP2P & "," & aP2L + aP3L & ")='000000'"
                    milevel = "1"
                ElseIf Mid(cboLevel.Text, 1, 1) = "2" Then
                    StrFilter = "Right(B.Code," & aP2L + aP3L & ")<>'000000' And SubString(B.Code," & aP3P & "," & aP3L & ")='0000'"
                    milevel = "2"
                ElseIf Mid(cboLevel.Text, 1, 1) = "3" Then
                    StrFilter = "1=1"
                    milevel = "3"
                End If
        End Select
    End Sub

    Private Sub btnPreview_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnPreview.Click
        If Constants.ProjConst.pPara = 28 Then
            'Me.Caption = "Account Codes List"
            If milevel = "1" Then
                RepName = "GLCodeLevel1.rpt"
            ElseIf milevel = "2" Then
                RepName = "GLCodeLevel2.rpt"
            ElseIf milevel = "3" Then
                RepName = "GLCodeLevel3.rpt"
            End If
        End If
        Call CallReport(0)

    End Sub

    Private Sub btnPrint_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnPrint.Click
        Call CallReport(1)
    End Sub

    Private Sub rdCode_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles rdCode.Click
        mOrder = 0
        Label3.Text = "Starting Code"
        Label4.Text = "  Ending Code"
        txtStart.Width = 100
        txtEnd.Width = 100
        lblStart.Width = 330
        lblEnd.Width = 330
        Fld1 = SortFld1
        Fld2 = SortFld2
        txtBrCode.Text = ""
        txtStart.Text = ""
        txtEnd.Text = ""
        lblBrName.Text = ""
        lblStart.Text = ""
        lblEnd.Text = ""
        If Constants.ProjConst.pPara = 1 Then
            txtStart.MaxLength = 3
            txtEnd.MaxLength = 3
            txtStart.Text = frmBranch.txtBrCode.Text
            lblStart.Text = frmBranch.txtBrName.Text
            txtEnd.Text = frmBranch.txtBrCode.Text
            lblEnd.Text = frmBranch.txtBrName.Text
            txtStart.Focus()
        ElseIf (Constants.ProjConst.pPara = 2 Or Constants.ProjConst.pPara = 3) Then
            txtStart.MaxLength = 8
            txtEnd.MaxLength = 8
        ElseIf (Constants.ProjConst.pPara = 28) Then
            txtStart.MaxLength = 8
            txtEnd.MaxLength = 8
        End If
    End Sub

    Private Sub rdAlphabet_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles rdAlphabet.Click
        mOrder = 1
        Label3.Text = "Starting Desc"
        Label4.Text = "  Ending Desc"
        txtStart.Width = 330
        txtEnd.Width = 330
        lblStart.Width = 100
        lblEnd.Width = 100
        Fld2 = SortFld1
        Fld1 = SortFld2
        txtBrCode.Text = ""
        txtStart.Text = ""
        txtEnd.Text = ""
        lblBrName.Text = ""
        lblStart.Text = ""
        lblEnd.Text = ""
        If Constants.ProjConst.pPara = 1 Then
            txtStart.MaxLength = 30
            txtEnd.MaxLength = 30
            txtStart.Text = frmBranch.txtBrName.Text
            lblStart.Text = frmBranch.txtBrCode.Text
            txtEnd.Text = frmBranch.txtBrName.Text
            lblEnd.Text = frmBranch.txtBrCode.Text
        ElseIf (Constants.ProjConst.pPara = 2 Or Constants.ProjConst.pPara = 3) Then
            txtStart.MaxLength = 60
            txtEnd.MaxLength = 60
        ElseIf (Constants.ProjConst.pPara = 28) Then
            txtStart.MaxLength = 60
            txtEnd.MaxLength = 60
        End If
    End Sub

    Private Sub txtStart_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtStart.KeyDown
        ObjFind = New Grid_Help
        If e.KeyCode = Keys.F1 Then
            If Constants.ProjConst.pPara = 1 Then
                ObjFind.PMessage = "Branch"
            ElseIf Constants.ProjConst.pPara = 2 Then
                ObjFind.PMessage = "Codes"
            ElseIf Constants.ProjConst.pPara = 3 Then
                ObjFind.PMessage = "Budget"
            ElseIf Constants.ProjConst.pPara = 4 Then
                ObjFind.PMessage = "Pftloss"
            End If
            If (Constants.ProjConst.pPara = 1 Or Constants.ProjConst.pPara = 2) Then
                strFind = "SELECT " & Fld1 & "," & Fld2 & " FROM " & TableName & _
                           " Where " & StrFilter & " Order By " & Fld1 & ""
            ElseIf Constants.ProjConst.pPara = 3 Then
                'strFind = StrFilter
                Fld1 = "Code"
                Fld2 = "Description"
                TableName = "Codes"
                strFind = "SELECT " & Fld1 & "," & Fld2 & " FROM " & TableName & _
                           " Where " & StrFilter & " Order By " & Fld1 & ""
            ElseIf Constants.ProjConst.pPara = 4 Then
                strFind = "SELECT Max(PageNo) PageNo,Max(HDR) Header FROM Pftloss " & _
                           " Group By PageNo"
            ElseIf (Constants.ProjConst.pPara = 28) Then
                strFind = "SELECT " & Fld1 & "," & Fld2 & " FROM " & TableName & _
                           " Where " & StrFilter & " Order By " & Fld1 & ""
            End If
            ObjFind.sqlqueryFun = strFind
            ObjFind.ShowDialog()

            If ObjFind.PbOk = True Then
                tmp = ObjFind.strPKfun & ""
                Me.txtStart.Text = tmp
            End If
        ElseIf e.KeyCode = Keys.Enter Then
            SendKeys.Send("{TAB}")
        End If
    End Sub

    Private Sub txtStart_Validating(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles txtStart.Validating
        If txtStart.Text <> "" Then
            If (Constants.ProjConst.pPara = 1 Or Constants.ProjConst.pPara = 2) Then
                mSQL = "SELECT " & Fld2 & " FROM " & TableName & _
                       " WHERE " & Fld1 & " = '" & txtStart.Text & "'"
            ElseIf (Constants.ProjConst.pPara = 28) Then
                mSQL = "SELECT " & Fld2 & " FROM " & TableName & _
                       " WHERE " & Fld1 & " = '" & txtStart.Text & "'"
            ElseIf Constants.ProjConst.pPara = 3 Then
                'If rdCode.Checked = True Then
                Fld1 = "Code"
                Fld2 = "Description"
                TableName = "Codes"
                mSQL = "SELECT " & Fld2 & " FROM " & TableName & _
                       " WHERE " & Fld1 & " = '" & txtStart.Text & "'"
                'ElseIf rdAlphabet.Checked = True Then
                '    Fld1 = "Description"
                '    Fld2 = "Code"
                '    TableName = "Codes"
                '    mSQL = "SELECT " & Fld2 & " FROM " & TableName & _
                '           " WHERE " & Fld1 & " = '" & txtStart.Text & "'"
                'End If
            ElseIf Constants.ProjConst.pPara = 4 Then
                mSQL = "Select HDR From Pftloss Where PageNo = '" & Trim(txtStart.Text) & "'"
            End If
            mSQL = GetFldValue(mSQL, Fld2)
            lblStart.Text = mSQL
            If mSQL = "" Then
                MsgBox("No Record Found", vbInformation, Constants.ProjConst.SysCompany)
                txtStart.Text = ""
                e.Cancel = True
            End If
        End If
    End Sub

    Private Sub txtEnd_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtEnd.KeyDown
        ObjFind = New Grid_Help
        If e.KeyCode = Keys.F1 Then
            If Constants.ProjConst.pPara = 1 Then
                ObjFind.PMessage = "Branch"
            ElseIf Constants.ProjConst.pPara = 2 Then
                ObjFind.PMessage = "Codes"
            ElseIf Constants.ProjConst.pPara = 3 Then
                ObjFind.PMessage = "Budget"
            ElseIf Constants.ProjConst.pPara = 4 Then
                ObjFind.PMessage = "Pftloss"
            End If
            If (Constants.ProjConst.pPara = 1 Or Constants.ProjConst.pPara = 2) Then
                strFind = "SELECT " & Fld1 & "," & Fld2 & " FROM " & TableName & _
                           " Where " & StrFilter & " Order By " & Fld1 & ""
            ElseIf (Constants.ProjConst.pPara = 28) Then
                strFind = "SELECT " & Fld1 & "," & Fld2 & " FROM " & TableName & _
                           " Where " & StrFilter & " Order By " & Fld1 & ""
            ElseIf Constants.ProjConst.pPara = 3 Then
                Fld1 = "Code"
                Fld2 = "Description"
                TableName = "Codes"
                strFind = "SELECT " & Fld1 & "," & Fld2 & " FROM " & TableName & _
                           " Where " & StrFilter & " Order By " & Fld1 & ""
            ElseIf Constants.ProjConst.pPara = 4 Then
                strFind = "SELECT Max(PageNo) PageNo,Max(HDR) Header FROM Pftloss " & _
                           " Group By PageNo"
            End If
            ObjFind.sqlqueryFun = strFind
            ObjFind.ShowDialog()

            If ObjFind.PbOk = True Then
                tmp = ObjFind.strPKfun & ""
                Me.txtEnd.Text = tmp
            End If
        ElseIf e.KeyCode = Keys.Enter Then
            SendKeys.Send("{TAB}")
        End If
    End Sub

    Private Sub txtEnd_Validating(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles txtEnd.Validating
        If txtEnd.Text <> "" Then
            If (Constants.ProjConst.pPara = 1 Or Constants.ProjConst.pPara = 2) Then
                mSQL = "SELECT " & Fld2 & " FROM " & TableName & _
                       " WHERE " & Fld1 & " = '" & txtEnd.Text & "'"
            ElseIf (Constants.ProjConst.pPara = 28) Then
                mSQL = "SELECT " & Fld2 & " FROM " & TableName & _
                       " WHERE " & Fld1 & " = '" & txtEnd.Text & "'"
            ElseIf Constants.ProjConst.pPara = 3 Then
                'If rdCode.Checked = True Then
                Fld1 = "Code"
                Fld2 = "Description"
                TableName = "Codes"
                mSQL = "SELECT " & Fld2 & " FROM " & TableName & _
                       " WHERE " & Fld1 & " = '" & txtEnd.Text & "'"
                TableName = "Budget"
                'ElseIf rdAlphabet.Checked = True Then
                '    Fld2 = "Code"
                '    Fld1 = "Description"
                '    TableName = "Codes"
                '    mSQL = "SELECT " & Fld2 & " FROM " & TableName & _
                '           " WHERE " & Fld1 & " = '" & txtEnd.Text & "'"
                '    TableName = "Budget"
                'End If
            ElseIf Constants.ProjConst.pPara = 4 Then
                mSQL = "Select HDR From Pftloss Where PageNo = '" & Trim(txtEnd.Text) & "'"
            End If
            mSQL = GetFldValue(mSQL, Fld2)
            lblEnd.Text = mSQL
            If mSQL = "" Then
                MsgBox("No Record Found", vbInformation, Constants.ProjConst.SysCompany)
                txtEnd.Text = ""
                e.Cancel = True
            End If
        End If
    End Sub

    Sub CallReport(ByVal mPara As Integer)
        'Dim CReports As New Crystal.CrystalReport
        Try

            If Trim(txtStart.Text) = "" Then
                MsgBox("Starting Value Can't be Empty", vbInformation, Constants.ProjConst.SysCompany)
                txtStart.Focus()
                Exit Sub
            ElseIf Trim(txtEnd.Text) = "" Then
                MsgBox("Ending Value Can't be Empty", vbInformation, Constants.ProjConst.SysCompany)
                txtEnd.Focus()
                Exit Sub
            End If

            ' ''***************
            ' ''Report Printing
            ' ''***************

            Dim path As String = Constants.ProjConst.RepTitle & RepName
            Dim rptParamter As New frmReport
            Dim rptDoc As New CrystalDecisions.CrystalReports.Engine.ReportDocument
            rptDoc.Load(path)

            'frmReport.rptView.SelectionFormula = "{@Company} = 'Chartec'"
            'frmReport.rptView.SelectionFormula = "{Report Title} = 'Chartec title'"
            'frmReport.rptView.SelectionFormula = "{@Report Header1} = 'Chartec Header 1 '"
            'frmReport.rptView.SelectionFormula = "{@Report Header2} = 'Chartec Header 2'"

            '1 for Branch 2 for Account Code List 3 for Budget Allocation

            If (Constants.ProjConst.pPara = 1 Or Constants.ProjConst.pPara = 2) Then
                rptDoc.SetParameterValue(0, TableName)
                rptDoc.SetParameterValue(1, SortFld1)
                rptDoc.SetParameterValue(2, SortFld2)
                rptDoc.SetParameterValue(3, Trim(txtStart.Text))
                rptDoc.SetParameterValue(4, Trim(txtEnd.Text))
                rptDoc.SetParameterValue(5, mOrder)
                rptDoc.SetParameterValue(6, StrFilter)
                'rptDoc.SetParameterValue(6, "1=1")
                If Constants.ProjConst.pPara = 1 Then
                    Constants.ProjConst.RepTitle = "Branch List"
                Else
                    Constants.ProjConst.RepTitle = "Account Code List"
                    RepHeader1 = "Code From   " + txtStart.Text + "   To   " + txtEnd.Text
                    rptDoc.SetParameterValue("@RepHeader1", RepHeader1)
                End If
            ElseIf Constants.ProjConst.pPara = 3 Then
                rptDoc.SetParameterValue(0, Trim(txtBrCode.Text))
                rptDoc.SetParameterValue(1, Trim(txtStart.Text))
                rptDoc.SetParameterValue(2, Trim(txtEnd.Text))
                rptDoc.SetParameterValue(3, mOrder)
                rptDoc.SetParameterValue(4, StrFilter)
                Constants.ProjConst.RepTitle = "Budget Allocation List"
                RepHeader1 = "Code From   " + aStr2Code(txtStart.Text) + "   To   " + aStr2Code(txtEnd.Text)
                rptDoc.SetParameterValue("@RepHeader1", RepHeader1)
            ElseIf Constants.ProjConst.pPara = 4 Then
                rptDoc.SetParameterValue(0, Trim(txtStart.Text))
                rptDoc.SetParameterValue(1, Trim(txtEnd.Text))
                Constants.ProjConst.RepTitle = "Report Defination List"
                RepHeader1 = "From   " + txtStart.Text + "   To   " + txtEnd.Text
                rptDoc.SetParameterValue("@RepHeader1", RepHeader1)
            ElseIf Constants.ProjConst.pPara = 28 Then
                If rdCode.Checked = True Then
                    Constants.ProjConst.RepTitle = "Account Code List"
                    RepHeader1 = "Code From   " + txtStart.Text + "   To   " + txtEnd.Text
                    'rptDoc.SetParameterValue(0, TableName)
                    rptDoc.SetParameterValue(0, Trim(txtStart.Text))
                    rptDoc.SetParameterValue(1, Trim(txtEnd.Text))
                    rptDoc.SetParameterValue(2, mOrder)
                ElseIf rdAlphabet.Checked = True Then
                    Constants.ProjConst.RepTitle = "Account Code List"
                    RepHeader1 = "Code From   " + lblStart.Text + "   To   " + lblEnd.Text
                    'rptDoc.SetParameterValue(0, TableName)
                    rptDoc.SetParameterValue(0, Trim(lblStart.Text))
                    rptDoc.SetParameterValue(1, Trim(lblEnd.Text))
                    rptDoc.SetParameterValue(2, mOrder)
                End If
                'End If
            End If
            'RepTitle = "Account Code List"
            rptDoc.SetParameterValue("@ComDetail", "")
            rptDoc.SetParameterValue("@RepTitle", Constants.ProjConst.RepTitle)
            If Constants.ProjConst.pPara = 28 Then
                rptDoc.SetParameterValue("@RepHeader1", RepHeader1)
            End If
            If rptDoc.DataDefinition.ParameterFields().Count > 0 Then
                'rptDoc.DataDefinition.FormulaFields(0).Text = StrRepHdr1
                'rptDoc.SetParameterValue("Company", Company)
                ''rptDoc.SetParameterValue("Company", Company)
                'rptDoc.SetParameterValue("Report Header1", "abc")
                'rptDoc.SetParameterValue("Report Header2", "abc")

                'frmReport.rptView.SelectionFormula = "{@Company} = 'Chartec'"
                'frmReport.rptView.SelectionFormula = "@company = 'Chartec'"
                'frmReport.rptView.SelectionFormula = "{Report Title} = 'Chartec title'"
                'frmReport.rptView.SelectionFormula = "{@Report Header1} = 'Chartec Header 1 '"
                'frmReport.rptView.SelectionFormula = "{@Report Header2} = 'Chartec Header 2'"

            End If

            'frmReport.Refresh()
            frmReport.Show()
            frmReport.ShowTransReport(rptDoc, RepName)
        Catch ex As Exception
            ex.Message.ToString()
        End Try
    End Sub

    Private Sub txtBrCode_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtBrCode.KeyDown
        ObjFind = New Grid_Help
        If e.KeyCode = Keys.F1 Then
            ObjFind.PMessage = "Branch"
            strFind = "SELECT BrCode,BrName FROM Branch Order By BrCode"
            ObjFind.sqlqueryFun = strFind
            ObjFind.ShowDialog()

            If ObjFind.PbOk = True Then
                tmp = ObjFind.strPKfun & ""
                Me.txtBrCode.Text = tmp
            End If
        ElseIf e.KeyCode = Keys.Enter Then
            SendKeys.Send("{TAB}")
        End If
    End Sub

    Private Sub txtBrCode_Validating(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles txtBrCode.Validating
        If txtBrCode.Text <> "" Then
            mSQL = "SELECT BrName FROM Branch WHERE BrCode = '" & Trim(txtBrCode.Text) & "'"
            mSQL = GetFldValue(mSQL, "BrName")
            lblBrName.Text = mSQL
            If mSQL = "" Then
                MsgBox("No Record Found", vbInformation, Constants.ProjConst.SysCompany)
                txtBrCode.Text = ""
                e.Cancel = True
            End If
            If rdCode.Checked = True Then
                StrFilter = "Select B.Code, C.Description From Budget B " & _
                            "Inner Join Codes C On C.Code = B.Code " & _
                            "Where B.BrCode = '" & Trim(txtBrCode.Text) & "' " & _
                            "And " & StrFilter & " Order By B.Code"
            ElseIf rdAlphabet.Checked = True Then
                StrFilter = "Select C.Description,B.Code From Budget B " & _
                            "Inner Join Codes C On C.Code = B.Code " & _
                            "Where B.BrCode = '" & Trim(txtBrCode.Text) & "' " & _
                            "And " & StrFilter & " Order By C.Description"
            End If
        End If
    End Sub

    Private Sub rdAlphabet_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles rdAlphabet.KeyDown
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub

    Private Sub rdCode_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles rdCode.KeyDown
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub

    Private Sub txtStart_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtStart.TextChanged

    End Sub

    Private Sub txtStart_MarginChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtStart.MarginChanged

    End Sub

    Private Sub txtStart_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtStart.LostFocus
        txtEnd.Text = txtStart.Text
        lblEnd.Text = lblStart.Text
    End Sub

    Private Sub txtEnd_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtEnd.TextChanged

    End Sub

    Private Sub txtEnd_RightToLeftChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtEnd.RightToLeftChanged

    End Sub

    Private Sub rdCode_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles rdCode.CheckedChanged

    End Sub

    Private Sub rdAlphabet_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles rdAlphabet.CheckedChanged

    End Sub

    Private Sub cboLevel_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cboLevel.SelectedIndexChanged

    End Sub
    Public Sub rptPreview(ByVal sender As Object, ByVal e As System.EventArgs)
        'RepName = "Branches.Rpt"
        Dim path As String = Constants.ProjConst.RepPath & RepName
        Dim rptDoc As New CrystalDecisions.CrystalReports.Engine.ReportDocument
        rptDoc.Load(path)

        Dim obj As Object
        obj = Trim(Constants.ProjConst.SysCompany)
        Try
            If mReport = 0 Then
                Constants.ProjConst.RepTitle = "Branch List"
                rptDoc.SetParameterValue(0, TableName)
                rptDoc.SetParameterValue(1, SortFld1)
                rptDoc.SetParameterValue(2, SortFld2)
                mCode = GetFldValue("Select Min(BrCode) BrCode From Branch", "BrCode")
                rptDoc.SetParameterValue(3, Trim(mCode))
                mCode = GetFldValue("Select Max(BrCode) BrCode From Branch", "BrCode")
                rptDoc.SetParameterValue(4, Trim(mCode))
                rptDoc.SetParameterValue(5, mOrder)
                rptDoc.SetParameterValue(6, StrFilter)
                rptDoc.SetParameterValue("@ComDetail", "")
                rptDoc.SetParameterValue("@RepTitle", Constants.ProjConst.RepTitle)
            ElseIf mReport = 1 Then
                Constants.ProjConst.RepTitle = "Account Code List"
                RepHeader1 = "Code From   " + mCode + "   To   " + mECode
                rptDoc.SetParameterValue(0, Trim(mCode))
                rptDoc.SetParameterValue(1, Trim(mECode))
                rptDoc.SetParameterValue(2, mOrder)
                Constants.ProjConst.RepTitle = "Account Code List"
                rptDoc.SetParameterValue("@ComDetail", "")
                rptDoc.SetParameterValue("@RepTitle", Constants.ProjConst.RepTitle)
                rptDoc.SetParameterValue("@RepHeader1", RepHeader1)
            ElseIf mReport = 3 Then
                rptDoc.SetParameterValue(0, Trim(mCode))
                rptDoc.SetParameterValue(1, Trim(mCode))
                Constants.ProjConst.RepTitle = "Report Defination List"
                RepHeader1 = "From   " + mCode + "   To   " + mCode
                rptDoc.SetParameterValue("@ComDetail", "")
                rptDoc.SetParameterValue("@RepTitle", Constants.ProjConst.RepTitle)
                rptDoc.SetParameterValue("@RepHeader1", RepHeader1)
            ElseIf mReport = 4 Then
                'RepName = "Budget.Rpt"
                Constants.ProjConst.RepTitle = "Budget Allocation List"
                rptDoc.SetParameterValue(0, Trim(mBrCode))
                rptDoc.SetParameterValue(1, Trim(mCode))
                rptDoc.SetParameterValue(2, Trim(mECode))
                rptDoc.SetParameterValue(3, mOrder)
                rptDoc.SetParameterValue(4, StrFilter)
                RepHeader1 = "Code From   " + aStr2Code(mCode) + "   To   " + aStr2Code(mECode)
                rptDoc.SetParameterValue("@ComDetail", "")
                rptDoc.SetParameterValue("@RepTitle", Constants.ProjConst.RepTitle)
                rptDoc.SetParameterValue("@RepHeader1", RepHeader1)
            End If
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.DefaultButton1, Constants.ProjConst.SysCompany)
        End Try
        frmReport.Refresh()
        frmReport.Show()
        frmReport.ShowTransReport(rptDoc, Constants.ProjConst.SysCompany)
    End Sub
End Class