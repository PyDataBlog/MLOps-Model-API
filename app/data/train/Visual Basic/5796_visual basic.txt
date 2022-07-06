Imports System.Data
Imports System.Data.SqlClient
Imports Basic.Constants.ProjConst
Imports Basic.DAL.Utils
Imports Basic.DAL
Imports Basic.Reports


Public Class frmBudget
    Inherits System.Windows.Forms.Form

    Dim objBudget As New cBudget

    Dim mAdd As Boolean
    Dim mEdit As Boolean
    Dim mDelete As Boolean
    Dim mPost As Boolean
    Dim mPrint As Boolean

    Dim PODS As New DataSet
    Dim objRow As Data.DataRow
    Dim ObjFind As Grid_Help
    Dim TableName As String = "Budget"
    Dim strFind As String
    Dim mMenuStr As String
    Dim Flag As Boolean
    Dim AddMode As Boolean
    Dim EditMode As Boolean
    Dim tmp As String
    Dim dtBudget As New DataTable
    Dim rowNum As Integer
    Dim dtLookup As DataTable
    Dim vColumn As Integer
    Private sqlquery As String
    Private strPKValue As String
    Private mBudgt As String

    Private Sub txtRefBrCode_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtRefBrCode.KeyDown
        ObjFind = New Grid_Help
        If e.KeyCode = Keys.F1 Then
            ObjFind.PMessage = "Branch"
            strFind = "SELECT BrCode,BrName FROM Branch Order By BrCode"
            ObjFind.sqlqueryFun = strFind
            ObjFind.ShowDialog()

            If ObjFind.PbOk = True Then
                tmp = ObjFind.strPKfun & ""
                Me.txtRefBrCode.Text = tmp
            End If
        ElseIf e.KeyCode = Keys.Enter Then
            SendKeys.Send("{TAB}")
        End If
    End Sub

    Private Sub frmBudget_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
        mBudget = False
    End Sub

    Private Sub frmBudget_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Me.WindowState = FormWindowState.Maximized
        Me.MdiParent = frmMdi
        GpRefAC.Enabled = False
        GpBudget.Enabled = False
        Flag = True
        Call SetAccParam()
        Call SetEntryMode()
        mskCode.Mask = aMaskCode
        mskCode.Text = ""
        mskCode.SelectionStart = 0
        btnSave.Enabled = False
        btnCancel.Enabled = False
        dtBudget = objBudget.LoadAllBudget()
        dtLookup = objBudget.LoadAllBudget()
        mMenuStr = "Select ROW_NUMBER()OVER (ORDER BY B.Code) AS Row,B.BrCode,B.Code,C.Description  from Budget B " & _
                "  Inner Join Codes C on B.Code = C.Code "
        Call MenuGridLoad(mMenuStr)
        rowNum = dtBudget.Rows.Count - 1
        If rowNum >= 0 Then
            'rowNum = dtBudget.Rows.Count - 1
            Call LoadValue()
        End If
        btnAdd.Enabled = True
        btnView.Enabled = False
        btnStatus(False)
        Call SetFormSecurity(Me)
        Call SetButtonsSurity(Me)
        Call SetButtonPrinciple()
        Call SetButton()
        btnExit.Focus()
    End Sub

    Sub LoadValue()

        If (rowNum >= 0) Then
            txtRefBrCode.Text = dtBudget.Rows(rowNum).Item("BrCode") 'objBudget.BrCode
            mSQL = "Select BrName From Branch Where BrCode = '" & Trim(txtRefBrCode.Text) & "'"
            lblBrName.Text = GetFldValue(mSQL, "BrName")
            mskCode.Text = aStr2Code(dtBudget.Rows(rowNum).Item("Code")) 'objBudget.Code
            mSQL = "Select Description From Codes Where Code = '" & aCode2Str(mskCode.Text) & "'"
            lblACName.Text = GetFldValue(mSQL, "Description")
            txtJuly.Text = dtBudget.Rows(rowNum).Item("Budget1") 'objBudget.Budget1
            txtAugust.Text = dtBudget.Rows(rowNum).Item("Budget2") 'objBudget.Budget2
            txtSeptember.Text = dtBudget.Rows(rowNum).Item("Budget3") 'objBudget.Budget3
            txtOctober.Text = dtBudget.Rows(rowNum).Item("Budget4") 'objBudget.Budget4
            txtNovember.Text = dtBudget.Rows(rowNum).Item("Budget5") 'objBudget.Budget5
            txtDecember.Text = dtBudget.Rows(rowNum).Item("Budget6") 'objBudget.Budget6
            txtJanuary.Text = dtBudget.Rows(rowNum).Item("Budget7") 'objBudget.Budget7
            txtFebruary.Text = dtBudget.Rows(rowNum).Item("Budget8") 'objBudget.Budget8
            txtMarch.Text = dtBudget.Rows(rowNum).Item("Budget9") 'objBudget.Budget9
            txtApril.Text = dtBudget.Rows(rowNum).Item("Budget10") 'objBudget.Budget10
            txtMay.Text = dtBudget.Rows(rowNum).Item("Budget11") 'objBudget.Budget11
            txtJune.Text = dtBudget.Rows(rowNum).Item("Budget12") 'objBudget.Budget12
            lblCompany.Text = "Recorded On : " & dtBudget.Rows(rowNum).Item("AddOn") 'objBudget.AddOn
            lblBy.Text = "Recorded By : " & dtBudget.Rows(rowNum).Item("AddBy") 'objBudget.AddBy
            lblToolTip.Text = "Close Form"
        End If
    End Sub

    Private Sub mskCode_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles mskCode.KeyDown
        ObjFind = New Grid_Help
        If e.KeyCode = Keys.F1 Then
            ObjFind.PMessage = "Codes"
            strFind = "SELECT Code,Description FROM Codes Order By Code"
            ObjFind.sqlqueryFun = strFind
            ObjFind.ShowDialog()

            If ObjFind.PbOk = True Then
                tmp = ObjFind.strPKfun & ""
                Me.mskCode.Text = tmp
            End If
        ElseIf e.KeyCode = Keys.Enter Then
            SendKeys.Send("{TAB}")
        End If
    End Sub

    Sub SetEntryMode()
        btnAdd.Enabled = Flag
        btnEdit.Enabled = Flag
        btnView.Enabled = Flag
        btnDelete.Enabled = Flag
        btnPost.Enabled = Flag
        btnPrint.Enabled = Flag
        btnSave.Enabled = Flag
        btnCancel.Enabled = Flag
        btnFind.Enabled = Flag
        btnRefresh.Enabled = Flag
        btnTop.Enabled = Flag
        btnPrevious.Enabled = Flag
        btnNext.Enabled = Flag
        btnBottom.Enabled = Flag
        btnExit.Enabled = Flag
    End Sub

    Sub ClearAll()
        txtRefBrCode.Text = ""
        lblBrName.Text = ""
        mskCode.Text = aEmptyCode
        lblACName.Text = ""
        txtJuly.Text = ""
        txtAugust.Text = ""
        txtSeptember.Text = ""
        txtOctober.Text = ""
        txtNovember.Text = ""
        txtDecember.Text = ""
        txtJanuary.Text = ""
        txtFebruary.Text = ""
        txtMarch.Text = ""
        txtApril.Text = ""
        txtMay.Text = ""
        txtJune.Text = ""
    End Sub

    Sub LoadRefValues()

        objBudget.BrCode = Trim(txtRefBrCode.Text)
        objBudget.Code = aCode2Str(mskCode.Text)
        objBudget.LoadBudget()

        mSQL = "Select BrName From Branch Where BrCode = '" & Trim(txtRefBrCode.Text) & "'"
        lblBrName.Text = GetFldValue(mSQL, "BrName")
        mSQL = "Select Description From Codes Where Code = '" & aCode2Str(mskCode.Text) & "'"
        lblACName.Text = GetFldValue(mSQL, "Description")
        txtJuly.Text = objBudget.Budget1
        txtAugust.Text = objBudget.Budget2
        txtSeptember.Text = objBudget.Budget3
        txtOctober.Text = objBudget.Budget4
        txtNovember.Text = objBudget.Budget5
        txtDecember.Text = objBudget.Budget6
        txtJanuary.Text = objBudget.Budget7
        txtFebruary.Text = objBudget.Budget8
        txtMarch.Text = objBudget.Budget9
        txtApril.Text = objBudget.Budget10
        txtMay.Text = objBudget.Budget11
        txtJune.Text = objBudget.Budget12
        lblCompany.Text = "Recorded On : " & objBudget.AddOn
        lblBy.Text = "Recorded By : " & objBudget.AddBy
        lblToolTip.Text = "Close Form"
    End Sub

    Private Sub txtApril_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtApril.KeyDown
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub

    Private Sub txtAugust_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtAugust.KeyDown
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub

    Private Sub txtDecember_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtDecember.KeyDown
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub

    Private Sub txtFebruary_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtFebruary.KeyDown
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub

    Private Sub txtJanuary_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtJanuary.KeyDown
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub

    Private Sub txtJuly_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtJuly.KeyDown
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub

    Private Sub txtJune_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtJune.KeyDown
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub

    Private Sub txtMarch_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtMarch.KeyDown
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub

    Private Sub txtMay_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtMay.KeyDown
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub

    Private Sub txtNovember_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtNovember.KeyDown
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub

    Private Sub txtOctober_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtOctober.KeyDown
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub

    Private Sub txtSeptember_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtSeptember.KeyDown
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub

    Private Sub txtRefBrCode_Validating(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles txtRefBrCode.Validating
        If Trim(txtRefBrCode.Text) <> "" Then
            txtRefBrCode.Text = Format(Val(Trim(txtRefBrCode.Text)), "000")
            mSQL = "SELECT BrName FROM Branch WHERE BrCode = '" & Trim(txtRefBrCode.Text) & "'"
            mSQL = GetFldValue(mSQL, "BrName")
            lblBrName.Text = mSQL
            If mSQL = "" Then
                MsgBox("No Record Found", vbInformation, SysCompany)
                txtRefBrCode.Text = ""
                e.Cancel = True
            End If
        End If
    End Sub

    Private Sub mskCode_Validating(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles mskCode.Validating
        If mskCode.Text <> aEmptyCode Then
            If Not (Len(aCode2Str(mskCode.Text)) = aP1L Or Len(aCode2Str(mskCode.Text)) = aP1L + aP2L + aP3L Or Len(aCode2Str(mskCode.Text)) = aCodeL) Then
                MsgBox("Invalid Code Length", vbInformation, SysCompany)
                mskCode.Mask = aMaskCode
                mskCode.Text = ""
                mskCode.SelectionStart = 0
                e.Cancel = True
                Exit Sub
            End If
            mSQL = "SELECT Description FROM Codes WHERE Code = '" & aCode2Str(mskCode.Text) & "'"
            mSQL = GetFldValue(mSQL, "Description")
            lblACName.Text = mSQL
            If mSQL = "" Then
                MsgBox("No Record Found", vbInformation, SysCompany)
                mskCode.Text = aEmptyCode
                e.Cancel = True
            End If
        End If
    End Sub

    Private Sub SetData()
        objBudget.BrCode = Me.txtRefBrCode.Text
        objBudget.Code = aCode2Str(Me.mskCode.Text)
        objBudget.Budget1 = Val(Me.txtJuly.Text)
        objBudget.Budget2 = Val(Me.txtAugust.Text)
        objBudget.Budget3 = Val(Me.txtSeptember.Text)
        objBudget.Budget4 = Val(Me.txtOctober.Text)
        objBudget.Budget5 = Val(Me.txtNovember.Text)
        objBudget.Budget6 = Val(Me.txtDecember.Text)
        objBudget.Budget7 = Val(Me.txtJanuary.Text)
        objBudget.Budget8 = Val(Me.txtFebruary.Text)
        objBudget.Budget9 = Val(Me.txtMarch.Text)
        objBudget.Budget10 = Val(Me.txtApril.Text)
        objBudget.Budget11 = Val(Me.txtMay.Text)
        objBudget.Budget12 = Val(Me.txtJune.Text)
        If AddMode Then
            objBudget.AddOn = SySDate
            objBudget.AddBy = SysUserID
            'objBranch.AddBy = SysUserID
        ElseIf EditMode Then
            objBudget.EditOn = SySDate
            objBudget.EditBy = SysUserID
            'objBranch.AddBy = SysUserID
        End If
    End Sub

    Private Function CheckValidation() As Boolean
        If txtRefBrCode.Text = "" Then
            MsgBox("Please enter Branch Name.", MsgBoxStyle.Information, SysCompany)
            txtRefBrCode.Focus()
            Return False
        ElseIf mskCode.Text = aEmptyCode Then
            MsgBox("Please Enter A/C Code", MsgBoxStyle.Information, SysCompany)
            mskCode.Focus()
            Return False
        End If
        Return True
    End Function

    Private Sub btnExit_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnExit.Click
        lblToolTip.Text = "Close Form"
        Me.Close()
    End Sub

    Private Sub btnPrint_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnPrint.Click
        Dim frmList As New frmRPT_List

        lblToolTip.Text = "Print Record(s)"
        pPara = 3
        frmList.ShowDialog()
    End Sub

    Private Sub btnFind_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnFind.Click
        'Dim frmList As New frmRPT_List
        frmRPT_List.RepName = "Budget.rpt"
        frmRPT_List.mReport = 4
        frmRPT_List.mBrCode = txtRefBrCode.Text
        frmRPT_List.mCode = aCode2Str(mskCode.Text)
        frmRPT_List.mECode = aCode2Str(mskCode.Text)
        'SetUp.mOrder = 0
        mOrder = 0
        frmRPT_List.StrFilter = "1=1"
        lblToolTip.Text = "Preview Record(s)"
        frmRPT_List.rptPreview(sender, e)

        'lblToolTip.Text = "Find A Record"
        'ObjFind = New Grid_Help
        'mOrder = 0
        'strFind = "Select BrCode,Code from Budget Order by BrCode,Code"
        'ObjFind.PMessage = "Budget"
        'ObjFind.sqlqueryFun = strFind
        'ObjFind.ShowDialog()

        'If ObjFind.PbOk = True Then
        '    Call ClearAll()
        '    txtRefBrCode.Text = ObjFind.strPKfun & ""
        '    mskCode.Text = aStr2Code(ObjFind.sqlqueryFun) & ""
        '    'Call SetData()
        '    Call LoadRefValues()
        'End If
        'Call SetFormSecurity(Me)
        'Call SetButtonsSurity(Me)
        'Call SetButtonPrinciple()
        'Call SetButton()
    End Sub

    Private Sub btnCancel_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnCancel.Click
        Dim opt As MsgBoxResult
        lblToolTip.Text = "Cancel Last Action"
        opt = MsgBox("Do you wish to Abort ?", MsgBoxStyle.YesNo)
        If opt = MsgBoxResult.No Then
            txtRefBrCode.Focus()
            Exit Sub
        End If
        Call ClearAll()
        dtBudget = objBudget.LoadAllBudget()
        rowNum = dtBudget.Rows.Count - 1
        If rowNum >= 0 Then
            rowNum = dtBudget.Rows.Count - 1
            Call LoadValue()
        End If
        Flag = True
        Call SetEntryMode()
        GpRefAC.Enabled = False
        GpBudget.Enabled = False
        btnSave.Enabled = False
        btnCancel.Enabled = False
        Call MenuGridLoad(mMenuStr)
        rowNum = dtBudget.Rows.Count - 1
        Call SetFormSecurity(Me)
        Call SetButtonsSurity(Me)
        Call SetButtonPrinciple()
        Call SetButton()
        AddMode = False
        EditMode = False
    End Sub

    Private Sub btnDelete_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnDelete.Click
        lblToolTip.Text = "Delete Current Record"
        Opt = MsgBox("Are You Sure To Delete Current Record", MsgBoxStyle.YesNo)
        If Opt = MsgBoxResult.Yes Then
            objBudget.BrCode = Trim(txtRefBrCode.Text)
            objBudget.Code = aCode2Str(mskCode.Text)
            objBudget.DelBranch()
            Call ClearAll()
            If rowNum > 0 Then
                rowNum = rowNum - 1
                Call LoadValue()
            Else
                MsgBox("No Record Found", MsgBoxStyle.Information)
            End If
        End If
        Call MenuGridLoad(mMenuStr)
        rowNum = dtBudget.Rows.Count - 1
        Call SetButtonPrinciple()
        Call SetButton()
    End Sub

    Private Sub btnAdd_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnAdd.Click
        AddMode = True
        EditMode = False
        Flag = True
        Call SetEntryMode()
        btnSave.Enabled = True
        btnCancel.Enabled = True
        Call ClearAll()
        Call SetEntryMode()
        GpRefAC.Enabled = True
        GpBudget.Enabled = True
        lblCompany.Text = "Recorded On " & SySDate
        lblToolTip.Text = "Add New Record"
        lblBy.Text = "Recorded By : " & SysUserID
        'lblBy.Text = "Recorded By : " & SysUserID
        btnSave.Enabled = True
        btnCancel.Enabled = True
        EmptyControls(Me)
        txtRefBrCode.Focus()

    End Sub

    Private Sub btnTop_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnTop.Click
        rowNum = 0
        btnTop.Enabled = False
        btnPrevious.Enabled = False
        btnNext.Enabled = True
        btnBottom.Enabled = True
        Call LoadValue()
        lblToolTip.Text = "Move To First Record"
        Call SetButtonPrinciple()
        Call SetButton()
    End Sub

    Private Sub btnBottom_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnBottom.Click
        rowNum = dtBudget.Rows.Count - 1
        btnTop.Enabled = True
        btnPrevious.Enabled = True
        btnNext.Enabled = False
        btnBottom.Enabled = False
        Call LoadValue()
        lblToolTip.Text = "Move To Last Record"
        Call SetButtonPrinciple()
        Call SetButton()
    End Sub

    Private Sub btnEdit_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnEdit.Click
        lblToolTip.Text = "Edit Current Record"
        EditMode = True
        AddMode = False
        Flag = False
        Call SetEntryMode()
        GpBudget.Enabled = True
        txtJuly.Focus()
        btnEdit.Enabled = False
        btnSave.Enabled = True
        btnCancel.Enabled = True
    End Sub

    Private Sub btnSave_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnSave.Click
        lblToolTip.Text = "Save Current Record"
        objBudget.getConnection()
        objBudget.BeginTransaction()
        If CheckValidation() Then
            SetData()
            If AddMode Then
                Try
                    objBudget.SaveBudget()
                Catch ex As Exception
                    MsgBox(ex.Message)
                    objBudget.RollBack()
                End Try
                Call btnAdd_Click(Nothing, Nothing)
            ElseIf EditMode Then
                Try
                    objBudget.EditBudget()
                Catch ex As Exception
                    MsgBox(ex.Message)
                    objBudget.RollBack()
                End Try
                Flag = True
                dtBudget = objBudget.LoadAllBudget()
                Call SetEntryMode()
                GpRefAC.Enabled = False
                GpBudget.Enabled = False
                btnSave.Enabled = False
                btnCancel.Enabled = False
            End If
            objBudget.CommitTransction()
        End If
        'rowNum = dtBudget.Rows.Count - 1
        Call MenuGridLoad(mMenuStr)
        rowNum = dtBudget.Rows.Count - 1
        EmptyControls(Me)
        ClearAll()
        'btnRefresh_Click(sender, e)
        Call SetButton()
    End Sub

    Private Sub btnNext_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnNext.Click
        'If rowNum < dtBudget.Rows.Count - 1 Then
        '    rowNum = rowNum + 1
        '    btnNext.Enabled = True
        '    btnBottom.Enabled = True
        '    btnPrevious.Enabled = True
        '    btnTop.Enabled = True
        'Else
        '    rowNum = dtBudget.Rows.Count - 1
        '    btnNext.Enabled = False
        '    btnBottom.Enabled = False
        'End If
        If rowNum < dtBudget.Rows.Count - 1 Then
            rowNum = rowNum + 1
            btnNext.Enabled = True
            btnBottom.Enabled = True
            btnPrevious.Enabled = True
            btnTop.Enabled = True
        Else
            rowNum = dtBudget.Rows.Count - 1
            btnNext.Enabled = False
            btnBottom.Enabled = False
            btnTop.Enabled = True
            btnPrevious.Enabled = True
        End If
        LoadValue()
        lblToolTip.Text = "Move To Next Record"
        Call SetButtonPrinciple()
        Call SetButton()
    End Sub

    Private Sub btnPrevious_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnPrevious.Click
        'If rowNum > 0 Then
        '    rowNum = rowNum - 1
        '    btnNext.Enabled = True
        '    btnBottom.Enabled = True
        '    btnPrevious.Enabled = True
        '    btnTop.Enabled = True
        'Else
        '    rowNum = 0
        '    btnTop.Enabled = False
        '    btnPrevious.Enabled = False
        'End If
        If rowNum > 0 Then
            rowNum = rowNum - 1
            btnNext.Enabled = True
            btnBottom.Enabled = True
            btnPrevious.Enabled = True
            btnTop.Enabled = True
        Else
            rowNum = 0
            btnTop.Enabled = False
            btnPrevious.Enabled = False
            btnNext.Enabled = True
            btnBottom.Enabled = True
        End If
        LoadValue()
        lblToolTip.Text = "Move To Previous Record"
        Call SetButtonPrinciple()
        Call SetButton()
    End Sub

    Private Sub btnRefresh_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnRefresh.Click
        lblToolTip.Text = "Refresh Records"
        rowNum = dtBudget.Rows.Count - 1
        If rowNum >= 0 Then
            Call LoadValue()
            Call SetFormSecurity(Me)
            Call SetButtonsSurity(Me)
            Call SetButtonPrinciple()
            Call SetButton()
        End If
        Call MenuGridLoad(mMenuStr)
        rowNum = dtBudget.Rows.Count - 1
    End Sub

    Private Sub btnView_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnView.Click

    End Sub

    Private Sub mskCode_MaskInputRejected(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MaskInputRejectedEventArgs) Handles mskCode.MaskInputRejected

    End Sub
    Private Sub btnStatus(ByVal status As Boolean)
        btnAdd.Enabled = status
        btnEdit.Enabled = status
        btnDelete.Enabled = status
        btnPost.Enabled = status
        btnPrint.Enabled = status
    End Sub
    Private Sub SetButtonPrinciple()
        mAdd = mbtnAdd
        mEdit = mbtnEdit
        mDelete = mbtnDelete
        mPrint = mbtnPrint
        mPost = mbtnPost
    End Sub
    Private Sub SetButton()
        btnAdd.Enabled = mAdd
        btnEdit.Enabled = mEdit
        btnDelete.Enabled = mDelete
        btnPrint.Enabled = mPrint
        btnPost.Enabled = mPost
    End Sub

    Private Sub btnPost_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnPost.Click
        Call SetButtonPrinciple()
        Call SetButton()
    End Sub

    Private Sub GVHelp_CellClick(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles GVHelp.CellClick
        vColumn = e.ColumnIndex
    End Sub

    Private Sub GVHelp_CellContentClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles GVHelp.CellContentClick

    End Sub

    Private Sub GVHelp_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles GVHelp.DoubleClick
        If AddMode <> True And EditMode <> True Then
            If GVHelp.RowCount <> 0 Then
                Me.sqlquery = Me.GVHelp.Item(0, GVHelp.CurrentRow.Index).Value
                rowNum = sqlquery - 1
                dtBudget = objBudget.LoadAllBudget()
                If rowNum >= 0 Then
                    Call LoadValue()
                End If
                If rowNum = 0 Then
                    btnTop.Enabled = False
                    btnPrevious.Enabled = False
                    btnNext.Enabled = True
                    btnBottom.Enabled = True
                ElseIf rowNum = dtBudget.Rows.Count - 1 Then
                    btnTop.Enabled = True
                    btnPrevious.Enabled = True
                    btnNext.Enabled = False
                    btnBottom.Enabled = False
                ElseIf rowNum <> 0 And rowNum < dtBudget.Rows.Count - 1 Then
                    btnTop.Enabled = True
                    btnPrevious.Enabled = True
                    btnNext.Enabled = True
                    btnBottom.Enabled = True
                End If
            End If
        End If
    End Sub

    Private Sub txtSearch_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtSearch.KeyDown
        If e.KeyCode = Keys.Enter And Me.GVHelp.Rows.Count > 0 Then
            If mOrder = 0 Then
                Me.strPKValue = Me.GVHelp.Item(0, GVHelp.CurrentRow.Index).Value
                Me.sqlquery = Me.GVHelp.Item(1, GVHelp.CurrentRow.Index).Value
            ElseIf mOrder = 1 Then
                Me.strPKValue = Me.GVHelp.Item(1, GVHelp.CurrentRow.Index).Value
                Me.sqlquery = Me.GVHelp.Item(0, GVHelp.CurrentRow.Index).Value
            End If
        ElseIf e.KeyCode = Keys.Enter And Me.GVHelp.Rows.Count <= 0 Then
            'MsgBox("Record Does Not Exist", MsgBoxStyle.Information, SysCompany)
            Me.txtSearch.Text = String.Empty

            Me.strPKValue = 0
        End If
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub

    Private Sub txtSearch_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles txtSearch.KeyPress
        If (Char.IsNumber(e.KeyChar)) <> True And (Char.IsLetter(e.KeyChar)) <> True And (Char.IsWhiteSpace(e.KeyChar) <> True) And (Char.IsSymbol(e.KeyChar)) <> True And (Char.IsControl(e.KeyChar)) <> True And (Char.IsPunctuation(e.KeyChar)) <> True Then
            e.Handled = True
        End If
    End Sub

    Private Sub txtSearch_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtSearch.Resize

    End Sub

    Private Sub txtSearch_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtSearch.TextChanged
        If txtSearch.Text = "" Then
            vColumn = 0
            dtLookup.DefaultView.RowFilter = Nothing
            'lblNoOfRecords.Text = Me.GVHelp.RowCount & " Records Found"
        Else
            With dtLookup
                If .Columns(vColumn).DataType.ToString = "System.Int32" Or .Columns(vColumn).DataType.ToString = "System.Double" Then
                    .DefaultView.RowFilter = .Columns(vColumn).Caption & " = " & Val(txtSearch.Text)
                ElseIf .Columns(vColumn).DataType.ToString = "System.String" Or .Columns(vColumn).DataType.ToString = "System.char" Then
                    .DefaultView.RowFilter = .Columns(vColumn).Caption & " like '" & txtSearch.Text & "%'"
                End If
                'lblNoOfRecords.Text = Me.GVHelp.RowCount & " Records Found"
            End With
        End If
    End Sub
    Private Sub MenuGridLoad(ByVal mQuery As String)
        Dim mcol As Integer
        Dim mcolName As String
        Try
            If mQuery <> "" And mQuery <> Nothing Then
                dtLookup = Lookup(mQuery).Tables(0)
                Me.GVHelp.DataSource = dtLookup.DefaultView

                Dim header_style As New DataGridViewCellStyle
                header_style.BackColor = Color.Yellow
                GVHelp.Columns(2).HeaderCell.Style = header_style
                GVHelp.Columns(0).Visible = 0
                If GVHelp.Columns.Count > 0 Then
                    For mcol = 1 To GVHelp.Columns.Count - 1
                        mcolName = GVHelp.Columns(mcol).Name
                        If GVHelp.Columns(mcol).Name = "RefRemarks" Then
                            GVHelp.Columns(mcol).Width = 160
                        ElseIf GVHelp.Columns(mcol).Name <> "BrCode" And GVHelp.Columns(mcol).Name <> "Type" And GVHelp.Columns(mcol).Name <> "TaxRate" And GVHelp.Columns(mcol).Name <> "Posted" Then
                            GVHelp.Columns(mcol).Width = 80

                        Else
                            GVHelp.Columns(mcol).Width = 40
                        End If
                    Next
                End If
                GVHelp.Columns(0).Visible = 0
                If GVHelp.Rows.Count > 0 Then
                    For mRow = 0 To GVHelp.Rows.Count - 1
                        If mRow Mod 2 = 0 Then
                            GVHelp.Rows(mRow).DefaultCellStyle.BackColor = Color.White
                        Else
                            GVHelp.Rows(mRow).DefaultCellStyle.BackColor = Color.PowderBlue
                        End If
                    Next
                End If
                GVHelp.Columns(0).Visible = 0
            End If
        Catch ex As Exception
            'MsgBox(ex.Message)
        End Try
    End Sub

    Private Sub txtRefBrCode_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtRefBrCode.TextChanged

    End Sub

    Private Sub frmBudget_Activated(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Activated
        mMenuStr = "Select ROW_NUMBER()OVER (ORDER BY B.Code) AS Row,B.BrCode,B.Code,C.Description  from Budget B " & _
                        "  Inner Join Codes C on B.Code = C.Code "
        Call MenuGridLoad(mMenuStr)
    End Sub
End Class