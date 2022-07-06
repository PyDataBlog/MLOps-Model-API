Imports System.Data
Imports System.Data.SqlClient
Imports Basic.DAL
Imports Basic.DAL.Utils
Imports Basic.Constants.ProjConst

Public Class frmAccPostAll
    Inherits System.Windows.Forms.Form

    Dim objAccPost As New cAccPost

    Dim Posting As Boolean
    Dim PostOk As Boolean
    Public mVType As String
    Public mBrCode As String
    Public mVNo As String
    Dim dtAccPost As New DataTable
    Dim dtPostGLHead As New DataTable
    Dim dtPostGLedg As New DataTable
    Dim rowNum As Integer
    Dim rowNum1 As Integer

    Private Sub btnCancel_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnCancel.Click
        cboStVoucher.Items.Clear()
        cboEndVoucher.Items.Clear()
        ProgBar.Value = 0
        Me.Close()
    End Sub

    Private Sub frmAccPostAll_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles Me.KeyDown
        If e.KeyCode = Keys.F2 And Not Posting Then
            mSQL = "Select Min (Type+vno) StVchr, Max(Type+vno) EnVchr From GLHead " & _
                   "where Posted = 'N' and type = '" & mVType & "'"
            cboStVoucher.Text = GetFldValue(mSQL, "stvchr")
            cboEndVoucher.Text = GetFldValue(mSQL, "envchr")
            btnPost.Focus()
        ElseIf e.KeyCode = Keys.F9 And Posting Then
            If MsgBox("Do you want to Stop Posting", vbYesNo, SysCompany) = vbYes Then
                Me.Close()
            End If
        End If
    End Sub

    Private Sub frmAccPostAll_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        'cboStVoucher.Items.Clear()
        'cboEndVoucher.Items.Clear()
        Dim index As Int32
        ProgBar.Value = 0
        objAccPost.Type = mVType
        dtAccPost = objAccPost.FillVouchers
        rowNum = dtAccPost.Rows.Count - 1
        If rowNum >= 0 Then
            Call LoadVouchers()
        End If
        index = cboEndVoucher.FindString(SetUp.mType + SetUp.mVno)
        If index > 0 Then
            cboEndVoucher.SelectedIndex = index
        End If
        index = cboStVoucher.FindString(SetUp.mType + SetUp.mVno)
        If index > 0 Then
            cboStVoucher.SelectedIndex = index
        End If
        'cboStVoucher.Text = lblVNo.Text
        'cboEndVoucher.Text = frmRecPay.lblVNo.Text
        'cboStVoucher.Text = mTemp.ToString()
        Posting = False
    End Sub

    Sub LoadVouchers()
        Dim mi As Integer

        For mi = 0 To rowNum
            cboStVoucher.Items.Add(dtAccPost.Rows(mi).Item("VCHR"))
        Next
        For mi = 0 To rowNum
            cboEndVoucher.Items.Add(dtAccPost.Rows(mi).Item("VCHR"))
        Next
    End Sub

    Private Sub btnPost_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnPost.Click
        On Error GoTo ErrCmdTrans
        'Frame1.Visible = False
        'Label1.Visible = True
        'cmdPost.Visible = False
        'cmdCancel.Visible = False
        ProgBar.Visible = True
        Posting = True
        PostOk = True
        Call PostAll()
        Posting = False
        If PostOk Then
            MsgBox("Voucher(s) Posted Successfully", vbInformation, SysCompany)
        Else
            MsgBox("One Or More Voucher(s) Not Posted Successfully", vbInformation, SysCompany)
        End If
        Me.Close()
        Exit Sub
ErrCmdTrans:
        MsgBox(Err.Description, vbCritical, "Posting")
    End Sub

    Sub PostAll()
        Dim TotRecs As Double

        mSQL = "Select Count(*) A From GLHead Where Posted = 'N' And Type+Vno " & _
              "Between '" & cboStVoucher.Text & " 'and '" & cboEndVoucher.Text & "'"
        TotRecs = Val(GetFldValue(mSQL, "A"))
        If TotRecs < 1 Then
            MsgBox("No Voucher To Post.", vbInformation)
        Else
            ProgBar.Maximum = TotRecs
        End If

        objAccPost.BrCode = Branch
        objAccPost.Type = mType
        objAccPost.StVoucher = cboStVoucher.Text
        objAccPost.EndVoucher = cboEndVoucher.Text
        dtAccPost = objAccPost.PostAllVouchers
        rowNum = dtAccPost.Rows.Count - 1
        If rowNum >= 0 Then
            rowNum = 0
            Do While rowNum < dtAccPost.Rows.Count
                mBrCode = dtAccPost.Rows(rowNum).Item("BrCode")
                mVType = dtAccPost.Rows(rowNum).Item("Type")
                mVNo = dtAccPost.Rows(rowNum).Item("VNo")
                SetGLHead()
                dtPostGLHead = objAccPost.PostGLHead
                SetGLedg()
                objAccPost.PostGLedg()
                ProgBar.Value = ProgBar.Value + 1
                rowNum = rowNum + 1
            Loop
        End If
    End Sub

    Sub SetGLHead()
        objAccPost.BrCode = mBrCode
        objAccPost.Type = mType
        objAccPost.VNo = mVNo
        objAccPost.Posted = "Y"
        objAccPost.PostOn = SySDate
        objAccPost.PostBy = SysUserID
    End Sub

    Sub SetGLedg()
        objAccPost.BrCode = mBrCode
        objAccPost.Type = mType
        objAccPost.VNo = mVNo
        objAccPost.Posted = "Y"
    End Sub

    Private Sub cboStVoucher_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles cboStVoucher.KeyDown
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub

    Private Sub cboEndVoucher_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles cboEndVoucher.KeyDown
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub
End Class
