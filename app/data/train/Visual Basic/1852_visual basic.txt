
Imports System.Data
Imports System.Data.SqlClient
Imports Basic.DAL
Imports Basic.DAL.Utils
Imports Basic.Constants.ProjConst

Public Class frmVoucherDetail

    Dim objVoucher As New cVoucherDetail

    Public mBrCode As String
    Public mType As String
    Public mVNo As String
    Dim dtMaster As New DataTable
    Dim dtDetail As New DataTable
    Dim rowNum As Integer
    Dim rowNum1 As Integer

    Private Sub frmVoucherDetail_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        vspGrid.Col = 1
        vspGrid.TypePicMask = aMaskFGrid
        vspGrid.Col = 6
        vspGrid.TypePicMask = aMaskFGrid
        vspGrid.TypeDateCentury = True
        dtpVDate.Value = SySDate

        Call SetAccParam()
        GpData.Enabled = False

        If mBrCode <> "" Then
            objVoucher.BrCode = mBrCode
            objVoucher.Type = mType
            objVoucher.VNo = mVNo
        End If

        dtMaster = objVoucher.LoadAllMaster()
        rowNum = dtMaster.Rows.Count - 1
        If rowNum >= 0 Then
            Call LoadMaster()
            objVoucher.BrCode = lblBrCode.Text
            objVoucher.Type = lblType.Text
            objVoucher.VNo = lblVNo.Text
            dtDetail = objVoucher.LoadAllDetail()
            'rowNum = dtDetail.Rows.Count - 1
            Call LoadDetail()
            btnClose.Focus()
        Else
            MsgBox("No Record Found", MsgBoxStyle.Information)
            'Call ClearAll()
        End If
        'vspGrid.Enabled = False
    End Sub

    Sub LoadMaster()
        Try
            lblBrCode.Text = dtMaster.Rows(rowNum).Item("BrCode")
            lblType.Text = dtMaster.Rows(rowNum).Item("Type")
            lblVNo.Text = dtMaster.Rows(rowNum).Item("VNo")
            dtpVDate.Text = dtMaster.Rows(rowNum).Item("VDate")
            txtPurpose.Text = dtMaster.Rows(rowNum).Item("RefRemarks")
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Information)
        End Try
    End Sub

    Sub LoadDetail()
        Dim mi As Integer
        Dim mtotDr As Double
        Dim mtotCr As Double

        Try
            ''
            mtotDr = 0
            mtotCr = 0
            rowNum1 = 0
            If dtDetail.Rows.Count > 0 Then
                vspGrid.MaxRows = 1
                vspGrid.SetText(1, 1, "")
                vspGrid.SetText(2, 1, "")
                vspGrid.SetText(3, 1, "")
                vspGrid.SetText(4, 1, "")
                vspGrid.SetText(5, 1, "")
                vspGrid.SetText(6, 1, "")
                vspGrid.SetText(7, 1, "")

                For mi = 1 To dtDetail.Rows.Count
                    Dim mStr As String
                    If mi > vspGrid.MaxRows Then
                        vspGrid.MaxRows = vspGrid.MaxRows + 1
                    End If
                    mStr = dtDetail.Rows(rowNum1).Item("Code").ToString()
                    vspGrid.SetText(1, mi, aStr2Code(mStr))
                    'vspGrid.SetText(1, mi, mStr)
                    mStr = dtDetail.Rows(rowNum1).Item("Description").ToString()
                    vspGrid.SetText(2, mi, mStr)
                    mStr = dtDetail.Rows(rowNum1).Item("Remarks").ToString()
                    vspGrid.SetText(3, mi, mStr)
                    If Not IsDBNull(dtDetail.Rows(rowNum1).Item("Debit")) Then
                        mStr = dtDetail.Rows(rowNum1).Item("Debit").ToString
                        vspGrid.SetText(4, mi, Format(Val(mStr), "##,###,###,###.00"))
                        mtotDr = mtotDr + Val(mStr)
                    End If
                    If Not IsDBNull(dtDetail.Rows(rowNum1).Item("Credit")) Then
                        mStr = dtDetail.Rows(rowNum1).Item("Credit").ToString
                        vspGrid.SetText(5, mi, Format(Val(mStr), "##,###,###,###.00"))
                        mtotCr = mtotCr + Val(mStr)
                    End If
                    If Not IsDBNull(dtDetail.Rows(rowNum1).Item("RfCode")) Then
                        mStr = dtDetail.Rows(rowNum1).Item("RfCode")
                        vspGrid.SetText(6, mi, aStr2Code(mStr))
                        mSQL = "SELECT Description FROM Codes WHERE Code ='" & mStr & "'"
                        mSQL = GetFldValue(mSQL, "Description")
                        mStr = mSQL
                        vspGrid.SetText(7, mi, mStr)
                    End If
                    If mi < dtDetail.Rows.Count Then
                        vspGrid.MaxRows = vspGrid.MaxRows + 1
                        rowNum1 = rowNum1 + 1
                    End If
                Next
                LBLTotDr.Text = Format(mtotDr, "##,###,###,###.00")
                LBLTotCr.Text = Format(mtotCr, "##,###,###,###.00")
            Else
                vspGrid.MaxRows = 1
                vspGrid.SetText(1, 1, "")
                vspGrid.SetText(2, 1, "")
                vspGrid.SetText(3, 1, "")
                vspGrid.SetText(4, 1, "")
                vspGrid.SetText(5, 1, "")
                vspGrid.SetText(6, 1, "")
                vspGrid.SetText(7, 1, "")
            End If
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Information)
        End Try
    End Sub

    Private Sub btnClose_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnClose.Click
        mBrCode = ""
        mType = ""
        mVNo = ""
        Me.Close()
    End Sub
End Class