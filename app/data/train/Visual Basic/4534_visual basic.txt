<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmTempConverter
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.cmdEnd = New System.Windows.Forms.Button()
        Me.cmdClear = New System.Windows.Forms.Button()
        Me.cmdConvert = New System.Windows.Forms.Button()
        Me.txtInputTemp = New System.Windows.Forms.TextBox()
        Me.txtDisplayTemp = New System.Windows.Forms.Label()
        Me.lblInputTemp = New System.Windows.Forms.Label()
        Me.lblInfo = New System.Windows.Forms.Label()
        Me.lblCelciusTemp = New System.Windows.Forms.Label()
        Me.SuspendLayout()
        '
        'cmdEnd
        '
        Me.cmdEnd.BackColor = System.Drawing.Color.Red
        Me.cmdEnd.Cursor = System.Windows.Forms.Cursors.Default
        Me.cmdEnd.Font = New System.Drawing.Font("Arial", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.cmdEnd.ForeColor = System.Drawing.SystemColors.ControlText
        Me.cmdEnd.Location = New System.Drawing.Point(231, 321)
        Me.cmdEnd.Name = "cmdEnd"
        Me.cmdEnd.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.cmdEnd.Size = New System.Drawing.Size(129, 49)
        Me.cmdEnd.TabIndex = 13
        Me.cmdEnd.Text = "END"
        Me.cmdEnd.UseVisualStyleBackColor = False
        '
        'cmdClear
        '
        Me.cmdClear.BackColor = System.Drawing.Color.Red
        Me.cmdClear.Cursor = System.Windows.Forms.Cursors.Default
        Me.cmdClear.Font = New System.Drawing.Font("Arial", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.cmdClear.ForeColor = System.Drawing.SystemColors.ControlText
        Me.cmdClear.Location = New System.Drawing.Point(26, 321)
        Me.cmdClear.Name = "cmdClear"
        Me.cmdClear.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.cmdClear.Size = New System.Drawing.Size(145, 49)
        Me.cmdClear.TabIndex = 12
        Me.cmdClear.Text = "CLEAR"
        Me.cmdClear.UseVisualStyleBackColor = False
        '
        'cmdConvert
        '
        Me.cmdConvert.BackColor = System.Drawing.Color.Red
        Me.cmdConvert.Cursor = System.Windows.Forms.Cursors.Default
        Me.cmdConvert.Font = New System.Drawing.Font("Arial", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.cmdConvert.ForeColor = System.Drawing.SystemColors.ControlText
        Me.cmdConvert.Location = New System.Drawing.Point(61, 188)
        Me.cmdConvert.Name = "cmdConvert"
        Me.cmdConvert.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.cmdConvert.Size = New System.Drawing.Size(249, 49)
        Me.cmdConvert.TabIndex = 11
        Me.cmdConvert.Text = "Convert"
        Me.cmdConvert.UseVisualStyleBackColor = False
        '
        'txtInputTemp
        '
        Me.txtInputTemp.AcceptsReturn = True
        Me.txtInputTemp.BackColor = System.Drawing.SystemColors.Window
        Me.txtInputTemp.Cursor = System.Windows.Forms.Cursors.IBeam
        Me.txtInputTemp.Font = New System.Drawing.Font("Arial", 24.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txtInputTemp.ForeColor = System.Drawing.SystemColors.WindowText
        Me.txtInputTemp.Location = New System.Drawing.Point(243, 103)
        Me.txtInputTemp.MaxLength = 0
        Me.txtInputTemp.Name = "txtInputTemp"
        Me.txtInputTemp.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.txtInputTemp.Size = New System.Drawing.Size(111, 44)
        Me.txtInputTemp.TabIndex = 10
        Me.txtInputTemp.Text = " "
        Me.txtInputTemp.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'txtDisplayTemp
        '
        Me.txtDisplayTemp.BackColor = System.Drawing.SystemColors.Control
        Me.txtDisplayTemp.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.txtDisplayTemp.Cursor = System.Windows.Forms.Cursors.Default
        Me.txtDisplayTemp.FlatStyle = System.Windows.Forms.FlatStyle.Popup
        Me.txtDisplayTemp.Font = New System.Drawing.Font("Arial", 24.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txtDisplayTemp.ForeColor = System.Drawing.SystemColors.ControlText
        Me.txtDisplayTemp.Location = New System.Drawing.Point(243, 252)
        Me.txtDisplayTemp.Name = "txtDisplayTemp"
        Me.txtDisplayTemp.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.txtDisplayTemp.Size = New System.Drawing.Size(111, 41)
        Me.txtDisplayTemp.TabIndex = 15
        Me.txtDisplayTemp.Text = " "
        Me.txtDisplayTemp.TextAlign = System.Drawing.ContentAlignment.TopCenter
        '
        'lblInputTemp
        '
        Me.lblInputTemp.BackColor = System.Drawing.Color.SpringGreen
        Me.lblInputTemp.Cursor = System.Windows.Forms.Cursors.Default
        Me.lblInputTemp.Font = New System.Drawing.Font("Arial", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblInputTemp.ForeColor = System.Drawing.SystemColors.ControlText
        Me.lblInputTemp.Location = New System.Drawing.Point(29, 106)
        Me.lblInputTemp.Name = "lblInputTemp"
        Me.lblInputTemp.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.lblInputTemp.Size = New System.Drawing.Size(204, 41)
        Me.lblInputTemp.TabIndex = 9
        Me.lblInputTemp.Text = "Enter Temperature:"
        Me.lblInputTemp.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'lblInfo
        '
        Me.lblInfo.BackColor = System.Drawing.Color.SpringGreen
        Me.lblInfo.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.lblInfo.Cursor = System.Windows.Forms.Cursors.Default
        Me.lblInfo.FlatStyle = System.Windows.Forms.FlatStyle.Popup
        Me.lblInfo.Font = New System.Drawing.Font("Arial", 18.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblInfo.ForeColor = System.Drawing.SystemColors.ControlText
        Me.lblInfo.Location = New System.Drawing.Point(29, 21)
        Me.lblInfo.Name = "lblInfo"
        Me.lblInfo.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.lblInfo.Size = New System.Drawing.Size(325, 77)
        Me.lblInfo.TabIndex = 8
        Me.lblInfo.Text = " "
        Me.lblInfo.TextAlign = System.Drawing.ContentAlignment.TopCenter
        '
        'lblCelciusTemp
        '
        Me.lblCelciusTemp.Font = New System.Drawing.Font("Arial", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblCelciusTemp.Location = New System.Drawing.Point(29, 265)
        Me.lblCelciusTemp.Name = "lblCelciusTemp"
        Me.lblCelciusTemp.Size = New System.Drawing.Size(214, 21)
        Me.lblCelciusTemp.TabIndex = 16
        Me.lblCelciusTemp.Text = "Celcius Temperature:"
        '
        'frmTempConverter
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.SpringGreen
        Me.ClientSize = New System.Drawing.Size(399, 402)
        Me.Controls.Add(Me.lblCelciusTemp)
        Me.Controls.Add(Me.cmdEnd)
        Me.Controls.Add(Me.cmdClear)
        Me.Controls.Add(Me.cmdConvert)
        Me.Controls.Add(Me.txtInputTemp)
        Me.Controls.Add(Me.txtDisplayTemp)
        Me.Controls.Add(Me.lblInputTemp)
        Me.Controls.Add(Me.lblInfo)
        Me.Name = "frmTempConverter"
        Me.Text = "Temperature Converter"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Public WithEvents cmdEnd As System.Windows.Forms.Button
    Public WithEvents cmdClear As System.Windows.Forms.Button
    Public WithEvents cmdConvert As System.Windows.Forms.Button
    Public WithEvents txtInputTemp As System.Windows.Forms.TextBox
    Public WithEvents txtDisplayTemp As System.Windows.Forms.Label
    Public WithEvents lblInputTemp As System.Windows.Forms.Label
    Public WithEvents lblInfo As System.Windows.Forms.Label
    Friend WithEvents lblCelciusTemp As System.Windows.Forms.Label

End Class
