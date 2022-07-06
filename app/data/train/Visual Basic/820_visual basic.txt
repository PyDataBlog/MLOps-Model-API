<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frm_changetime
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
        Me.mtb_worktime = New System.Windows.Forms.MaskedTextBox()
        Me.btn_OK = New System.Windows.Forms.Button()
        Me.SuspendLayout()
        '
        'mtb_worktime
        '
        Me.mtb_worktime.Font = New System.Drawing.Font("Microsoft Sans Serif", 15.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.mtb_worktime.Location = New System.Drawing.Point(12, 12)
        Me.mtb_worktime.Mask = "00:00:00"
        Me.mtb_worktime.Name = "mtb_worktime"
        Me.mtb_worktime.Size = New System.Drawing.Size(100, 31)
        Me.mtb_worktime.TabIndex = 0
        '
        'btn_OK
        '
        Me.btn_OK.Location = New System.Drawing.Point(119, 13)
        Me.btn_OK.Name = "btn_OK"
        Me.btn_OK.Size = New System.Drawing.Size(89, 30)
        Me.btn_OK.TabIndex = 1
        Me.btn_OK.Text = "OK"
        Me.btn_OK.UseVisualStyleBackColor = True
        '
        'frm_changetime
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(220, 61)
        Me.Controls.Add(Me.btn_OK)
        Me.Controls.Add(Me.mtb_worktime)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.Name = "frm_changetime"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents mtb_worktime As MaskedTextBox
    Friend WithEvents btn_OK As Button
End Class
