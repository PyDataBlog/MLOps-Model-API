<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class CetakLaporanTransaksi
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then
            components.Dispose()
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(CetakLaporanTransaksi))
        Me.Label1 = New System.Windows.Forms.Label
        Me.btnMember = New System.Windows.Forms.Button
        Me.btnNonMember = New System.Windows.Forms.Button
        Me.btnbatal = New System.Windows.Forms.Button
        Me.GroupBox1 = New System.Windows.Forms.GroupBox
        Me.GroupBox1.SuspendLayout()
        Me.SuspendLayout()
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.BackColor = System.Drawing.Color.Transparent
        Me.Label1.Font = New System.Drawing.Font("Times New Roman", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.Location = New System.Drawing.Point(74, 98)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(222, 22)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "Cetak Laporan Penjualan"
        '
        'btnMember
        '
        Me.btnMember.Location = New System.Drawing.Point(17, 29)
        Me.btnMember.Name = "btnMember"
        Me.btnMember.Size = New System.Drawing.Size(75, 23)
        Me.btnMember.TabIndex = 1
        Me.btnMember.Text = "Member"
        Me.btnMember.UseVisualStyleBackColor = True
        '
        'btnNonMember
        '
        Me.btnNonMember.Location = New System.Drawing.Point(116, 29)
        Me.btnNonMember.Name = "btnNonMember"
        Me.btnNonMember.Size = New System.Drawing.Size(122, 23)
        Me.btnNonMember.TabIndex = 2
        Me.btnNonMember.Text = "Bukan Member"
        Me.btnNonMember.UseVisualStyleBackColor = True
        '
        'btnbatal
        '
        Me.btnbatal.Location = New System.Drawing.Point(141, 207)
        Me.btnbatal.Name = "btnbatal"
        Me.btnbatal.Size = New System.Drawing.Size(75, 23)
        Me.btnbatal.TabIndex = 3
        Me.btnbatal.Text = "Batal"
        Me.btnbatal.UseVisualStyleBackColor = True
        '
        'GroupBox1
        '
        Me.GroupBox1.BackColor = System.Drawing.Color.Transparent
        Me.GroupBox1.Controls.Add(Me.btnMember)
        Me.GroupBox1.Controls.Add(Me.btnNonMember)
        Me.GroupBox1.Location = New System.Drawing.Point(45, 125)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(282, 76)
        Me.GroupBox1.TabIndex = 4
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Laporan Transaksi"
        '
        'CetakLaporanTransaksi
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackgroundImage = CType(resources.GetObject("$this.BackgroundImage"), System.Drawing.Image)
        Me.ClientSize = New System.Drawing.Size(358, 237)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.btnbatal)
        Me.Controls.Add(Me.Label1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.Fixed3D
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "CetakLaporanTransaksi"
        Me.Text = "Cetak Laporan Transaksi"
        Me.GroupBox1.ResumeLayout(False)
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents btnMember As System.Windows.Forms.Button
    Friend WithEvents btnNonMember As System.Windows.Forms.Button
    Friend WithEvents btnbatal As System.Windows.Forms.Button
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
End Class
