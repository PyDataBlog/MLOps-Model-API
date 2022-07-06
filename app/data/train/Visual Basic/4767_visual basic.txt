<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Perakitan
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
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle
        Me.Label1 = New System.Windows.Forms.Label
        Me.GroupBox1 = New System.Windows.Forms.GroupBox
        Me.Button2 = New System.Windows.Forms.Button
        Me.Button1 = New System.Windows.Forms.Button
        Me.DataKebutuhanProduk = New System.Windows.Forms.DataGridView
        Me.pilih = New System.Windows.Forms.DataGridViewCheckBoxColumn
        Me.waktu = New System.Windows.Forms.TextBox
        Me.Label2 = New System.Windows.Forms.Label
        Me.upah = New System.Windows.Forms.TextBox
        Me.jumlah_pekerja = New System.Windows.Forms.TextBox
        Me.Label4 = New System.Windows.Forms.Label
        Me.Label3 = New System.Windows.Forms.Label
        Me.GroupBox1.SuspendLayout()
        CType(Me.DataKebutuhanProduk, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Font = New System.Drawing.Font("Modern No. 20", 20.25!, CType((System.Drawing.FontStyle.Bold Or System.Drawing.FontStyle.Italic), System.Drawing.FontStyle))
        Me.Label1.Location = New System.Drawing.Point(326, 13)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(133, 29)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "Perakitan"
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.Button2)
        Me.GroupBox1.Controls.Add(Me.Button1)
        Me.GroupBox1.Location = New System.Drawing.Point(0, 518)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(785, 50)
        Me.GroupBox1.TabIndex = 1
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Operasi"
        '
        'Button2
        '
        Me.Button2.Location = New System.Drawing.Point(623, 19)
        Me.Button2.Name = "Button2"
        Me.Button2.Size = New System.Drawing.Size(75, 23)
        Me.Button2.TabIndex = 1
        Me.Button2.Text = "Selanjutnya"
        Me.Button2.UseVisualStyleBackColor = True
        '
        'Button1
        '
        Me.Button1.Location = New System.Drawing.Point(704, 19)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(75, 23)
        Me.Button1.TabIndex = 0
        Me.Button1.Text = "Batal"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'DataKebutuhanProduk
        '
        Me.DataKebutuhanProduk.AllowUserToAddRows = False
        Me.DataKebutuhanProduk.AllowUserToDeleteRows = False
        Me.DataKebutuhanProduk.AllowUserToResizeColumns = False
        Me.DataKebutuhanProduk.AllowUserToResizeRows = False
        Me.DataKebutuhanProduk.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        DataGridViewCellStyle1.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter
        DataGridViewCellStyle1.BackColor = System.Drawing.SystemColors.Control
        DataGridViewCellStyle1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        DataGridViewCellStyle1.ForeColor = System.Drawing.SystemColors.WindowText
        DataGridViewCellStyle1.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle1.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        DataGridViewCellStyle1.WrapMode = System.Windows.Forms.DataGridViewTriState.[True]
        Me.DataKebutuhanProduk.ColumnHeadersDefaultCellStyle = DataGridViewCellStyle1
        Me.DataKebutuhanProduk.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DataKebutuhanProduk.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.pilih})
        Me.DataKebutuhanProduk.Location = New System.Drawing.Point(12, 71)
        Me.DataKebutuhanProduk.Name = "DataKebutuhanProduk"
        Me.DataKebutuhanProduk.RowHeadersVisible = False
        Me.DataKebutuhanProduk.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.DisableResizing
        Me.DataKebutuhanProduk.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        Me.DataKebutuhanProduk.ShowCellToolTips = False
        Me.DataKebutuhanProduk.Size = New System.Drawing.Size(760, 324)
        Me.DataKebutuhanProduk.TabIndex = 8
        '
        'pilih
        '
        Me.pilih.HeaderText = "Pilih"
        Me.pilih.Name = "pilih"
        '
        'waktu
        '
        Me.waktu.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.waktu.Location = New System.Drawing.Point(385, 413)
        Me.waktu.Name = "waktu"
        Me.waktu.Size = New System.Drawing.Size(121, 26)
        Me.waktu.TabIndex = 15
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label2.Location = New System.Drawing.Point(263, 419)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(55, 20)
        Me.Label2.TabIndex = 14
        Me.Label2.Text = "Waktu"
        '
        'upah
        '
        Me.upah.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!)
        Me.upah.Location = New System.Drawing.Point(385, 478)
        Me.upah.Name = "upah"
        Me.upah.Size = New System.Drawing.Size(121, 26)
        Me.upah.TabIndex = 27
        '
        'jumlah_pekerja
        '
        Me.jumlah_pekerja.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!)
        Me.jumlah_pekerja.Location = New System.Drawing.Point(385, 445)
        Me.jumlah_pekerja.Name = "jumlah_pekerja"
        Me.jumlah_pekerja.Size = New System.Drawing.Size(121, 26)
        Me.jumlah_pekerja.TabIndex = 26
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!)
        Me.Label4.Location = New System.Drawing.Point(262, 481)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(48, 20)
        Me.Label4.TabIndex = 25
        Me.Label4.Text = "Upah"
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!)
        Me.Label3.Location = New System.Drawing.Point(262, 448)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(117, 20)
        Me.Label3.TabIndex = 24
        Me.Label3.Text = "Jumlah Pekerja"
        '
        'Perakitan
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(784, 562)
        Me.ControlBox = False
        Me.Controls.Add(Me.upah)
        Me.Controls.Add(Me.jumlah_pekerja)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.waktu)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.DataKebutuhanProduk)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.Label1)
        Me.Name = "Perakitan"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Stasiun Kerja"
        Me.GroupBox1.ResumeLayout(False)
        CType(Me.DataKebutuhanProduk, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents Button2 As System.Windows.Forms.Button
    Friend WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents DataKebutuhanProduk As System.Windows.Forms.DataGridView
    Friend WithEvents pilih As System.Windows.Forms.DataGridViewCheckBoxColumn
    Friend WithEvents waktu As System.Windows.Forms.TextBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents upah As System.Windows.Forms.TextBox
    Friend WithEvents jumlah_pekerja As System.Windows.Forms.TextBox
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
End Class
