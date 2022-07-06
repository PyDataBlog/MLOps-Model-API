<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FrmPrefix
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
        Dim DataGridViewCellStyle3 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.TxtProfix = New System.Windows.Forms.TextBox()
        Me.Btn_Delete = New System.Windows.Forms.Button()
        Me.TxtID = New System.Windows.Forms.TextBox()
        Me.BtnEdit = New System.Windows.Forms.Button()
        Me.BttInsert = New System.Windows.Forms.Button()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.DataGridView1 = New System.Windows.Forms.DataGridView()
        Me.txt_id = New System.Windows.Forms.TextBox()
        Me.GroupBox1.SuspendLayout()
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Font = New System.Drawing.Font("Angsana New", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(222, Byte))
        Me.Label6.Location = New System.Drawing.Point(6, 68)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(62, 26)
        Me.Label6.TabIndex = 325
        Me.Label6.Text = "คำนำหน้า"
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.TxtProfix)
        Me.GroupBox1.Controls.Add(Me.Btn_Delete)
        Me.GroupBox1.Controls.Add(Me.TxtID)
        Me.GroupBox1.Controls.Add(Me.BtnEdit)
        Me.GroupBox1.Controls.Add(Me.BttInsert)
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Controls.Add(Me.Label6)
        Me.GroupBox1.Location = New System.Drawing.Point(415, 12)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(373, 165)
        Me.GroupBox1.TabIndex = 328
        Me.GroupBox1.TabStop = False
        '
        'TxtProfix
        '
        Me.TxtProfix.Location = New System.Drawing.Point(123, 65)
        Me.TxtProfix.MaxLength = 10
        Me.TxtProfix.Name = "TxtProfix"
        Me.TxtProfix.Size = New System.Drawing.Size(214, 33)
        Me.TxtProfix.TabIndex = 2
        '
        'Btn_Delete
        '
        Me.Btn_Delete.BackColor = System.Drawing.Color.Snow
        Me.Btn_Delete.Font = New System.Drawing.Font("Angsana New", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(222, Byte))
        Me.Btn_Delete.Location = New System.Drawing.Point(255, 113)
        Me.Btn_Delete.Name = "Btn_Delete"
        Me.Btn_Delete.Size = New System.Drawing.Size(82, 37)
        Me.Btn_Delete.TabIndex = 5
        Me.Btn_Delete.Text = "ลบข้อมูล"
        Me.Btn_Delete.UseVisualStyleBackColor = False
        '
        'TxtID
        '
        Me.TxtID.Location = New System.Drawing.Point(123, 26)
        Me.TxtID.Name = "TxtID"
        Me.TxtID.ReadOnly = True
        Me.TxtID.Size = New System.Drawing.Size(214, 33)
        Me.TxtID.TabIndex = 1
        Me.TxtID.TabStop = False
        '
        'BtnEdit
        '
        Me.BtnEdit.BackColor = System.Drawing.Color.Snow
        Me.BtnEdit.Font = New System.Drawing.Font("Angsana New", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(222, Byte))
        Me.BtnEdit.Location = New System.Drawing.Point(167, 113)
        Me.BtnEdit.Name = "BtnEdit"
        Me.BtnEdit.Size = New System.Drawing.Size(82, 37)
        Me.BtnEdit.TabIndex = 4
        Me.BtnEdit.Text = "แก้ไข"
        Me.BtnEdit.UseVisualStyleBackColor = False
        '
        'BttInsert
        '
        Me.BttInsert.BackColor = System.Drawing.Color.Snow
        Me.BttInsert.Font = New System.Drawing.Font("Angsana New", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(222, Byte))
        Me.BttInsert.Location = New System.Drawing.Point(79, 113)
        Me.BttInsert.Name = "BttInsert"
        Me.BttInsert.Size = New System.Drawing.Size(82, 37)
        Me.BttInsert.TabIndex = 3
        Me.BttInsert.Text = "บันทึก"
        Me.BttInsert.UseVisualStyleBackColor = False
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Font = New System.Drawing.Font("Angsana New", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(222, Byte))
        Me.Label1.Location = New System.Drawing.Point(6, 29)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(94, 26)
        Me.Label1.TabIndex = 324
        Me.Label1.Text = "รหัสคำนำหน้า :"
        '
        'DataGridView1
        '
        Me.DataGridView1.AllowUserToAddRows = False
        Me.DataGridView1.AllowUserToDeleteRows = False
        Me.DataGridView1.BackgroundColor = System.Drawing.SystemColors.ActiveCaption
        DataGridViewCellStyle3.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter
        DataGridViewCellStyle3.BackColor = System.Drawing.SystemColors.Control
        DataGridViewCellStyle3.Font = New System.Drawing.Font("Angsana New", 14.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(222, Byte))
        DataGridViewCellStyle3.ForeColor = System.Drawing.SystemColors.WindowText
        DataGridViewCellStyle3.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle3.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        DataGridViewCellStyle3.WrapMode = System.Windows.Forms.DataGridViewTriState.[True]
        Me.DataGridView1.ColumnHeadersDefaultCellStyle = DataGridViewCellStyle3
        Me.DataGridView1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DataGridView1.Location = New System.Drawing.Point(415, 183)
        Me.DataGridView1.Name = "DataGridView1"
        Me.DataGridView1.ReadOnly = True
        Me.DataGridView1.Size = New System.Drawing.Size(373, 150)
        Me.DataGridView1.TabIndex = 329
        '
        'txt_id
        '
        Me.txt_id.Location = New System.Drawing.Point(415, 339)
        Me.txt_id.Name = "txt_id"
        Me.txt_id.Size = New System.Drawing.Size(100, 33)
        Me.txt_id.TabIndex = 330
        Me.txt_id.Visible = False
        '
        'FrmPrefix
        '
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.None
        Me.ClientSize = New System.Drawing.Size(1163, 366)
        Me.ControlBox = False
        Me.Controls.Add(Me.txt_id)
        Me.Controls.Add(Me.DataGridView1)
        Me.Controls.Add(Me.GroupBox1)
        Me.Font = New System.Drawing.Font("Angsana New", 14.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(222, Byte))
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.Fixed3D
        Me.Margin = New System.Windows.Forms.Padding(3, 6, 3, 6)
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "FrmPrefix"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "คำนำหน้า"
        Me.WindowState = System.Windows.Forms.FormWindowState.Maximized
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents Label6 As Label
    Friend WithEvents GroupBox1 As GroupBox
    Friend WithEvents Btn_Delete As Button
    Friend WithEvents BtnEdit As Button
    Friend WithEvents BttInsert As Button
    Friend WithEvents DataGridView1 As DataGridView
    Friend WithEvents TxtProfix As TextBox
    Friend WithEvents TxtID As TextBox
    Friend WithEvents Label1 As Label
    Friend WithEvents txt_id As TextBox
End Class
