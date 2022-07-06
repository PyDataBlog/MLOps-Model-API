<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FRMoptions
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
        Me.BTNok = New System.Windows.Forms.Button()
        Me.BTNcancel = New System.Windows.Forms.Button()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.RADrandom = New System.Windows.Forms.RadioButton()
        Me.RADalternate = New System.Windows.Forms.RadioButton()
        Me.RADloser = New System.Windows.Forms.RadioButton()
        Me.RADwinner = New System.Windows.Forms.RadioButton()
        Me.CHKundo = New System.Windows.Forms.CheckBox()
        Me.CHKredo = New System.Windows.Forms.CheckBox()
        Me.CHKtimed = New System.Windows.Forms.CheckBox()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.TextBox1 = New System.Windows.Forms.TextBox()
        Me.GroupBox4 = New System.Windows.Forms.GroupBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.PictureBox2 = New System.Windows.Forms.PictureBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.TextBox2 = New System.Windows.Forms.TextBox()
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.BTNresetBGcolor = New System.Windows.Forms.Button()
        Me.BTNbgcolor = New System.Windows.Forms.Button()
        Me.PNLbgcolor = New System.Windows.Forms.Panel()
        Me.Panel4 = New System.Windows.Forms.Panel()
        Me.Panel3 = New System.Windows.Forms.Panel()
        Me.Panel2 = New System.Windows.Forms.Panel()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.CLDbg = New System.Windows.Forms.ColorDialog()
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox4.SuspendLayout()
        CType(Me.PictureBox2, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox5.SuspendLayout()
        Me.PNLbgcolor.SuspendLayout()
        Me.SuspendLayout()
        '
        'BTNok
        '
        Me.BTNok.Location = New System.Drawing.Point(183, 228)
        Me.BTNok.Name = "BTNok"
        Me.BTNok.Size = New System.Drawing.Size(92, 23)
        Me.BTNok.TabIndex = 0
        Me.BTNok.Text = "OK"
        Me.BTNok.UseVisualStyleBackColor = True
        '
        'BTNcancel
        '
        Me.BTNcancel.Location = New System.Drawing.Point(281, 228)
        Me.BTNcancel.Name = "BTNcancel"
        Me.BTNcancel.Size = New System.Drawing.Size(92, 23)
        Me.BTNcancel.TabIndex = 1
        Me.BTNcancel.Text = "Cancel"
        Me.BTNcancel.UseVisualStyleBackColor = True
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.RADrandom)
        Me.GroupBox1.Controls.Add(Me.RADalternate)
        Me.GroupBox1.Controls.Add(Me.RADloser)
        Me.GroupBox1.Controls.Add(Me.RADwinner)
        Me.GroupBox1.FlatStyle = System.Windows.Forms.FlatStyle.System
        Me.GroupBox1.Location = New System.Drawing.Point(281, 12)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(90, 118)
        Me.GroupBox1.TabIndex = 8
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "First Player"
        '
        'RADrandom
        '
        Me.RADrandom.AutoSize = True
        Me.RADrandom.Location = New System.Drawing.Point(6, 88)
        Me.RADrandom.Name = "RADrandom"
        Me.RADrandom.Size = New System.Drawing.Size(68, 17)
        Me.RADrandom.TabIndex = 8
        Me.RADrandom.Text = "Random"
        Me.RADrandom.UseVisualStyleBackColor = True
        '
        'RADalternate
        '
        Me.RADalternate.AutoSize = True
        Me.RADalternate.Checked = True
        Me.RADalternate.Location = New System.Drawing.Point(6, 19)
        Me.RADalternate.Name = "RADalternate"
        Me.RADalternate.Size = New System.Drawing.Size(72, 17)
        Me.RADalternate.TabIndex = 2
        Me.RADalternate.TabStop = True
        Me.RADalternate.Text = "Alternate"
        Me.RADalternate.UseVisualStyleBackColor = True
        '
        'RADloser
        '
        Me.RADloser.AutoSize = True
        Me.RADloser.Location = New System.Drawing.Point(6, 65)
        Me.RADloser.Name = "RADloser"
        Me.RADloser.Size = New System.Drawing.Size(52, 17)
        Me.RADloser.TabIndex = 1
        Me.RADloser.Text = "Loser"
        Me.RADloser.UseVisualStyleBackColor = True
        '
        'RADwinner
        '
        Me.RADwinner.AutoSize = True
        Me.RADwinner.Location = New System.Drawing.Point(6, 42)
        Me.RADwinner.Name = "RADwinner"
        Me.RADwinner.Size = New System.Drawing.Size(63, 17)
        Me.RADwinner.TabIndex = 0
        Me.RADwinner.Text = "Winner"
        Me.RADwinner.UseVisualStyleBackColor = True
        '
        'CHKundo
        '
        Me.CHKundo.AutoSize = True
        Me.CHKundo.Checked = True
        Me.CHKundo.CheckState = System.Windows.Forms.CheckState.Checked
        Me.CHKundo.Location = New System.Drawing.Point(6, 24)
        Me.CHKundo.Name = "CHKundo"
        Me.CHKundo.Size = New System.Drawing.Size(87, 17)
        Me.CHKundo.TabIndex = 9
        Me.CHKundo.TabStop = False
        Me.CHKundo.Text = "Allow Undo"
        Me.CHKundo.UseVisualStyleBackColor = True
        '
        'CHKredo
        '
        Me.CHKredo.AutoSize = True
        Me.CHKredo.Checked = True
        Me.CHKredo.CheckState = System.Windows.Forms.CheckState.Checked
        Me.CHKredo.Location = New System.Drawing.Point(6, 54)
        Me.CHKredo.Name = "CHKredo"
        Me.CHKredo.Size = New System.Drawing.Size(85, 17)
        Me.CHKredo.TabIndex = 10
        Me.CHKredo.TabStop = False
        Me.CHKredo.Text = "Allow Redo"
        Me.CHKredo.UseVisualStyleBackColor = True
        '
        'CHKtimed
        '
        Me.CHKtimed.AutoSize = True
        Me.CHKtimed.Checked = True
        Me.CHKtimed.CheckState = System.Windows.Forms.CheckState.Checked
        Me.CHKtimed.Location = New System.Drawing.Point(6, 84)
        Me.CHKtimed.Name = "CHKtimed"
        Me.CHKtimed.Size = New System.Drawing.Size(87, 17)
        Me.CHKtimed.TabIndex = 11
        Me.CHKtimed.TabStop = False
        Me.CHKtimed.Text = "Timed game"
        Me.CHKtimed.UseVisualStyleBackColor = True
        '
        'GroupBox2
        '
        Me.GroupBox2.Controls.Add(Me.CHKundo)
        Me.GroupBox2.Controls.Add(Me.CHKredo)
        Me.GroupBox2.Controls.Add(Me.CHKtimed)
        Me.GroupBox2.Location = New System.Drawing.Point(178, 12)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(97, 118)
        Me.GroupBox2.TabIndex = 12
        Me.GroupBox2.TabStop = False
        Me.GroupBox2.Text = "Gameplay"
        '
        'GroupBox3
        '
        Me.GroupBox3.Controls.Add(Me.Label3)
        Me.GroupBox3.Controls.Add(Me.Label2)
        Me.GroupBox3.Controls.Add(Me.PictureBox1)
        Me.GroupBox3.Controls.Add(Me.Label1)
        Me.GroupBox3.Controls.Add(Me.TextBox1)
        Me.GroupBox3.Location = New System.Drawing.Point(12, 12)
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.Size = New System.Drawing.Size(160, 118)
        Me.GroupBox3.TabIndex = 13
        Me.GroupBox3.TabStop = False
        Me.GroupBox3.Text = "Player 1 (X)"
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Label3.Font = New System.Drawing.Font("Segoe UI", 20.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label3.Location = New System.Drawing.Point(15, 60)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(36, 37)
        Me.Label3.TabIndex = 4
        Me.Label3.Text = "<"
        Me.Label3.TextAlign = System.Drawing.ContentAlignment.TopCenter
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Label2.Font = New System.Drawing.Font("Segoe UI", 20.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label2.Location = New System.Drawing.Point(111, 60)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(36, 37)
        Me.Label2.TabIndex = 3
        Me.Label2.Text = ">"
        Me.Label2.TextAlign = System.Drawing.ContentAlignment.TopCenter
        '
        'PictureBox1
        '
        Me.PictureBox1.Image = Global.TicTacToe.My.Resources.Resources.X
        Me.PictureBox1.Location = New System.Drawing.Point(57, 57)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(48, 48)
        Me.PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage
        Me.PictureBox1.TabIndex = 2
        Me.PictureBox1.TabStop = False
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(6, 22)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(39, 13)
        Me.Label1.TabIndex = 1
        Me.Label1.Text = "Name:"
        '
        'TextBox1
        '
        Me.TextBox1.Location = New System.Drawing.Point(50, 19)
        Me.TextBox1.Name = "TextBox1"
        Me.TextBox1.Size = New System.Drawing.Size(104, 22)
        Me.TextBox1.TabIndex = 0
        Me.TextBox1.TabStop = False
        '
        'GroupBox4
        '
        Me.GroupBox4.Controls.Add(Me.Label4)
        Me.GroupBox4.Controls.Add(Me.Label5)
        Me.GroupBox4.Controls.Add(Me.PictureBox2)
        Me.GroupBox4.Controls.Add(Me.Label6)
        Me.GroupBox4.Controls.Add(Me.TextBox2)
        Me.GroupBox4.Location = New System.Drawing.Point(12, 136)
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.Size = New System.Drawing.Size(160, 118)
        Me.GroupBox4.TabIndex = 14
        Me.GroupBox4.TabStop = False
        Me.GroupBox4.Text = "Player 2 (O)"
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Label4.Font = New System.Drawing.Font("Segoe UI", 20.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label4.Location = New System.Drawing.Point(15, 60)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(36, 37)
        Me.Label4.TabIndex = 4
        Me.Label4.Text = "<"
        Me.Label4.TextAlign = System.Drawing.ContentAlignment.TopCenter
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Label5.Font = New System.Drawing.Font("Segoe UI", 20.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label5.Location = New System.Drawing.Point(111, 60)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(36, 37)
        Me.Label5.TabIndex = 3
        Me.Label5.Text = ">"
        Me.Label5.TextAlign = System.Drawing.ContentAlignment.TopCenter
        '
        'PictureBox2
        '
        Me.PictureBox2.Image = Global.TicTacToe.My.Resources.Resources.O
        Me.PictureBox2.Location = New System.Drawing.Point(57, 57)
        Me.PictureBox2.Name = "PictureBox2"
        Me.PictureBox2.Size = New System.Drawing.Size(48, 48)
        Me.PictureBox2.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage
        Me.PictureBox2.TabIndex = 2
        Me.PictureBox2.TabStop = False
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Location = New System.Drawing.Point(6, 22)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(39, 13)
        Me.Label6.TabIndex = 1
        Me.Label6.Text = "Name:"
        '
        'TextBox2
        '
        Me.TextBox2.Location = New System.Drawing.Point(50, 19)
        Me.TextBox2.Name = "TextBox2"
        Me.TextBox2.Size = New System.Drawing.Size(104, 22)
        Me.TextBox2.TabIndex = 0
        Me.TextBox2.TabStop = False
        '
        'GroupBox5
        '
        Me.GroupBox5.Controls.Add(Me.BTNresetBGcolor)
        Me.GroupBox5.Controls.Add(Me.BTNbgcolor)
        Me.GroupBox5.Controls.Add(Me.PNLbgcolor)
        Me.GroupBox5.Location = New System.Drawing.Point(184, 136)
        Me.GroupBox5.Name = "GroupBox5"
        Me.GroupBox5.Size = New System.Drawing.Size(187, 84)
        Me.GroupBox5.TabIndex = 15
        Me.GroupBox5.TabStop = False
        Me.GroupBox5.Text = "Background Color"
        '
        'BTNresetBGcolor
        '
        Me.BTNresetBGcolor.AutoSize = True
        Me.BTNresetBGcolor.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink
        Me.BTNresetBGcolor.Location = New System.Drawing.Point(92, 51)
        Me.BTNresetBGcolor.Name = "BTNresetBGcolor"
        Me.BTNresetBGcolor.Size = New System.Drawing.Size(45, 23)
        Me.BTNresetBGcolor.TabIndex = 2
        Me.BTNresetBGcolor.TabStop = False
        Me.BTNresetBGcolor.Text = "Reset"
        Me.BTNresetBGcolor.UseVisualStyleBackColor = True
        '
        'BTNbgcolor
        '
        Me.BTNbgcolor.AutoSize = True
        Me.BTNbgcolor.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink
        Me.BTNbgcolor.Location = New System.Drawing.Point(92, 22)
        Me.BTNbgcolor.Name = "BTNbgcolor"
        Me.BTNbgcolor.Size = New System.Drawing.Size(65, 23)
        Me.BTNbgcolor.TabIndex = 0
        Me.BTNbgcolor.TabStop = False
        Me.BTNbgcolor.Text = "Choose..."
        Me.BTNbgcolor.UseVisualStyleBackColor = True
        '
        'PNLbgcolor
        '
        Me.PNLbgcolor.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.PNLbgcolor.Controls.Add(Me.Panel4)
        Me.PNLbgcolor.Controls.Add(Me.Panel3)
        Me.PNLbgcolor.Controls.Add(Me.Panel2)
        Me.PNLbgcolor.Controls.Add(Me.Panel1)
        Me.PNLbgcolor.Cursor = System.Windows.Forms.Cursors.Hand
        Me.PNLbgcolor.Location = New System.Drawing.Point(35, 22)
        Me.PNLbgcolor.Name = "PNLbgcolor"
        Me.PNLbgcolor.Size = New System.Drawing.Size(50, 50)
        Me.PNLbgcolor.TabIndex = 1
        '
        'Panel4
        '
        Me.Panel4.BackColor = System.Drawing.Color.Black
        Me.Panel4.Location = New System.Drawing.Point(31, 0)
        Me.Panel4.Name = "Panel4"
        Me.Panel4.Size = New System.Drawing.Size(3, 50)
        Me.Panel4.TabIndex = 2
        '
        'Panel3
        '
        Me.Panel3.BackColor = System.Drawing.Color.Black
        Me.Panel3.Location = New System.Drawing.Point(15, 0)
        Me.Panel3.Name = "Panel3"
        Me.Panel3.Size = New System.Drawing.Size(3, 50)
        Me.Panel3.TabIndex = 1
        '
        'Panel2
        '
        Me.Panel2.BackColor = System.Drawing.Color.Black
        Me.Panel2.Location = New System.Drawing.Point(0, 31)
        Me.Panel2.Name = "Panel2"
        Me.Panel2.Size = New System.Drawing.Size(50, 3)
        Me.Panel2.TabIndex = 1
        '
        'Panel1
        '
        Me.Panel1.BackColor = System.Drawing.Color.Black
        Me.Panel1.Location = New System.Drawing.Point(0, 15)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(50, 3)
        Me.Panel1.TabIndex = 0
        '
        'FRMoptions
        '
        Me.AcceptButton = Me.BTNok
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(383, 261)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox4)
        Me.Controls.Add(Me.GroupBox3)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.BTNcancel)
        Me.Controls.Add(Me.BTNok)
        Me.Font = New System.Drawing.Font("Segoe UI", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "FRMoptions"
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Options"
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBox4.PerformLayout()
        CType(Me.PictureBox2, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.PNLbgcolor.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents BTNok As System.Windows.Forms.Button
    Friend WithEvents BTNcancel As System.Windows.Forms.Button
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents RADrandom As System.Windows.Forms.RadioButton
    Friend WithEvents RADalternate As System.Windows.Forms.RadioButton
    Friend WithEvents RADloser As System.Windows.Forms.RadioButton
    Friend WithEvents RADwinner As System.Windows.Forms.RadioButton
    Friend WithEvents CHKundo As System.Windows.Forms.CheckBox
    Friend WithEvents CHKredo As System.Windows.Forms.CheckBox
    Friend WithEvents CHKtimed As System.Windows.Forms.CheckBox
    Friend WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Friend WithEvents GroupBox3 As System.Windows.Forms.GroupBox
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents TextBox1 As System.Windows.Forms.TextBox
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents GroupBox4 As System.Windows.Forms.GroupBox
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents PictureBox2 As System.Windows.Forms.PictureBox
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents TextBox2 As System.Windows.Forms.TextBox
    Friend WithEvents GroupBox5 As System.Windows.Forms.GroupBox
    Friend WithEvents BTNbgcolor As System.Windows.Forms.Button
    Friend WithEvents CLDbg As System.Windows.Forms.ColorDialog
    Friend WithEvents PNLbgcolor As System.Windows.Forms.Panel
    Friend WithEvents BTNresetBGcolor As System.Windows.Forms.Button
    Friend WithEvents Panel4 As System.Windows.Forms.Panel
    Friend WithEvents Panel3 As System.Windows.Forms.Panel
    Friend WithEvents Panel2 As System.Windows.Forms.Panel
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
End Class
