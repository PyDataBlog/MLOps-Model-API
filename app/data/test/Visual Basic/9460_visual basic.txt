<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Guess_The_Number_98
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
        Me.components = New System.ComponentModel.Container()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.btnimpossible = New System.Windows.Forms.Button()
        Me.btnveryhard = New System.Windows.Forms.Button()
        Me.btnhard = New System.Windows.Forms.Button()
        Me.btnmedium = New System.Windows.Forms.Button()
        Me.btneasy = New System.Windows.Forms.Button()
        Me.btnveryeasy = New System.Windows.Forms.Button()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.higherorlower = New System.Windows.Forms.Label()
        Me.txtGuessedNumber = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.flasher = New System.Windows.Forms.Timer(Me.components)
        Me.selector = New System.Windows.Forms.Timer(Me.components)
        Me.look = New System.Windows.Forms.Timer(Me.components)
        Me.pullbs = New System.Windows.Forms.Timer(Me.components)
        Me.pullbottom = New System.Windows.Forms.Timer(Me.components)
        Me.pullside = New System.Windows.Forms.Timer(Me.components)
        Me.program = New System.Windows.Forms.Panel()
        Me.programtopbar = New System.Windows.Forms.Panel()
        Me.programname = New System.Windows.Forms.Label()
        Me.maximizebutton = New System.Windows.Forms.PictureBox()
        Me.minimizebutton = New System.Windows.Forms.PictureBox()
        Me.closebutton = New System.Windows.Forms.PictureBox()
        Me.toprightcorner = New System.Windows.Forms.Panel()
        Me.bottomrightcorner = New System.Windows.Forms.Panel()
        Me.bottomleftcorner = New System.Windows.Forms.Panel()
        Me.topleftcorner = New System.Windows.Forms.Panel()
        Me.left = New System.Windows.Forms.Panel()
        Me.bottom = New System.Windows.Forms.Panel()
        Me.right = New System.Windows.Forms.Panel()
        Me.top = New System.Windows.Forms.Panel()
        Me.Panel1.SuspendLayout()
        Me.program.SuspendLayout()
        Me.programtopbar.SuspendLayout()
        CType(Me.maximizebutton, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.minimizebutton, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.closebutton, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Panel1
        '
        Me.Panel1.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Panel1.Controls.Add(Me.btnimpossible)
        Me.Panel1.Controls.Add(Me.btnveryhard)
        Me.Panel1.Controls.Add(Me.btnhard)
        Me.Panel1.Controls.Add(Me.btnmedium)
        Me.Panel1.Controls.Add(Me.btneasy)
        Me.Panel1.Controls.Add(Me.btnveryeasy)
        Me.Panel1.Controls.Add(Me.Label4)
        Me.Panel1.Location = New System.Drawing.Point(4, 22)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(299, 141)
        Me.Panel1.TabIndex = 13
        '
        'btnimpossible
        '
        Me.btnimpossible.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnimpossible.Location = New System.Drawing.Point(217, 102)
        Me.btnimpossible.Name = "btnimpossible"
        Me.btnimpossible.Size = New System.Drawing.Size(74, 32)
        Me.btnimpossible.TabIndex = 6
        Me.btnimpossible.Text = "Impossible"
        Me.btnimpossible.UseVisualStyleBackColor = True
        '
        'btnveryhard
        '
        Me.btnveryhard.Anchor = System.Windows.Forms.AnchorStyles.Bottom
        Me.btnveryhard.Location = New System.Drawing.Point(113, 102)
        Me.btnveryhard.Name = "btnveryhard"
        Me.btnveryhard.Size = New System.Drawing.Size(74, 32)
        Me.btnveryhard.TabIndex = 5
        Me.btnveryhard.Text = "Very Hard"
        Me.btnveryhard.UseVisualStyleBackColor = True
        '
        'btnhard
        '
        Me.btnhard.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.btnhard.Location = New System.Drawing.Point(10, 102)
        Me.btnhard.Name = "btnhard"
        Me.btnhard.Size = New System.Drawing.Size(74, 32)
        Me.btnhard.TabIndex = 4
        Me.btnhard.Text = "Hard"
        Me.btnhard.UseVisualStyleBackColor = True
        '
        'btnmedium
        '
        Me.btnmedium.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnmedium.Location = New System.Drawing.Point(217, 41)
        Me.btnmedium.Name = "btnmedium"
        Me.btnmedium.Size = New System.Drawing.Size(74, 32)
        Me.btnmedium.TabIndex = 3
        Me.btnmedium.Text = "Medium"
        Me.btnmedium.UseVisualStyleBackColor = True
        '
        'btneasy
        '
        Me.btneasy.Anchor = System.Windows.Forms.AnchorStyles.Top
        Me.btneasy.Location = New System.Drawing.Point(113, 41)
        Me.btneasy.Name = "btneasy"
        Me.btneasy.Size = New System.Drawing.Size(74, 32)
        Me.btneasy.TabIndex = 2
        Me.btneasy.Text = "Easy"
        Me.btneasy.UseVisualStyleBackColor = True
        '
        'btnveryeasy
        '
        Me.btnveryeasy.Location = New System.Drawing.Point(10, 41)
        Me.btnveryeasy.Name = "btnveryeasy"
        Me.btnveryeasy.Size = New System.Drawing.Size(74, 32)
        Me.btnveryeasy.TabIndex = 1
        Me.btnveryeasy.Text = "Very Easy"
        Me.btnveryeasy.UseVisualStyleBackColor = True
        '
        'Label4
        '
        Me.Label4.Anchor = System.Windows.Forms.AnchorStyles.Top
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(69, 14)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(156, 13)
        Me.Label4.TabIndex = 0
        Me.Label4.Text = "Please Choose A Difficulty level"
        '
        'Button2
        '
        Me.Button2.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.Button2.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Button2.Location = New System.Drawing.Point(4, 136)
        Me.Button2.Name = "Button2"
        Me.Button2.Size = New System.Drawing.Size(300, 27)
        Me.Button2.TabIndex = 12
        Me.Button2.Text = "Restart"
        Me.Button2.UseVisualStyleBackColor = True
        '
        'Label3
        '
        Me.Label3.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(208, 65)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(89, 13)
        Me.Label3.TabIndex = 11
        Me.Label3.Text = "then press check"
        '
        'Button1
        '
        Me.Button1.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Button1.Location = New System.Drawing.Point(219, 92)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(75, 37)
        Me.Button1.TabIndex = 10
        Me.Button1.Text = "Check"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'higherorlower
        '
        Me.higherorlower.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.higherorlower.AutoSize = True
        Me.higherorlower.Font = New System.Drawing.Font("Microsoft Sans Serif", 11.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.higherorlower.Location = New System.Drawing.Point(52, 93)
        Me.higherorlower.Name = "higherorlower"
        Me.higherorlower.Size = New System.Drawing.Size(78, 18)
        Me.higherorlower.TabIndex = 9
        Me.higherorlower.Text = "Unknown"
        '
        'txtGuessedNumber
        '
        Me.txtGuessedNumber.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.txtGuessedNumber.Location = New System.Drawing.Point(127, 62)
        Me.txtGuessedNumber.Name = "txtGuessedNumber"
        Me.txtGuessedNumber.Size = New System.Drawing.Size(79, 20)
        Me.txtGuessedNumber.TabIndex = 8
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(12, 65)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(112, 13)
        Me.Label2.TabIndex = 7
        Me.Label2.Text = "Type your guess here:"
        '
        'Label1
        '
        Me.Label1.Anchor = System.Windows.Forms.AnchorStyles.Top
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(48, 34)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(200, 13)
        Me.Label1.TabIndex = 6
        Me.Label1.Text = "I am thinking of a number between 1-100"
        '
        'flasher
        '
        '
        'selector
        '
        Me.selector.Interval = 1000
        '
        'look
        '
        '
        'pullbs
        '
        Me.pullbs.Interval = 1
        '
        'pullbottom
        '
        Me.pullbottom.Interval = 1
        '
        'pullside
        '
        Me.pullside.Interval = 1
        '
        'program
        '
        Me.program.BackColor = System.Drawing.Color.Silver
        Me.program.Controls.Add(Me.Panel1)
        Me.program.Controls.Add(Me.programtopbar)
        Me.program.Controls.Add(Me.Button2)
        Me.program.Controls.Add(Me.toprightcorner)
        Me.program.Controls.Add(Me.bottomrightcorner)
        Me.program.Controls.Add(Me.Label3)
        Me.program.Controls.Add(Me.bottomleftcorner)
        Me.program.Controls.Add(Me.Button1)
        Me.program.Controls.Add(Me.topleftcorner)
        Me.program.Controls.Add(Me.txtGuessedNumber)
        Me.program.Controls.Add(Me.left)
        Me.program.Controls.Add(Me.higherorlower)
        Me.program.Controls.Add(Me.bottom)
        Me.program.Controls.Add(Me.Label1)
        Me.program.Controls.Add(Me.Label2)
        Me.program.Controls.Add(Me.right)
        Me.program.Controls.Add(Me.top)
        Me.program.Dock = System.Windows.Forms.DockStyle.Fill
        Me.program.Location = New System.Drawing.Point(0, 0)
        Me.program.Name = "program"
        Me.program.Size = New System.Drawing.Size(308, 167)
        Me.program.TabIndex = 14
        '
        'programtopbar
        '
        Me.programtopbar.BackColor = System.Drawing.Color.DarkBlue
        Me.programtopbar.Controls.Add(Me.maximizebutton)
        Me.programtopbar.Controls.Add(Me.minimizebutton)
        Me.programtopbar.Controls.Add(Me.programname)
        Me.programtopbar.Controls.Add(Me.closebutton)
        Me.programtopbar.Dock = System.Windows.Forms.DockStyle.Top
        Me.programtopbar.Location = New System.Drawing.Point(4, 4)
        Me.programtopbar.Name = "programtopbar"
        Me.programtopbar.Size = New System.Drawing.Size(300, 18)
        Me.programtopbar.TabIndex = 0
        '
        'programname
        '
        Me.programname.AutoSize = True
        Me.programname.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.programname.ForeColor = System.Drawing.Color.White
        Me.programname.Location = New System.Drawing.Point(3, 2)
        Me.programname.Name = "programname"
        Me.programname.Size = New System.Drawing.Size(126, 13)
        Me.programname.TabIndex = 3
        Me.programname.Text = "Guess The Number 2"
        '
        'maximizebutton
        '
        Me.maximizebutton.Anchor = System.Windows.Forms.AnchorStyles.Right
        Me.maximizebutton.Image = Global.Histacom.My.Resources.Resources.Maximize
        Me.maximizebutton.Location = New System.Drawing.Point(266, 2)
        Me.maximizebutton.Name = "maximizebutton"
        Me.maximizebutton.Size = New System.Drawing.Size(16, 14)
        Me.maximizebutton.TabIndex = 6
        Me.maximizebutton.TabStop = False
        '
        'minimizebutton
        '
        Me.minimizebutton.Anchor = System.Windows.Forms.AnchorStyles.Right
        Me.minimizebutton.Image = Global.Histacom.My.Resources.Resources.minimize
        Me.minimizebutton.Location = New System.Drawing.Point(250, 2)
        Me.minimizebutton.Name = "minimizebutton"
        Me.minimizebutton.Size = New System.Drawing.Size(16, 14)
        Me.minimizebutton.TabIndex = 5
        Me.minimizebutton.TabStop = False
        '
        'closebutton
        '
        Me.closebutton.Anchor = System.Windows.Forms.AnchorStyles.Right
        Me.closebutton.Image = Global.Histacom.My.Resources.Resources.close
        Me.closebutton.Location = New System.Drawing.Point(283, 2)
        Me.closebutton.Name = "closebutton"
        Me.closebutton.Size = New System.Drawing.Size(16, 14)
        Me.closebutton.TabIndex = 4
        Me.closebutton.TabStop = False
        '
        'toprightcorner
        '
        Me.toprightcorner.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.toprightcorner.BackgroundImage = Global.Histacom.My.Resources.Resources.windows95toprightcorner
        Me.toprightcorner.Location = New System.Drawing.Point(304, 0)
        Me.toprightcorner.Name = "toprightcorner"
        Me.toprightcorner.Size = New System.Drawing.Size(4, 4)
        Me.toprightcorner.TabIndex = 6
        '
        'bottomrightcorner
        '
        Me.bottomrightcorner.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.bottomrightcorner.BackgroundImage = Global.Histacom.My.Resources.Resources.windows95bottomrightcorner
        Me.bottomrightcorner.Cursor = System.Windows.Forms.Cursors.SizeNWSE
        Me.bottomrightcorner.Location = New System.Drawing.Point(304, 163)
        Me.bottomrightcorner.Name = "bottomrightcorner"
        Me.bottomrightcorner.Size = New System.Drawing.Size(4, 4)
        Me.bottomrightcorner.TabIndex = 4
        '
        'bottomleftcorner
        '
        Me.bottomleftcorner.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.bottomleftcorner.BackgroundImage = Global.Histacom.My.Resources.Resources.windows95bottomleftcorner
        Me.bottomleftcorner.Location = New System.Drawing.Point(0, 163)
        Me.bottomleftcorner.Name = "bottomleftcorner"
        Me.bottomleftcorner.Size = New System.Drawing.Size(4, 4)
        Me.bottomleftcorner.TabIndex = 2
        '
        'topleftcorner
        '
        Me.topleftcorner.BackgroundImage = Global.Histacom.My.Resources.Resources.windows95topleftcorner
        Me.topleftcorner.Location = New System.Drawing.Point(0, 0)
        Me.topleftcorner.Name = "topleftcorner"
        Me.topleftcorner.Size = New System.Drawing.Size(4, 4)
        Me.topleftcorner.TabIndex = 1
        '
        'left
        '
        Me.left.BackgroundImage = Global.Histacom.My.Resources.Resources.windows95leftside
        Me.left.Dock = System.Windows.Forms.DockStyle.Left
        Me.left.Location = New System.Drawing.Point(0, 4)
        Me.left.Name = "left"
        Me.left.Size = New System.Drawing.Size(4, 159)
        Me.left.TabIndex = 3
        '
        'bottom
        '
        Me.bottom.BackgroundImage = Global.Histacom.My.Resources.Resources.windows95bottom
        Me.bottom.Cursor = System.Windows.Forms.Cursors.SizeNS
        Me.bottom.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.bottom.Location = New System.Drawing.Point(0, 163)
        Me.bottom.Name = "bottom"
        Me.bottom.Size = New System.Drawing.Size(304, 4)
        Me.bottom.TabIndex = 5
        '
        'right
        '
        Me.right.BackgroundImage = Global.Histacom.My.Resources.Resources.windows95rightside
        Me.right.Cursor = System.Windows.Forms.Cursors.SizeWE
        Me.right.Dock = System.Windows.Forms.DockStyle.Right
        Me.right.Location = New System.Drawing.Point(304, 4)
        Me.right.Name = "right"
        Me.right.Size = New System.Drawing.Size(4, 163)
        Me.right.TabIndex = 7
        '
        'top
        '
        Me.top.BackgroundImage = Global.Histacom.My.Resources.Resources.windows95top
        Me.top.Dock = System.Windows.Forms.DockStyle.Top
        Me.top.Location = New System.Drawing.Point(0, 0)
        Me.top.Name = "top"
        Me.top.Size = New System.Drawing.Size(308, 4)
        Me.top.TabIndex = 8
        '
        'Guess_The_Number_98
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(308, 167)
        Me.Controls.Add(Me.program)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
        Me.Name = "Guess_The_Number_98"
        Me.Text = "Guess_The_Number_98"
        Me.TopMost = True
        Me.Panel1.ResumeLayout(False)
        Me.Panel1.PerformLayout()
        Me.program.ResumeLayout(False)
        Me.program.PerformLayout()
        Me.programtopbar.ResumeLayout(False)
        Me.programtopbar.PerformLayout()
        CType(Me.maximizebutton, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.minimizebutton, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.closebutton, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents Button2 As System.Windows.Forms.Button
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents higherorlower As System.Windows.Forms.Label
    Friend WithEvents txtGuessedNumber As System.Windows.Forms.TextBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents btnimpossible As System.Windows.Forms.Button
    Friend WithEvents btnveryhard As System.Windows.Forms.Button
    Friend WithEvents btnhard As System.Windows.Forms.Button
    Friend WithEvents btnmedium As System.Windows.Forms.Button
    Friend WithEvents btneasy As System.Windows.Forms.Button
    Friend WithEvents btnveryeasy As System.Windows.Forms.Button
    Friend WithEvents flasher As System.Windows.Forms.Timer
    Friend WithEvents selector As System.Windows.Forms.Timer
    Friend WithEvents look As System.Windows.Forms.Timer
    Friend WithEvents pullbs As System.Windows.Forms.Timer
    Friend WithEvents pullbottom As System.Windows.Forms.Timer
    Friend WithEvents pullside As System.Windows.Forms.Timer
    Friend WithEvents program As System.Windows.Forms.Panel
    Friend WithEvents programtopbar As System.Windows.Forms.Panel
    Friend WithEvents maximizebutton As System.Windows.Forms.PictureBox
    Friend WithEvents minimizebutton As System.Windows.Forms.PictureBox
    Friend WithEvents programname As System.Windows.Forms.Label
    Friend WithEvents closebutton As System.Windows.Forms.PictureBox
    Friend WithEvents toprightcorner As System.Windows.Forms.Panel
    Friend WithEvents bottomrightcorner As System.Windows.Forms.Panel
    Friend WithEvents bottomleftcorner As System.Windows.Forms.Panel
    Friend WithEvents topleftcorner As System.Windows.Forms.Panel
    Friend WithEvents left As System.Windows.Forms.Panel
    Friend WithEvents bottom As System.Windows.Forms.Panel
    Friend WithEvents right As System.Windows.Forms.Panel
    Friend WithEvents top As System.Windows.Forms.Panel
End Class
