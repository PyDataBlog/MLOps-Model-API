<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class screenSelect
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
        Me.lblSquare = New System.Windows.Forms.Label()
        Me.lblTriangle = New System.Windows.Forms.Label()
        Me.lblRectangle = New System.Windows.Forms.Label()
        Me.lblPentagon = New System.Windows.Forms.Label()
        Me.lblHexagon = New System.Windows.Forms.Label()
        Me.lblCircle = New System.Windows.Forms.Label()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.picPentagon = New System.Windows.Forms.PictureBox()
        Me.picRectangle = New System.Windows.Forms.PictureBox()
        Me.picHexagon = New System.Windows.Forms.PictureBox()
        Me.picCorner = New System.Windows.Forms.PictureBox()
        Me.picSquare = New System.Windows.Forms.PictureBox()
        Me.picTriangle = New System.Windows.Forms.PictureBox()
        Me.picCircle = New System.Windows.Forms.PictureBox()
        Me.PictureBox2 = New System.Windows.Forms.PictureBox()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.picPentagon, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.picRectangle, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.picHexagon, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.picCorner, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.picSquare, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.picTriangle, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.picCircle, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PictureBox2, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'lblSquare
        '
        Me.lblSquare.AutoSize = True
        Me.lblSquare.BackColor = System.Drawing.Color.Transparent
        Me.lblSquare.Font = New System.Drawing.Font("Microsoft Sans Serif", 24.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblSquare.Location = New System.Drawing.Point(524, 156)
        Me.lblSquare.Name = "lblSquare"
        Me.lblSquare.Size = New System.Drawing.Size(122, 38)
        Me.lblSquare.TabIndex = 5
        Me.lblSquare.Text = "Square"
        '
        'lblTriangle
        '
        Me.lblTriangle.AutoSize = True
        Me.lblTriangle.BackColor = System.Drawing.Color.Transparent
        Me.lblTriangle.Font = New System.Drawing.Font("Microsoft Sans Serif", 24.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblTriangle.Location = New System.Drawing.Point(780, 156)
        Me.lblTriangle.Name = "lblTriangle"
        Me.lblTriangle.Size = New System.Drawing.Size(134, 38)
        Me.lblTriangle.TabIndex = 6
        Me.lblTriangle.Text = "Triangle"
        '
        'lblRectangle
        '
        Me.lblRectangle.AutoSize = True
        Me.lblRectangle.BackColor = System.Drawing.Color.Transparent
        Me.lblRectangle.Font = New System.Drawing.Font("Microsoft Sans Serif", 24.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblRectangle.Location = New System.Drawing.Point(767, 410)
        Me.lblRectangle.Name = "lblRectangle"
        Me.lblRectangle.Size = New System.Drawing.Size(164, 38)
        Me.lblRectangle.TabIndex = 12
        Me.lblRectangle.Text = "Rectangle"
        '
        'lblPentagon
        '
        Me.lblPentagon.AutoSize = True
        Me.lblPentagon.BackColor = System.Drawing.Color.Transparent
        Me.lblPentagon.Font = New System.Drawing.Font("Microsoft Sans Serif", 24.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblPentagon.Location = New System.Drawing.Point(503, 410)
        Me.lblPentagon.Name = "lblPentagon"
        Me.lblPentagon.Size = New System.Drawing.Size(156, 38)
        Me.lblPentagon.TabIndex = 11
        Me.lblPentagon.Text = "Pentagon"
        '
        'lblHexagon
        '
        Me.lblHexagon.AutoSize = True
        Me.lblHexagon.BackColor = System.Drawing.Color.Transparent
        Me.lblHexagon.Font = New System.Drawing.Font("Microsoft Sans Serif", 24.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblHexagon.Location = New System.Drawing.Point(182, 410)
        Me.lblHexagon.Name = "lblHexagon"
        Me.lblHexagon.Size = New System.Drawing.Size(148, 38)
        Me.lblHexagon.TabIndex = 10
        Me.lblHexagon.Text = "Hexagon"
        '
        'lblCircle
        '
        Me.lblCircle.AutoSize = True
        Me.lblCircle.BackColor = System.Drawing.Color.Transparent
        Me.lblCircle.Font = New System.Drawing.Font("Microsoft Sans Serif", 24.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblCircle.Location = New System.Drawing.Point(205, 156)
        Me.lblCircle.Name = "lblCircle"
        Me.lblCircle.Size = New System.Drawing.Size(101, 38)
        Me.lblCircle.TabIndex = 4
        Me.lblCircle.Text = "Circle"
        '
        'PictureBox1
        '
        Me.PictureBox1.Image = Global.BatMath.My.Resources.Resources.HomeIcon
        Me.PictureBox1.Location = New System.Drawing.Point(949, 12)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(63, 63)
        Me.PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage
        Me.PictureBox1.TabIndex = 14
        Me.PictureBox1.TabStop = False
        '
        'picPentagon
        '
        Me.picPentagon.BackColor = System.Drawing.Color.Transparent
        Me.picPentagon.Image = Global.BatMath.My.Resources.Resources.pentagon
        Me.picPentagon.Location = New System.Drawing.Point(510, 356)
        Me.picPentagon.Name = "picPentagon"
        Me.picPentagon.Size = New System.Drawing.Size(150, 150)
        Me.picPentagon.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage
        Me.picPentagon.TabIndex = 8
        Me.picPentagon.TabStop = False
        '
        'picRectangle
        '
        Me.picRectangle.BackColor = System.Drawing.Color.Transparent
        Me.picRectangle.Image = Global.BatMath.My.Resources.Resources.rectangle
        Me.picRectangle.Location = New System.Drawing.Point(774, 386)
        Me.picRectangle.Name = "picRectangle"
        Me.picRectangle.Size = New System.Drawing.Size(150, 100)
        Me.picRectangle.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage
        Me.picRectangle.TabIndex = 7
        Me.picRectangle.TabStop = False
        '
        'picHexagon
        '
        Me.picHexagon.BackColor = System.Drawing.Color.Transparent
        Me.picHexagon.Image = Global.BatMath.My.Resources.Resources.hexagon
        Me.picHexagon.Location = New System.Drawing.Point(180, 356)
        Me.picHexagon.Name = "picHexagon"
        Me.picHexagon.Size = New System.Drawing.Size(150, 150)
        Me.picHexagon.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage
        Me.picHexagon.TabIndex = 9
        Me.picHexagon.TabStop = False
        '
        'picCorner
        '
        Me.picCorner.Image = Global.BatMath.My.Resources.Resources.atman
        Me.picCorner.Location = New System.Drawing.Point(0, 316)
        Me.picCorner.Name = "picCorner"
        Me.picCorner.Size = New System.Drawing.Size(380, 260)
        Me.picCorner.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage
        Me.picCorner.TabIndex = 3
        Me.picCorner.TabStop = False
        '
        'picSquare
        '
        Me.picSquare.BackColor = System.Drawing.Color.Transparent
        Me.picSquare.Image = Global.BatMath.My.Resources.Resources.square
        Me.picSquare.Location = New System.Drawing.Point(510, 100)
        Me.picSquare.Name = "picSquare"
        Me.picSquare.Size = New System.Drawing.Size(150, 150)
        Me.picSquare.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage
        Me.picSquare.TabIndex = 1
        Me.picSquare.TabStop = False
        '
        'picTriangle
        '
        Me.picTriangle.BackColor = System.Drawing.Color.Transparent
        Me.picTriangle.Image = Global.BatMath.My.Resources.Resources.triangle
        Me.picTriangle.Location = New System.Drawing.Point(774, 100)
        Me.picTriangle.Name = "picTriangle"
        Me.picTriangle.Size = New System.Drawing.Size(150, 150)
        Me.picTriangle.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage
        Me.picTriangle.TabIndex = 0
        Me.picTriangle.TabStop = False
        '
        'picCircle
        '
        Me.picCircle.BackColor = System.Drawing.Color.Transparent
        Me.picCircle.Image = Global.BatMath.My.Resources.Resources.circle
        Me.picCircle.Location = New System.Drawing.Point(180, 100)
        Me.picCircle.Name = "picCircle"
        Me.picCircle.Size = New System.Drawing.Size(150, 150)
        Me.picCircle.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage
        Me.picCircle.TabIndex = 13
        Me.picCircle.TabStop = False
        '
        'PictureBox2
        '
        Me.PictureBox2.Image = Global.BatMath.My.Resources.Resources.iconsineed_cross_24_128
        Me.PictureBox2.Location = New System.Drawing.Point(880, 12)
        Me.PictureBox2.Name = "PictureBox2"
        Me.PictureBox2.Size = New System.Drawing.Size(63, 63)
        Me.PictureBox2.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage
        Me.PictureBox2.TabIndex = 15
        Me.PictureBox2.TabStop = False
        '
        'screenSelect
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.FromArgb(CType(CType(54, Byte), Integer), CType(CType(69, Byte), Integer), CType(CType(79, Byte), Integer))
        Me.ClientSize = New System.Drawing.Size(1024, 576)
        Me.Controls.Add(Me.PictureBox2)
        Me.Controls.Add(Me.PictureBox1)
        Me.Controls.Add(Me.lblRectangle)
        Me.Controls.Add(Me.lblPentagon)
        Me.Controls.Add(Me.lblHexagon)
        Me.Controls.Add(Me.lblTriangle)
        Me.Controls.Add(Me.lblSquare)
        Me.Controls.Add(Me.lblCircle)
        Me.Controls.Add(Me.picPentagon)
        Me.Controls.Add(Me.picRectangle)
        Me.Controls.Add(Me.picHexagon)
        Me.Controls.Add(Me.picCorner)
        Me.Controls.Add(Me.picSquare)
        Me.Controls.Add(Me.picTriangle)
        Me.Controls.Add(Me.picCircle)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
        Me.Name = "screenSelect"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "screenSelect"
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.picPentagon, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.picRectangle, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.picHexagon, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.picCorner, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.picSquare, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.picTriangle, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.picCircle, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PictureBox2, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents picTriangle As System.Windows.Forms.PictureBox
    Friend WithEvents picSquare As System.Windows.Forms.PictureBox
    Friend WithEvents picCorner As System.Windows.Forms.PictureBox
    Friend WithEvents lblSquare As System.Windows.Forms.Label
    Friend WithEvents lblTriangle As System.Windows.Forms.Label
    Friend WithEvents lblRectangle As System.Windows.Forms.Label
    Friend WithEvents lblPentagon As System.Windows.Forms.Label
    Friend WithEvents lblHexagon As System.Windows.Forms.Label
    Friend WithEvents picPentagon As System.Windows.Forms.PictureBox
    Friend WithEvents picRectangle As System.Windows.Forms.PictureBox
    Friend WithEvents picHexagon As System.Windows.Forms.PictureBox
    Friend WithEvents lblCircle As System.Windows.Forms.Label
    Friend WithEvents picCircle As System.Windows.Forms.PictureBox
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents PictureBox2 As System.Windows.Forms.PictureBox
End Class
