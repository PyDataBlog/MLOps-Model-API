<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class BMI_Rechner
    Inherits System.Windows.Forms.Form

    'Das Formular überschreibt den Löschvorgang, um die Komponentenliste zu bereinigen.
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

    'Wird vom Windows Form-Designer benötigt.
    Private components As System.ComponentModel.IContainer

    'Hinweis: Die folgende Prozedur ist für den Windows Form-Designer erforderlich.
    'Das Bearbeiten ist mit dem Windows Form-Designer möglich.  
    'Das Bearbeiten mit dem Code-Editor ist nicht möglich.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(BMI_Rechner))
        Me.cmd_End = New System.Windows.Forms.Button
        Me.chk_M = New System.Windows.Forms.CheckBox
        Me.chk_W = New System.Windows.Forms.CheckBox
        Me.Label1 = New System.Windows.Forms.Label
        Me.Label2 = New System.Windows.Forms.Label
        Me.Label3 = New System.Windows.Forms.Label
        Me.Label4 = New System.Windows.Forms.Label
        Me.txt_Eingabe1 = New System.Windows.Forms.TextBox
        Me.txt_Eingabe2 = New System.Windows.Forms.TextBox
        Me.txt_Ergebnis = New System.Windows.Forms.TextBox
        Me.txt_Klass = New System.Windows.Forms.TextBox
        Me.cmd_clear = New System.Windows.Forms.Button
        Me.cmd_Berechnen = New System.Windows.Forms.Button
        Me.PictureBox1 = New System.Windows.Forms.PictureBox
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'cmd_End
        '
        Me.cmd_End.Location = New System.Drawing.Point(404, 186)
        Me.cmd_End.Name = "cmd_End"
        Me.cmd_End.Size = New System.Drawing.Size(75, 23)
        Me.cmd_End.TabIndex = 0
        Me.cmd_End.Text = "Beenden"
        Me.cmd_End.UseVisualStyleBackColor = True
        '
        'chk_M
        '
        Me.chk_M.AutoSize = True
        Me.chk_M.Location = New System.Drawing.Point(34, 34)
        Me.chk_M.Name = "chk_M"
        Me.chk_M.Size = New System.Drawing.Size(69, 17)
        Me.chk_M.TabIndex = 1
        Me.chk_M.Text = "Männlich"
        Me.chk_M.UseVisualStyleBackColor = True
        '
        'chk_W
        '
        Me.chk_W.AutoSize = True
        Me.chk_W.Location = New System.Drawing.Point(109, 34)
        Me.chk_W.Name = "chk_W"
        Me.chk_W.Size = New System.Drawing.Size(67, 17)
        Me.chk_W.TabIndex = 2
        Me.chk_W.Text = "Weiblich"
        Me.chk_W.UseVisualStyleBackColor = True
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(12, 86)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(72, 13)
        Me.Label1.TabIndex = 3
        Me.Label1.Text = "Gewicht in kg"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(12, 118)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(64, 13)
        Me.Label2.TabIndex = 4
        Me.Label2.Text = "Größe in cm"
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(205, 86)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(26, 13)
        Me.Label3.TabIndex = 5
        Me.Label3.Text = "BMI"
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(205, 118)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(68, 13)
        Me.Label4.TabIndex = 6
        Me.Label4.Text = "Klassifikation"
        '
        'txt_Eingabe1
        '
        Me.txt_Eingabe1.Location = New System.Drawing.Point(99, 86)
        Me.txt_Eingabe1.Name = "txt_Eingabe1"
        Me.txt_Eingabe1.Size = New System.Drawing.Size(100, 20)
        Me.txt_Eingabe1.TabIndex = 7
        '
        'txt_Eingabe2
        '
        Me.txt_Eingabe2.Location = New System.Drawing.Point(99, 115)
        Me.txt_Eingabe2.Name = "txt_Eingabe2"
        Me.txt_Eingabe2.Size = New System.Drawing.Size(100, 20)
        Me.txt_Eingabe2.TabIndex = 8
        '
        'txt_Ergebnis
        '
        Me.txt_Ergebnis.Location = New System.Drawing.Point(287, 83)
        Me.txt_Ergebnis.Name = "txt_Ergebnis"
        Me.txt_Ergebnis.Size = New System.Drawing.Size(100, 20)
        Me.txt_Ergebnis.TabIndex = 9
        '
        'txt_Klass
        '
        Me.txt_Klass.Location = New System.Drawing.Point(287, 115)
        Me.txt_Klass.Name = "txt_Klass"
        Me.txt_Klass.Size = New System.Drawing.Size(100, 20)
        Me.txt_Klass.TabIndex = 10
        '
        'cmd_clear
        '
        Me.cmd_clear.Location = New System.Drawing.Point(278, 186)
        Me.cmd_clear.Name = "cmd_clear"
        Me.cmd_clear.Size = New System.Drawing.Size(89, 23)
        Me.cmd_clear.TabIndex = 11
        Me.cmd_clear.Text = "Neue Eingabe"
        Me.cmd_clear.UseVisualStyleBackColor = True
        '
        'cmd_Berechnen
        '
        Me.cmd_Berechnen.Location = New System.Drawing.Point(178, 186)
        Me.cmd_Berechnen.Name = "cmd_Berechnen"
        Me.cmd_Berechnen.Size = New System.Drawing.Size(75, 23)
        Me.cmd_Berechnen.TabIndex = 12
        Me.cmd_Berechnen.Text = "Berechnen"
        Me.cmd_Berechnen.UseVisualStyleBackColor = True
        '
        'PictureBox1
        '
        Me.PictureBox1.Image = CType(resources.GetObject("PictureBox1.Image"), System.Drawing.Image)
        Me.PictureBox1.Location = New System.Drawing.Point(16, 258)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(633, 237)
        Me.PictureBox1.TabIndex = 13
        Me.PictureBox1.TabStop = False
        '
        'BMI_Rechner
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(661, 507)
        Me.Controls.Add(Me.PictureBox1)
        Me.Controls.Add(Me.cmd_Berechnen)
        Me.Controls.Add(Me.cmd_clear)
        Me.Controls.Add(Me.txt_Klass)
        Me.Controls.Add(Me.txt_Ergebnis)
        Me.Controls.Add(Me.txt_Eingabe2)
        Me.Controls.Add(Me.txt_Eingabe1)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.chk_W)
        Me.Controls.Add(Me.chk_M)
        Me.Controls.Add(Me.cmd_End)
        Me.Name = "BMI_Rechner"
        Me.Text = "BMI_Rechner"
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents cmd_End As System.Windows.Forms.Button
    Friend WithEvents chk_M As System.Windows.Forms.CheckBox
    Friend WithEvents chk_W As System.Windows.Forms.CheckBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents txt_Eingabe1 As System.Windows.Forms.TextBox
    Friend WithEvents txt_Eingabe2 As System.Windows.Forms.TextBox
    Friend WithEvents txt_Ergebnis As System.Windows.Forms.TextBox
    Friend WithEvents txt_Klass As System.Windows.Forms.TextBox
    Friend WithEvents cmd_clear As System.Windows.Forms.Button
    Friend WithEvents cmd_Berechnen As System.Windows.Forms.Button
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
End Class
