<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class ÖhmischerRechner
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
        Me.cmd_Teilen2 = New System.Windows.Forms.Button()
        Me.cmd_Teilen1 = New System.Windows.Forms.Button()
        Me.cmd_Mal = New System.Windows.Forms.Button()
        Me.TextBox3 = New System.Windows.Forms.TextBox()
        Me.TextBox2 = New System.Windows.Forms.TextBox()
        Me.TextBox1 = New System.Windows.Forms.TextBox()
        Me.txt_Ergebnis = New System.Windows.Forms.TextBox()
        Me.SpannungTEXT = New System.Windows.Forms.TextBox()
        Me.StromstärkeTEXT = New System.Windows.Forms.TextBox()
        Me.WiderstandTEXT = New System.Windows.Forms.TextBox()
        Me.cmd_Hauptmenü = New System.Windows.Forms.Button()
        Me.cmd_Clear = New System.Windows.Forms.Button()
        Me.cmd_Beenden = New System.Windows.Forms.Button()
        Me.cmd_Stromstärke = New System.Windows.Forms.Button()
        Me.cmd_Spannung = New System.Windows.Forms.Button()
        Me.cmd_Widerstand = New System.Windows.Forms.Button()
        Me.txt_Spannung = New System.Windows.Forms.TextBox()
        Me.txt_Stromstärke = New System.Windows.Forms.TextBox()
        Me.txt_Widerstand = New System.Windows.Forms.TextBox()
        Me.lbl_Widerstand = New System.Windows.Forms.Label()
        Me.lbl_Spannung = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.SuspendLayout()
        '
        'cmd_Teilen2
        '
        Me.cmd_Teilen2.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.cmd_Teilen2.Location = New System.Drawing.Point(12, 230)
        Me.cmd_Teilen2.Name = "cmd_Teilen2"
        Me.cmd_Teilen2.Size = New System.Drawing.Size(86, 30)
        Me.cmd_Teilen2.TabIndex = 39
        Me.cmd_Teilen2.Text = "Ausrechnen"
        Me.cmd_Teilen2.UseVisualStyleBackColor = True
        '
        'cmd_Teilen1
        '
        Me.cmd_Teilen1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.cmd_Teilen1.Location = New System.Drawing.Point(12, 230)
        Me.cmd_Teilen1.Name = "cmd_Teilen1"
        Me.cmd_Teilen1.Size = New System.Drawing.Size(86, 30)
        Me.cmd_Teilen1.TabIndex = 38
        Me.cmd_Teilen1.Text = "Ausrechnen"
        Me.cmd_Teilen1.UseVisualStyleBackColor = True
        '
        'cmd_Mal
        '
        Me.cmd_Mal.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.cmd_Mal.Location = New System.Drawing.Point(12, 230)
        Me.cmd_Mal.Name = "cmd_Mal"
        Me.cmd_Mal.Size = New System.Drawing.Size(86, 30)
        Me.cmd_Mal.TabIndex = 37
        Me.cmd_Mal.Text = "Ausrechnen"
        Me.cmd_Mal.UseVisualStyleBackColor = True
        '
        'TextBox3
        '
        Me.TextBox3.Location = New System.Drawing.Point(12, 72)
        Me.TextBox3.Name = "TextBox3"
        Me.TextBox3.Size = New System.Drawing.Size(100, 20)
        Me.TextBox3.TabIndex = 36
        Me.TextBox3.Text = "Stromstärke=U/R"
        '
        'TextBox2
        '
        Me.TextBox2.Location = New System.Drawing.Point(12, 72)
        Me.TextBox2.Name = "TextBox2"
        Me.TextBox2.Size = New System.Drawing.Size(100, 20)
        Me.TextBox2.TabIndex = 35
        Me.TextBox2.Text = "Spannung=I*R"
        '
        'TextBox1
        '
        Me.TextBox1.Location = New System.Drawing.Point(12, 72)
        Me.TextBox1.Name = "TextBox1"
        Me.TextBox1.Size = New System.Drawing.Size(100, 20)
        Me.TextBox1.TabIndex = 34
        Me.TextBox1.Text = "Widerstand=U/I"
        '
        'txt_Ergebnis
        '
        Me.txt_Ergebnis.BackColor = System.Drawing.Color.Lime
        Me.txt_Ergebnis.Location = New System.Drawing.Point(12, 204)
        Me.txt_Ergebnis.Name = "txt_Ergebnis"
        Me.txt_Ergebnis.ReadOnly = True
        Me.txt_Ergebnis.Size = New System.Drawing.Size(100, 20)
        Me.txt_Ergebnis.TabIndex = 33
        '
        'SpannungTEXT
        '
        Me.SpannungTEXT.Location = New System.Drawing.Point(133, 124)
        Me.SpannungTEXT.Name = "SpannungTEXT"
        Me.SpannungTEXT.Size = New System.Drawing.Size(65, 20)
        Me.SpannungTEXT.TabIndex = 32
        Me.SpannungTEXT.Text = "Spannung"
        '
        'StromstärkeTEXT
        '
        Me.StromstärkeTEXT.Location = New System.Drawing.Point(133, 150)
        Me.StromstärkeTEXT.Name = "StromstärkeTEXT"
        Me.StromstärkeTEXT.Size = New System.Drawing.Size(65, 20)
        Me.StromstärkeTEXT.TabIndex = 31
        Me.StromstärkeTEXT.Text = "Stromstärke"
        '
        'WiderstandTEXT
        '
        Me.WiderstandTEXT.Location = New System.Drawing.Point(133, 98)
        Me.WiderstandTEXT.Name = "WiderstandTEXT"
        Me.WiderstandTEXT.Size = New System.Drawing.Size(65, 20)
        Me.WiderstandTEXT.TabIndex = 30
        Me.WiderstandTEXT.Text = "Widerstand"
        '
        'cmd_Hauptmenü
        '
        Me.cmd_Hauptmenü.Location = New System.Drawing.Point(246, 12)
        Me.cmd_Hauptmenü.Name = "cmd_Hauptmenü"
        Me.cmd_Hauptmenü.Size = New System.Drawing.Size(75, 42)
        Me.cmd_Hauptmenü.TabIndex = 29
        Me.cmd_Hauptmenü.Text = "Zurück zur Auswahl"
        Me.cmd_Hauptmenü.UseVisualStyleBackColor = True
        '
        'cmd_Clear
        '
        Me.cmd_Clear.Location = New System.Drawing.Point(104, 230)
        Me.cmd_Clear.Name = "cmd_Clear"
        Me.cmd_Clear.Size = New System.Drawing.Size(80, 30)
        Me.cmd_Clear.TabIndex = 28
        Me.cmd_Clear.Text = "Clear"
        Me.cmd_Clear.UseVisualStyleBackColor = True
        '
        'cmd_Beenden
        '
        Me.cmd_Beenden.Location = New System.Drawing.Point(244, 219)
        Me.cmd_Beenden.Name = "cmd_Beenden"
        Me.cmd_Beenden.Size = New System.Drawing.Size(75, 38)
        Me.cmd_Beenden.TabIndex = 27
        Me.cmd_Beenden.Text = "End"
        Me.cmd_Beenden.UseVisualStyleBackColor = True
        '
        'cmd_Stromstärke
        '
        Me.cmd_Stromstärke.BackColor = System.Drawing.Color.FromArgb(CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(255, Byte), Integer))
        Me.cmd_Stromstärke.Location = New System.Drawing.Point(168, 12)
        Me.cmd_Stromstärke.Name = "cmd_Stromstärke"
        Me.cmd_Stromstärke.Size = New System.Drawing.Size(72, 38)
        Me.cmd_Stromstärke.TabIndex = 26
        Me.cmd_Stromstärke.Text = "Stromstärke I"
        Me.cmd_Stromstärke.UseVisualStyleBackColor = False
        '
        'cmd_Spannung
        '
        Me.cmd_Spannung.BackColor = System.Drawing.Color.FromArgb(CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(255, Byte), Integer))
        Me.cmd_Spannung.Location = New System.Drawing.Point(90, 12)
        Me.cmd_Spannung.Name = "cmd_Spannung"
        Me.cmd_Spannung.Size = New System.Drawing.Size(72, 38)
        Me.cmd_Spannung.TabIndex = 25
        Me.cmd_Spannung.Text = "Spannung U"
        Me.cmd_Spannung.UseVisualStyleBackColor = False
        '
        'cmd_Widerstand
        '
        Me.cmd_Widerstand.BackColor = System.Drawing.Color.FromArgb(CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(255, Byte), Integer))
        Me.cmd_Widerstand.Location = New System.Drawing.Point(12, 12)
        Me.cmd_Widerstand.Name = "cmd_Widerstand"
        Me.cmd_Widerstand.Size = New System.Drawing.Size(72, 38)
        Me.cmd_Widerstand.TabIndex = 24
        Me.cmd_Widerstand.Text = "Widerstand R"
        Me.cmd_Widerstand.UseVisualStyleBackColor = False
        '
        'txt_Spannung
        '
        Me.txt_Spannung.BackColor = System.Drawing.SystemColors.MenuHighlight
        Me.txt_Spannung.Location = New System.Drawing.Point(12, 124)
        Me.txt_Spannung.Name = "txt_Spannung"
        Me.txt_Spannung.Size = New System.Drawing.Size(115, 20)
        Me.txt_Spannung.TabIndex = 23
        '
        'txt_Stromstärke
        '
        Me.txt_Stromstärke.BackColor = System.Drawing.SystemColors.MenuHighlight
        Me.txt_Stromstärke.Location = New System.Drawing.Point(12, 150)
        Me.txt_Stromstärke.Name = "txt_Stromstärke"
        Me.txt_Stromstärke.Size = New System.Drawing.Size(115, 20)
        Me.txt_Stromstärke.TabIndex = 22
        '
        'txt_Widerstand
        '
        Me.txt_Widerstand.BackColor = System.Drawing.SystemColors.MenuHighlight
        Me.txt_Widerstand.Location = New System.Drawing.Point(12, 98)
        Me.txt_Widerstand.Name = "txt_Widerstand"
        Me.txt_Widerstand.Size = New System.Drawing.Size(115, 20)
        Me.txt_Widerstand.TabIndex = 21
        '
        'lbl_Widerstand
        '
        Me.lbl_Widerstand.AutoSize = True
        Me.lbl_Widerstand.Location = New System.Drawing.Point(216, 103)
        Me.lbl_Widerstand.Name = "lbl_Widerstand"
        Me.lbl_Widerstand.Size = New System.Drawing.Size(61, 13)
        Me.lbl_Widerstand.TabIndex = 40
        Me.lbl_Widerstand.Text = "Widerstand"
        '
        'lbl_Spannung
        '
        Me.lbl_Spannung.AutoSize = True
        Me.lbl_Spannung.Location = New System.Drawing.Point(222, 123)
        Me.lbl_Spannung.Name = "lbl_Spannung"
        Me.lbl_Spannung.Size = New System.Drawing.Size(56, 13)
        Me.lbl_Spannung.TabIndex = 41
        Me.lbl_Spannung.Text = "Spannung"
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(217, 161)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(39, 13)
        Me.Label3.TabIndex = 42
        Me.Label3.Text = "Label3"
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(127, 77)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(90, 13)
        Me.Label4.TabIndex = 43
        Me.Label4.Text = "Stromstärke=U/R"
        '
        'ÖhmischerRechner
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(331, 269)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.lbl_Spannung)
        Me.Controls.Add(Me.lbl_Widerstand)
        Me.Controls.Add(Me.cmd_Teilen2)
        Me.Controls.Add(Me.cmd_Teilen1)
        Me.Controls.Add(Me.cmd_Mal)
        Me.Controls.Add(Me.TextBox3)
        Me.Controls.Add(Me.TextBox2)
        Me.Controls.Add(Me.TextBox1)
        Me.Controls.Add(Me.txt_Ergebnis)
        Me.Controls.Add(Me.SpannungTEXT)
        Me.Controls.Add(Me.StromstärkeTEXT)
        Me.Controls.Add(Me.WiderstandTEXT)
        Me.Controls.Add(Me.cmd_Hauptmenü)
        Me.Controls.Add(Me.cmd_Clear)
        Me.Controls.Add(Me.cmd_Beenden)
        Me.Controls.Add(Me.cmd_Stromstärke)
        Me.Controls.Add(Me.cmd_Spannung)
        Me.Controls.Add(Me.cmd_Widerstand)
        Me.Controls.Add(Me.txt_Spannung)
        Me.Controls.Add(Me.txt_Stromstärke)
        Me.Controls.Add(Me.txt_Widerstand)
        Me.Name = "ÖhmischerRechner"
        Me.Text = "Öhmischer Rechner"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents cmd_Teilen2 As System.Windows.Forms.Button
    Friend WithEvents cmd_Teilen1 As System.Windows.Forms.Button
    Friend WithEvents cmd_Mal As System.Windows.Forms.Button
    Friend WithEvents TextBox3 As System.Windows.Forms.TextBox
    Friend WithEvents TextBox2 As System.Windows.Forms.TextBox
    Friend WithEvents TextBox1 As System.Windows.Forms.TextBox
    Friend WithEvents txt_Ergebnis As System.Windows.Forms.TextBox
    Friend WithEvents SpannungTEXT As System.Windows.Forms.TextBox
    Friend WithEvents StromstärkeTEXT As System.Windows.Forms.TextBox
    Friend WithEvents WiderstandTEXT As System.Windows.Forms.TextBox
    Friend WithEvents cmd_Hauptmenü As System.Windows.Forms.Button
    Friend WithEvents cmd_Clear As System.Windows.Forms.Button
    Friend WithEvents cmd_Beenden As System.Windows.Forms.Button
    Friend WithEvents cmd_Stromstärke As System.Windows.Forms.Button
    Friend WithEvents cmd_Spannung As System.Windows.Forms.Button
    Friend WithEvents cmd_Widerstand As System.Windows.Forms.Button
    Friend WithEvents txt_Spannung As System.Windows.Forms.TextBox
    Friend WithEvents txt_Stromstärke As System.Windows.Forms.TextBox
    Friend WithEvents txt_Widerstand As System.Windows.Forms.TextBox
    Friend WithEvents lbl_Widerstand As System.Windows.Forms.Label
    Friend WithEvents lbl_Spannung As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents Label4 As System.Windows.Forms.Label
End Class
