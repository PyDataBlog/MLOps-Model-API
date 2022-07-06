<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Zahlenraten
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
        Me.cmd_Neu = New System.Windows.Forms.Button()
        Me.cmd_Ende = New System.Windows.Forms.Button()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.lbl_Zähler = New System.Windows.Forms.Label()
        Me.txt_Eingabe = New System.Windows.Forms.TextBox()
        Me.lbl_Überprüfung = New System.Windows.Forms.Label()
        Me.cmd_prüfen = New System.Windows.Forms.Button()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.SuspendLayout()
        '
        'cmd_Neu
        '
        Me.cmd_Neu.Location = New System.Drawing.Point(15, 166)
        Me.cmd_Neu.Name = "cmd_Neu"
        Me.cmd_Neu.Size = New System.Drawing.Size(75, 43)
        Me.cmd_Neu.TabIndex = 0
        Me.cmd_Neu.Text = "Neu"
        Me.cmd_Neu.UseVisualStyleBackColor = True
        '
        'cmd_Ende
        '
        Me.cmd_Ende.Location = New System.Drawing.Point(142, 166)
        Me.cmd_Ende.Name = "cmd_Ende"
        Me.cmd_Ende.Size = New System.Drawing.Size(75, 43)
        Me.cmd_Ende.TabIndex = 1
        Me.cmd_Ende.Text = "Beenden"
        Me.cmd_Ende.UseVisualStyleBackColor = True
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label5.Location = New System.Drawing.Point(139, 9)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(68, 16)
        Me.Label5.TabIndex = 2
        Me.Label5.Text = "Versuche:"
        '
        'lbl_Zähler
        '
        Me.lbl_Zähler.AutoSize = True
        Me.lbl_Zähler.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lbl_Zähler.Location = New System.Drawing.Point(139, 25)
        Me.lbl_Zähler.Name = "lbl_Zähler"
        Me.lbl_Zähler.Size = New System.Drawing.Size(0, 16)
        Me.lbl_Zähler.TabIndex = 3
        '
        'txt_Eingabe
        '
        Me.txt_Eingabe.Location = New System.Drawing.Point(15, 28)
        Me.txt_Eingabe.Name = "txt_Eingabe"
        Me.txt_Eingabe.Size = New System.Drawing.Size(88, 20)
        Me.txt_Eingabe.TabIndex = 5
        '
        'lbl_Überprüfung
        '
        Me.lbl_Überprüfung.AutoSize = True
        Me.lbl_Überprüfung.Location = New System.Drawing.Point(12, 60)
        Me.lbl_Überprüfung.Name = "lbl_Überprüfung"
        Me.lbl_Überprüfung.Size = New System.Drawing.Size(0, 13)
        Me.lbl_Überprüfung.TabIndex = 6
        '
        'cmd_prüfen
        '
        Me.cmd_prüfen.Location = New System.Drawing.Point(142, 60)
        Me.cmd_prüfen.Name = "cmd_prüfen"
        Me.cmd_prüfen.Size = New System.Drawing.Size(75, 23)
        Me.cmd_prüfen.TabIndex = 7
        Me.cmd_prüfen.Text = "Prüfen"
        Me.cmd_prüfen.UseVisualStyleBackColor = True
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label2.Location = New System.Drawing.Point(12, 9)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(62, 16)
        Me.Label2.TabIndex = 8
        Me.Label2.Text = "Eingabe:"
        '
        'Zahlenraten
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(235, 219)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.cmd_prüfen)
        Me.Controls.Add(Me.lbl_Überprüfung)
        Me.Controls.Add(Me.txt_Eingabe)
        Me.Controls.Add(Me.lbl_Zähler)
        Me.Controls.Add(Me.Label5)
        Me.Controls.Add(Me.cmd_Ende)
        Me.Controls.Add(Me.cmd_Neu)
        Me.Name = "Zahlenraten"
        Me.Text = "Zahlenraten"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents cmd_Neu As System.Windows.Forms.Button
    Friend WithEvents cmd_Ende As System.Windows.Forms.Button
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents lbl_Zähler As System.Windows.Forms.Label
    Friend WithEvents txt_Eingabe As System.Windows.Forms.TextBox
    Friend WithEvents lbl_Überprüfung As System.Windows.Forms.Label
    Friend WithEvents cmd_prüfen As System.Windows.Forms.Button
    Friend WithEvents Label2 As System.Windows.Forms.Label
End Class
