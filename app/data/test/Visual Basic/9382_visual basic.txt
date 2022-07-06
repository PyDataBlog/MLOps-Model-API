<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class gameForm
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
        Me.gamePanel = New System.Windows.Forms.Panel()
        Me.btn_new = New System.Windows.Forms.Button()
        Me.lblActPlayerText = New System.Windows.Forms.Label()
        Me.lblActPlayer = New System.Windows.Forms.Label()
        Me.lblWinner = New System.Windows.Forms.Label()
        Me.SuspendLayout()
        '
        'gamePanel
        '
        Me.gamePanel.BackColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.gamePanel.Location = New System.Drawing.Point(2, 57)
        Me.gamePanel.Name = "gamePanel"
        Me.gamePanel.Size = New System.Drawing.Size(600, 600)
        Me.gamePanel.TabIndex = 0
        '
        'btn_new
        '
        Me.btn_new.Location = New System.Drawing.Point(609, 57)
        Me.btn_new.Name = "btn_new"
        Me.btn_new.Size = New System.Drawing.Size(106, 23)
        Me.btn_new.TabIndex = 1
        Me.btn_new.Text = "Neues Spiel"
        Me.btn_new.UseVisualStyleBackColor = True
        '
        'lblActPlayerText
        '
        Me.lblActPlayerText.AutoSize = True
        Me.lblActPlayerText.Location = New System.Drawing.Point(621, 83)
        Me.lblActPlayerText.Name = "lblActPlayerText"
        Me.lblActPlayerText.Size = New System.Drawing.Size(86, 13)
        Me.lblActPlayerText.TabIndex = 2
        Me.lblActPlayerText.Text = "Aktueller Spieler:"
        '
        'lblActPlayer
        '
        Me.lblActPlayer.AutoSize = True
        Me.lblActPlayer.Font = New System.Drawing.Font("Microsoft Sans Serif", 20.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblActPlayer.Location = New System.Drawing.Point(647, 107)
        Me.lblActPlayer.Name = "lblActPlayer"
        Me.lblActPlayer.Size = New System.Drawing.Size(29, 31)
        Me.lblActPlayer.TabIndex = 3
        Me.lblActPlayer.Text = "1"
        '
        'lblWinner
        '
        Me.lblWinner.AutoSize = True
        Me.lblWinner.Font = New System.Drawing.Font("Microsoft Sans Serif", 30.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblWinner.ForeColor = System.Drawing.Color.Red
        Me.lblWinner.Location = New System.Drawing.Point(2, 2)
        Me.lblWinner.Name = "lblWinner"
        Me.lblWinner.Size = New System.Drawing.Size(0, 46)
        Me.lblWinner.TabIndex = 4
        '
        'gameForm
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(719, 659)
        Me.Controls.Add(Me.lblWinner)
        Me.Controls.Add(Me.lblActPlayer)
        Me.Controls.Add(Me.lblActPlayerText)
        Me.Controls.Add(Me.btn_new)
        Me.Controls.Add(Me.gamePanel)
        Me.Name = "gameForm"
        Me.Text = " 4Gewinnt "
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents gamePanel As System.Windows.Forms.Panel
    Friend WithEvents btn_new As System.Windows.Forms.Button
    Friend WithEvents lblActPlayerText As System.Windows.Forms.Label
    Friend WithEvents lblActPlayer As System.Windows.Forms.Label
    Friend WithEvents lblWinner As System.Windows.Forms.Label

End Class
