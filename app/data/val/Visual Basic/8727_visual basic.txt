<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form_20_Scarichi_Rdy_Seriali
    Inherits System.Windows.Forms.Form

    'Form esegue l'override del metodo Dispose per pulire l'elenco dei componenti.
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

    'Richiesto da Progettazione Windows Form
    Private components As System.ComponentModel.IContainer

    'NOTA: la procedura che segue è richiesta da Progettazione Windows Form
    'Può essere modificata in Progettazione Windows Form.  
    'Non modificarla nell'editor del codice.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.WebBrowser1 = New System.Windows.Forms.WebBrowser()
        Me.DataIns2 = New System.Windows.Forms.DateTimePicker()
        Me.DataIns = New System.Windows.Forms.DateTimePicker()
        Me.btnMail = New System.Windows.Forms.Button()
        Me.btnCerca = New System.Windows.Forms.Button()
        Me.SuspendLayout()
        '
        'WebBrowser1
        '
        Me.WebBrowser1.Location = New System.Drawing.Point(12, 36)
        Me.WebBrowser1.MinimumSize = New System.Drawing.Size(20, 20)
        Me.WebBrowser1.Name = "WebBrowser1"
        Me.WebBrowser1.Size = New System.Drawing.Size(1160, 648)
        Me.WebBrowser1.TabIndex = 0
        '
        'DataIns2
        '
        Me.DataIns2.Location = New System.Drawing.Point(222, 7)
        Me.DataIns2.Name = "DataIns2"
        Me.DataIns2.Size = New System.Drawing.Size(200, 20)
        Me.DataIns2.TabIndex = 9
        '
        'DataIns
        '
        Me.DataIns.Location = New System.Drawing.Point(11, 7)
        Me.DataIns.Name = "DataIns"
        Me.DataIns.Size = New System.Drawing.Size(200, 20)
        Me.DataIns.TabIndex = 8
        '
        'btnMail
        '
        Me.btnMail.Location = New System.Drawing.Point(519, 6)
        Me.btnMail.Name = "btnMail"
        Me.btnMail.Size = New System.Drawing.Size(75, 23)
        Me.btnMail.TabIndex = 7
        Me.btnMail.Text = "Invia Email"
        Me.btnMail.UseVisualStyleBackColor = True
        '
        'btnCerca
        '
        Me.btnCerca.Location = New System.Drawing.Point(433, 6)
        Me.btnCerca.Name = "btnCerca"
        Me.btnCerca.Size = New System.Drawing.Size(75, 23)
        Me.btnCerca.TabIndex = 6
        Me.btnCerca.Text = "Verifica"
        Me.btnCerca.UseVisualStyleBackColor = True
        '
        'Form_20_Scarichi_Rdy_Seriali
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(1184, 696)
        Me.Controls.Add(Me.DataIns2)
        Me.Controls.Add(Me.DataIns)
        Me.Controls.Add(Me.btnMail)
        Me.Controls.Add(Me.btnCerca)
        Me.Controls.Add(Me.WebBrowser1)
        Me.Name = "Form_20_Scarichi_Rdy_Seriali"
        Me.Text = "Confronto Scarichi da Ready a Seriali"
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents WebBrowser1 As System.Windows.Forms.WebBrowser
    Friend WithEvents DataIns2 As DateTimePicker
    Friend WithEvents DataIns As DateTimePicker
    Friend WithEvents btnMail As Button
    Friend WithEvents btnCerca As Button
End Class
