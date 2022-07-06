<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form_10_Carichi_Rdy_Seriali
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
        Me.btnCerca = New System.Windows.Forms.Button()
        Me.DataIns = New System.Windows.Forms.DateTimePicker()
        Me.DataIns2 = New System.Windows.Forms.DateTimePicker()
        Me.SuspendLayout()
        '
        'WebBrowser1
        '
        Me.WebBrowser1.Location = New System.Drawing.Point(13, 46)
        Me.WebBrowser1.MinimumSize = New System.Drawing.Size(20, 20)
        Me.WebBrowser1.Name = "WebBrowser1"
        Me.WebBrowser1.Size = New System.Drawing.Size(1318, 714)
        Me.WebBrowser1.TabIndex = 0
        '
        'btnCerca
        '
        Me.btnCerca.Location = New System.Drawing.Point(455, 11)
        Me.btnCerca.Name = "btnCerca"
        Me.btnCerca.Size = New System.Drawing.Size(75, 23)
        Me.btnCerca.TabIndex = 1
        Me.btnCerca.Text = "Cerca"
        Me.btnCerca.UseVisualStyleBackColor = True
        '
        'DataIns
        '
        Me.DataIns.Location = New System.Drawing.Point(13, 12)
        Me.DataIns.Name = "DataIns"
        Me.DataIns.Size = New System.Drawing.Size(200, 20)
        Me.DataIns.TabIndex = 2
        '
        'DataIns2
        '
        Me.DataIns2.Location = New System.Drawing.Point(234, 12)
        Me.DataIns2.Name = "DataIns2"
        Me.DataIns2.Size = New System.Drawing.Size(200, 20)
        Me.DataIns2.TabIndex = 3
        '
        'Form_10_Carichi_Rdy_Seriali
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(1343, 772)
        Me.Controls.Add(Me.DataIns2)
        Me.Controls.Add(Me.DataIns)
        Me.Controls.Add(Me.btnCerca)
        Me.Controls.Add(Me.WebBrowser1)
        Me.Name = "Form_10_Carichi_Rdy_Seriali"
        Me.Text = "Confronto Carichi da Ready a Seriali"
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents WebBrowser1 As System.Windows.Forms.WebBrowser
    Friend WithEvents btnCerca As System.Windows.Forms.Button
    Friend WithEvents DataIns As System.Windows.Forms.DateTimePicker
    Friend WithEvents DataIns2 As System.Windows.Forms.DateTimePicker
End Class
