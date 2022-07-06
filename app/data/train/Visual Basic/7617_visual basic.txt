<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class scrSerial
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(scrSerial))
        Me.btnGuardarPto = New System.Windows.Forms.Button()
        Me.cmbPorts = New System.Windows.Forms.ComboBox()
        Me.btnDeterminarPuertos = New System.Windows.Forms.Button()
        Me.SuspendLayout()
        '
        'btnGuardarPto
        '
        Me.btnGuardarPto.Image = CType(resources.GetObject("btnGuardarPto.Image"), System.Drawing.Image)
        Me.btnGuardarPto.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.btnGuardarPto.Location = New System.Drawing.Point(189, 197)
        Me.btnGuardarPto.Name = "btnGuardarPto"
        Me.btnGuardarPto.Size = New System.Drawing.Size(194, 53)
        Me.btnGuardarPto.TabIndex = 4
        Me.btnGuardarPto.Text = "&Guardar"
        Me.btnGuardarPto.UseVisualStyleBackColor = True
        '
        'cmbPorts
        '
        Me.cmbPorts.BackColor = System.Drawing.SystemColors.InactiveBorder
        Me.cmbPorts.FormattingEnabled = True
        Me.cmbPorts.Location = New System.Drawing.Point(320, 70)
        Me.cmbPorts.Name = "cmbPorts"
        Me.cmbPorts.Size = New System.Drawing.Size(192, 33)
        Me.cmbPorts.TabIndex = 3
        '
        'btnDeterminarPuertos
        '
        Me.btnDeterminarPuertos.Image = CType(resources.GetObject("btnDeterminarPuertos.Image"), System.Drawing.Image)
        Me.btnDeterminarPuertos.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.btnDeterminarPuertos.Location = New System.Drawing.Point(47, 60)
        Me.btnDeterminarPuertos.Name = "btnDeterminarPuertos"
        Me.btnDeterminarPuertos.Size = New System.Drawing.Size(246, 50)
        Me.btnDeterminarPuertos.TabIndex = 2
        Me.btnDeterminarPuertos.Text = "&Determinar Puertos"
        Me.btnDeterminarPuertos.UseVisualStyleBackColor = True
        '
        'scrSerial
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(12.0!, 25.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(569, 311)
        Me.Controls.Add(Me.btnGuardarPto)
        Me.Controls.Add(Me.cmbPorts)
        Me.Controls.Add(Me.btnDeterminarPuertos)
        Me.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Margin = New System.Windows.Forms.Padding(4, 5, 4, 5)
        Me.Name = "scrSerial"
        Me.Text = "Parámetros de conexión Serial Rekór RFID & WebCam."
        Me.TopMost = True
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents btnGuardarPto As System.Windows.Forms.Button
    Friend WithEvents cmbPorts As System.Windows.Forms.ComboBox
    Friend WithEvents btnDeterminarPuertos As System.Windows.Forms.Button
End Class
