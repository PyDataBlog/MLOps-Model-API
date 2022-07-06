<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Sonne_Mond
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Sonne_Mond))
        Me.cmd_back = New System.Windows.Forms.Button
        Me.cmd_sun = New System.Windows.Forms.Button
        Me.cmd_moon = New System.Windows.Forms.Button
        Me.pb_Sun = New System.Windows.Forms.PictureBox
        Me.pb_moon = New System.Windows.Forms.PictureBox
        CType(Me.pb_Sun, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.pb_moon, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'cmd_back
        '
        Me.cmd_back.Location = New System.Drawing.Point(362, 481)
        Me.cmd_back.Name = "cmd_back"
        Me.cmd_back.Size = New System.Drawing.Size(75, 23)
        Me.cmd_back.TabIndex = 0
        Me.cmd_back.Text = "Zurück"
        Me.cmd_back.UseVisualStyleBackColor = True
        '
        'cmd_sun
        '
        Me.cmd_sun.Location = New System.Drawing.Point(175, 413)
        Me.cmd_sun.Name = "cmd_sun"
        Me.cmd_sun.Size = New System.Drawing.Size(75, 23)
        Me.cmd_sun.TabIndex = 1
        Me.cmd_sun.Text = "Sonne"
        Me.cmd_sun.UseVisualStyleBackColor = True
        '
        'cmd_moon
        '
        Me.cmd_moon.Location = New System.Drawing.Point(550, 413)
        Me.cmd_moon.Name = "cmd_moon"
        Me.cmd_moon.Size = New System.Drawing.Size(75, 23)
        Me.cmd_moon.TabIndex = 2
        Me.cmd_moon.Text = "Mond"
        Me.cmd_moon.UseVisualStyleBackColor = True
        '
        'pb_Sun
        '
        Me.pb_Sun.Image = CType(resources.GetObject("pb_Sun.Image"), System.Drawing.Image)
        Me.pb_Sun.Location = New System.Drawing.Point(59, 84)
        Me.pb_Sun.Name = "pb_Sun"
        Me.pb_Sun.Size = New System.Drawing.Size(300, 300)
        Me.pb_Sun.TabIndex = 3
        Me.pb_Sun.TabStop = False
        '
        'pb_moon
        '
        Me.pb_moon.Image = CType(resources.GetObject("pb_moon.Image"), System.Drawing.Image)
        Me.pb_moon.Location = New System.Drawing.Point(433, 84)
        Me.pb_moon.Name = "pb_moon"
        Me.pb_moon.Size = New System.Drawing.Size(300, 300)
        Me.pb_moon.TabIndex = 4
        Me.pb_moon.TabStop = False
        '
        'Sonne_Mond
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(810, 546)
        Me.Controls.Add(Me.pb_moon)
        Me.Controls.Add(Me.pb_Sun)
        Me.Controls.Add(Me.cmd_moon)
        Me.Controls.Add(Me.cmd_sun)
        Me.Controls.Add(Me.cmd_back)
        Me.Name = "Sonne_Mond"
        Me.Text = "Sonne_Mond"
        CType(Me.pb_Sun, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.pb_moon, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents cmd_back As System.Windows.Forms.Button
    Friend WithEvents cmd_sun As System.Windows.Forms.Button
    Friend WithEvents cmd_moon As System.Windows.Forms.Button
    Friend WithEvents pb_Sun As System.Windows.Forms.PictureBox
    Friend WithEvents pb_moon As System.Windows.Forms.PictureBox
End Class
