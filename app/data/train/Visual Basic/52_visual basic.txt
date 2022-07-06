<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class home
    Inherits System.Windows.Forms.Form

    'Form remplace la méthode Dispose pour nettoyer la liste des composants.
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

    'Requise par le Concepteur Windows Form
    Private components As System.ComponentModel.IContainer

    'REMARQUE : la procédure suivante est requise par le Concepteur Windows Form
    'Elle peut être modifiée à l'aide du Concepteur Windows Form.  
    'Ne la modifiez pas à l'aide de l'éditeur de code.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(home))
        Me.LZ = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.LB_EN = New System.Windows.Forms.Label()
        Me.LB_EN_TXT = New System.Windows.Forms.Label()
        Me.BT_LAUNCH = New System.Windows.Forms.Button()
        Me.BT_LANCER = New System.Windows.Forms.Button()
        Me.BT_EXIT_HOME = New System.Windows.Forms.Button()
        Me.BT_ABOUT = New System.Windows.Forms.Button()
        Me.SuspendLayout()
        '
        'LZ
        '
        Me.LZ.AutoSize = True
        Me.LZ.Font = New System.Drawing.Font("Comic Sans MS", 14.25!, CType((System.Drawing.FontStyle.Bold Or System.Drawing.FontStyle.Underline), System.Drawing.FontStyle), System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.LZ.Location = New System.Drawing.Point(177, 9)
        Me.LZ.Name = "LZ"
        Me.LZ.Size = New System.Drawing.Size(115, 27)
        Me.LZ.TabIndex = 5
        Me.LZ.Text = "Lokiwiz 0.3"
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(298, 23)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(73, 13)
        Me.Label1.TabIndex = 6
        Me.Label1.Text = "By VPARRES"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Underline, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label2.Location = New System.Drawing.Point(13, 50)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(60, 15)
        Me.Label2.TabIndex = 7
        Me.Label2.Text = "Français :"
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label3.ForeColor = System.Drawing.Color.Red
        Me.Label3.Location = New System.Drawing.Point(12, 65)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(497, 117)
        Me.Label3.TabIndex = 8
        Me.Label3.Text = resources.GetString("Label3.Text")
        '
        'LB_EN
        '
        Me.LB_EN.AutoSize = True
        Me.LB_EN.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Underline, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.LB_EN.Location = New System.Drawing.Point(13, 195)
        Me.LB_EN.Name = "LB_EN"
        Me.LB_EN.Size = New System.Drawing.Size(54, 15)
        Me.LB_EN.TabIndex = 10
        Me.LB_EN.Text = "English :"
        '
        'LB_EN_TXT
        '
        Me.LB_EN_TXT.AutoSize = True
        Me.LB_EN_TXT.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.LB_EN_TXT.ForeColor = System.Drawing.Color.Red
        Me.LB_EN_TXT.Location = New System.Drawing.Point(12, 210)
        Me.LB_EN_TXT.Name = "LB_EN_TXT"
        Me.LB_EN_TXT.Size = New System.Drawing.Size(464, 117)
        Me.LB_EN_TXT.TabIndex = 11
        Me.LB_EN_TXT.Text = resources.GetString("LB_EN_TXT.Text")
        '
        'BT_LAUNCH
        '
        Me.BT_LAUNCH.Enabled = False
        Me.BT_LAUNCH.Location = New System.Drawing.Point(16, 341)
        Me.BT_LAUNCH.Name = "BT_LAUNCH"
        Me.BT_LAUNCH.Size = New System.Drawing.Size(126, 29)
        Me.BT_LAUNCH.TabIndex = 12
        Me.BT_LAUNCH.Text = "Continue (EN)"
        Me.BT_LAUNCH.UseVisualStyleBackColor = True
        '
        'BT_LANCER
        '
        Me.BT_LANCER.Location = New System.Drawing.Point(182, 341)
        Me.BT_LANCER.Name = "BT_LANCER"
        Me.BT_LANCER.Size = New System.Drawing.Size(126, 29)
        Me.BT_LANCER.TabIndex = 13
        Me.BT_LANCER.Text = "Continuer (FR)"
        Me.BT_LANCER.UseVisualStyleBackColor = True
        '
        'BT_EXIT_HOME
        '
        Me.BT_EXIT_HOME.Location = New System.Drawing.Point(441, 347)
        Me.BT_EXIT_HOME.Name = "BT_EXIT_HOME"
        Me.BT_EXIT_HOME.Size = New System.Drawing.Size(75, 23)
        Me.BT_EXIT_HOME.TabIndex = 14
        Me.BT_EXIT_HOME.Text = "Exit"
        Me.BT_EXIT_HOME.UseVisualStyleBackColor = True
        '
        'BT_ABOUT
        '
        Me.BT_ABOUT.Location = New System.Drawing.Point(441, 318)
        Me.BT_ABOUT.Name = "BT_ABOUT"
        Me.BT_ABOUT.Size = New System.Drawing.Size(75, 23)
        Me.BT_ABOUT.TabIndex = 15
        Me.BT_ABOUT.Text = "About"
        Me.BT_ABOUT.UseVisualStyleBackColor = True
        '
        'home
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(528, 380)
        Me.Controls.Add(Me.BT_ABOUT)
        Me.Controls.Add(Me.BT_EXIT_HOME)
        Me.Controls.Add(Me.BT_LANCER)
        Me.Controls.Add(Me.BT_LAUNCH)
        Me.Controls.Add(Me.LB_EN_TXT)
        Me.Controls.Add(Me.LB_EN)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.LZ)
        Me.Name = "home"
        Me.Text = ".mUn. Lokiwiz 0.3 --- Home Screen"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents LZ As System.Windows.Forms.Label
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents LB_EN As System.Windows.Forms.Label
    Friend WithEvents LB_EN_TXT As System.Windows.Forms.Label
    Friend WithEvents BT_LAUNCH As System.Windows.Forms.Button
    Friend WithEvents BT_LANCER As System.Windows.Forms.Button
    Friend WithEvents BT_EXIT_HOME As System.Windows.Forms.Button
    Friend WithEvents BT_ABOUT As System.Windows.Forms.Button
End Class
