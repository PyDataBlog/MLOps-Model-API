<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class NetworkSpeed
    Inherits System.Windows.Forms.Form

    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        ' Clean up any components being used.
        If disposing Then
            If (components IsNot Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub 'Dispose

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(NetworkSpeed))
        Me.Button_Cancel = New System.Windows.Forms.Button()
        Me.Label_NameOfNetworkAdapter = New System.Windows.Forms.Label()
        Me.TextBox_FoundAllOfNetworkAdapters = New System.Windows.Forms.TextBox()
        Me.Label_Loop = New System.Windows.Forms.Label()
        Me.ContextMenuStrip1 = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.ShowToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ExitToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.NotifyIcon1 = New System.Windows.Forms.NotifyIcon(Me.components)
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.INIFileEditaceToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.INIFileEditaceToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.NačteníAktloguToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.AboutToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ExitToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.Button_ReadLog = New System.Windows.Forms.Button()
        Me.Label_ControledAdapter = New System.Windows.Forms.Label()
        Me.TextBox_ControledAdapter = New System.Windows.Forms.TextBox()
        Me.TextBox_Cas = New System.Windows.Forms.TextBox()
        Me.TextBox_AktualniRychlost = New System.Windows.Forms.TextBox()
        Me.TextBox_NejvyssiRychlost = New System.Windows.Forms.TextBox()
        Me.TextBox_ProcentoStahovani = New System.Windows.Forms.TextBox()
        Me.TextBox_Smycka = New System.Windows.Forms.TextBox()
        Me.Label_Cas = New System.Windows.Forms.Label()
        Me.Label_AktualniRychlost = New System.Windows.Forms.Label()
        Me.Label_NejvyssiRychlost = New System.Windows.Forms.Label()
        Me.Label_NejnizsiRychlost = New System.Windows.Forms.Label()
        Me.Label_PocetSekund = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.StatusStrip1 = New System.Windows.Forms.StatusStrip()
        Me.ToolStripStatusLabel1 = New System.Windows.Forms.ToolStripStatusLabel()
        Me.Button_Schovat = New System.Windows.Forms.Button()
        Me.ContextMenuStrip1.SuspendLayout()
        Me.MenuStrip1.SuspendLayout()
        Me.StatusStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'Button_Cancel
        '
        Me.Button_Cancel.Image = Global.NetworkSpeed.My.Resources.Resources.system_shutdown
        Me.Button_Cancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.Button_Cancel.Location = New System.Drawing.Point(322, 261)
        Me.Button_Cancel.Name = "Button_Cancel"
        Me.Button_Cancel.Size = New System.Drawing.Size(121, 52)
        Me.Button_Cancel.TabIndex = 0
        Me.Button_Cancel.Text = "Konec"
        Me.Button_Cancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.Button_Cancel.UseVisualStyleBackColor = True
        '
        'Label_NameOfNetworkAdapter
        '
        Me.Label_NameOfNetworkAdapter.AutoSize = True
        Me.Label_NameOfNetworkAdapter.BackColor = System.Drawing.Color.Transparent
        Me.Label_NameOfNetworkAdapter.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(238, Byte))
        Me.Label_NameOfNetworkAdapter.ForeColor = System.Drawing.Color.Gold
        Me.Label_NameOfNetworkAdapter.Location = New System.Drawing.Point(9, 41)
        Me.Label_NameOfNetworkAdapter.Name = "Label_NameOfNetworkAdapter"
        Me.Label_NameOfNetworkAdapter.Size = New System.Drawing.Size(154, 13)
        Me.Label_NameOfNetworkAdapter.TabIndex = 1
        Me.Label_NameOfNetworkAdapter.Text = "Seznam network adaptérů"
        '
        'TextBox_FoundAllOfNetworkAdapters
        '
        Me.TextBox_FoundAllOfNetworkAdapters.BackColor = System.Drawing.SystemColors.GradientInactiveCaption
        Me.TextBox_FoundAllOfNetworkAdapters.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.TextBox_FoundAllOfNetworkAdapters.Location = New System.Drawing.Point(12, 57)
        Me.TextBox_FoundAllOfNetworkAdapters.Multiline = True
        Me.TextBox_FoundAllOfNetworkAdapters.Name = "TextBox_FoundAllOfNetworkAdapters"
        Me.TextBox_FoundAllOfNetworkAdapters.ReadOnly = True
        Me.TextBox_FoundAllOfNetworkAdapters.Size = New System.Drawing.Size(431, 89)
        Me.TextBox_FoundAllOfNetworkAdapters.TabIndex = 2
        '
        'Label_Loop
        '
        Me.Label_Loop.AutoSize = True
        Me.Label_Loop.BackColor = System.Drawing.Color.Transparent
        Me.Label_Loop.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(238, Byte))
        Me.Label_Loop.ForeColor = System.Drawing.Color.Gold
        Me.Label_Loop.Location = New System.Drawing.Point(12, 178)
        Me.Label_Loop.Name = "Label_Loop"
        Me.Label_Loop.Size = New System.Drawing.Size(51, 13)
        Me.Label_Loop.TabIndex = 3
        Me.Label_Loop.Text = "Smyčka"
        '
        'ContextMenuStrip1
        '
        Me.ContextMenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ShowToolStripMenuItem, Me.ExitToolStripMenuItem})
        Me.ContextMenuStrip1.Name = "ContextMenuStrip1"
        Me.ContextMenuStrip1.Size = New System.Drawing.Size(104, 48)
        '
        'ShowToolStripMenuItem
        '
        Me.ShowToolStripMenuItem.Name = "ShowToolStripMenuItem"
        Me.ShowToolStripMenuItem.Size = New System.Drawing.Size(103, 22)
        Me.ShowToolStripMenuItem.Text = "Show"
        '
        'ExitToolStripMenuItem
        '
        Me.ExitToolStripMenuItem.Name = "ExitToolStripMenuItem"
        Me.ExitToolStripMenuItem.Size = New System.Drawing.Size(103, 22)
        Me.ExitToolStripMenuItem.Text = "Exit"
        '
        'NotifyIcon1
        '
        Me.NotifyIcon1.ContextMenuStrip = Me.ContextMenuStrip1
        Me.NotifyIcon1.Icon = CType(resources.GetObject("NotifyIcon1.Icon"), System.Drawing.Icon)
        Me.NotifyIcon1.Text = "NetworkSpeed"
        Me.NotifyIcon1.Visible = True
        '
        'MenuStrip1
        '
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.INIFileEditaceToolStripMenuItem})
        Me.MenuStrip1.Location = New System.Drawing.Point(0, 0)
        Me.MenuStrip1.Name = "MenuStrip1"
        Me.MenuStrip1.Size = New System.Drawing.Size(473, 24)
        Me.MenuStrip1.TabIndex = 4
        Me.MenuStrip1.Text = "MenuStrip1"
        '
        'INIFileEditaceToolStripMenuItem
        '
        Me.INIFileEditaceToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.INIFileEditaceToolStripMenuItem1, Me.NačteníAktloguToolStripMenuItem, Me.AboutToolStripMenuItem, Me.ExitToolStripMenuItem1})
        Me.INIFileEditaceToolStripMenuItem.Image = Global.NetworkSpeed.My.Resources.Resources.Backup
        Me.INIFileEditaceToolStripMenuItem.Name = "INIFileEditaceToolStripMenuItem"
        Me.INIFileEditaceToolStripMenuItem.Size = New System.Drawing.Size(81, 20)
        Me.INIFileEditaceToolStripMenuItem.Text = "Program"
        '
        'INIFileEditaceToolStripMenuItem1
        '
        Me.INIFileEditaceToolStripMenuItem1.Image = Global.NetworkSpeed.My.Resources.Resources.ini_img
        Me.INIFileEditaceToolStripMenuItem1.Name = "INIFileEditaceToolStripMenuItem1"
        Me.INIFileEditaceToolStripMenuItem1.Size = New System.Drawing.Size(161, 22)
        Me.INIFileEditaceToolStripMenuItem1.Text = "INI file - editace"
        '
        'NačteníAktloguToolStripMenuItem
        '
        Me.NačteníAktloguToolStripMenuItem.Image = Global.NetworkSpeed.My.Resources.Resources.log_icon
        Me.NačteníAktloguToolStripMenuItem.Name = "NačteníAktloguToolStripMenuItem"
        Me.NačteníAktloguToolStripMenuItem.Size = New System.Drawing.Size(161, 22)
        Me.NačteníAktloguToolStripMenuItem.Text = "Načtení akt.logu"
        '
        'AboutToolStripMenuItem
        '
        Me.AboutToolStripMenuItem.Image = Global.NetworkSpeed.My.Resources.Resources.About
        Me.AboutToolStripMenuItem.Name = "AboutToolStripMenuItem"
        Me.AboutToolStripMenuItem.Size = New System.Drawing.Size(161, 22)
        Me.AboutToolStripMenuItem.Text = "About"
        '
        'ExitToolStripMenuItem1
        '
        Me.ExitToolStripMenuItem1.Image = Global.NetworkSpeed.My.Resources.Resources._Exit
        Me.ExitToolStripMenuItem1.Name = "ExitToolStripMenuItem1"
        Me.ExitToolStripMenuItem1.Size = New System.Drawing.Size(161, 22)
        Me.ExitToolStripMenuItem1.Text = "Exit"
        '
        'Button_ReadLog
        '
        Me.Button_ReadLog.Image = Global.NetworkSpeed.My.Resources.Resources.log_icon_small
        Me.Button_ReadLog.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.Button_ReadLog.Location = New System.Drawing.Point(12, 261)
        Me.Button_ReadLog.Name = "Button_ReadLog"
        Me.Button_ReadLog.Size = New System.Drawing.Size(121, 52)
        Me.Button_ReadLog.TabIndex = 5
        Me.Button_ReadLog.Text = "Log File"
        Me.Button_ReadLog.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.Button_ReadLog.UseVisualStyleBackColor = True
        '
        'Label_ControledAdapter
        '
        Me.Label_ControledAdapter.AutoSize = True
        Me.Label_ControledAdapter.BackColor = System.Drawing.Color.Transparent
        Me.Label_ControledAdapter.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(238, Byte))
        Me.Label_ControledAdapter.ForeColor = System.Drawing.Color.Gold
        Me.Label_ControledAdapter.Location = New System.Drawing.Point(12, 152)
        Me.Label_ControledAdapter.Name = "Label_ControledAdapter"
        Me.Label_ControledAdapter.Size = New System.Drawing.Size(96, 13)
        Me.Label_ControledAdapter.TabIndex = 6
        Me.Label_ControledAdapter.Text = "Měřený adaptér"
        '
        'TextBox_ControledAdapter
        '
        Me.TextBox_ControledAdapter.BackColor = System.Drawing.Color.LightSalmon
        Me.TextBox_ControledAdapter.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.TextBox_ControledAdapter.Location = New System.Drawing.Point(114, 152)
        Me.TextBox_ControledAdapter.Name = "TextBox_ControledAdapter"
        Me.TextBox_ControledAdapter.ReadOnly = True
        Me.TextBox_ControledAdapter.Size = New System.Drawing.Size(329, 20)
        Me.TextBox_ControledAdapter.TabIndex = 7
        '
        'TextBox_Cas
        '
        Me.TextBox_Cas.BackColor = System.Drawing.SystemColors.GradientInactiveCaption
        Me.TextBox_Cas.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.TextBox_Cas.Location = New System.Drawing.Point(15, 228)
        Me.TextBox_Cas.Name = "TextBox_Cas"
        Me.TextBox_Cas.ReadOnly = True
        Me.TextBox_Cas.Size = New System.Drawing.Size(36, 20)
        Me.TextBox_Cas.TabIndex = 8
        '
        'TextBox_AktualniRychlost
        '
        Me.TextBox_AktualniRychlost.BackColor = System.Drawing.Color.LightSalmon
        Me.TextBox_AktualniRychlost.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.TextBox_AktualniRychlost.Location = New System.Drawing.Point(83, 228)
        Me.TextBox_AktualniRychlost.Name = "TextBox_AktualniRychlost"
        Me.TextBox_AktualniRychlost.ReadOnly = True
        Me.TextBox_AktualniRychlost.Size = New System.Drawing.Size(42, 20)
        Me.TextBox_AktualniRychlost.TabIndex = 9
        '
        'TextBox_NejvyssiRychlost
        '
        Me.TextBox_NejvyssiRychlost.BackColor = System.Drawing.SystemColors.GradientInactiveCaption
        Me.TextBox_NejvyssiRychlost.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.TextBox_NejvyssiRychlost.Location = New System.Drawing.Point(196, 228)
        Me.TextBox_NejvyssiRychlost.Name = "TextBox_NejvyssiRychlost"
        Me.TextBox_NejvyssiRychlost.ReadOnly = True
        Me.TextBox_NejvyssiRychlost.Size = New System.Drawing.Size(42, 20)
        Me.TextBox_NejvyssiRychlost.TabIndex = 10
        '
        'TextBox_ProcentoStahovani
        '
        Me.TextBox_ProcentoStahovani.BackColor = System.Drawing.SystemColors.GradientInactiveCaption
        Me.TextBox_ProcentoStahovani.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.TextBox_ProcentoStahovani.Location = New System.Drawing.Point(308, 228)
        Me.TextBox_ProcentoStahovani.Name = "TextBox_ProcentoStahovani"
        Me.TextBox_ProcentoStahovani.ReadOnly = True
        Me.TextBox_ProcentoStahovani.Size = New System.Drawing.Size(39, 20)
        Me.TextBox_ProcentoStahovani.TabIndex = 11
        '
        'TextBox_Smycka
        '
        Me.TextBox_Smycka.BackColor = System.Drawing.SystemColors.GradientInactiveCaption
        Me.TextBox_Smycka.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.TextBox_Smycka.Location = New System.Drawing.Point(66, 178)
        Me.TextBox_Smycka.Name = "TextBox_Smycka"
        Me.TextBox_Smycka.ReadOnly = True
        Me.TextBox_Smycka.Size = New System.Drawing.Size(31, 20)
        Me.TextBox_Smycka.TabIndex = 12
        '
        'Label_Cas
        '
        Me.Label_Cas.AutoSize = True
        Me.Label_Cas.BackColor = System.Drawing.Color.Transparent
        Me.Label_Cas.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(238, Byte))
        Me.Label_Cas.ForeColor = System.Drawing.Color.Gold
        Me.Label_Cas.Location = New System.Drawing.Point(12, 212)
        Me.Label_Cas.Name = "Label_Cas"
        Me.Label_Cas.Size = New System.Drawing.Size(28, 13)
        Me.Label_Cas.TabIndex = 13
        Me.Label_Cas.Text = "Čas"
        '
        'Label_AktualniRychlost
        '
        Me.Label_AktualniRychlost.AutoSize = True
        Me.Label_AktualniRychlost.BackColor = System.Drawing.Color.Transparent
        Me.Label_AktualniRychlost.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(238, Byte))
        Me.Label_AktualniRychlost.ForeColor = System.Drawing.Color.Gold
        Me.Label_AktualniRychlost.Location = New System.Drawing.Point(79, 211)
        Me.Label_AktualniRychlost.Name = "Label_AktualniRychlost"
        Me.Label_AktualniRychlost.Size = New System.Drawing.Size(103, 13)
        Me.Label_AktualniRychlost.TabIndex = 14
        Me.Label_AktualniRychlost.Text = "Aktuální rychlost"
        '
        'Label_NejvyssiRychlost
        '
        Me.Label_NejvyssiRychlost.AutoSize = True
        Me.Label_NejvyssiRychlost.BackColor = System.Drawing.Color.Transparent
        Me.Label_NejvyssiRychlost.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(238, Byte))
        Me.Label_NejvyssiRychlost.ForeColor = System.Drawing.Color.DarkGoldenrod
        Me.Label_NejvyssiRychlost.Location = New System.Drawing.Point(192, 212)
        Me.Label_NejvyssiRychlost.Name = "Label_NejvyssiRychlost"
        Me.Label_NejvyssiRychlost.Size = New System.Drawing.Size(104, 13)
        Me.Label_NejvyssiRychlost.TabIndex = 15
        Me.Label_NejvyssiRychlost.Text = "Nejvyšší rychlost"
        '
        'Label_NejnizsiRychlost
        '
        Me.Label_NejnizsiRychlost.AutoSize = True
        Me.Label_NejnizsiRychlost.BackColor = System.Drawing.Color.Transparent
        Me.Label_NejnizsiRychlost.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(238, Byte))
        Me.Label_NejnizsiRychlost.ForeColor = System.Drawing.Color.DarkGoldenrod
        Me.Label_NejnizsiRychlost.Location = New System.Drawing.Point(306, 212)
        Me.Label_NejnizsiRychlost.Name = "Label_NejnizsiRychlost"
        Me.Label_NejnizsiRychlost.Size = New System.Drawing.Size(119, 13)
        Me.Label_NejnizsiRychlost.TabIndex = 16
        Me.Label_NejnizsiRychlost.Text = "Procento stahování"
        '
        'Label_PocetSekund
        '
        Me.Label_PocetSekund.AutoSize = True
        Me.Label_PocetSekund.BackColor = System.Drawing.Color.Transparent
        Me.Label_PocetSekund.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(238, Byte))
        Me.Label_PocetSekund.ForeColor = System.Drawing.Color.Gold
        Me.Label_PocetSekund.Location = New System.Drawing.Point(101, 179)
        Me.Label_PocetSekund.Name = "Label_PocetSekund"
        Me.Label_PocetSekund.Size = New System.Drawing.Size(52, 13)
        Me.Label_PocetSekund.TabIndex = 17
        Me.Label_PocetSekund.Text = "sekund."
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.BackColor = System.Drawing.Color.Transparent
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(238, Byte))
        Me.Label1.ForeColor = System.Drawing.Color.Gold
        Me.Label1.Location = New System.Drawing.Point(133, 231)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(35, 13)
        Me.Label1.TabIndex = 18
        Me.Label1.Text = "Kbps"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.BackColor = System.Drawing.Color.Transparent
        Me.Label2.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(238, Byte))
        Me.Label2.ForeColor = System.Drawing.Color.Gold
        Me.Label2.Location = New System.Drawing.Point(244, 231)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(35, 13)
        Me.Label2.TabIndex = 19
        Me.Label2.Text = "Kbps"
        '
        'StatusStrip1
        '
        Me.StatusStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripStatusLabel1})
        Me.StatusStrip1.Location = New System.Drawing.Point(0, 348)
        Me.StatusStrip1.Name = "StatusStrip1"
        Me.StatusStrip1.Size = New System.Drawing.Size(473, 22)
        Me.StatusStrip1.TabIndex = 20
        Me.StatusStrip1.Text = "StatusStrip1"
        '
        'ToolStripStatusLabel1
        '
        Me.ToolStripStatusLabel1.Name = "ToolStripStatusLabel1"
        Me.ToolStripStatusLabel1.Size = New System.Drawing.Size(0, 17)
        '
        'Button_Schovat
        '
        Me.Button_Schovat.Image = CType(resources.GetObject("Button_Schovat.Image"), System.Drawing.Image)
        Me.Button_Schovat.ImageAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.Button_Schovat.Location = New System.Drawing.Point(12, 319)
        Me.Button_Schovat.Margin = New System.Windows.Forms.Padding(0)
        Me.Button_Schovat.Name = "Button_Schovat"
        Me.Button_Schovat.Size = New System.Drawing.Size(431, 23)
        Me.Button_Schovat.TabIndex = 21
        Me.Button_Schovat.Text = "↓↓↓ Schovat ↓↓↓"
        Me.Button_Schovat.UseVisualStyleBackColor = False
        '
        'NetworkSpeed
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackgroundImage = Global.NetworkSpeed.My.Resources.Resources.NetworkSpeed1
        Me.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch
        Me.ClientSize = New System.Drawing.Size(473, 370)
        Me.Controls.Add(Me.Button_Schovat)
        Me.Controls.Add(Me.StatusStrip1)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.Label_PocetSekund)
        Me.Controls.Add(Me.Label_NejnizsiRychlost)
        Me.Controls.Add(Me.Label_NejvyssiRychlost)
        Me.Controls.Add(Me.Label_AktualniRychlost)
        Me.Controls.Add(Me.Label_Cas)
        Me.Controls.Add(Me.TextBox_Smycka)
        Me.Controls.Add(Me.TextBox_ProcentoStahovani)
        Me.Controls.Add(Me.TextBox_NejvyssiRychlost)
        Me.Controls.Add(Me.TextBox_AktualniRychlost)
        Me.Controls.Add(Me.TextBox_Cas)
        Me.Controls.Add(Me.TextBox_ControledAdapter)
        Me.Controls.Add(Me.Label_ControledAdapter)
        Me.Controls.Add(Me.Button_ReadLog)
        Me.Controls.Add(Me.MenuStrip1)
        Me.Controls.Add(Me.Label_Loop)
        Me.Controls.Add(Me.TextBox_FoundAllOfNetworkAdapters)
        Me.Controls.Add(Me.Label_NameOfNetworkAdapter)
        Me.Controls.Add(Me.Button_Cancel)
        Me.DoubleBuffered = True
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.MainMenuStrip = Me.MenuStrip1
        Me.Name = "NetworkSpeed"
        Me.Text = "NetworkSpeed"
        Me.ContextMenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        Me.StatusStrip1.ResumeLayout(False)
        Me.StatusStrip1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents Button_Cancel As System.Windows.Forms.Button
    Friend WithEvents Label_NameOfNetworkAdapter As System.Windows.Forms.Label
    Friend WithEvents TextBox_FoundAllOfNetworkAdapters As System.Windows.Forms.TextBox
    Friend WithEvents TextBox_ControledAdapter As System.Windows.Forms.TextBox
    Friend WithEvents TextBox_Cas As System.Windows.Forms.TextBox
    Friend WithEvents TextBox_AktualniRychlost As System.Windows.Forms.TextBox
    Friend WithEvents TextBox_NejvyssiRychlost As System.Windows.Forms.TextBox
    Friend WithEvents TextBox_ProcentoStahovani As System.Windows.Forms.TextBox
    Friend WithEvents TextBox_Smycka As System.Windows.Forms.TextBox
    Friend WithEvents Label_Loop As System.Windows.Forms.Label
    Friend WithEvents ContextMenuStrip1 As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents ShowToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ExitToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents NotifyIcon1 As System.Windows.Forms.NotifyIcon
    Friend WithEvents MenuStrip1 As System.Windows.Forms.MenuStrip
    Friend WithEvents INIFileEditaceToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents INIFileEditaceToolStripMenuItem1 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents NačteníAktloguToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents AboutToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ExitToolStripMenuItem1 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents Button_ReadLog As System.Windows.Forms.Button
    Friend WithEvents Label_ControledAdapter As System.Windows.Forms.Label
    Friend WithEvents Label_Cas As System.Windows.Forms.Label
    Friend WithEvents Label_AktualniRychlost As System.Windows.Forms.Label
    Friend WithEvents Label_NejvyssiRychlost As System.Windows.Forms.Label
    Friend WithEvents Label_NejnizsiRychlost As System.Windows.Forms.Label
    Friend WithEvents Label_PocetSekund As System.Windows.Forms.Label
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents StatusStrip1 As System.Windows.Forms.StatusStrip
    Friend WithEvents ToolStripStatusLabel1 As System.Windows.Forms.ToolStripStatusLabel
    Friend WithEvents Button_Schovat As System.Windows.Forms.Button

End Class
