<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Launcher
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
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Launcher))
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.Button4 = New System.Windows.Forms.Button()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.PremPass = New System.Windows.Forms.MaskedTextBox()
        Me.PremUser = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.PremLogin = New System.Windows.Forms.Button()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.CheckBox1 = New System.Windows.Forms.CheckBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.TxtBox_CrackedUser = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Button3 = New System.Windows.Forms.Button()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.PremLoginBox = New System.Windows.Forms.GroupBox()
        Me.RichTextBox1 = New System.Windows.Forms.RichTextBox()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.TextBox3 = New System.Windows.Forms.TextBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.TextBox2 = New System.Windows.Forms.TextBox()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.TextBox1 = New System.Windows.Forms.TextBox()
        Me.Btn_save_settings = New System.Windows.Forms.Button()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.TxtBox_RAM = New System.Windows.Forms.TextBox()
        Me.ProgressBar1 = New System.Windows.Forms.ProgressBar()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.Btn_about = New System.Windows.Forms.Button()
        Me.Btn_credits = New System.Windows.Forms.Button()
        Me.Btn_MP_info = New System.Windows.Forms.Button()
        Me.Btn_GitHub = New System.Windows.Forms.Button()
        Me.Btn_Website = New System.Windows.Forms.Button()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.TabPage2.SuspendLayout()
        Me.PremLoginBox.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.SuspendLayout()
        '
        'ToolTip1
        '
        Me.ToolTip1.AutoPopDelay = 5000
        Me.ToolTip1.InitialDelay = 300
        Me.ToolTip1.ReshowDelay = 100
        Me.ToolTip1.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Warning
        Me.ToolTip1.ToolTipTitle = "WARNING:"
        '
        'Button4
        '
        resources.ApplyResources(Me.Button4, "Button4")
        Me.Button4.Cursor = System.Windows.Forms.Cursors.No
        Me.Button4.Name = "Button4"
        Me.ToolTip1.SetToolTip(Me.Button4, resources.GetString("Button4.ToolTip"))
        Me.Button4.UseVisualStyleBackColor = True
        '
        'Button2
        '
        resources.ApplyResources(Me.Button2, "Button2")
        Me.Button2.Cursor = System.Windows.Forms.Cursors.No
        Me.Button2.Name = "Button2"
        Me.ToolTip1.SetToolTip(Me.Button2, resources.GetString("Button2.ToolTip"))
        Me.Button2.UseVisualStyleBackColor = True
        '
        'PictureBox1
        '
        Me.PictureBox1.BackColor = System.Drawing.Color.White
        Me.PictureBox1.Cursor = System.Windows.Forms.Cursors.Hand
        resources.ApplyResources(Me.PictureBox1, "PictureBox1")
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.TabStop = False
        '
        'TabControl1
        '
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPage2)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        '
        'TabPage1
        '
        Me.TabPage1.Controls.Add(Me.PremPass)
        Me.TabPage1.Controls.Add(Me.PremUser)
        Me.TabPage1.Controls.Add(Me.Label1)
        Me.TabPage1.Controls.Add(Me.PremLogin)
        Me.TabPage1.Controls.Add(Me.Label2)
        Me.TabPage1.Controls.Add(Me.CheckBox1)
        Me.TabPage1.Controls.Add(Me.Button2)
        Me.TabPage1.Controls.Add(Me.Label3)
        resources.ApplyResources(Me.TabPage1, "TabPage1")
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'PremPass
        '
        resources.ApplyResources(Me.PremPass, "PremPass")
        Me.PremPass.Name = "PremPass"
        '
        'PremUser
        '
        resources.ApplyResources(Me.PremUser, "PremUser")
        Me.PremUser.Name = "PremUser"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.ForeColor = System.Drawing.SystemColors.ControlDarkDark
        Me.Label1.Name = "Label1"
        '
        'PremLogin
        '
        resources.ApplyResources(Me.PremLogin, "PremLogin")
        Me.PremLogin.Name = "PremLogin"
        Me.PremLogin.UseVisualStyleBackColor = True
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'CheckBox1
        '
        resources.ApplyResources(Me.CheckBox1, "CheckBox1")
        Me.CheckBox1.Name = "CheckBox1"
        Me.CheckBox1.UseVisualStyleBackColor = True
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'TabPage2
        '
        Me.TabPage2.Controls.Add(Me.TxtBox_CrackedUser)
        Me.TabPage2.Controls.Add(Me.Label4)
        Me.TabPage2.Controls.Add(Me.Button3)
        Me.TabPage2.Controls.Add(Me.Label5)
        Me.TabPage2.Controls.Add(Me.Button4)
        resources.ApplyResources(Me.TabPage2, "TabPage2")
        Me.TabPage2.Name = "TabPage2"
        '
        'TxtBox_CrackedUser
        '
        resources.ApplyResources(Me.TxtBox_CrackedUser, "TxtBox_CrackedUser")
        Me.TxtBox_CrackedUser.Name = "TxtBox_CrackedUser"
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.ForeColor = System.Drawing.SystemColors.ControlDarkDark
        Me.Label4.Name = "Label4"
        '
        'Button3
        '
        resources.ApplyResources(Me.Button3, "Button3")
        Me.Button3.Name = "Button3"
        Me.Button3.UseVisualStyleBackColor = True
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        '
        'PremLoginBox
        '
        Me.PremLoginBox.BackColor = System.Drawing.SystemColors.ButtonFace
        Me.PremLoginBox.Controls.Add(Me.TabControl1)
        Me.PremLoginBox.Controls.Add(Me.PictureBox1)
        resources.ApplyResources(Me.PremLoginBox, "PremLoginBox")
        Me.PremLoginBox.Name = "PremLoginBox"
        Me.PremLoginBox.TabStop = False
        '
        'RichTextBox1
        '
        resources.ApplyResources(Me.RichTextBox1, "RichTextBox1")
        Me.RichTextBox1.Name = "RichTextBox1"
        Me.RichTextBox1.ReadOnly = True
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.Label9)
        Me.GroupBox1.Controls.Add(Me.TextBox3)
        Me.GroupBox1.Controls.Add(Me.Label8)
        Me.GroupBox1.Controls.Add(Me.TextBox2)
        Me.GroupBox1.Controls.Add(Me.Label7)
        Me.GroupBox1.Controls.Add(Me.TextBox1)
        Me.GroupBox1.Controls.Add(Me.Btn_save_settings)
        Me.GroupBox1.Controls.Add(Me.Label6)
        Me.GroupBox1.Controls.Add(Me.TxtBox_RAM)
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        '
        'TextBox3
        '
        Me.TextBox3.AutoCompleteCustomSource.AddRange(New String() {resources.GetString("TextBox3.AutoCompleteCustomSource"), resources.GetString("TextBox3.AutoCompleteCustomSource1"), resources.GetString("TextBox3.AutoCompleteCustomSource2"), resources.GetString("TextBox3.AutoCompleteCustomSource3"), resources.GetString("TextBox3.AutoCompleteCustomSource4"), resources.GetString("TextBox3.AutoCompleteCustomSource5"), resources.GetString("TextBox3.AutoCompleteCustomSource6"), resources.GetString("TextBox3.AutoCompleteCustomSource7")})
        Me.TextBox3.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.SuggestAppend
        Me.TextBox3.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.CustomSource
        resources.ApplyResources(Me.TextBox3, "TextBox3")
        Me.TextBox3.Name = "TextBox3"
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        '
        'TextBox2
        '
        Me.TextBox2.AutoCompleteCustomSource.AddRange(New String() {resources.GetString("TextBox2.AutoCompleteCustomSource"), resources.GetString("TextBox2.AutoCompleteCustomSource1"), resources.GetString("TextBox2.AutoCompleteCustomSource2"), resources.GetString("TextBox2.AutoCompleteCustomSource3"), resources.GetString("TextBox2.AutoCompleteCustomSource4"), resources.GetString("TextBox2.AutoCompleteCustomSource5"), resources.GetString("TextBox2.AutoCompleteCustomSource6"), resources.GetString("TextBox2.AutoCompleteCustomSource7")})
        Me.TextBox2.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.SuggestAppend
        Me.TextBox2.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.CustomSource
        resources.ApplyResources(Me.TextBox2, "TextBox2")
        Me.TextBox2.Name = "TextBox2"
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        '
        'TextBox1
        '
        Me.TextBox1.AutoCompleteCustomSource.AddRange(New String() {resources.GetString("TextBox1.AutoCompleteCustomSource"), resources.GetString("TextBox1.AutoCompleteCustomSource1"), resources.GetString("TextBox1.AutoCompleteCustomSource2"), resources.GetString("TextBox1.AutoCompleteCustomSource3"), resources.GetString("TextBox1.AutoCompleteCustomSource4"), resources.GetString("TextBox1.AutoCompleteCustomSource5"), resources.GetString("TextBox1.AutoCompleteCustomSource6"), resources.GetString("TextBox1.AutoCompleteCustomSource7")})
        Me.TextBox1.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.SuggestAppend
        Me.TextBox1.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.CustomSource
        resources.ApplyResources(Me.TextBox1, "TextBox1")
        Me.TextBox1.Name = "TextBox1"
        '
        'Btn_save_settings
        '
        resources.ApplyResources(Me.Btn_save_settings, "Btn_save_settings")
        Me.Btn_save_settings.Name = "Btn_save_settings"
        Me.Btn_save_settings.UseVisualStyleBackColor = True
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        '
        'TxtBox_RAM
        '
        Me.TxtBox_RAM.AutoCompleteCustomSource.AddRange(New String() {resources.GetString("TxtBox_RAM.AutoCompleteCustomSource"), resources.GetString("TxtBox_RAM.AutoCompleteCustomSource1"), resources.GetString("TxtBox_RAM.AutoCompleteCustomSource2"), resources.GetString("TxtBox_RAM.AutoCompleteCustomSource3"), resources.GetString("TxtBox_RAM.AutoCompleteCustomSource4"), resources.GetString("TxtBox_RAM.AutoCompleteCustomSource5"), resources.GetString("TxtBox_RAM.AutoCompleteCustomSource6"), resources.GetString("TxtBox_RAM.AutoCompleteCustomSource7")})
        Me.TxtBox_RAM.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.SuggestAppend
        Me.TxtBox_RAM.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.CustomSource
        resources.ApplyResources(Me.TxtBox_RAM, "TxtBox_RAM")
        Me.TxtBox_RAM.Name = "TxtBox_RAM"
        '
        'ProgressBar1
        '
        resources.ApplyResources(Me.ProgressBar1, "ProgressBar1")
        Me.ProgressBar1.Name = "ProgressBar1"
        '
        'GroupBox2
        '
        Me.GroupBox2.Controls.Add(Me.Btn_about)
        Me.GroupBox2.Controls.Add(Me.Btn_credits)
        Me.GroupBox2.Controls.Add(Me.Btn_MP_info)
        Me.GroupBox2.Controls.Add(Me.Btn_GitHub)
        Me.GroupBox2.Controls.Add(Me.Btn_Website)
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'Btn_about
        '
        resources.ApplyResources(Me.Btn_about, "Btn_about")
        Me.Btn_about.Name = "Btn_about"
        Me.Btn_about.UseVisualStyleBackColor = True
        '
        'Btn_credits
        '
        resources.ApplyResources(Me.Btn_credits, "Btn_credits")
        Me.Btn_credits.Name = "Btn_credits"
        Me.Btn_credits.UseVisualStyleBackColor = True
        '
        'Btn_MP_info
        '
        resources.ApplyResources(Me.Btn_MP_info, "Btn_MP_info")
        Me.Btn_MP_info.Name = "Btn_MP_info"
        Me.Btn_MP_info.UseVisualStyleBackColor = True
        '
        'Btn_GitHub
        '
        resources.ApplyResources(Me.Btn_GitHub, "Btn_GitHub")
        Me.Btn_GitHub.Name = "Btn_GitHub"
        Me.Btn_GitHub.UseVisualStyleBackColor = True
        '
        'Btn_Website
        '
        resources.ApplyResources(Me.Btn_Website, "Btn_Website")
        Me.Btn_Website.Name = "Btn_Website"
        Me.Btn_Website.UseVisualStyleBackColor = True
        '
        'Launcher
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackgroundImage = Global.FrangaCraftLauncher.My.Resources.Resources.body_bg1
        Me.Controls.Add(Me.ProgressBar1)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.RichTextBox1)
        Me.Controls.Add(Me.PremLoginBox)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.Name = "Launcher"
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage1.PerformLayout()
        Me.TabPage2.ResumeLayout(False)
        Me.TabPage2.PerformLayout()
        Me.PremLoginBox.ResumeLayout(False)
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
    Friend WithEvents PremPass As System.Windows.Forms.MaskedTextBox
    Friend WithEvents PremUser As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents PremLogin As System.Windows.Forms.Button
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents CheckBox1 As System.Windows.Forms.CheckBox
    Friend WithEvents Button2 As System.Windows.Forms.Button
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents TabPage2 As System.Windows.Forms.TabPage
    Friend WithEvents TxtBox_CrackedUser As System.Windows.Forms.TextBox
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents Button3 As System.Windows.Forms.Button
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents Button4 As System.Windows.Forms.Button
    Friend WithEvents PremLoginBox As System.Windows.Forms.GroupBox
    Friend WithEvents RichTextBox1 As System.Windows.Forms.RichTextBox
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents TxtBox_RAM As System.Windows.Forms.TextBox
    Friend WithEvents ProgressBar1 As System.Windows.Forms.ProgressBar
    Friend WithEvents Label9 As System.Windows.Forms.Label
    Friend WithEvents TextBox3 As System.Windows.Forms.TextBox
    Friend WithEvents Label8 As System.Windows.Forms.Label
    Friend WithEvents TextBox2 As System.Windows.Forms.TextBox
    Friend WithEvents Label7 As System.Windows.Forms.Label
    Friend WithEvents TextBox1 As System.Windows.Forms.TextBox
    Friend WithEvents Btn_save_settings As System.Windows.Forms.Button
    Friend WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Friend WithEvents Btn_about As System.Windows.Forms.Button
    Friend WithEvents Btn_credits As System.Windows.Forms.Button
    Friend WithEvents Btn_MP_info As System.Windows.Forms.Button
    Friend WithEvents Btn_GitHub As System.Windows.Forms.Button
    Friend WithEvents Btn_Website As System.Windows.Forms.Button

End Class
