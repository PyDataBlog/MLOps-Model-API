<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class adb_pushpull
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
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.btn_Push = New System.Windows.Forms.Button()
        Me.btn_PushBrowse = New System.Windows.Forms.Button()
        Me.lbl_Push1 = New System.Windows.Forms.Label()
        Me.lbl_Push2 = New System.Windows.Forms.Label()
        Me.txt_PushSDPath = New System.Windows.Forms.TextBox()
        Me.txt_PushPath = New System.Windows.Forms.TextBox()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.btn_pull = New System.Windows.Forms.Button()
        Me.btn_browse_pull = New System.Windows.Forms.Button()
        Me.lbl_pull1 = New System.Windows.Forms.Label()
        Me.lbl_pull2 = New System.Windows.Forms.Label()
        Me.txt_PullPathSD = New System.Windows.Forms.TextBox()
        Me.txt_PullPath = New System.Windows.Forms.TextBox()
        Me.txtx_PullFileName = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.TabPage2.SuspendLayout()
        Me.SuspendLayout()
        '
        'TabControl1
        '
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPage2)
        Me.TabControl1.Location = New System.Drawing.Point(13, 12)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        Me.TabControl1.Size = New System.Drawing.Size(369, 123)
        Me.TabControl1.TabIndex = 0
        '
        'TabPage1
        '
        Me.TabPage1.Controls.Add(Me.btn_Push)
        Me.TabPage1.Controls.Add(Me.btn_PushBrowse)
        Me.TabPage1.Controls.Add(Me.lbl_Push1)
        Me.TabPage1.Controls.Add(Me.lbl_Push2)
        Me.TabPage1.Controls.Add(Me.txt_PushSDPath)
        Me.TabPage1.Controls.Add(Me.txt_PushPath)
        Me.TabPage1.Location = New System.Drawing.Point(4, 22)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage1.Size = New System.Drawing.Size(361, 97)
        Me.TabPage1.TabIndex = 0
        Me.TabPage1.Text = "Push File"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'btn_Push
        '
        Me.btn_Push.Location = New System.Drawing.Point(259, 67)
        Me.btn_Push.Name = "btn_Push"
        Me.btn_Push.Size = New System.Drawing.Size(95, 20)
        Me.btn_Push.TabIndex = 5
        Me.btn_Push.Text = "Push"
        Me.btn_Push.UseVisualStyleBackColor = True
        '
        'btn_PushBrowse
        '
        Me.btn_PushBrowse.Location = New System.Drawing.Point(260, 28)
        Me.btn_PushBrowse.Name = "btn_PushBrowse"
        Me.btn_PushBrowse.Size = New System.Drawing.Size(95, 20)
        Me.btn_PushBrowse.TabIndex = 4
        Me.btn_PushBrowse.Text = "Browse..."
        Me.btn_PushBrowse.UseVisualStyleBackColor = True
        '
        'lbl_Push1
        '
        Me.lbl_Push1.AutoSize = True
        Me.lbl_Push1.Location = New System.Drawing.Point(3, 12)
        Me.lbl_Push1.Name = "lbl_Push1"
        Me.lbl_Push1.Size = New System.Drawing.Size(93, 13)
        Me.lbl_Push1.TabIndex = 3
        Me.lbl_Push1.Text = "File to Push Path :"
        '
        'lbl_Push2
        '
        Me.lbl_Push2.AutoSize = True
        Me.lbl_Push2.Location = New System.Drawing.Point(3, 51)
        Me.lbl_Push2.Name = "lbl_Push2"
        Me.lbl_Push2.Size = New System.Drawing.Size(158, 13)
        Me.lbl_Push2.TabIndex = 2
        Me.lbl_Push2.Text = "Path on Android (eg: /sdcard/) :"
        '
        'txt_PushSDPath
        '
        Me.txt_PushSDPath.Location = New System.Drawing.Point(6, 67)
        Me.txt_PushSDPath.Name = "txt_PushSDPath"
        Me.txt_PushSDPath.Size = New System.Drawing.Size(247, 20)
        Me.txt_PushSDPath.TabIndex = 1
        '
        'txt_PushPath
        '
        Me.txt_PushPath.Location = New System.Drawing.Point(6, 28)
        Me.txt_PushPath.Name = "txt_PushPath"
        Me.txt_PushPath.ReadOnly = True
        Me.txt_PushPath.Size = New System.Drawing.Size(247, 20)
        Me.txt_PushPath.TabIndex = 0
        '
        'TabPage2
        '
        Me.TabPage2.Controls.Add(Me.Label1)
        Me.TabPage2.Controls.Add(Me.txtx_PullFileName)
        Me.TabPage2.Controls.Add(Me.btn_pull)
        Me.TabPage2.Controls.Add(Me.btn_browse_pull)
        Me.TabPage2.Controls.Add(Me.lbl_pull1)
        Me.TabPage2.Controls.Add(Me.lbl_pull2)
        Me.TabPage2.Controls.Add(Me.txt_PullPathSD)
        Me.TabPage2.Controls.Add(Me.txt_PullPath)
        Me.TabPage2.Location = New System.Drawing.Point(4, 22)
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage2.Size = New System.Drawing.Size(361, 97)
        Me.TabPage2.TabIndex = 1
        Me.TabPage2.Text = "Pull File"
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'btn_pull
        '
        Me.btn_pull.Location = New System.Drawing.Point(260, 28)
        Me.btn_pull.Name = "btn_pull"
        Me.btn_pull.Size = New System.Drawing.Size(95, 20)
        Me.btn_pull.TabIndex = 11
        Me.btn_pull.Text = "Push"
        Me.btn_pull.UseVisualStyleBackColor = True
        '
        'btn_browse_pull
        '
        Me.btn_browse_pull.Location = New System.Drawing.Point(260, 67)
        Me.btn_browse_pull.Name = "btn_browse_pull"
        Me.btn_browse_pull.Size = New System.Drawing.Size(95, 20)
        Me.btn_browse_pull.TabIndex = 10
        Me.btn_browse_pull.Text = "Browse..."
        Me.btn_browse_pull.UseVisualStyleBackColor = True
        '
        'lbl_pull1
        '
        Me.lbl_pull1.AutoSize = True
        Me.lbl_pull1.Location = New System.Drawing.Point(3, 12)
        Me.lbl_pull1.Name = "lbl_pull1"
        Me.lbl_pull1.Size = New System.Drawing.Size(104, 13)
        Me.lbl_pull1.TabIndex = 9
        Me.lbl_pull1.Text = "Path (eg: /sdcard/) :"
        '
        'lbl_pull2
        '
        Me.lbl_pull2.AutoSize = True
        Me.lbl_pull2.Location = New System.Drawing.Point(3, 51)
        Me.lbl_pull2.Name = "lbl_pull2"
        Me.lbl_pull2.Size = New System.Drawing.Size(56, 13)
        Me.lbl_pull2.TabIndex = 8
        Me.lbl_pull2.Text = "Saved to :"
        '
        'txt_PullPathSD
        '
        Me.txt_PullPathSD.Location = New System.Drawing.Point(7, 28)
        Me.txt_PullPathSD.Name = "txt_PullPathSD"
        Me.txt_PullPathSD.Size = New System.Drawing.Size(122, 20)
        Me.txt_PullPathSD.TabIndex = 7
        '
        'txt_PullPath
        '
        Me.txt_PullPath.Location = New System.Drawing.Point(7, 67)
        Me.txt_PullPath.Name = "txt_PullPath"
        Me.txt_PullPath.ReadOnly = True
        Me.txt_PullPath.Size = New System.Drawing.Size(247, 20)
        Me.txt_PullPath.TabIndex = 6
        '
        'txtx_PullFileName
        '
        Me.txtx_PullFileName.Location = New System.Drawing.Point(135, 28)
        Me.txtx_PullFileName.Name = "txtx_PullFileName"
        Me.txtx_PullFileName.Size = New System.Drawing.Size(119, 20)
        Me.txtx_PullFileName.TabIndex = 12
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(132, 12)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(105, 13)
        Me.Label1.TabIndex = 13
        Me.Label1.Text = "File (eg: demo.mp4) :"
        '
        'adb_pushpull
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.SystemColors.ButtonHighlight
        Me.ClientSize = New System.Drawing.Size(394, 141)
        Me.Controls.Add(Me.TabControl1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "adb_pushpull"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Push And Pull File"
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage1.PerformLayout()
        Me.TabPage2.ResumeLayout(False)
        Me.TabPage2.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
    Friend WithEvents btn_Push As System.Windows.Forms.Button
    Friend WithEvents btn_PushBrowse As System.Windows.Forms.Button
    Friend WithEvents lbl_Push1 As System.Windows.Forms.Label
    Friend WithEvents lbl_Push2 As System.Windows.Forms.Label
    Friend WithEvents txt_PushSDPath As System.Windows.Forms.TextBox
    Friend WithEvents txt_PushPath As System.Windows.Forms.TextBox
    Friend WithEvents TabPage2 As System.Windows.Forms.TabPage
    Friend WithEvents btn_pull As System.Windows.Forms.Button
    Friend WithEvents btn_browse_pull As System.Windows.Forms.Button
    Friend WithEvents lbl_pull1 As System.Windows.Forms.Label
    Friend WithEvents lbl_pull2 As System.Windows.Forms.Label
    Friend WithEvents txt_PullPathSD As System.Windows.Forms.TextBox
    Friend WithEvents txt_PullPath As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents txtx_PullFileName As System.Windows.Forms.TextBox
End Class
