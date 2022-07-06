<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Terminal
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
        Me.terminaltext = New System.Windows.Forms.TextBox()
        Me.tmrfirstuse = New System.Windows.Forms.Timer(Me.components)
        Me.pnltop = New System.Windows.Forms.Panel()
        Me.lbtitle = New System.Windows.Forms.Label()
        Me.pnltop.SuspendLayout()
        Me.SuspendLayout()
        '
        'terminaltext
        '
        Me.terminaltext.BackColor = System.Drawing.Color.Black
        Me.terminaltext.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.terminaltext.Cursor = System.Windows.Forms.Cursors.IBeam
        Me.terminaltext.Dock = System.Windows.Forms.DockStyle.Fill
        Me.terminaltext.Font = New System.Drawing.Font("Courier New", 8.25!)
        Me.terminaltext.ForeColor = System.Drawing.Color.White
        Me.terminaltext.Location = New System.Drawing.Point(0, 30)
        Me.terminaltext.Multiline = True
        Me.terminaltext.Name = "terminaltext"
        Me.terminaltext.Size = New System.Drawing.Size(635, 388)
        Me.terminaltext.TabIndex = 0
        Me.terminaltext.Text = "ShiftOS Beta 1.0"
        '
        'tmrfirstuse
        '
        Me.tmrfirstuse.Interval = 5000
        '
        'pnltop
        '
        Me.pnltop.BackColor = System.Drawing.Color.Gray
        Me.pnltop.Controls.Add(Me.lbtitle)
        Me.pnltop.Dock = System.Windows.Forms.DockStyle.Top
        Me.pnltop.Font = New System.Drawing.Font("Courier New", 8.25!)
        Me.pnltop.ForeColor = System.Drawing.Color.White
        Me.pnltop.Location = New System.Drawing.Point(0, 0)
        Me.pnltop.Name = "pnltop"
        Me.pnltop.Size = New System.Drawing.Size(635, 30)
        Me.pnltop.TabIndex = 1
        '
        'lbtitle
        '
        Me.lbtitle.AutoSize = True
        Me.lbtitle.Location = New System.Drawing.Point(12, 9)
        Me.lbtitle.Name = "lbtitle"
        Me.lbtitle.Size = New System.Drawing.Size(63, 14)
        Me.lbtitle.TabIndex = 0
        Me.lbtitle.Text = "Terminal"
        '
        'Terminal
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(635, 418)
        Me.Controls.Add(Me.terminaltext)
        Me.Controls.Add(Me.pnltop)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
        Me.Name = "Terminal"
        Me.Text = "Terminal"
        Me.WindowState = System.Windows.Forms.FormWindowState.Maximized
        Me.pnltop.ResumeLayout(False)
        Me.pnltop.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents terminaltext As System.Windows.Forms.TextBox
    Friend WithEvents tmrfirstuse As System.Windows.Forms.Timer
    Friend WithEvents pnltop As System.Windows.Forms.Panel
    Friend WithEvents lbtitle As System.Windows.Forms.Label
End Class
