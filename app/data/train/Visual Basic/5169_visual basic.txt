<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frm_IPScanner
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frm_IPScanner))
        Me.dgv_Ansicht = New System.Windows.Forms.DataGridView()
        Me.btn_PingIP = New System.Windows.Forms.Button()
        Me.BottomToolStripPanel = New System.Windows.Forms.ToolStripPanel()
        Me.TopToolStripPanel = New System.Windows.Forms.ToolStripPanel()
        Me.RightToolStripPanel = New System.Windows.Forms.ToolStripPanel()
        Me.LeftToolStripPanel = New System.Windows.Forms.ToolStripPanel()
        Me.ContentPanel = New System.Windows.Forms.ToolStripContentPanel()
        Me.txt_Host = New System.Windows.Forms.TextBox()
        Me.StatusStrip1 = New System.Windows.Forms.StatusStrip()
        Me.tsl_Status = New System.Windows.Forms.ToolStripStatusLabel()
        Me.btn_Exit = New System.Windows.Forms.Button()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.btn_DGVLeeren = New System.Windows.Forms.Button()
        Me.btn_PingStop = New System.Windows.Forms.Button()
        Me.txt_IPEnde = New System.Windows.Forms.TextBox()
        Me.txt_IPStart = New System.Windows.Forms.TextBox()
        CType(Me.dgv_Ansicht, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.StatusStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'dgv_Ansicht
        '
        Me.dgv_Ansicht.AllowUserToAddRows = False
        Me.dgv_Ansicht.AllowUserToDeleteRows = False
        Me.dgv_Ansicht.AllowUserToResizeRows = False
        Me.dgv_Ansicht.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.dgv_Ansicht.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgv_Ansicht.Location = New System.Drawing.Point(12, 66)
        Me.dgv_Ansicht.Name = "dgv_Ansicht"
        Me.dgv_Ansicht.Size = New System.Drawing.Size(408, 460)
        Me.dgv_Ansicht.TabIndex = 0
        '
        'btn_PingIP
        '
        Me.btn_PingIP.Location = New System.Drawing.Point(184, 17)
        Me.btn_PingIP.Name = "btn_PingIP"
        Me.btn_PingIP.Size = New System.Drawing.Size(59, 36)
        Me.btn_PingIP.TabIndex = 1
        Me.btn_PingIP.Text = "&Ping"
        Me.btn_PingIP.UseVisualStyleBackColor = True
        '
        'BottomToolStripPanel
        '
        Me.BottomToolStripPanel.Location = New System.Drawing.Point(0, 0)
        Me.BottomToolStripPanel.Name = "BottomToolStripPanel"
        Me.BottomToolStripPanel.Orientation = System.Windows.Forms.Orientation.Horizontal
        Me.BottomToolStripPanel.RowMargin = New System.Windows.Forms.Padding(3, 0, 0, 0)
        Me.BottomToolStripPanel.Size = New System.Drawing.Size(0, 0)
        '
        'TopToolStripPanel
        '
        Me.TopToolStripPanel.Location = New System.Drawing.Point(0, 0)
        Me.TopToolStripPanel.Name = "TopToolStripPanel"
        Me.TopToolStripPanel.Orientation = System.Windows.Forms.Orientation.Horizontal
        Me.TopToolStripPanel.RowMargin = New System.Windows.Forms.Padding(3, 0, 0, 0)
        Me.TopToolStripPanel.Size = New System.Drawing.Size(0, 0)
        '
        'RightToolStripPanel
        '
        Me.RightToolStripPanel.Location = New System.Drawing.Point(0, 0)
        Me.RightToolStripPanel.Name = "RightToolStripPanel"
        Me.RightToolStripPanel.Orientation = System.Windows.Forms.Orientation.Horizontal
        Me.RightToolStripPanel.RowMargin = New System.Windows.Forms.Padding(3, 0, 0, 0)
        Me.RightToolStripPanel.Size = New System.Drawing.Size(0, 0)
        '
        'LeftToolStripPanel
        '
        Me.LeftToolStripPanel.Location = New System.Drawing.Point(0, 0)
        Me.LeftToolStripPanel.Name = "LeftToolStripPanel"
        Me.LeftToolStripPanel.Orientation = System.Windows.Forms.Orientation.Horizontal
        Me.LeftToolStripPanel.RowMargin = New System.Windows.Forms.Padding(3, 0, 0, 0)
        Me.LeftToolStripPanel.Size = New System.Drawing.Size(0, 0)
        '
        'ContentPanel
        '
        Me.ContentPanel.Size = New System.Drawing.Size(150, 150)
        '
        'txt_Host
        '
        Me.txt_Host.Location = New System.Drawing.Point(12, 40)
        Me.txt_Host.Name = "txt_Host"
        Me.txt_Host.Size = New System.Drawing.Size(166, 20)
        Me.txt_Host.TabIndex = 2
        '
        'StatusStrip1
        '
        Me.StatusStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.tsl_Status})
        Me.StatusStrip1.Location = New System.Drawing.Point(0, 583)
        Me.StatusStrip1.Name = "StatusStrip1"
        Me.StatusStrip1.Size = New System.Drawing.Size(428, 22)
        Me.StatusStrip1.TabIndex = 3
        Me.StatusStrip1.Text = "StatusStrip1"
        '
        'tsl_Status
        '
        Me.tsl_Status.Name = "tsl_Status"
        Me.tsl_Status.Size = New System.Drawing.Size(0, 17)
        '
        'btn_Exit
        '
        Me.btn_Exit.Location = New System.Drawing.Point(345, 532)
        Me.btn_Exit.Name = "btn_Exit"
        Me.btn_Exit.Size = New System.Drawing.Size(75, 48)
        Me.btn_Exit.TabIndex = 4
        Me.btn_Exit.Text = "&Schliessen"
        Me.btn_Exit.UseVisualStyleBackColor = True
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(9, 17)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(85, 13)
        Me.Label1.TabIndex = 5
        Me.Label1.Text = "Host / Netzwerk"
        '
        'btn_DGVLeeren
        '
        Me.btn_DGVLeeren.Location = New System.Drawing.Point(316, 17)
        Me.btn_DGVLeeren.Name = "btn_DGVLeeren"
        Me.btn_DGVLeeren.Size = New System.Drawing.Size(104, 36)
        Me.btn_DGVLeeren.TabIndex = 6
        Me.btn_DGVLeeren.Text = "Ergebnis &leeren"
        Me.btn_DGVLeeren.UseVisualStyleBackColor = True
        '
        'btn_PingStop
        '
        Me.btn_PingStop.Location = New System.Drawing.Point(249, 17)
        Me.btn_PingStop.Name = "btn_PingStop"
        Me.btn_PingStop.Size = New System.Drawing.Size(61, 36)
        Me.btn_PingStop.TabIndex = 7
        Me.btn_PingStop.Text = "&Stop"
        Me.btn_PingStop.UseVisualStyleBackColor = True
        '
        'txt_IPEnde
        '
        Me.txt_IPEnde.Location = New System.Drawing.Point(146, 14)
        Me.txt_IPEnde.Name = "txt_IPEnde"
        Me.txt_IPEnde.Size = New System.Drawing.Size(32, 20)
        Me.txt_IPEnde.TabIndex = 8
        Me.txt_IPEnde.Text = "254"
        '
        'txt_IPStart
        '
        Me.txt_IPStart.Location = New System.Drawing.Point(108, 14)
        Me.txt_IPStart.Name = "txt_IPStart"
        Me.txt_IPStart.Size = New System.Drawing.Size(32, 20)
        Me.txt_IPStart.TabIndex = 9
        Me.txt_IPStart.Text = "1"
        '
        'frm_IPScanner
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(428, 605)
        Me.Controls.Add(Me.txt_IPStart)
        Me.Controls.Add(Me.txt_IPEnde)
        Me.Controls.Add(Me.btn_PingStop)
        Me.Controls.Add(Me.btn_DGVLeeren)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.btn_Exit)
        Me.Controls.Add(Me.StatusStrip1)
        Me.Controls.Add(Me.txt_Host)
        Me.Controls.Add(Me.btn_PingIP)
        Me.Controls.Add(Me.dgv_Ansicht)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.Fixed3D
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.MaximizeBox = False
        Me.Name = "frm_IPScanner"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "LAN Explorer"
        CType(Me.dgv_Ansicht, System.ComponentModel.ISupportInitialize).EndInit()
        Me.StatusStrip1.ResumeLayout(False)
        Me.StatusStrip1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents dgv_Ansicht As System.Windows.Forms.DataGridView
    Friend WithEvents btn_PingIP As System.Windows.Forms.Button
    Friend WithEvents BottomToolStripPanel As System.Windows.Forms.ToolStripPanel
    Friend WithEvents TopToolStripPanel As System.Windows.Forms.ToolStripPanel
    Friend WithEvents RightToolStripPanel As System.Windows.Forms.ToolStripPanel
    Friend WithEvents LeftToolStripPanel As System.Windows.Forms.ToolStripPanel
    Friend WithEvents ContentPanel As System.Windows.Forms.ToolStripContentPanel
    Friend WithEvents txt_Host As System.Windows.Forms.TextBox
    Friend WithEvents StatusStrip1 As System.Windows.Forms.StatusStrip
    Friend WithEvents tsl_Status As System.Windows.Forms.ToolStripStatusLabel
    Friend WithEvents btn_Exit As System.Windows.Forms.Button
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents btn_DGVLeeren As System.Windows.Forms.Button
    Friend WithEvents btn_PingStop As System.Windows.Forms.Button
    Friend WithEvents txt_IPEnde As System.Windows.Forms.TextBox
    Friend WithEvents txt_IPStart As System.Windows.Forms.TextBox

End Class
