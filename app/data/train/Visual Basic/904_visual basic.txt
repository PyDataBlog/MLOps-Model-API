<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class OverviewForm
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(OverviewForm))
        Me.RefreshTimer = New System.Windows.Forms.Timer(Me.components)
        Me.BackgroundPanel = New System.Windows.Forms.Panel()
        Me.WindowsUserListView = New System.Windows.Forms.ListView()
        Me.UsernameColumnHeader = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.DrivesListView = New System.Windows.Forms.ListView()
        Me.PartitionColumnHeader = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.TypeColumnHeader = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.FileSystemColumnHeader = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.TotalSpaceColumnHeader = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.FreeSpaceColumnHeader = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.NetworkGroupBox = New System.Windows.Forms.GroupBox()
        Me.InternalIPLabel = New System.Windows.Forms.Label()
        Me.ExternalIPLabel = New System.Windows.Forms.Label()
        Me.InternalIPLabelText = New System.Windows.Forms.Label()
        Me.ExternalIPLabelText = New System.Windows.Forms.Label()
        Me.BasicGroupBox = New System.Windows.Forms.GroupBox()
        Me.CPUUsageLabel = New System.Windows.Forms.Label()
        Me.CPUUsageLabelText = New System.Windows.Forms.Label()
        Me.AvailableMemoryLabel = New System.Windows.Forms.Label()
        Me.OperatingSystemLabel = New System.Windows.Forms.Label()
        Me.AvailableMemoryLabelText = New System.Windows.Forms.Label()
        Me.ComputerNameLabel = New System.Windows.Forms.Label()
        Me.OperatingSystemLabelText = New System.Windows.Forms.Label()
        Me.ComputerNameLabelText = New System.Windows.Forms.Label()
        Me.BottomPanel = New System.Windows.Forms.Panel()
        Me.CloseButton = New System.Windows.Forms.Button()
        Me.ExitButton = New System.Windows.Forms.Button()
        Me.FormTitleLabel = New System.Windows.Forms.Label()
        Me.BackgroundPanel.SuspendLayout()
        Me.NetworkGroupBox.SuspendLayout()
        Me.BasicGroupBox.SuspendLayout()
        Me.BottomPanel.SuspendLayout()
        Me.SuspendLayout()
        '
        'RefreshTimer
        '
        Me.RefreshTimer.Enabled = True
        Me.RefreshTimer.Interval = 1000
        '
        'BackgroundPanel
        '
        Me.BackgroundPanel.BackColor = System.Drawing.Color.White
        Me.BackgroundPanel.Controls.Add(Me.WindowsUserListView)
        Me.BackgroundPanel.Controls.Add(Me.DrivesListView)
        Me.BackgroundPanel.Controls.Add(Me.NetworkGroupBox)
        Me.BackgroundPanel.Controls.Add(Me.BasicGroupBox)
        Me.BackgroundPanel.Controls.Add(Me.BottomPanel)
        Me.BackgroundPanel.Location = New System.Drawing.Point(5, 34)
        Me.BackgroundPanel.Margin = New System.Windows.Forms.Padding(0)
        Me.BackgroundPanel.Name = "BackgroundPanel"
        Me.BackgroundPanel.Size = New System.Drawing.Size(605, 335)
        Me.BackgroundPanel.TabIndex = 7
        '
        'WindowsUserListView
        '
        Me.WindowsUserListView.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.UsernameColumnHeader})
        Me.WindowsUserListView.Location = New System.Drawing.Point(423, 116)
        Me.WindowsUserListView.Margin = New System.Windows.Forms.Padding(2, 2, 2, 2)
        Me.WindowsUserListView.Name = "WindowsUserListView"
        Me.WindowsUserListView.Size = New System.Drawing.Size(175, 172)
        Me.WindowsUserListView.TabIndex = 10
        Me.WindowsUserListView.UseCompatibleStateImageBehavior = False
        Me.WindowsUserListView.View = System.Windows.Forms.View.Details
        '
        'UsernameColumnHeader
        '
        Me.UsernameColumnHeader.Text = "Username"
        Me.UsernameColumnHeader.Width = 150
        '
        'DrivesListView
        '
        Me.DrivesListView.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.PartitionColumnHeader, Me.TypeColumnHeader, Me.FileSystemColumnHeader, Me.TotalSpaceColumnHeader, Me.FreeSpaceColumnHeader})
        Me.DrivesListView.FullRowSelect = True
        Me.DrivesListView.Location = New System.Drawing.Point(7, 116)
        Me.DrivesListView.Margin = New System.Windows.Forms.Padding(2, 2, 2, 2)
        Me.DrivesListView.MultiSelect = False
        Me.DrivesListView.Name = "DrivesListView"
        Me.DrivesListView.Size = New System.Drawing.Size(414, 172)
        Me.DrivesListView.TabIndex = 9
        Me.DrivesListView.UseCompatibleStateImageBehavior = False
        Me.DrivesListView.View = System.Windows.Forms.View.Details
        '
        'PartitionColumnHeader
        '
        Me.PartitionColumnHeader.Text = "Partition"
        Me.PartitionColumnHeader.Width = 80
        '
        'TypeColumnHeader
        '
        Me.TypeColumnHeader.Text = "Drive Type"
        Me.TypeColumnHeader.Width = 80
        '
        'FileSystemColumnHeader
        '
        Me.FileSystemColumnHeader.Text = "Filesystem"
        Me.FileSystemColumnHeader.Width = 80
        '
        'TotalSpaceColumnHeader
        '
        Me.TotalSpaceColumnHeader.Text = "Drive Size"
        Me.TotalSpaceColumnHeader.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        Me.TotalSpaceColumnHeader.Width = 80
        '
        'FreeSpaceColumnHeader
        '
        Me.FreeSpaceColumnHeader.Text = "Free Space"
        Me.FreeSpaceColumnHeader.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        Me.FreeSpaceColumnHeader.Width = 80
        '
        'NetworkGroupBox
        '
        Me.NetworkGroupBox.Controls.Add(Me.InternalIPLabel)
        Me.NetworkGroupBox.Controls.Add(Me.ExternalIPLabel)
        Me.NetworkGroupBox.Controls.Add(Me.InternalIPLabelText)
        Me.NetworkGroupBox.Controls.Add(Me.ExternalIPLabelText)
        Me.NetworkGroupBox.Font = New System.Drawing.Font("Tahoma", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.NetworkGroupBox.Location = New System.Drawing.Point(367, 3)
        Me.NetworkGroupBox.Name = "NetworkGroupBox"
        Me.NetworkGroupBox.Size = New System.Drawing.Size(231, 107)
        Me.NetworkGroupBox.TabIndex = 8
        Me.NetworkGroupBox.TabStop = False
        Me.NetworkGroupBox.Text = "Network Information"
        '
        'InternalIPLabel
        '
        Me.InternalIPLabel.AutoSize = True
        Me.InternalIPLabel.Font = New System.Drawing.Font("Tahoma", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.InternalIPLabel.ForeColor = System.Drawing.Color.FromArgb(CType(CType(51, Byte), Integer), CType(CType(51, Byte), Integer), CType(CType(51, Byte), Integer))
        Me.InternalIPLabel.Location = New System.Drawing.Point(5, 42)
        Me.InternalIPLabel.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.InternalIPLabel.Name = "InternalIPLabel"
        Me.InternalIPLabel.Size = New System.Drawing.Size(72, 16)
        Me.InternalIPLabel.TabIndex = 2
        Me.InternalIPLabel.Text = "Internal IP:"
        '
        'ExternalIPLabel
        '
        Me.ExternalIPLabel.AutoSize = True
        Me.ExternalIPLabel.Font = New System.Drawing.Font("Tahoma", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ExternalIPLabel.ForeColor = System.Drawing.Color.FromArgb(CType(CType(51, Byte), Integer), CType(CType(51, Byte), Integer), CType(CType(51, Byte), Integer))
        Me.ExternalIPLabel.Location = New System.Drawing.Point(5, 23)
        Me.ExternalIPLabel.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.ExternalIPLabel.Name = "ExternalIPLabel"
        Me.ExternalIPLabel.Size = New System.Drawing.Size(74, 16)
        Me.ExternalIPLabel.TabIndex = 0
        Me.ExternalIPLabel.Text = "External IP:"
        '
        'InternalIPLabelText
        '
        Me.InternalIPLabelText.AutoSize = True
        Me.InternalIPLabelText.Font = New System.Drawing.Font("Tahoma", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.InternalIPLabelText.ForeColor = System.Drawing.Color.OliveDrab
        Me.InternalIPLabelText.Location = New System.Drawing.Point(132, 42)
        Me.InternalIPLabelText.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.InternalIPLabelText.Name = "InternalIPLabelText"
        Me.InternalIPLabelText.Size = New System.Drawing.Size(83, 16)
        Me.InternalIPLabelText.TabIndex = 3
        Me.InternalIPLabelText.Text = "{ InternalIP }"
        '
        'ExternalIPLabelText
        '
        Me.ExternalIPLabelText.AutoSize = True
        Me.ExternalIPLabelText.Font = New System.Drawing.Font("Tahoma", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ExternalIPLabelText.ForeColor = System.Drawing.Color.OliveDrab
        Me.ExternalIPLabelText.Location = New System.Drawing.Point(132, 23)
        Me.ExternalIPLabelText.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.ExternalIPLabelText.Name = "ExternalIPLabelText"
        Me.ExternalIPLabelText.Size = New System.Drawing.Size(85, 16)
        Me.ExternalIPLabelText.TabIndex = 1
        Me.ExternalIPLabelText.Text = "{ ExternalIP }"
        '
        'BasicGroupBox
        '
        Me.BasicGroupBox.Controls.Add(Me.CPUUsageLabel)
        Me.BasicGroupBox.Controls.Add(Me.CPUUsageLabelText)
        Me.BasicGroupBox.Controls.Add(Me.AvailableMemoryLabel)
        Me.BasicGroupBox.Controls.Add(Me.OperatingSystemLabel)
        Me.BasicGroupBox.Controls.Add(Me.AvailableMemoryLabelText)
        Me.BasicGroupBox.Controls.Add(Me.ComputerNameLabel)
        Me.BasicGroupBox.Controls.Add(Me.OperatingSystemLabelText)
        Me.BasicGroupBox.Controls.Add(Me.ComputerNameLabelText)
        Me.BasicGroupBox.Font = New System.Drawing.Font("Tahoma", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.BasicGroupBox.Location = New System.Drawing.Point(7, 3)
        Me.BasicGroupBox.Name = "BasicGroupBox"
        Me.BasicGroupBox.Size = New System.Drawing.Size(355, 107)
        Me.BasicGroupBox.TabIndex = 7
        Me.BasicGroupBox.TabStop = False
        Me.BasicGroupBox.Text = "Basic Information"
        '
        'CPUUsageLabel
        '
        Me.CPUUsageLabel.AutoSize = True
        Me.CPUUsageLabel.Font = New System.Drawing.Font("Tahoma", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.CPUUsageLabel.ForeColor = System.Drawing.Color.FromArgb(CType(CType(51, Byte), Integer), CType(CType(51, Byte), Integer), CType(CType(51, Byte), Integer))
        Me.CPUUsageLabel.Location = New System.Drawing.Point(5, 80)
        Me.CPUUsageLabel.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.CPUUsageLabel.Name = "CPUUsageLabel"
        Me.CPUUsageLabel.Size = New System.Drawing.Size(75, 16)
        Me.CPUUsageLabel.TabIndex = 6
        Me.CPUUsageLabel.Text = "CPU Usage:"
        '
        'CPUUsageLabelText
        '
        Me.CPUUsageLabelText.AutoSize = True
        Me.CPUUsageLabelText.Font = New System.Drawing.Font("Tahoma", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.CPUUsageLabelText.ForeColor = System.Drawing.Color.OliveDrab
        Me.CPUUsageLabelText.Location = New System.Drawing.Point(132, 80)
        Me.CPUUsageLabelText.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.CPUUsageLabelText.Name = "CPUUsageLabelText"
        Me.CPUUsageLabelText.Size = New System.Drawing.Size(90, 16)
        Me.CPUUsageLabelText.TabIndex = 7
        Me.CPUUsageLabelText.Text = "{ CPU Usage }"
        '
        'AvailableMemoryLabel
        '
        Me.AvailableMemoryLabel.AutoSize = True
        Me.AvailableMemoryLabel.Font = New System.Drawing.Font("Tahoma", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.AvailableMemoryLabel.ForeColor = System.Drawing.Color.FromArgb(CType(CType(51, Byte), Integer), CType(CType(51, Byte), Integer), CType(CType(51, Byte), Integer))
        Me.AvailableMemoryLabel.Location = New System.Drawing.Point(5, 61)
        Me.AvailableMemoryLabel.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.AvailableMemoryLabel.Name = "AvailableMemoryLabel"
        Me.AvailableMemoryLabel.Size = New System.Drawing.Size(114, 16)
        Me.AvailableMemoryLabel.TabIndex = 4
        Me.AvailableMemoryLabel.Text = "Available Memory:"
        '
        'OperatingSystemLabel
        '
        Me.OperatingSystemLabel.AutoSize = True
        Me.OperatingSystemLabel.Font = New System.Drawing.Font("Tahoma", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.OperatingSystemLabel.ForeColor = System.Drawing.Color.FromArgb(CType(CType(51, Byte), Integer), CType(CType(51, Byte), Integer), CType(CType(51, Byte), Integer))
        Me.OperatingSystemLabel.Location = New System.Drawing.Point(5, 42)
        Me.OperatingSystemLabel.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.OperatingSystemLabel.Name = "OperatingSystemLabel"
        Me.OperatingSystemLabel.Size = New System.Drawing.Size(119, 16)
        Me.OperatingSystemLabel.TabIndex = 2
        Me.OperatingSystemLabel.Text = "Operating System: "
        '
        'AvailableMemoryLabelText
        '
        Me.AvailableMemoryLabelText.AutoSize = True
        Me.AvailableMemoryLabelText.Font = New System.Drawing.Font("Tahoma", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.AvailableMemoryLabelText.ForeColor = System.Drawing.Color.OliveDrab
        Me.AvailableMemoryLabelText.Location = New System.Drawing.Point(132, 61)
        Me.AvailableMemoryLabelText.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.AvailableMemoryLabelText.Name = "AvailableMemoryLabelText"
        Me.AvailableMemoryLabelText.Size = New System.Drawing.Size(155, 16)
        Me.AvailableMemoryLabelText.TabIndex = 5
        Me.AvailableMemoryLabelText.Text = "{ AvailableMemoryLabel }"
        '
        'ComputerNameLabel
        '
        Me.ComputerNameLabel.AutoSize = True
        Me.ComputerNameLabel.Font = New System.Drawing.Font("Tahoma", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ComputerNameLabel.ForeColor = System.Drawing.Color.FromArgb(CType(CType(51, Byte), Integer), CType(CType(51, Byte), Integer), CType(CType(51, Byte), Integer))
        Me.ComputerNameLabel.Location = New System.Drawing.Point(5, 23)
        Me.ComputerNameLabel.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.ComputerNameLabel.Name = "ComputerNameLabel"
        Me.ComputerNameLabel.Size = New System.Drawing.Size(96, 16)
        Me.ComputerNameLabel.TabIndex = 0
        Me.ComputerNameLabel.Text = "System Name: "
        '
        'OperatingSystemLabelText
        '
        Me.OperatingSystemLabelText.AutoSize = True
        Me.OperatingSystemLabelText.Font = New System.Drawing.Font("Tahoma", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.OperatingSystemLabelText.ForeColor = System.Drawing.Color.OliveDrab
        Me.OperatingSystemLabelText.Location = New System.Drawing.Point(132, 42)
        Me.OperatingSystemLabelText.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.OperatingSystemLabelText.Name = "OperatingSystemLabelText"
        Me.OperatingSystemLabelText.Size = New System.Drawing.Size(156, 16)
        Me.OperatingSystemLabelText.TabIndex = 3
        Me.OperatingSystemLabelText.Text = "{ OperatingSystemLabel }"
        '
        'ComputerNameLabelText
        '
        Me.ComputerNameLabelText.AutoSize = True
        Me.ComputerNameLabelText.Font = New System.Drawing.Font("Tahoma", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ComputerNameLabelText.ForeColor = System.Drawing.Color.OliveDrab
        Me.ComputerNameLabelText.Location = New System.Drawing.Point(132, 23)
        Me.ComputerNameLabelText.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.ComputerNameLabelText.Name = "ComputerNameLabelText"
        Me.ComputerNameLabelText.Size = New System.Drawing.Size(147, 16)
        Me.ComputerNameLabelText.TabIndex = 1
        Me.ComputerNameLabelText.Text = "{ ComputerNameLabel }"
        '
        'BottomPanel
        '
        Me.BottomPanel.BackColor = System.Drawing.Color.FromArgb(CType(CType(221, Byte), Integer), CType(CType(221, Byte), Integer), CType(CType(221, Byte), Integer))
        Me.BottomPanel.Controls.Add(Me.CloseButton)
        Me.BottomPanel.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.BottomPanel.Location = New System.Drawing.Point(0, 295)
        Me.BottomPanel.Name = "BottomPanel"
        Me.BottomPanel.Size = New System.Drawing.Size(605, 40)
        Me.BottomPanel.TabIndex = 6
        '
        'CloseButton
        '
        Me.CloseButton.BackColor = System.Drawing.Color.WhiteSmoke
        Me.CloseButton.FlatAppearance.BorderColor = System.Drawing.Color.Silver
        Me.CloseButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.CloseButton.Font = New System.Drawing.Font("Tahoma", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.CloseButton.Location = New System.Drawing.Point(539, 3)
        Me.CloseButton.Name = "CloseButton"
        Me.CloseButton.Size = New System.Drawing.Size(61, 31)
        Me.CloseButton.TabIndex = 0
        Me.CloseButton.Text = "Exit"
        Me.CloseButton.UseVisualStyleBackColor = False
        '
        'ExitButton
        '
        Me.ExitButton.BackColor = System.Drawing.Color.FromArgb(CType(CType(200, Byte), Integer), CType(CType(80, Byte), Integer), CType(CType(80, Byte), Integer))
        Me.ExitButton.FlatAppearance.BorderSize = 0
        Me.ExitButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.ExitButton.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ExitButton.ForeColor = System.Drawing.Color.White
        Me.ExitButton.Location = New System.Drawing.Point(567, 3)
        Me.ExitButton.Name = "ExitButton"
        Me.ExitButton.Size = New System.Drawing.Size(43, 29)
        Me.ExitButton.TabIndex = 8
        Me.ExitButton.Text = "X"
        Me.ExitButton.UseVisualStyleBackColor = False
        '
        'FormTitleLabel
        '
        Me.FormTitleLabel.Font = New System.Drawing.Font("Tahoma", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.FormTitleLabel.Location = New System.Drawing.Point(5, 5)
        Me.FormTitleLabel.Name = "FormTitleLabel"
        Me.FormTitleLabel.Size = New System.Drawing.Size(615, 23)
        Me.FormTitleLabel.TabIndex = 9
        Me.FormTitleLabel.Text = "{ Form Title }"
        Me.FormTitleLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'OverviewForm
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.FromArgb(CType(CType(52, Byte), Integer), CType(CType(131, Byte), Integer), CType(CType(211, Byte), Integer))
        Me.ClientSize = New System.Drawing.Size(615, 375)
        Me.Controls.Add(Me.ExitButton)
        Me.Controls.Add(Me.BackgroundPanel)
        Me.Controls.Add(Me.FormTitleLabel)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Margin = New System.Windows.Forms.Padding(2, 2, 2, 2)
        Me.Name = "OverviewForm"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Overview"
        Me.BackgroundPanel.ResumeLayout(False)
        Me.NetworkGroupBox.ResumeLayout(False)
        Me.NetworkGroupBox.PerformLayout()
        Me.BasicGroupBox.ResumeLayout(False)
        Me.BasicGroupBox.PerformLayout()
        Me.BottomPanel.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents RefreshTimer As System.Windows.Forms.Timer
    Friend WithEvents BackgroundPanel As System.Windows.Forms.Panel
    Friend WithEvents DrivesListView As System.Windows.Forms.ListView
    Friend WithEvents PartitionColumnHeader As System.Windows.Forms.ColumnHeader
    Friend WithEvents TypeColumnHeader As System.Windows.Forms.ColumnHeader
    Friend WithEvents FileSystemColumnHeader As System.Windows.Forms.ColumnHeader
    Friend WithEvents TotalSpaceColumnHeader As System.Windows.Forms.ColumnHeader
    Friend WithEvents FreeSpaceColumnHeader As System.Windows.Forms.ColumnHeader
    Friend WithEvents NetworkGroupBox As System.Windows.Forms.GroupBox
    Friend WithEvents InternalIPLabel As System.Windows.Forms.Label
    Friend WithEvents ExternalIPLabel As System.Windows.Forms.Label
    Friend WithEvents InternalIPLabelText As System.Windows.Forms.Label
    Friend WithEvents ExternalIPLabelText As System.Windows.Forms.Label
    Friend WithEvents BasicGroupBox As System.Windows.Forms.GroupBox
    Friend WithEvents CPUUsageLabel As System.Windows.Forms.Label
    Friend WithEvents CPUUsageLabelText As System.Windows.Forms.Label
    Friend WithEvents AvailableMemoryLabel As System.Windows.Forms.Label
    Friend WithEvents OperatingSystemLabel As System.Windows.Forms.Label
    Friend WithEvents AvailableMemoryLabelText As System.Windows.Forms.Label
    Friend WithEvents ComputerNameLabel As System.Windows.Forms.Label
    Friend WithEvents OperatingSystemLabelText As System.Windows.Forms.Label
    Friend WithEvents ComputerNameLabelText As System.Windows.Forms.Label
    Friend WithEvents BottomPanel As System.Windows.Forms.Panel
    Friend WithEvents CloseButton As System.Windows.Forms.Button
    Friend WithEvents ExitButton As System.Windows.Forms.Button
    Friend WithEvents FormTitleLabel As System.Windows.Forms.Label
    Friend WithEvents WindowsUserListView As System.Windows.Forms.ListView
    Friend WithEvents UsernameColumnHeader As System.Windows.Forms.ColumnHeader

End Class
