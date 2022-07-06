<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class RomsContainerControl
    Inherits System.Windows.Forms.UserControl

    'UserControl overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
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
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Me.MainTableLayoutPanel = New System.Windows.Forms.TableLayoutPanel()
        Me.NavigationFlowLayoutPanel = New System.Windows.Forms.FlowLayoutPanel()
        Me.ButtonNext = New System.Windows.Forms.Button()
        Me.ButtonPrevious = New System.Windows.Forms.Button()
        Me.StepsProgressBar = New System.Windows.Forms.ProgressBar()
        Me.HeaderLabel = New System.Windows.Forms.Label()
        Me.MainTableLayoutPanel.SuspendLayout()
        Me.NavigationFlowLayoutPanel.SuspendLayout()
        Me.SuspendLayout()
        '
        'MainTableLayoutPanel
        '
        Me.MainTableLayoutPanel.ColumnCount = 1
        Me.MainTableLayoutPanel.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.MainTableLayoutPanel.Controls.Add(Me.NavigationFlowLayoutPanel, 0, 3)
        Me.MainTableLayoutPanel.Controls.Add(Me.StepsProgressBar, 0, 1)
        Me.MainTableLayoutPanel.Controls.Add(Me.HeaderLabel, 0, 0)
        Me.MainTableLayoutPanel.Dock = System.Windows.Forms.DockStyle.Fill
        Me.MainTableLayoutPanel.Location = New System.Drawing.Point(0, 0)
        Me.MainTableLayoutPanel.Margin = New System.Windows.Forms.Padding(0)
        Me.MainTableLayoutPanel.Name = "MainTableLayoutPanel"
        Me.MainTableLayoutPanel.RowCount = 4
        Me.MainTableLayoutPanel.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 25.0!))
        Me.MainTableLayoutPanel.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 5.0!))
        Me.MainTableLayoutPanel.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.MainTableLayoutPanel.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 30.0!))
        Me.MainTableLayoutPanel.Size = New System.Drawing.Size(878, 787)
        Me.MainTableLayoutPanel.TabIndex = 0
        '
        'NavigationFlowLayoutPanel
        '
        Me.NavigationFlowLayoutPanel.Controls.Add(Me.ButtonNext)
        Me.NavigationFlowLayoutPanel.Controls.Add(Me.ButtonPrevious)
        Me.NavigationFlowLayoutPanel.Dock = System.Windows.Forms.DockStyle.Fill
        Me.NavigationFlowLayoutPanel.FlowDirection = System.Windows.Forms.FlowDirection.RightToLeft
        Me.NavigationFlowLayoutPanel.Location = New System.Drawing.Point(0, 757)
        Me.NavigationFlowLayoutPanel.Margin = New System.Windows.Forms.Padding(0)
        Me.NavigationFlowLayoutPanel.Name = "NavigationFlowLayoutPanel"
        Me.NavigationFlowLayoutPanel.Size = New System.Drawing.Size(878, 30)
        Me.NavigationFlowLayoutPanel.TabIndex = 0
        '
        'ButtonNext
        '
        Me.ButtonNext.Font = New System.Drawing.Font("Ubuntu", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ButtonNext.Location = New System.Drawing.Point(800, 3)
        Me.ButtonNext.Name = "ButtonNext"
        Me.ButtonNext.Size = New System.Drawing.Size(75, 23)
        Me.ButtonNext.TabIndex = 3
        Me.ButtonNext.Text = "Next"
        Me.ButtonNext.UseVisualStyleBackColor = True
        '
        'ButtonPrevious
        '
        Me.ButtonPrevious.Font = New System.Drawing.Font("Ubuntu", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ButtonPrevious.Location = New System.Drawing.Point(719, 3)
        Me.ButtonPrevious.Name = "ButtonPrevious"
        Me.ButtonPrevious.Size = New System.Drawing.Size(75, 23)
        Me.ButtonPrevious.TabIndex = 2
        Me.ButtonPrevious.Text = "Previous"
        Me.ButtonPrevious.UseVisualStyleBackColor = True
        Me.ButtonPrevious.Visible = False
        '
        'StepsProgressBar
        '
        Me.StepsProgressBar.Dock = System.Windows.Forms.DockStyle.Fill
        Me.StepsProgressBar.Location = New System.Drawing.Point(0, 25)
        Me.StepsProgressBar.Margin = New System.Windows.Forms.Padding(0)
        Me.StepsProgressBar.Maximum = 13
        Me.StepsProgressBar.Name = "StepsProgressBar"
        Me.StepsProgressBar.Size = New System.Drawing.Size(878, 5)
        Me.StepsProgressBar.Style = System.Windows.Forms.ProgressBarStyle.Continuous
        Me.StepsProgressBar.TabIndex = 1
        '
        'HeaderLabel
        '
        Me.HeaderLabel.BackColor = System.Drawing.Color.Gainsboro
        Me.HeaderLabel.Dock = System.Windows.Forms.DockStyle.Fill
        Me.HeaderLabel.Font = New System.Drawing.Font("Ubuntu Light", 9.749999!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.HeaderLabel.ForeColor = System.Drawing.SystemColors.ControlText
        Me.HeaderLabel.Location = New System.Drawing.Point(0, 0)
        Me.HeaderLabel.Margin = New System.Windows.Forms.Padding(0)
        Me.HeaderLabel.Name = "HeaderLabel"
        Me.HeaderLabel.Padding = New System.Windows.Forms.Padding(15, 0, 15, 0)
        Me.HeaderLabel.Size = New System.Drawing.Size(878, 25)
        Me.HeaderLabel.TabIndex = 0
        Me.HeaderLabel.Text = "Roms Scraping"
        Me.HeaderLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'RomsContainerControl
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.White
        Me.Controls.Add(Me.MainTableLayoutPanel)
        Me.Margin = New System.Windows.Forms.Padding(0)
        Me.Name = "RomsContainerControl"
        Me.Size = New System.Drawing.Size(878, 787)
        Me.MainTableLayoutPanel.ResumeLayout(False)
        Me.NavigationFlowLayoutPanel.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents MainTableLayoutPanel As TableLayoutPanel
    Friend WithEvents NavigationFlowLayoutPanel As FlowLayoutPanel
    Friend WithEvents ButtonNext As Button
    Friend WithEvents ButtonPrevious As Button
    Friend WithEvents StepsProgressBar As ProgressBar
    Friend WithEvents HeaderLabel As Label
End Class