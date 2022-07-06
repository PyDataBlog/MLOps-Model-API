<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form1
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Form1))
        Me.TextBoxSource = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.CheckBoxWordWrap = New System.Windows.Forms.CheckBox()
        Me.SplitContainer1 = New System.Windows.Forms.SplitContainer()
        Me.TableLayoutPanel1 = New System.Windows.Forms.TableLayoutPanel()
        Me.TableLayoutPanel2 = New System.Windows.Forms.TableLayoutPanel()
        Me.RichTextBoxResult = New System.Windows.Forms.RichTextBox()
        Me.TableLayoutPanel3 = New System.Windows.Forms.TableLayoutPanel()
        Me.FlowLayoutPanelMain = New System.Windows.Forms.FlowLayoutPanel()
        Me.CheckBoxDumpInput = New System.Windows.Forms.CheckBox()
        Me.GroupBoxIn = New System.Windows.Forms.GroupBox()
        Me.TextBoxInput = New System.Windows.Forms.TextBox()
        Me.GroupBoxInstructions = New System.Windows.Forms.GroupBox()
        Me.CheckBoxColor = New System.Windows.Forms.CheckBox()
        Me.ButtonShowInstructions = New System.Windows.Forms.Button()
        Me.FlowLayoutPanelInstructions = New System.Windows.Forms.FlowLayoutPanel()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.GroupBoxOptions = New System.Windows.Forms.GroupBox()
        Me.FlowLayoutPanelOptions1 = New System.Windows.Forms.FlowLayoutPanel()
        CType(Me.SplitContainer1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SplitContainer1.Panel1.SuspendLayout()
        Me.SplitContainer1.Panel2.SuspendLayout()
        Me.SplitContainer1.SuspendLayout()
        Me.TableLayoutPanel1.SuspendLayout()
        Me.TableLayoutPanel2.SuspendLayout()
        Me.TableLayoutPanel3.SuspendLayout()
        Me.FlowLayoutPanelMain.SuspendLayout()
        Me.GroupBoxIn.SuspendLayout()
        Me.GroupBoxInstructions.SuspendLayout()
        Me.FlowLayoutPanelInstructions.SuspendLayout()
        Me.GroupBoxOptions.SuspendLayout()
        Me.FlowLayoutPanelOptions1.SuspendLayout()
        Me.SuspendLayout()
        '
        'TextBoxSource
        '
        Me.TextBoxSource.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TextBoxSource.Font = New System.Drawing.Font("Consolas", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.TextBoxSource.Location = New System.Drawing.Point(0, 22)
        Me.TextBoxSource.Margin = New System.Windows.Forms.Padding(0, 3, 0, 3)
        Me.TextBoxSource.Multiline = True
        Me.TextBoxSource.Name = "TextBoxSource"
        Me.TextBoxSource.Size = New System.Drawing.Size(267, 410)
        Me.TextBoxSource.TabIndex = 0
        Me.TextBoxSource.Text = resources.GetString("TextBoxSource.Text")
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(0, 3)
        Me.Label1.Margin = New System.Windows.Forms.Padding(0, 3, 0, 3)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(81, 13)
        Me.Label1.TabIndex = 1
        Me.Label1.Text = "VB.net Snippet:"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(0, 3)
        Me.Label2.Margin = New System.Windows.Forms.Padding(0, 3, 0, 3)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(45, 13)
        Me.Label2.TabIndex = 1
        Me.Label2.Text = "Results:"
        '
        'Button1
        '
        Me.Button1.Anchor = System.Windows.Forms.AnchorStyles.Top
        Me.Button1.Location = New System.Drawing.Point(13, 20)
        Me.Button1.Margin = New System.Windows.Forms.Padding(3, 20, 3, 20)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(75, 23)
        Me.Button1.TabIndex = 0
        Me.Button1.Text = "&Run"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'CheckBoxWordWrap
        '
        Me.CheckBoxWordWrap.AutoSize = True
        Me.CheckBoxWordWrap.Location = New System.Drawing.Point(3, 3)
        Me.CheckBoxWordWrap.Name = "CheckBoxWordWrap"
        Me.CheckBoxWordWrap.Size = New System.Drawing.Size(81, 17)
        Me.CheckBoxWordWrap.TabIndex = 1
        Me.CheckBoxWordWrap.Text = "&Word Wrap"
        Me.CheckBoxWordWrap.UseVisualStyleBackColor = True
        '
        'SplitContainer1
        '
        Me.SplitContainer1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SplitContainer1.Location = New System.Drawing.Point(3, 3)
        Me.SplitContainer1.Name = "SplitContainer1"
        '
        'SplitContainer1.Panel1
        '
        Me.SplitContainer1.Panel1.Controls.Add(Me.TableLayoutPanel1)
        Me.SplitContainer1.Panel1MinSize = 125
        '
        'SplitContainer1.Panel2
        '
        Me.SplitContainer1.Panel2.Controls.Add(Me.TableLayoutPanel2)
        Me.SplitContainer1.Panel2MinSize = 125
        Me.SplitContainer1.Size = New System.Drawing.Size(535, 435)
        Me.SplitContainer1.SplitterDistance = 267
        Me.SplitContainer1.TabIndex = 0
        '
        'TableLayoutPanel1
        '
        Me.TableLayoutPanel1.ColumnCount = 1
        Me.TableLayoutPanel1.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.TableLayoutPanel1.Controls.Add(Me.Label1, 0, 0)
        Me.TableLayoutPanel1.Controls.Add(Me.TextBoxSource, 0, 1)
        Me.TableLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TableLayoutPanel1.Location = New System.Drawing.Point(0, 0)
        Me.TableLayoutPanel1.Name = "TableLayoutPanel1"
        Me.TableLayoutPanel1.RowCount = 2
        Me.TableLayoutPanel1.RowStyles.Add(New System.Windows.Forms.RowStyle())
        Me.TableLayoutPanel1.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.TableLayoutPanel1.Size = New System.Drawing.Size(267, 435)
        Me.TableLayoutPanel1.TabIndex = 0
        '
        'TableLayoutPanel2
        '
        Me.TableLayoutPanel2.ColumnCount = 1
        Me.TableLayoutPanel2.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.TableLayoutPanel2.Controls.Add(Me.Label2, 0, 0)
        Me.TableLayoutPanel2.Controls.Add(Me.RichTextBoxResult, 0, 1)
        Me.TableLayoutPanel2.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TableLayoutPanel2.Location = New System.Drawing.Point(0, 0)
        Me.TableLayoutPanel2.Name = "TableLayoutPanel2"
        Me.TableLayoutPanel2.RowCount = 2
        Me.TableLayoutPanel2.RowStyles.Add(New System.Windows.Forms.RowStyle())
        Me.TableLayoutPanel2.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.TableLayoutPanel2.Size = New System.Drawing.Size(264, 435)
        Me.TableLayoutPanel2.TabIndex = 0
        '
        'RichTextBoxResult
        '
        Me.RichTextBoxResult.BackColor = System.Drawing.Color.Black
        Me.RichTextBoxResult.Dock = System.Windows.Forms.DockStyle.Fill
        Me.RichTextBoxResult.Font = New System.Drawing.Font("Consolas", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(136, Byte))
        Me.RichTextBoxResult.ForeColor = System.Drawing.Color.LightGray
        Me.RichTextBoxResult.Location = New System.Drawing.Point(0, 22)
        Me.RichTextBoxResult.Margin = New System.Windows.Forms.Padding(0, 3, 3, 3)
        Me.RichTextBoxResult.Name = "RichTextBoxResult"
        Me.RichTextBoxResult.Size = New System.Drawing.Size(261, 410)
        Me.RichTextBoxResult.TabIndex = 2
        Me.RichTextBoxResult.Text = ""
        '
        'TableLayoutPanel3
        '
        Me.TableLayoutPanel3.ColumnCount = 2
        Me.TableLayoutPanel3.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.TableLayoutPanel3.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle())
        Me.TableLayoutPanel3.Controls.Add(Me.SplitContainer1, 0, 0)
        Me.TableLayoutPanel3.Controls.Add(Me.FlowLayoutPanelMain, 1, 0)
        Me.TableLayoutPanel3.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TableLayoutPanel3.Location = New System.Drawing.Point(8, 8)
        Me.TableLayoutPanel3.Name = "TableLayoutPanel3"
        Me.TableLayoutPanel3.RowCount = 1
        Me.TableLayoutPanel3.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.TableLayoutPanel3.Size = New System.Drawing.Size(657, 441)
        Me.TableLayoutPanel3.TabIndex = 0
        '
        'FlowLayoutPanelMain
        '
        Me.FlowLayoutPanelMain.AutoSize = True
        Me.FlowLayoutPanelMain.Controls.Add(Me.Button1)
        Me.FlowLayoutPanelMain.Controls.Add(Me.GroupBoxOptions)
        Me.FlowLayoutPanelMain.Controls.Add(Me.GroupBoxIn)
        Me.FlowLayoutPanelMain.Controls.Add(Me.GroupBoxInstructions)
        Me.FlowLayoutPanelMain.FlowDirection = System.Windows.Forms.FlowDirection.TopDown
        Me.FlowLayoutPanelMain.Location = New System.Drawing.Point(552, 3)
        Me.FlowLayoutPanelMain.Margin = New System.Windows.Forms.Padding(11, 3, 3, 3)
        Me.FlowLayoutPanelMain.Name = "FlowLayoutPanelMain"
        Me.FlowLayoutPanelMain.Size = New System.Drawing.Size(102, 413)
        Me.FlowLayoutPanelMain.TabIndex = 1
        '
        'CheckBoxDumpInput
        '
        Me.CheckBoxDumpInput.AutoSize = True
        Me.CheckBoxDumpInput.Checked = True
        Me.CheckBoxDumpInput.CheckState = System.Windows.Forms.CheckState.Checked
        Me.CheckBoxDumpInput.Location = New System.Drawing.Point(3, 26)
        Me.CheckBoxDumpInput.Name = "CheckBoxDumpInput"
        Me.CheckBoxDumpInput.Size = New System.Drawing.Size(81, 17)
        Me.CheckBoxDumpInput.TabIndex = 5
        Me.CheckBoxDumpInput.Text = "&Dump Input"
        Me.CheckBoxDumpInput.UseVisualStyleBackColor = True
        '
        'GroupBoxIn
        '
        Me.GroupBoxIn.AutoSize = True
        Me.GroupBoxIn.Controls.Add(Me.TextBoxInput)
        Me.GroupBoxIn.Dock = System.Windows.Forms.DockStyle.Fill
        Me.GroupBoxIn.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(136, Byte))
        Me.GroupBoxIn.Location = New System.Drawing.Point(3, 177)
        Me.GroupBoxIn.Margin = New System.Windows.Forms.Padding(3, 20, 3, 3)
        Me.GroupBoxIn.Name = "GroupBoxIn"
        Me.GroupBoxIn.Padding = New System.Windows.Forms.Padding(6, 3, 6, 0)
        Me.GroupBoxIn.Size = New System.Drawing.Size(96, 92)
        Me.GroupBoxIn.TabIndex = 3
        Me.GroupBoxIn.TabStop = False
        Me.GroupBoxIn.Text = "Console In:"
        '
        'TextBoxInput
        '
        Me.TextBoxInput.Font = New System.Drawing.Font("Consolas", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.TextBoxInput.Location = New System.Drawing.Point(9, 20)
        Me.TextBoxInput.Margin = New System.Windows.Forms.Padding(3, 0, 3, 0)
        Me.TextBoxInput.Multiline = True
        Me.TextBoxInput.Name = "TextBoxInput"
        Me.TextBoxInput.Size = New System.Drawing.Size(78, 59)
        Me.TextBoxInput.TabIndex = 0
        '
        'GroupBoxInstructions
        '
        Me.GroupBoxInstructions.AutoSize = True
        Me.GroupBoxInstructions.Controls.Add(Me.FlowLayoutPanelInstructions)
        Me.GroupBoxInstructions.Dock = System.Windows.Forms.DockStyle.Fill
        Me.GroupBoxInstructions.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(136, Byte))
        Me.GroupBoxInstructions.Location = New System.Drawing.Point(3, 292)
        Me.GroupBoxInstructions.Margin = New System.Windows.Forms.Padding(3, 20, 3, 3)
        Me.GroupBoxInstructions.Name = "GroupBoxInstructions"
        Me.GroupBoxInstructions.Padding = New System.Windows.Forms.Padding(6, 3, 6, 7)
        Me.GroupBoxInstructions.Size = New System.Drawing.Size(96, 118)
        Me.GroupBoxInstructions.TabIndex = 4
        Me.GroupBoxInstructions.TabStop = False
        Me.GroupBoxInstructions.Text = "Usage:"
        '
        'CheckBoxColor
        '
        Me.CheckBoxColor.AutoSize = True
        Me.CheckBoxColor.Checked = True
        Me.CheckBoxColor.CheckState = System.Windows.Forms.CheckState.Checked
        Me.CheckBoxColor.Location = New System.Drawing.Point(3, 49)
        Me.CheckBoxColor.Name = "CheckBoxColor"
        Me.CheckBoxColor.Size = New System.Drawing.Size(55, 17)
        Me.CheckBoxColor.TabIndex = 6
        Me.CheckBoxColor.Text = "&Colors"
        Me.CheckBoxColor.UseVisualStyleBackColor = True
        '
        'Button2
        '
        Me.ButtonShowInstructions.Location = New System.Drawing.Point(0, 62)
        Me.ButtonShowInstructions.Margin = New System.Windows.Forms.Padding(0, 10, 0, 10)
        Me.ButtonShowInstructions.Name = "Button2"
        Me.ButtonShowInstructions.Size = New System.Drawing.Size(75, 23)
        Me.ButtonShowInstructions.TabIndex = 2
        Me.ButtonShowInstructions.Text = "Details"
        Me.ButtonShowInstructions.UseVisualStyleBackColor = True
        '
        'FlowLayoutPanelInstructions
        '
        Me.FlowLayoutPanelInstructions.AutoSize = True
        Me.FlowLayoutPanelInstructions.Controls.Add(Me.Label4)
        Me.FlowLayoutPanelInstructions.Controls.Add(Me.ButtonShowInstructions)
        Me.FlowLayoutPanelInstructions.Dock = System.Windows.Forms.DockStyle.Fill
        Me.FlowLayoutPanelInstructions.FlowDirection = System.Windows.Forms.FlowDirection.TopDown
        Me.FlowLayoutPanelInstructions.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(136, Byte))
        Me.FlowLayoutPanelInstructions.Location = New System.Drawing.Point(6, 16)
        Me.FlowLayoutPanelInstructions.Name = "FlowLayoutPanelInstructions"
        Me.FlowLayoutPanelInstructions.Size = New System.Drawing.Size(84, 95)
        Me.FlowLayoutPanelInstructions.TabIndex = 2
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Label4.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(136, Byte))
        Me.Label4.Location = New System.Drawing.Point(0, 0)
        Me.Label4.Margin = New System.Windows.Forms.Padding(0)
        Me.Label4.MaximumSize = New System.Drawing.Size(84, 0)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(84, 52)
        Me.Label4.TabIndex = 2
        Me.Label4.Text = "Enter your snippet and press ""Run"" to run the program."
        '
        'GroupBoxOptions
        '
        Me.GroupBoxOptions.AutoSize = True
        Me.GroupBoxOptions.Controls.Add(Me.FlowLayoutPanelOptions1)
        Me.GroupBoxOptions.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(136, Byte))
        Me.GroupBoxOptions.Location = New System.Drawing.Point(3, 66)
        Me.GroupBoxOptions.Name = "GroupBoxOptions"
        Me.GroupBoxOptions.Size = New System.Drawing.Size(93, 88)
        Me.GroupBoxOptions.TabIndex = 7
        Me.GroupBoxOptions.TabStop = False
        Me.GroupBoxOptions.Text = "Options:"
        '
        'FlowLayoutPanelOptions1
        '
        Me.FlowLayoutPanelOptions1.AutoSize = True
        Me.FlowLayoutPanelOptions1.Controls.Add(Me.CheckBoxWordWrap)
        Me.FlowLayoutPanelOptions1.Controls.Add(Me.CheckBoxDumpInput)
        Me.FlowLayoutPanelOptions1.Controls.Add(Me.CheckBoxColor)
        Me.FlowLayoutPanelOptions1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.FlowLayoutPanelOptions1.FlowDirection = System.Windows.Forms.FlowDirection.TopDown
        Me.FlowLayoutPanelOptions1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(136, Byte))
        Me.FlowLayoutPanelOptions1.Location = New System.Drawing.Point(3, 16)
        Me.FlowLayoutPanelOptions1.Name = "FlowLayoutPanelOptions1"
        Me.FlowLayoutPanelOptions1.Size = New System.Drawing.Size(87, 69)
        Me.FlowLayoutPanelOptions1.TabIndex = 0
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(673, 457)
        Me.Controls.Add(Me.TableLayoutPanel3)
        Me.Name = "Form1"
        Me.Padding = New System.Windows.Forms.Padding(8)
        Me.Text = "VB.net Snippet Runner"
        Me.SplitContainer1.Panel1.ResumeLayout(False)
        Me.SplitContainer1.Panel2.ResumeLayout(False)
        CType(Me.SplitContainer1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.SplitContainer1.ResumeLayout(False)
        Me.TableLayoutPanel1.ResumeLayout(False)
        Me.TableLayoutPanel1.PerformLayout()
        Me.TableLayoutPanel2.ResumeLayout(False)
        Me.TableLayoutPanel2.PerformLayout()
        Me.TableLayoutPanel3.ResumeLayout(False)
        Me.TableLayoutPanel3.PerformLayout()
        Me.FlowLayoutPanelMain.ResumeLayout(False)
        Me.FlowLayoutPanelMain.PerformLayout()
        Me.GroupBoxIn.ResumeLayout(False)
        Me.GroupBoxIn.PerformLayout()
        Me.GroupBoxInstructions.ResumeLayout(False)
        Me.GroupBoxInstructions.PerformLayout()
        Me.FlowLayoutPanelInstructions.ResumeLayout(False)
        Me.FlowLayoutPanelInstructions.PerformLayout()
        Me.GroupBoxOptions.ResumeLayout(False)
        Me.GroupBoxOptions.PerformLayout()
        Me.FlowLayoutPanelOptions1.ResumeLayout(False)
        Me.FlowLayoutPanelOptions1.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents TextBoxSource As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents CheckBoxWordWrap As System.Windows.Forms.CheckBox
    Friend WithEvents SplitContainer1 As System.Windows.Forms.SplitContainer
    Friend WithEvents TableLayoutPanel1 As System.Windows.Forms.TableLayoutPanel
    Friend WithEvents TableLayoutPanel2 As System.Windows.Forms.TableLayoutPanel
    Friend WithEvents TableLayoutPanel3 As System.Windows.Forms.TableLayoutPanel
    Friend WithEvents FlowLayoutPanelMain As System.Windows.Forms.FlowLayoutPanel
    Friend WithEvents GroupBoxIn As System.Windows.Forms.GroupBox
    Friend WithEvents RichTextBoxResult As System.Windows.Forms.RichTextBox
    Friend WithEvents TextBoxInput As System.Windows.Forms.TextBox
    Friend WithEvents GroupBoxInstructions As System.Windows.Forms.GroupBox
    Friend WithEvents CheckBoxDumpInput As System.Windows.Forms.CheckBox
    Friend WithEvents CheckBoxColor As System.Windows.Forms.CheckBox
    Friend WithEvents ButtonShowInstructions As System.Windows.Forms.Button
    Friend WithEvents FlowLayoutPanelInstructions As System.Windows.Forms.FlowLayoutPanel
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents GroupBoxOptions As System.Windows.Forms.GroupBox
    Friend WithEvents FlowLayoutPanelOptions1 As System.Windows.Forms.FlowLayoutPanel

End Class
