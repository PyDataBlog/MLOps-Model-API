<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class frmMain
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
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
        Me.components = New System.ComponentModel.Container()
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frmMain))
        Me.lbltimertext = New System.Windows.Forms.Label()
        Me.tmMain = New System.Windows.Forms.Timer(Me.components)
        Me.btnpause = New System.Windows.Forms.Button()
        Me.btnSplit = New System.Windows.Forms.Button()
        Me.txtdesc = New System.Windows.Forms.TextBox()
        Me.lbltime = New System.Windows.Forms.Label()
        Me.lblwktm = New System.Windows.Forms.Label()
        Me.btnMinusTime = New System.Windows.Forms.Button()
        Me.btnAddTime = New System.Windows.Forms.Button()
        Me.SplitsDataTableDataGridView = New System.Windows.Forms.DataGridView()
        Me.DataGridViewIDColumn = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewDescriptionColumn = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewStartTimeColumn = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewStopTimeColumn = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTimeWorkedColumn = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewRecordedColumn = New System.Windows.Forms.DataGridViewCheckBoxColumn()
        Me.DataGridViewColorColumn = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.cmsSplitsGridView = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.QuickHighlightToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.QuickHighlightToolStripMenuItem2 = New System.Windows.Forms.ToolStripMenuItem()
        Me.QuickHighlightToolStripMenuItem3 = New System.Windows.Forms.ToolStripMenuItem()
        Me.highlight_voidout = New System.Windows.Forms.ToolStripMenuItem()
        Me.RemoveHighlightToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator()
        Me.TotalGroupTimeToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SplitsDataTableBindingSource = New System.Windows.Forms.BindingSource(Me.components)
        Me.SplitsDataSet = New Timer__.SplitsDataSet()
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.FileToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem3 = New System.Windows.Forms.ToolStripMenuItem()
        Me.EditDBFileToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.OpenSplitLocationToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ExitToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SplitsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ImportToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ImportFromFileToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DeleteLastSplitToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ClearSplitsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.EndSplitToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem2 = New System.Windows.Forms.ToolStripSeparator()
        Me.TotalWorkTimeToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.TotalGroupTimeToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.HelpToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.HelpToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.AboutToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.btnSave = New System.Windows.Forms.Button()
        Me.ssBottomMain = New System.Windows.Forms.StatusStrip()
        Me.tsslFilePath = New System.Windows.Forms.ToolStripStatusLabel()
        Me.tsslactionstatus = New System.Windows.Forms.ToolStripStatusLabel()
        Me.tsslLastSaved = New System.Windows.Forms.ToolStripStatusLabel()
        Me.ToolTip = New System.Windows.Forms.ToolTip(Me.components)
        Me.btn_addtime10 = New System.Windows.Forms.Button()
        Me.btn_zerotime = New System.Windows.Forms.Button()
        Me.ColorPicker = New System.Windows.Forms.ColorDialog()
        Me.btnendsplit = New System.Windows.Forms.Button()
        Me.LastHighlightToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        CType(Me.SplitsDataTableDataGridView, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.cmsSplitsGridView.SuspendLayout()
        CType(Me.SplitsDataTableBindingSource, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.SplitsDataSet, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.MenuStrip1.SuspendLayout()
        Me.ssBottomMain.SuspendLayout()
        Me.SuspendLayout()
        '
        'lbltimertext
        '
        Me.lbltimertext.AutoSize = True
        Me.lbltimertext.Font = New System.Drawing.Font("Segoe UI", 15.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lbltimertext.Location = New System.Drawing.Point(5, 25)
        Me.lbltimertext.Name = "lbltimertext"
        Me.lbltimertext.Size = New System.Drawing.Size(212, 30)
        Me.lbltimertext.TabIndex = 0
        Me.lbltimertext.Text = "Click the Split Button!"
        Me.ToolTip.SetToolTip(Me.lbltimertext, "Current system time and date")
        '
        'tmMain
        '
        Me.tmMain.Interval = 1000
        '
        'btnpause
        '
        Me.btnpause.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnpause.Font = New System.Drawing.Font("Segoe UI", 21.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btnpause.Location = New System.Drawing.Point(474, 368)
        Me.btnpause.Name = "btnpause"
        Me.btnpause.Size = New System.Drawing.Size(144, 48)
        Me.btnpause.TabIndex = 2
        Me.btnpause.Text = "Pause"
        Me.ToolTip.SetToolTip(Me.btnpause, "Pause the Work Time counter")
        Me.btnpause.UseVisualStyleBackColor = True
        '
        'btnSplit
        '
        Me.btnSplit.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.btnSplit.Font = New System.Drawing.Font("Segoe UI", 21.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btnSplit.Location = New System.Drawing.Point(10, 368)
        Me.btnSplit.Name = "btnSplit"
        Me.btnSplit.Size = New System.Drawing.Size(348, 48)
        Me.btnSplit.TabIndex = 7
        Me.btnSplit.Text = "Split"
        Me.ToolTip.SetToolTip(Me.btnSplit, "Mark the current time in the split")
        Me.btnSplit.UseVisualStyleBackColor = True
        '
        'txtdesc
        '
        Me.txtdesc.Location = New System.Drawing.Point(10, 59)
        Me.txtdesc.Name = "txtdesc"
        Me.txtdesc.Size = New System.Drawing.Size(410, 20)
        Me.txtdesc.TabIndex = 8
        Me.ToolTip.SetToolTip(Me.txtdesc, "A description for the next split.")
        '
        'lbltime
        '
        Me.lbltime.AutoSize = True
        Me.lbltime.Font = New System.Drawing.Font("Segoe UI", 15.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lbltime.Location = New System.Drawing.Point(466, 25)
        Me.lbltime.Name = "lbltime"
        Me.lbltime.Size = New System.Drawing.Size(112, 30)
        Me.lbltime.TabIndex = 9
        Me.lbltime.Text = "Work Time"
        '
        'lblwktm
        '
        Me.lblwktm.AutoSize = True
        Me.lblwktm.Font = New System.Drawing.Font("Segoe UI", 15.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblwktm.Location = New System.Drawing.Point(475, 50)
        Me.lblwktm.Name = "lblwktm"
        Me.lblwktm.Size = New System.Drawing.Size(89, 30)
        Me.lblwktm.TabIndex = 10
        Me.lblwktm.Text = "00:00:00"
        '
        'btnMinusTime
        '
        Me.btnMinusTime.Font = New System.Drawing.Font("Segoe UI", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btnMinusTime.Location = New System.Drawing.Point(578, 54)
        Me.btnMinusTime.Name = "btnMinusTime"
        Me.btnMinusTime.Size = New System.Drawing.Size(40, 28)
        Me.btnMinusTime.TabIndex = 14
        Me.btnMinusTime.Text = "-"
        Me.ToolTip.SetToolTip(Me.btnMinusTime, "Subtracts one minute from the Work Time timer")
        Me.btnMinusTime.UseVisualStyleBackColor = True
        '
        'btnAddTime
        '
        Me.btnAddTime.Font = New System.Drawing.Font("Segoe UI", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btnAddTime.Location = New System.Drawing.Point(578, 27)
        Me.btnAddTime.Name = "btnAddTime"
        Me.btnAddTime.Size = New System.Drawing.Size(40, 28)
        Me.btnAddTime.TabIndex = 15
        Me.btnAddTime.Text = "+"
        Me.ToolTip.SetToolTip(Me.btnAddTime, "Adds one minute to the Work Time timer")
        Me.btnAddTime.UseVisualStyleBackColor = True
        '
        'SplitsDataTableDataGridView
        '
        Me.SplitsDataTableDataGridView.AllowUserToAddRows = False
        Me.SplitsDataTableDataGridView.AllowUserToDeleteRows = False
        Me.SplitsDataTableDataGridView.AllowUserToResizeColumns = False
        Me.SplitsDataTableDataGridView.AllowUserToResizeRows = False
        Me.SplitsDataTableDataGridView.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom), System.Windows.Forms.AnchorStyles)
        Me.SplitsDataTableDataGridView.AutoGenerateColumns = False
        Me.SplitsDataTableDataGridView.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.SplitsDataTableDataGridView.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewIDColumn, Me.DataGridViewDescriptionColumn, Me.DataGridViewStartTimeColumn, Me.DataGridViewStopTimeColumn, Me.DataGridViewTimeWorkedColumn, Me.DataGridViewRecordedColumn, Me.DataGridViewColorColumn})
        Me.SplitsDataTableDataGridView.ContextMenuStrip = Me.cmsSplitsGridView
        Me.SplitsDataTableDataGridView.DataSource = Me.SplitsDataTableBindingSource
        DataGridViewCellStyle1.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft
        DataGridViewCellStyle1.BackColor = System.Drawing.Color.White
        DataGridViewCellStyle1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        DataGridViewCellStyle1.ForeColor = System.Drawing.SystemColors.ControlText
        DataGridViewCellStyle1.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle1.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        DataGridViewCellStyle1.WrapMode = System.Windows.Forms.DataGridViewTriState.[False]
        Me.SplitsDataTableDataGridView.DefaultCellStyle = DataGridViewCellStyle1
        Me.SplitsDataTableDataGridView.Location = New System.Drawing.Point(10, 86)
        Me.SplitsDataTableDataGridView.MultiSelect = False
        Me.SplitsDataTableDataGridView.Name = "SplitsDataTableDataGridView"
        DataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft
        DataGridViewCellStyle2.BackColor = System.Drawing.SystemColors.Control
        DataGridViewCellStyle2.Font = New System.Drawing.Font("Segoe UI", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        DataGridViewCellStyle2.ForeColor = System.Drawing.SystemColors.WindowText
        DataGridViewCellStyle2.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle2.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        DataGridViewCellStyle2.WrapMode = System.Windows.Forms.DataGridViewTriState.[True]
        Me.SplitsDataTableDataGridView.RowHeadersDefaultCellStyle = DataGridViewCellStyle2
        Me.SplitsDataTableDataGridView.RowHeadersVisible = False
        Me.SplitsDataTableDataGridView.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        Me.SplitsDataTableDataGridView.Size = New System.Drawing.Size(608, 276)
        Me.SplitsDataTableDataGridView.TabIndex = 16
        '
        'DataGridViewIDColumn
        '
        Me.DataGridViewIDColumn.DataPropertyName = "ID"
        Me.DataGridViewIDColumn.HeaderText = "ID"
        Me.DataGridViewIDColumn.MinimumWidth = 30
        Me.DataGridViewIDColumn.Name = "DataGridViewIDColumn"
        Me.DataGridViewIDColumn.ReadOnly = True
        Me.DataGridViewIDColumn.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        Me.DataGridViewIDColumn.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        Me.DataGridViewIDColumn.Width = 30
        '
        'DataGridViewDescriptionColumn
        '
        Me.DataGridViewDescriptionColumn.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill
        Me.DataGridViewDescriptionColumn.DataPropertyName = "Description"
        Me.DataGridViewDescriptionColumn.FillWeight = 98.68021!
        Me.DataGridViewDescriptionColumn.HeaderText = "Description"
        Me.DataGridViewDescriptionColumn.Name = "DataGridViewDescriptionColumn"
        Me.DataGridViewDescriptionColumn.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        Me.DataGridViewDescriptionColumn.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'DataGridViewStartTimeColumn
        '
        Me.DataGridViewStartTimeColumn.DataPropertyName = "StartTime"
        Me.DataGridViewStartTimeColumn.HeaderText = "StartTime"
        Me.DataGridViewStartTimeColumn.MinimumWidth = 70
        Me.DataGridViewStartTimeColumn.Name = "DataGridViewStartTimeColumn"
        Me.DataGridViewStartTimeColumn.ReadOnly = True
        Me.DataGridViewStartTimeColumn.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        Me.DataGridViewStartTimeColumn.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        Me.DataGridViewStartTimeColumn.Width = 70
        '
        'DataGridViewStopTimeColumn
        '
        Me.DataGridViewStopTimeColumn.DataPropertyName = "StopTime"
        Me.DataGridViewStopTimeColumn.HeaderText = "StopTime"
        Me.DataGridViewStopTimeColumn.MinimumWidth = 70
        Me.DataGridViewStopTimeColumn.Name = "DataGridViewStopTimeColumn"
        Me.DataGridViewStopTimeColumn.ReadOnly = True
        Me.DataGridViewStopTimeColumn.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        Me.DataGridViewStopTimeColumn.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        Me.DataGridViewStopTimeColumn.Width = 70
        '
        'DataGridViewTimeWorkedColumn
        '
        Me.DataGridViewTimeWorkedColumn.DataPropertyName = "TimeWorked"
        Me.DataGridViewTimeWorkedColumn.HeaderText = "TimeWorked"
        Me.DataGridViewTimeWorkedColumn.MinimumWidth = 75
        Me.DataGridViewTimeWorkedColumn.Name = "DataGridViewTimeWorkedColumn"
        Me.DataGridViewTimeWorkedColumn.ReadOnly = True
        Me.DataGridViewTimeWorkedColumn.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        Me.DataGridViewTimeWorkedColumn.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        Me.DataGridViewTimeWorkedColumn.Width = 75
        '
        'DataGridViewRecordedColumn
        '
        Me.DataGridViewRecordedColumn.DataPropertyName = "Recorded"
        Me.DataGridViewRecordedColumn.HeaderText = "Recorded"
        Me.DataGridViewRecordedColumn.MinimumWidth = 60
        Me.DataGridViewRecordedColumn.Name = "DataGridViewRecordedColumn"
        Me.DataGridViewRecordedColumn.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        Me.DataGridViewRecordedColumn.Width = 60
        '
        'DataGridViewColorColumn
        '
        Me.DataGridViewColorColumn.DataPropertyName = "Color"
        Me.DataGridViewColorColumn.HeaderText = "Color"
        Me.DataGridViewColorColumn.Name = "DataGridViewColorColumn"
        Me.DataGridViewColorColumn.ReadOnly = True
        Me.DataGridViewColorColumn.Visible = False
        '
        'cmsSplitsGridView
        '
        Me.cmsSplitsGridView.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.LastHighlightToolStripMenuItem, Me.QuickHighlightToolStripMenuItem, Me.QuickHighlightToolStripMenuItem2, Me.QuickHighlightToolStripMenuItem3, Me.highlight_voidout, Me.RemoveHighlightToolStripMenuItem, Me.ToolStripSeparator2, Me.TotalGroupTimeToolStripMenuItem})
        Me.cmsSplitsGridView.Name = "cmsSplitsGridView"
        Me.cmsSplitsGridView.Size = New System.Drawing.Size(171, 164)
        '
        'QuickHighlightToolStripMenuItem
        '
        Me.QuickHighlightToolStripMenuItem.Name = "QuickHighlightToolStripMenuItem"
        Me.QuickHighlightToolStripMenuItem.Size = New System.Drawing.Size(170, 22)
        Me.QuickHighlightToolStripMenuItem.Text = "Quick Highlight 1"
        '
        'QuickHighlightToolStripMenuItem2
        '
        Me.QuickHighlightToolStripMenuItem2.Name = "QuickHighlightToolStripMenuItem2"
        Me.QuickHighlightToolStripMenuItem2.Size = New System.Drawing.Size(170, 22)
        Me.QuickHighlightToolStripMenuItem2.Text = "Quick Highlight 2"
        '
        'QuickHighlightToolStripMenuItem3
        '
        Me.QuickHighlightToolStripMenuItem3.Name = "QuickHighlightToolStripMenuItem3"
        Me.QuickHighlightToolStripMenuItem3.Size = New System.Drawing.Size(170, 22)
        Me.QuickHighlightToolStripMenuItem3.Text = "Quick Highlight 3"
        '
        'highlight_voidout
        '
        Me.highlight_voidout.BackColor = System.Drawing.Color.DimGray
        Me.highlight_voidout.Name = "highlight_voidout"
        Me.highlight_voidout.Size = New System.Drawing.Size(170, 22)
        Me.highlight_voidout.Text = "Void Out"
        '
        'RemoveHighlightToolStripMenuItem
        '
        Me.RemoveHighlightToolStripMenuItem.Name = "RemoveHighlightToolStripMenuItem"
        Me.RemoveHighlightToolStripMenuItem.Size = New System.Drawing.Size(170, 22)
        Me.RemoveHighlightToolStripMenuItem.Text = "Remove Highlight"
        '
        'ToolStripSeparator2
        '
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        Me.ToolStripSeparator2.Size = New System.Drawing.Size(167, 6)
        '
        'TotalGroupTimeToolStripMenuItem
        '
        Me.TotalGroupTimeToolStripMenuItem.Name = "TotalGroupTimeToolStripMenuItem"
        Me.TotalGroupTimeToolStripMenuItem.Size = New System.Drawing.Size(170, 22)
        Me.TotalGroupTimeToolStripMenuItem.Text = "Total Group Time"
        '
        'SplitsDataTableBindingSource
        '
        Me.SplitsDataTableBindingSource.DataMember = "SplitsDataTable"
        Me.SplitsDataTableBindingSource.DataSource = Me.SplitsDataSet
        '
        'SplitsDataSet
        '
        Me.SplitsDataSet.DataSetName = "SplitsDataSet"
        Me.SplitsDataSet.SchemaSerializationMode = System.Data.SchemaSerializationMode.IncludeSchema
        '
        'MenuStrip1
        '
        Me.MenuStrip1.BackColor = System.Drawing.SystemColors.Control
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.FileToolStripMenuItem, Me.SplitsToolStripMenuItem, Me.HelpToolStripMenuItem1})
        Me.MenuStrip1.Location = New System.Drawing.Point(0, 0)
        Me.MenuStrip1.Name = "MenuStrip1"
        Me.MenuStrip1.Size = New System.Drawing.Size(629, 24)
        Me.MenuStrip1.TabIndex = 17
        Me.MenuStrip1.Text = "MenuStrip1"
        '
        'FileToolStripMenuItem
        '
        Me.FileToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripMenuItem3, Me.EditDBFileToolStripMenuItem, Me.OpenSplitLocationToolStripMenuItem1, Me.ExitToolStripMenuItem})
        Me.FileToolStripMenuItem.Name = "FileToolStripMenuItem"
        Me.FileToolStripMenuItem.Size = New System.Drawing.Size(37, 20)
        Me.FileToolStripMenuItem.Text = "File"
        '
        'ToolStripMenuItem3
        '
        Me.ToolStripMenuItem3.Name = "ToolStripMenuItem3"
        Me.ToolStripMenuItem3.Size = New System.Drawing.Size(178, 22)
        Me.ToolStripMenuItem3.Text = "Settings"
        '
        'EditDBFileToolStripMenuItem
        '
        Me.EditDBFileToolStripMenuItem.Name = "EditDBFileToolStripMenuItem"
        Me.EditDBFileToolStripMenuItem.Size = New System.Drawing.Size(178, 22)
        Me.EditDBFileToolStripMenuItem.Text = "Edit DB File"
        '
        'OpenSplitLocationToolStripMenuItem1
        '
        Me.OpenSplitLocationToolStripMenuItem1.Name = "OpenSplitLocationToolStripMenuItem1"
        Me.OpenSplitLocationToolStripMenuItem1.Size = New System.Drawing.Size(178, 22)
        Me.OpenSplitLocationToolStripMenuItem1.Text = "Open Split Location"
        '
        'ExitToolStripMenuItem
        '
        Me.ExitToolStripMenuItem.Name = "ExitToolStripMenuItem"
        Me.ExitToolStripMenuItem.Size = New System.Drawing.Size(178, 22)
        Me.ExitToolStripMenuItem.Text = "Exit"
        '
        'SplitsToolStripMenuItem
        '
        Me.SplitsToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ImportToolStripMenuItem, Me.ImportFromFileToolStripMenuItem, Me.DeleteLastSplitToolStripMenuItem, Me.ClearSplitsToolStripMenuItem, Me.EndSplitToolStripMenuItem, Me.ToolStripMenuItem2, Me.TotalWorkTimeToolStripMenuItem, Me.TotalGroupTimeToolStripMenuItem1})
        Me.SplitsToolStripMenuItem.Name = "SplitsToolStripMenuItem"
        Me.SplitsToolStripMenuItem.Size = New System.Drawing.Size(47, 20)
        Me.SplitsToolStripMenuItem.Text = "Splits"
        '
        'ImportToolStripMenuItem
        '
        Me.ImportToolStripMenuItem.Name = "ImportToolStripMenuItem"
        Me.ImportToolStripMenuItem.Size = New System.Drawing.Size(174, 22)
        Me.ImportToolStripMenuItem.Text = "Import from Today"
        '
        'ImportFromFileToolStripMenuItem
        '
        Me.ImportFromFileToolStripMenuItem.Name = "ImportFromFileToolStripMenuItem"
        Me.ImportFromFileToolStripMenuItem.Size = New System.Drawing.Size(174, 22)
        Me.ImportFromFileToolStripMenuItem.Text = "Import from File..."
        '
        'DeleteLastSplitToolStripMenuItem
        '
        Me.DeleteLastSplitToolStripMenuItem.Name = "DeleteLastSplitToolStripMenuItem"
        Me.DeleteLastSplitToolStripMenuItem.Size = New System.Drawing.Size(174, 22)
        Me.DeleteLastSplitToolStripMenuItem.Text = "Delete last Row"
        '
        'ClearSplitsToolStripMenuItem
        '
        Me.ClearSplitsToolStripMenuItem.Name = "ClearSplitsToolStripMenuItem"
        Me.ClearSplitsToolStripMenuItem.Size = New System.Drawing.Size(174, 22)
        Me.ClearSplitsToolStripMenuItem.Text = "Clear Splits"
        '
        'EndSplitToolStripMenuItem
        '
        Me.EndSplitToolStripMenuItem.Name = "EndSplitToolStripMenuItem"
        Me.EndSplitToolStripMenuItem.Size = New System.Drawing.Size(174, 22)
        Me.EndSplitToolStripMenuItem.Text = "End Split"
        '
        'ToolStripMenuItem2
        '
        Me.ToolStripMenuItem2.Name = "ToolStripMenuItem2"
        Me.ToolStripMenuItem2.Size = New System.Drawing.Size(171, 6)
        '
        'TotalWorkTimeToolStripMenuItem
        '
        Me.TotalWorkTimeToolStripMenuItem.Name = "TotalWorkTimeToolStripMenuItem"
        Me.TotalWorkTimeToolStripMenuItem.Size = New System.Drawing.Size(174, 22)
        Me.TotalWorkTimeToolStripMenuItem.Text = "Total Work Time"
        '
        'TotalGroupTimeToolStripMenuItem1
        '
        Me.TotalGroupTimeToolStripMenuItem1.Name = "TotalGroupTimeToolStripMenuItem1"
        Me.TotalGroupTimeToolStripMenuItem1.Size = New System.Drawing.Size(174, 22)
        Me.TotalGroupTimeToolStripMenuItem1.Text = "Total Group Time"
        '
        'HelpToolStripMenuItem1
        '
        Me.HelpToolStripMenuItem1.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.HelpToolStripMenuItem, Me.AboutToolStripMenuItem1})
        Me.HelpToolStripMenuItem1.Name = "HelpToolStripMenuItem1"
        Me.HelpToolStripMenuItem1.Size = New System.Drawing.Size(44, 20)
        Me.HelpToolStripMenuItem1.Text = "Help"
        '
        'HelpToolStripMenuItem
        '
        Me.HelpToolStripMenuItem.Name = "HelpToolStripMenuItem"
        Me.HelpToolStripMenuItem.Size = New System.Drawing.Size(107, 22)
        Me.HelpToolStripMenuItem.Text = "Guide"
        '
        'AboutToolStripMenuItem1
        '
        Me.AboutToolStripMenuItem1.Name = "AboutToolStripMenuItem1"
        Me.AboutToolStripMenuItem1.Size = New System.Drawing.Size(107, 22)
        Me.AboutToolStripMenuItem1.Text = "About"
        '
        'btnSave
        '
        Me.btnSave.BackColor = System.Drawing.Color.Transparent
        Me.btnSave.Font = New System.Drawing.Font("Segoe UI", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btnSave.Image = Global.Timer__.My.Resources.Resources.save_bw
        Me.btnSave.ImageAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.btnSave.Location = New System.Drawing.Point(346, 27)
        Me.btnSave.Name = "btnSave"
        Me.btnSave.Size = New System.Drawing.Size(74, 30)
        Me.btnSave.TabIndex = 18
        Me.btnSave.Text = "Save"
        Me.btnSave.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.ToolTip.SetToolTip(Me.btnSave, "Save the current set of splits to file")
        Me.btnSave.UseVisualStyleBackColor = False
        '
        'ssBottomMain
        '
        Me.ssBottomMain.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.tsslFilePath, Me.tsslactionstatus, Me.tsslLastSaved})
        Me.ssBottomMain.Location = New System.Drawing.Point(0, 419)
        Me.ssBottomMain.Name = "ssBottomMain"
        Me.ssBottomMain.ShowItemToolTips = True
        Me.ssBottomMain.Size = New System.Drawing.Size(629, 22)
        Me.ssBottomMain.TabIndex = 19
        Me.ssBottomMain.Text = "ssBottom"
        '
        'tsslFilePath
        '
        Me.tsslFilePath.AutoSize = False
        Me.tsslFilePath.Name = "tsslFilePath"
        Me.tsslFilePath.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.tsslFilePath.Size = New System.Drawing.Size(375, 17)
        Me.tsslFilePath.Text = "File Path Goes Here"
        Me.tsslFilePath.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'tsslactionstatus
        '
        Me.tsslactionstatus.AutoSize = False
        Me.tsslactionstatus.BorderSides = CType((System.Windows.Forms.ToolStripStatusLabelBorderSides.Left Or System.Windows.Forms.ToolStripStatusLabelBorderSides.Right), System.Windows.Forms.ToolStripStatusLabelBorderSides)
        Me.tsslactionstatus.Name = "tsslactionstatus"
        Me.tsslactionstatus.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.tsslactionstatus.Size = New System.Drawing.Size(125, 17)
        Me.tsslactionstatus.Text = "No Status"
        '
        'tsslLastSaved
        '
        Me.tsslLastSaved.Name = "tsslLastSaved"
        Me.tsslLastSaved.Size = New System.Drawing.Size(114, 17)
        Me.tsslLastSaved.Spring = True
        Me.tsslLastSaved.Text = "File not yet saved"
        Me.tsslLastSaved.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'btn_addtime10
        '
        Me.btn_addtime10.Font = New System.Drawing.Font("Segoe UI", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_addtime10.Location = New System.Drawing.Point(426, 27)
        Me.btn_addtime10.Name = "btn_addtime10"
        Me.btn_addtime10.Size = New System.Drawing.Size(40, 28)
        Me.btn_addtime10.TabIndex = 20
        Me.btn_addtime10.Text = "10"
        Me.ToolTip.SetToolTip(Me.btn_addtime10, "Adds ten minutes to the Work Time timer")
        Me.btn_addtime10.UseVisualStyleBackColor = True
        '
        'btn_zerotime
        '
        Me.btn_zerotime.Font = New System.Drawing.Font("Segoe UI", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_zerotime.Location = New System.Drawing.Point(426, 54)
        Me.btn_zerotime.Name = "btn_zerotime"
        Me.btn_zerotime.Size = New System.Drawing.Size(40, 28)
        Me.btn_zerotime.TabIndex = 21
        Me.btn_zerotime.Text = "0"
        Me.ToolTip.SetToolTip(Me.btn_zerotime, "Resets the Work Time timer")
        Me.btn_zerotime.UseVisualStyleBackColor = True
        '
        'ColorPicker
        '
        Me.ColorPicker.Color = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer))
        Me.ColorPicker.SolidColorOnly = True
        '
        'btnendsplit
        '
        Me.btnendsplit.Anchor = System.Windows.Forms.AnchorStyles.Bottom
        Me.btnendsplit.Enabled = False
        Me.btnendsplit.Font = New System.Drawing.Font("Segoe UI", 21.75!)
        Me.btnendsplit.Location = New System.Drawing.Point(364, 368)
        Me.btnendsplit.Name = "btnendsplit"
        Me.btnendsplit.Size = New System.Drawing.Size(104, 48)
        Me.btnendsplit.TabIndex = 22
        Me.btnendsplit.Text = "End"
        Me.btnendsplit.UseVisualStyleBackColor = True
        '
        'LastHighlightToolStripMenuItem
        '
        Me.LastHighlightToolStripMenuItem.Name = "LastHighlightToolStripMenuItem"
        Me.LastHighlightToolStripMenuItem.Size = New System.Drawing.Size(170, 22)
        Me.LastHighlightToolStripMenuItem.Text = "Last Highlight"
        '
        'frmMain
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(629, 441)
        Me.Controls.Add(Me.btnendsplit)
        Me.Controls.Add(Me.btn_zerotime)
        Me.Controls.Add(Me.btn_addtime10)
        Me.Controls.Add(Me.ssBottomMain)
        Me.Controls.Add(Me.btnSave)
        Me.Controls.Add(Me.SplitsDataTableDataGridView)
        Me.Controls.Add(Me.btnAddTime)
        Me.Controls.Add(Me.btnMinusTime)
        Me.Controls.Add(Me.MenuStrip1)
        Me.Controls.Add(Me.lblwktm)
        Me.Controls.Add(Me.lbltime)
        Me.Controls.Add(Me.txtdesc)
        Me.Controls.Add(Me.btnSplit)
        Me.Controls.Add(Me.btnpause)
        Me.Controls.Add(Me.lbltimertext)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.MainMenuStrip = Me.MenuStrip1
        Me.MaximumSize = New System.Drawing.Size(645, 1080)
        Me.MinimumSize = New System.Drawing.Size(645, 480)
        Me.Name = "frmMain"
        Me.Text = "Timer++"
        CType(Me.SplitsDataTableDataGridView, System.ComponentModel.ISupportInitialize).EndInit()
        Me.cmsSplitsGridView.ResumeLayout(False)
        CType(Me.SplitsDataTableBindingSource, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.SplitsDataSet, System.ComponentModel.ISupportInitialize).EndInit()
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        Me.ssBottomMain.ResumeLayout(False)
        Me.ssBottomMain.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents lbltimertext As Label
    Friend WithEvents tmMain As Timer
    Friend WithEvents btnpause As Button
    Friend WithEvents btnSplit As Button
    Friend WithEvents txtdesc As TextBox
    Friend WithEvents lbltime As Label
    Friend WithEvents lblwktm As Label
    Friend WithEvents btnMinusTime As Button
    Friend WithEvents btnAddTime As Button
    Friend WithEvents SplitsDataSet As SplitsDataSet
    Friend WithEvents SplitsDataTableBindingSource As BindingSource
    Friend WithEvents SplitsDataTableDataGridView As DataGridView
    Friend WithEvents MenuStrip1 As MenuStrip
    Friend WithEvents FileToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ExitToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents SplitsToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ImportToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ClearSplitsToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem2 As ToolStripSeparator
    Friend WithEvents ImportFromFileToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents DeleteLastSplitToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents btnSave As Button
    Friend WithEvents ssBottomMain As StatusStrip
    Friend WithEvents tsslFilePath As ToolStripStatusLabel
    Friend WithEvents tsslLastSaved As ToolStripStatusLabel
    Friend WithEvents cmsSplitsGridView As ContextMenuStrip
    Friend WithEvents RemoveHighlightToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ToolTip As ToolTip
    Friend WithEvents ToolStripMenuItem3 As ToolStripMenuItem
    Friend WithEvents highlight_voidout As ToolStripMenuItem
    Friend WithEvents TotalWorkTimeToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents HelpToolStripMenuItem1 As ToolStripMenuItem
    Friend WithEvents HelpToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents AboutToolStripMenuItem1 As ToolStripMenuItem
    Friend WithEvents btn_addtime10 As Button
    Friend WithEvents btn_zerotime As Button
    Friend WithEvents ToolStripSeparator2 As ToolStripSeparator
    Friend WithEvents TotalGroupTimeToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents EndSplitToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents tsslactionstatus As ToolStripStatusLabel
    Friend WithEvents ColorPicker As ColorDialog
    Friend WithEvents DataGridViewIDColumn As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewDescriptionColumn As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewStartTimeColumn As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewStopTimeColumn As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTimeWorkedColumn As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewRecordedColumn As DataGridViewCheckBoxColumn
    Friend WithEvents DataGridViewColorColumn As DataGridViewTextBoxColumn
    Friend WithEvents QuickHighlightToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents btnendsplit As Button
    Friend WithEvents EditDBFileToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents OpenSplitLocationToolStripMenuItem1 As ToolStripMenuItem
    Friend WithEvents TotalGroupTimeToolStripMenuItem1 As ToolStripMenuItem
    Friend WithEvents QuickHighlightToolStripMenuItem2 As ToolStripMenuItem
    Friend WithEvents QuickHighlightToolStripMenuItem3 As ToolStripMenuItem
    Friend WithEvents LastHighlightToolStripMenuItem As ToolStripMenuItem
End Class
