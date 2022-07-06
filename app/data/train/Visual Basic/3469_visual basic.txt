<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Criteria
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
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.cb_event = New System.Windows.Forms.ComboBox()
        Me.txt_score = New System.Windows.Forms.TextBox()
        Me.txt_description = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.dgv_criteria = New System.Windows.Forms.DataGridView()
        Me.id = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.event_name = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.description = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.score = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.btn_add = New System.Windows.Forms.Button()
        Me.btn_save = New System.Windows.Forms.Button()
        Me.btn_update = New System.Windows.Forms.Button()
        Me.btn_edit = New System.Windows.Forms.Button()
        Me.btn_delete = New System.Windows.Forms.Button()
        Me.btn_cancel = New System.Windows.Forms.Button()
        Me.Panel1.SuspendLayout()
        CType(Me.dgv_criteria, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Panel1
        '
        Me.Panel1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.Panel1.Controls.Add(Me.cb_event)
        Me.Panel1.Controls.Add(Me.txt_score)
        Me.Panel1.Controls.Add(Me.txt_description)
        Me.Panel1.Controls.Add(Me.Label1)
        Me.Panel1.Controls.Add(Me.Label4)
        Me.Panel1.Controls.Add(Me.Label2)
        Me.Panel1.Location = New System.Drawing.Point(12, 12)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(290, 209)
        Me.Panel1.TabIndex = 6
        '
        'cb_event
        '
        Me.cb_event.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cb_event.Enabled = False
        Me.cb_event.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.cb_event.FormattingEnabled = True
        Me.cb_event.Location = New System.Drawing.Point(7, 33)
        Me.cb_event.Name = "cb_event"
        Me.cb_event.Size = New System.Drawing.Size(268, 28)
        Me.cb_event.TabIndex = 3
        '
        'txt_score
        '
        Me.txt_score.Enabled = False
        Me.txt_score.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txt_score.Location = New System.Drawing.Point(7, 163)
        Me.txt_score.Name = "txt_score"
        Me.txt_score.Size = New System.Drawing.Size(268, 26)
        Me.txt_score.TabIndex = 1
        '
        'txt_description
        '
        Me.txt_description.Enabled = False
        Me.txt_description.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txt_description.Location = New System.Drawing.Point(7, 99)
        Me.txt_description.Name = "txt_description"
        Me.txt_description.Size = New System.Drawing.Size(268, 26)
        Me.txt_description.TabIndex = 1
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.Location = New System.Drawing.Point(3, 76)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(99, 20)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "Description:*"
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label4.Location = New System.Drawing.Point(3, 10)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(60, 20)
        Me.Label4.TabIndex = 0
        Me.Label4.Text = "Event:*"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label2.Location = New System.Drawing.Point(3, 140)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(61, 20)
        Me.Label2.TabIndex = 0
        Me.Label2.Text = "Score:*"
        '
        'dgv_criteria
        '
        Me.dgv_criteria.AllowUserToAddRows = False
        Me.dgv_criteria.AllowUserToDeleteRows = False
        Me.dgv_criteria.AllowUserToOrderColumns = True
        Me.dgv_criteria.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgv_criteria.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.id, Me.event_name, Me.description, Me.score})
        Me.dgv_criteria.Location = New System.Drawing.Point(319, 12)
        Me.dgv_criteria.Name = "dgv_criteria"
        Me.dgv_criteria.ReadOnly = True
        Me.dgv_criteria.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect
        Me.dgv_criteria.Size = New System.Drawing.Size(677, 483)
        Me.dgv_criteria.TabIndex = 7
        '
        'id
        '
        Me.id.HeaderText = "Id"
        Me.id.Name = "id"
        Me.id.ReadOnly = True
        Me.id.Visible = False
        '
        'event_name
        '
        Me.event_name.HeaderText = "Event"
        Me.event_name.Name = "event_name"
        Me.event_name.ReadOnly = True
        Me.event_name.Width = 150
        '
        'description
        '
        Me.description.HeaderText = "Description"
        Me.description.Name = "description"
        Me.description.ReadOnly = True
        Me.description.Width = 350
        '
        'score
        '
        Me.score.HeaderText = "Score"
        Me.score.Name = "score"
        Me.score.ReadOnly = True
        '
        'btn_add
        '
        Me.btn_add.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_add.Location = New System.Drawing.Point(364, 501)
        Me.btn_add.Name = "btn_add"
        Me.btn_add.Size = New System.Drawing.Size(93, 40)
        Me.btn_add.TabIndex = 18
        Me.btn_add.Text = "Add"
        Me.btn_add.UseVisualStyleBackColor = True
        '
        'btn_save
        '
        Me.btn_save.Enabled = False
        Me.btn_save.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_save.Location = New System.Drawing.Point(463, 501)
        Me.btn_save.Name = "btn_save"
        Me.btn_save.Size = New System.Drawing.Size(93, 40)
        Me.btn_save.TabIndex = 19
        Me.btn_save.Text = "Save"
        Me.btn_save.UseVisualStyleBackColor = True
        '
        'btn_update
        '
        Me.btn_update.Enabled = False
        Me.btn_update.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_update.Location = New System.Drawing.Point(661, 501)
        Me.btn_update.Name = "btn_update"
        Me.btn_update.Size = New System.Drawing.Size(93, 40)
        Me.btn_update.TabIndex = 20
        Me.btn_update.Text = "Update"
        Me.btn_update.UseVisualStyleBackColor = True
        '
        'btn_edit
        '
        Me.btn_edit.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_edit.Location = New System.Drawing.Point(562, 501)
        Me.btn_edit.Name = "btn_edit"
        Me.btn_edit.Size = New System.Drawing.Size(93, 40)
        Me.btn_edit.TabIndex = 21
        Me.btn_edit.Text = "Edit"
        Me.btn_edit.UseVisualStyleBackColor = True
        '
        'btn_delete
        '
        Me.btn_delete.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_delete.Location = New System.Drawing.Point(760, 501)
        Me.btn_delete.Name = "btn_delete"
        Me.btn_delete.Size = New System.Drawing.Size(93, 40)
        Me.btn_delete.TabIndex = 22
        Me.btn_delete.Text = "Delete"
        Me.btn_delete.UseVisualStyleBackColor = True
        '
        'btn_cancel
        '
        Me.btn_cancel.Enabled = False
        Me.btn_cancel.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_cancel.Location = New System.Drawing.Point(859, 501)
        Me.btn_cancel.Name = "btn_cancel"
        Me.btn_cancel.Size = New System.Drawing.Size(93, 40)
        Me.btn_cancel.TabIndex = 23
        Me.btn_cancel.Text = "Cancel"
        Me.btn_cancel.UseVisualStyleBackColor = True
        '
        'Criteria
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(1008, 729)
        Me.Controls.Add(Me.btn_add)
        Me.Controls.Add(Me.btn_save)
        Me.Controls.Add(Me.btn_update)
        Me.Controls.Add(Me.btn_edit)
        Me.Controls.Add(Me.btn_delete)
        Me.Controls.Add(Me.btn_cancel)
        Me.Controls.Add(Me.dgv_criteria)
        Me.Controls.Add(Me.Panel1)
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "Criteria"
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Criteria"
        Me.Panel1.ResumeLayout(False)
        Me.Panel1.PerformLayout()
        CType(Me.dgv_criteria, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents cb_event As System.Windows.Forms.ComboBox
    Friend WithEvents txt_score As System.Windows.Forms.TextBox
    Friend WithEvents txt_description As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents dgv_criteria As System.Windows.Forms.DataGridView
    Friend WithEvents btn_add As System.Windows.Forms.Button
    Friend WithEvents btn_save As System.Windows.Forms.Button
    Friend WithEvents btn_update As System.Windows.Forms.Button
    Friend WithEvents btn_edit As System.Windows.Forms.Button
    Friend WithEvents btn_delete As System.Windows.Forms.Button
    Friend WithEvents btn_cancel As System.Windows.Forms.Button
    Friend WithEvents id As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents event_name As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents description As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents score As System.Windows.Forms.DataGridViewTextBoxColumn
End Class
