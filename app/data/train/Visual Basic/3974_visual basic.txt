<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class pagamentos_form
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(pagamentos_form))
        Me.pagamentosQuitBtn = New System.Windows.Forms.Button()
        Me.Ncc_2015DataSet = New NCC2015.ncc_2015DataSet()
        Me.Loo_paymentBindingSource = New System.Windows.Forms.BindingSource(Me.components)
        Me.Loo_paymentTableAdapter = New NCC2015.ncc_2015DataSetTableAdapters.loo_paymentTableAdapter()
        Me.TableAdapterManager = New NCC2015.ncc_2015DataSetTableAdapters.TableAdapterManager()
        Me.Loo_paymentDataGridView = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn3 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn4 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        CType(Me.Ncc_2015DataSet, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.Loo_paymentBindingSource, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.Loo_paymentDataGridView, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'pagamentosQuitBtn
        '
        Me.pagamentosQuitBtn.BackColor = System.Drawing.SystemColors.Menu
        Me.pagamentosQuitBtn.Image = Global.NCC2015.My.Resources.Resources.Close_Window_24
        Me.pagamentosQuitBtn.Location = New System.Drawing.Point(686, 499)
        Me.pagamentosQuitBtn.Name = "pagamentosQuitBtn"
        Me.pagamentosQuitBtn.Size = New System.Drawing.Size(50, 50)
        Me.pagamentosQuitBtn.TabIndex = 20
        Me.pagamentosQuitBtn.UseVisualStyleBackColor = False
        '
        'Ncc_2015DataSet
        '
        Me.Ncc_2015DataSet.DataSetName = "ncc_2015DataSet"
        Me.Ncc_2015DataSet.SchemaSerializationMode = System.Data.SchemaSerializationMode.IncludeSchema
        '
        'Loo_paymentBindingSource
        '
        Me.Loo_paymentBindingSource.DataMember = "loo_payment"
        Me.Loo_paymentBindingSource.DataSource = Me.Ncc_2015DataSet
        '
        'Loo_paymentTableAdapter
        '
        Me.Loo_paymentTableAdapter.ClearBeforeFill = True
        '
        'TableAdapterManager
        '
        Me.TableAdapterManager.BackupDataSetBeforeUpdate = False
        Me.TableAdapterManager.loo_imageTableAdapter = Nothing
        Me.TableAdapterManager.loo_medicTableAdapter = Nothing
        Me.TableAdapterManager.loo_partner_classTableAdapter = Nothing
        Me.TableAdapterManager.loo_partner_timetableTableAdapter = Nothing
        Me.TableAdapterManager.loo_partnerTableAdapter = Nothing
        Me.TableAdapterManager.loo_paymentTableAdapter = Me.Loo_paymentTableAdapter
        Me.TableAdapterManager.loo_sysTableAdapter = Nothing
        Me.TableAdapterManager.loo_userTableAdapter = Nothing
        Me.TableAdapterManager.UpdateOrder = NCC2015.ncc_2015DataSetTableAdapters.TableAdapterManager.UpdateOrderOption.InsertUpdateDelete
        '
        'Loo_paymentDataGridView
        '
        Me.Loo_paymentDataGridView.AllowUserToAddRows = False
        Me.Loo_paymentDataGridView.AllowUserToDeleteRows = False
        Me.Loo_paymentDataGridView.AutoGenerateColumns = False
        Me.Loo_paymentDataGridView.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.Loo_paymentDataGridView.CellBorderStyle = System.Windows.Forms.DataGridViewCellBorderStyle.None
        Me.Loo_paymentDataGridView.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.Loo_paymentDataGridView.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn1, Me.DataGridViewTextBoxColumn2, Me.DataGridViewTextBoxColumn3, Me.DataGridViewTextBoxColumn4})
        Me.Loo_paymentDataGridView.DataSource = Me.Loo_paymentBindingSource
        Me.Loo_paymentDataGridView.Location = New System.Drawing.Point(0, 0)
        Me.Loo_paymentDataGridView.Name = "Loo_paymentDataGridView"
        Me.Loo_paymentDataGridView.ReadOnly = True
        Me.Loo_paymentDataGridView.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.AutoSizeToAllHeaders
        Me.Loo_paymentDataGridView.Size = New System.Drawing.Size(748, 493)
        Me.Loo_paymentDataGridView.TabIndex = 21
        '
        'DataGridViewTextBoxColumn1
        '
        Me.DataGridViewTextBoxColumn1.DataPropertyName = "payment_id"
        Me.DataGridViewTextBoxColumn1.HeaderText = "Número de Pagamento"
        Me.DataGridViewTextBoxColumn1.Name = "DataGridViewTextBoxColumn1"
        Me.DataGridViewTextBoxColumn1.ReadOnly = True
        '
        'DataGridViewTextBoxColumn2
        '
        Me.DataGridViewTextBoxColumn2.DataPropertyName = "payment_date"
        Me.DataGridViewTextBoxColumn2.HeaderText = "Data"
        Me.DataGridViewTextBoxColumn2.Name = "DataGridViewTextBoxColumn2"
        Me.DataGridViewTextBoxColumn2.ReadOnly = True
        '
        'DataGridViewTextBoxColumn3
        '
        Me.DataGridViewTextBoxColumn3.DataPropertyName = "payment_value"
        Me.DataGridViewTextBoxColumn3.HeaderText = "Valor (€)"
        Me.DataGridViewTextBoxColumn3.Name = "DataGridViewTextBoxColumn3"
        Me.DataGridViewTextBoxColumn3.ReadOnly = True
        '
        'DataGridViewTextBoxColumn4
        '
        Me.DataGridViewTextBoxColumn4.DataPropertyName = "payment_partner"
        Me.DataGridViewTextBoxColumn4.HeaderText = "Número de sócio"
        Me.DataGridViewTextBoxColumn4.Name = "DataGridViewTextBoxColumn4"
        Me.DataGridViewTextBoxColumn4.ReadOnly = True
        '
        'pagamentos_form
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.CornflowerBlue
        Me.ClientSize = New System.Drawing.Size(748, 561)
        Me.ControlBox = False
        Me.Controls.Add(Me.Loo_paymentDataGridView)
        Me.Controls.Add(Me.pagamentosQuitBtn)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "pagamentos_form"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Loomart - SocGest | Pagamentos"
        CType(Me.Ncc_2015DataSet, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.Loo_paymentBindingSource, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.Loo_paymentDataGridView, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents pagamentosQuitBtn As Button
    Friend WithEvents Ncc_2015DataSet As ncc_2015DataSet
    Friend WithEvents Loo_paymentBindingSource As BindingSource
    Friend WithEvents Loo_paymentTableAdapter As ncc_2015DataSetTableAdapters.loo_paymentTableAdapter
    Friend WithEvents TableAdapterManager As ncc_2015DataSetTableAdapters.TableAdapterManager
    Friend WithEvents Loo_paymentDataGridView As DataGridView
    Friend WithEvents DataGridViewTextBoxColumn1 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn2 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn3 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn4 As DataGridViewTextBoxColumn
End Class
