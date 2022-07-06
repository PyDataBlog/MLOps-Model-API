<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class ReportEtatStock
    Inherits System.Windows.Forms.Form

    'Form remplace la méthode Dispose pour nettoyer la liste des composants.
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

    'Requise par le Concepteur Windows Form
    Private components As System.ComponentModel.IContainer

    'REMARQUE : la procédure suivante est requise par le Concepteur Windows Form
    'Elle peut être modifiée à l'aide du Concepteur Windows Form.  
    'Ne la modifiez pas à l'aide de l'éditeur de code.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim ReportDataSource1 As Microsoft.Reporting.WinForms.ReportDataSource = New Microsoft.Reporting.WinForms.ReportDataSource()
        Me.StockActuelleBindingSource = New System.Windows.Forms.BindingSource(Me.components)
        Me.DataSetTables = New HG.DataSetTables()
        Me.Timer1 = New System.Windows.Forms.Timer(Me.components)
        Me.Report = New Microsoft.Reporting.WinForms.ReportViewer()
        CType(Me.StockActuelleBindingSource, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.DataSetTables, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'StockActuelleBindingSource
        '
        Me.StockActuelleBindingSource.DataMember = "StockActuelle"
        Me.StockActuelleBindingSource.DataSource = Me.DataSetTables
        '
        'DataSetTables
        '
        Me.DataSetTables.DataSetName = "DataSetTables"
        Me.DataSetTables.SchemaSerializationMode = System.Data.SchemaSerializationMode.IncludeSchema
        '
        'Timer1
        '
        '
        'Report
        '
        Me.Report.Dock = System.Windows.Forms.DockStyle.Fill
        ReportDataSource1.Name = "DataSetTables"
        ReportDataSource1.Value = Me.StockActuelleBindingSource
        Me.Report.LocalReport.DataSources.Add(ReportDataSource1)
        Me.Report.LocalReport.EnableExternalImages = True
        Me.Report.LocalReport.ReportEmbeddedResource = "HG.ReportStockActuelle.rdlc"
        Me.Report.Location = New System.Drawing.Point(0, 0)
        Me.Report.Name = "Report"
        Me.Report.Size = New System.Drawing.Size(916, 546)
        Me.Report.TabIndex = 0
        '
        'ReportEtatStock
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(7.0!, 18.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.Turquoise
        Me.ClientSize = New System.Drawing.Size(916, 546)
        Me.Controls.Add(Me.Report)
        Me.Font = New System.Drawing.Font("Open Sans", 10.0!)
        Me.Margin = New System.Windows.Forms.Padding(3, 4, 3, 4)
        Me.Name = "ReportEtatStock"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "ReportEtatStock"
        Me.WindowState = System.Windows.Forms.FormWindowState.Maximized
        CType(Me.StockActuelleBindingSource, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.DataSetTables, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents Timer1 As System.Windows.Forms.Timer
    Friend WithEvents Report As Microsoft.Reporting.WinForms.ReportViewer
    Friend WithEvents StockActuelleBindingSource As System.Windows.Forms.BindingSource
    Friend WithEvents DataSetTables As HG.DataSetTables
End Class
