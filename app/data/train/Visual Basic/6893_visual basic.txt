<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmReport
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then
            components.Dispose()
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.rptView = New CrystalDecisions.Windows.Forms.CrystalReportViewer
        Me.SuspendLayout()
        '
        'rptView
        '
        Me.rptView.ActiveViewIndex = -1
        Me.rptView.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.rptView.DisplayGroupTree = False
        Me.rptView.Dock = System.Windows.Forms.DockStyle.Fill
        Me.rptView.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.rptView.Location = New System.Drawing.Point(0, 0)
        Me.rptView.Name = "rptView"
        'Me.rptView.SelectionFormula = ""
        Me.rptView.ShowRefreshButton = False
        Me.rptView.Size = New System.Drawing.Size(756, 608)
        Me.rptView.TabIndex = 0
        'Me.rptView.ViewTimeSelectionFormula = ""
        '
        'frmReport
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(756, 608)
        Me.Controls.Add(Me.rptView)
        Me.Name = "frmReport"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "frmReport"
        Me.WindowState = System.Windows.Forms.FormWindowState.Maximized
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents rptView As CrystalDecisions.Windows.Forms.CrystalReportViewer
End Class
