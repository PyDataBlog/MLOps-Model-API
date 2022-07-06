<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class menu
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
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.RegistrosToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PasienteToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.MedicamentoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.REceToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.FacturasToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PasienteToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.NuevaRecetaToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.HelpToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.AboutToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.MenuStrip1.SuspendLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'MenuStrip1
        '
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.RegistrosToolStripMenuItem, Me.PasienteToolStripMenuItem1, Me.HelpToolStripMenuItem})
        Me.MenuStrip1.Location = New System.Drawing.Point(0, 0)
        Me.MenuStrip1.Name = "MenuStrip1"
        Me.MenuStrip1.RenderMode = System.Windows.Forms.ToolStripRenderMode.Professional
        Me.MenuStrip1.Size = New System.Drawing.Size(684, 24)
        Me.MenuStrip1.TabIndex = 0
        Me.MenuStrip1.Text = "MenuStrip1"
        '
        'RegistrosToolStripMenuItem
        '
        Me.RegistrosToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.PasienteToolStripMenuItem, Me.MedicamentoToolStripMenuItem, Me.REceToolStripMenuItem, Me.FacturasToolStripMenuItem})
        Me.RegistrosToolStripMenuItem.Name = "RegistrosToolStripMenuItem"
        Me.RegistrosToolStripMenuItem.Size = New System.Drawing.Size(67, 20)
        Me.RegistrosToolStripMenuItem.Text = "Registros"
        '
        'PasienteToolStripMenuItem
        '
        Me.PasienteToolStripMenuItem.Name = "PasienteToolStripMenuItem"
        Me.PasienteToolStripMenuItem.Size = New System.Drawing.Size(148, 22)
        Me.PasienteToolStripMenuItem.Text = "Pasiente"
        '
        'MedicamentoToolStripMenuItem
        '
        Me.MedicamentoToolStripMenuItem.Name = "MedicamentoToolStripMenuItem"
        Me.MedicamentoToolStripMenuItem.Size = New System.Drawing.Size(148, 22)
        Me.MedicamentoToolStripMenuItem.Text = "Medicamento"
        '
        'REceToolStripMenuItem
        '
        Me.REceToolStripMenuItem.Name = "REceToolStripMenuItem"
        Me.REceToolStripMenuItem.Size = New System.Drawing.Size(148, 22)
        Me.REceToolStripMenuItem.Text = "Recetas"
        '
        'FacturasToolStripMenuItem
        '
        Me.FacturasToolStripMenuItem.Name = "FacturasToolStripMenuItem"
        Me.FacturasToolStripMenuItem.Size = New System.Drawing.Size(148, 22)
        Me.FacturasToolStripMenuItem.Text = "Facturas"
        '
        'PasienteToolStripMenuItem1
        '
        Me.PasienteToolStripMenuItem1.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.NuevaRecetaToolStripMenuItem})
        Me.PasienteToolStripMenuItem1.Name = "PasienteToolStripMenuItem1"
        Me.PasienteToolStripMenuItem1.Size = New System.Drawing.Size(63, 20)
        Me.PasienteToolStripMenuItem1.Text = "Pasiente"
        '
        'NuevaRecetaToolStripMenuItem
        '
        Me.NuevaRecetaToolStripMenuItem.Name = "NuevaRecetaToolStripMenuItem"
        Me.NuevaRecetaToolStripMenuItem.Size = New System.Drawing.Size(146, 22)
        Me.NuevaRecetaToolStripMenuItem.Text = "Nueva Receta"
        '
        'HelpToolStripMenuItem
        '
        Me.HelpToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.AboutToolStripMenuItem})
        Me.HelpToolStripMenuItem.Name = "HelpToolStripMenuItem"
        Me.HelpToolStripMenuItem.Size = New System.Drawing.Size(53, 20)
        Me.HelpToolStripMenuItem.Text = "Ayuda"
        '
        'AboutToolStripMenuItem
        '
        Me.AboutToolStripMenuItem.Name = "AboutToolStripMenuItem"
        Me.AboutToolStripMenuItem.Size = New System.Drawing.Size(152, 22)
        Me.AboutToolStripMenuItem.Text = "Contactanos.."
        '
        'PictureBox1
        '
        Me.PictureBox1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.PictureBox1.Location = New System.Drawing.Point(0, 24)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(684, 238)
        Me.PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage
        Me.PictureBox1.TabIndex = 2
        Me.PictureBox1.TabStop = False
        '
        'menu
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(684, 262)
        Me.Controls.Add(Me.PictureBox1)
        Me.Controls.Add(Me.MenuStrip1)
        Me.IsMdiContainer = True
        Me.MainMenuStrip = Me.MenuStrip1
        Me.Name = "menu"
        Me.Text = "menu"
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents MenuStrip1 As System.Windows.Forms.MenuStrip
    Friend WithEvents RegistrosToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PasienteToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents MedicamentoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents REceToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents FacturasToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PasienteToolStripMenuItem1 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents NuevaRecetaToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents HelpToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents AboutToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
End Class
