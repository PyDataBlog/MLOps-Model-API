<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class ListeClients
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(ListeClients))
        Me.PanelContainer = New System.Windows.Forms.Panel()
        Me.btnShow = New System.Windows.Forms.Button()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.btnPrint = New System.Windows.Forms.Button()
        Me.btnAdd = New System.Windows.Forms.Button()
        Me.txtNom = New System.Windows.Forms.TextBox()
        Me.labelProduitCF = New System.Windows.Forms.Label()
        Me.btnEdit = New System.Windows.Forms.Button()
        Me.ListClient = New System.Windows.Forms.ListView()
        Me.ColumnHeader1 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.ColumnHeader2 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.ColumnHeader3 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.ColumnHeader4 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.ColumnHeader5 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.ColumnHeader6 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.ContextMenuStrip1 = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.RapportDesAchatsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.btnDelete = New System.Windows.Forms.Button()
        Me.RadialPanel1 = New HG.UI.RadialPanel()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.PanelContainer.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.ContextMenuStrip1.SuspendLayout()
        Me.RadialPanel1.SuspendLayout()
        Me.SuspendLayout()
        '
        'PanelContainer
        '
        Me.PanelContainer.BackColor = System.Drawing.Color.Transparent
        Me.PanelContainer.Controls.Add(Me.btnShow)
        Me.PanelContainer.Controls.Add(Me.GroupBox1)
        Me.PanelContainer.Controls.Add(Me.btnEdit)
        Me.PanelContainer.Controls.Add(Me.ListClient)
        Me.PanelContainer.Controls.Add(Me.btnDelete)
        Me.PanelContainer.Controls.Add(Me.RadialPanel1)
        Me.PanelContainer.Location = New System.Drawing.Point(23, 12)
        Me.PanelContainer.Name = "PanelContainer"
        Me.PanelContainer.Size = New System.Drawing.Size(1154, 636)
        Me.PanelContainer.TabIndex = 0
        '
        'btnShow
        '
        Me.btnShow.BackColor = System.Drawing.Color.FromArgb(CType(CType(90, Byte), Integer), CType(CType(252, Byte), Integer), CType(CType(253, Byte), Integer), CType(CType(255, Byte), Integer))
        Me.btnShow.Cursor = System.Windows.Forms.Cursors.Hand
        Me.btnShow.FlatAppearance.CheckedBackColor = System.Drawing.Color.FromArgb(CType(CType(38, Byte), Integer), CType(CType(72, Byte), Integer), CType(CType(84, Byte), Integer))
        Me.btnShow.FlatAppearance.MouseDownBackColor = System.Drawing.Color.FromArgb(CType(CType(38, Byte), Integer), CType(CType(72, Byte), Integer), CType(CType(84, Byte), Integer))
        Me.btnShow.FlatAppearance.MouseOverBackColor = System.Drawing.Color.FromArgb(CType(CType(38, Byte), Integer), CType(CType(72, Byte), Integer), CType(CType(84, Byte), Integer))
        Me.btnShow.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.btnShow.Font = New System.Drawing.Font("Consolas", 10.0!)
        Me.btnShow.ForeColor = System.Drawing.Color.White
        Me.btnShow.Location = New System.Drawing.Point(298, 569)
        Me.btnShow.Margin = New System.Windows.Forms.Padding(4, 5, 4, 5)
        Me.btnShow.Name = "btnShow"
        Me.btnShow.Size = New System.Drawing.Size(181, 54)
        Me.btnShow.TabIndex = 19
        Me.btnShow.Text = "Afficher"
        Me.btnShow.UseVisualStyleBackColor = False
        '
        'GroupBox1
        '
        Me.GroupBox1.BackColor = System.Drawing.Color.Transparent
        Me.GroupBox1.Controls.Add(Me.btnPrint)
        Me.GroupBox1.Controls.Add(Me.btnAdd)
        Me.GroupBox1.Controls.Add(Me.txtNom)
        Me.GroupBox1.Controls.Add(Me.labelProduitCF)
        Me.GroupBox1.ForeColor = System.Drawing.Color.White
        Me.GroupBox1.Location = New System.Drawing.Point(3, 49)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(1148, 62)
        Me.GroupBox1.TabIndex = 73
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Filtres"
        '
        'btnPrint
        '
        Me.btnPrint.BackColor = System.Drawing.Color.FromArgb(CType(CType(120, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(171, Byte), Integer))
        Me.btnPrint.Cursor = System.Windows.Forms.Cursors.Hand
        Me.btnPrint.Font = New System.Drawing.Font("Consolas", 11.0!)
        Me.btnPrint.Image = Global.HG.My.Resources.Resources.printer
        Me.btnPrint.Location = New System.Drawing.Point(1068, 10)
        Me.btnPrint.Name = "btnPrint"
        Me.btnPrint.Size = New System.Drawing.Size(64, 49)
        Me.btnPrint.TabIndex = 18
        Me.btnPrint.UseVisualStyleBackColor = False
        '
        'btnAdd
        '
        Me.btnAdd.BackColor = System.Drawing.Color.FromArgb(CType(CType(120, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(171, Byte), Integer))
        Me.btnAdd.Cursor = System.Windows.Forms.Cursors.Hand
        Me.btnAdd.Font = New System.Drawing.Font("Consolas", 11.0!)
        Me.btnAdd.Image = Global.HG.My.Resources.Resources.add
        Me.btnAdd.Location = New System.Drawing.Point(1002, 11)
        Me.btnAdd.Name = "btnAdd"
        Me.btnAdd.Size = New System.Drawing.Size(63, 49)
        Me.btnAdd.TabIndex = 17
        Me.btnAdd.UseVisualStyleBackColor = False
        '
        'txtNom
        '
        Me.txtNom.Location = New System.Drawing.Point(197, 23)
        Me.txtNom.Name = "txtNom"
        Me.txtNom.Size = New System.Drawing.Size(594, 26)
        Me.txtNom.TabIndex = 3
        '
        'labelProduitCF
        '
        Me.labelProduitCF.AutoSize = True
        Me.labelProduitCF.ForeColor = System.Drawing.Color.White
        Me.labelProduitCF.Location = New System.Drawing.Point(57, 29)
        Me.labelProduitCF.Name = "labelProduitCF"
        Me.labelProduitCF.Size = New System.Drawing.Size(127, 19)
        Me.labelProduitCF.TabIndex = 1
        Me.labelProduitCF.Text = "Nom ou prenom :"
        '
        'btnEdit
        '
        Me.btnEdit.BackColor = System.Drawing.Color.FromArgb(CType(CType(90, Byte), Integer), CType(CType(252, Byte), Integer), CType(CType(253, Byte), Integer), CType(CType(255, Byte), Integer))
        Me.btnEdit.Cursor = System.Windows.Forms.Cursors.Hand
        Me.btnEdit.FlatAppearance.CheckedBackColor = System.Drawing.Color.FromArgb(CType(CType(38, Byte), Integer), CType(CType(72, Byte), Integer), CType(CType(84, Byte), Integer))
        Me.btnEdit.FlatAppearance.MouseDownBackColor = System.Drawing.Color.FromArgb(CType(CType(38, Byte), Integer), CType(CType(72, Byte), Integer), CType(CType(84, Byte), Integer))
        Me.btnEdit.FlatAppearance.MouseOverBackColor = System.Drawing.Color.FromArgb(CType(CType(38, Byte), Integer), CType(CType(72, Byte), Integer), CType(CType(84, Byte), Integer))
        Me.btnEdit.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.btnEdit.Font = New System.Drawing.Font("Consolas", 10.0!)
        Me.btnEdit.ForeColor = System.Drawing.Color.White
        Me.btnEdit.Image = CType(resources.GetObject("btnEdit.Image"), System.Drawing.Image)
        Me.btnEdit.Location = New System.Drawing.Point(487, 569)
        Me.btnEdit.Margin = New System.Windows.Forms.Padding(4, 5, 4, 5)
        Me.btnEdit.Name = "btnEdit"
        Me.btnEdit.Size = New System.Drawing.Size(181, 54)
        Me.btnEdit.TabIndex = 20
        Me.btnEdit.Text = "Modifier"
        Me.btnEdit.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText
        Me.btnEdit.UseVisualStyleBackColor = False
        '
        'ListClient
        '
        Me.ListClient.Activation = System.Windows.Forms.ItemActivation.TwoClick
        Me.ListClient.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader1, Me.ColumnHeader2, Me.ColumnHeader3, Me.ColumnHeader4, Me.ColumnHeader5, Me.ColumnHeader6})
        Me.ListClient.ContextMenuStrip = Me.ContextMenuStrip1
        Me.ListClient.Font = New System.Drawing.Font("Open Sans", 11.0!)
        Me.ListClient.FullRowSelect = True
        Me.ListClient.GridLines = True
        Me.ListClient.Location = New System.Drawing.Point(0, 114)
        Me.ListClient.Margin = New System.Windows.Forms.Padding(4, 5, 4, 5)
        Me.ListClient.MultiSelect = False
        Me.ListClient.Name = "ListClient"
        Me.ListClient.Size = New System.Drawing.Size(1153, 450)
        Me.ListClient.TabIndex = 69
        Me.ListClient.UseCompatibleStateImageBehavior = False
        Me.ListClient.View = System.Windows.Forms.View.Details
        '
        'ColumnHeader1
        '
        Me.ColumnHeader1.Text = "id"
        Me.ColumnHeader1.Width = 0
        '
        'ColumnHeader2
        '
        Me.ColumnHeader2.Text = "Nom"
        Me.ColumnHeader2.Width = 231
        '
        'ColumnHeader3
        '
        Me.ColumnHeader3.Text = "Prenom"
        Me.ColumnHeader3.Width = 189
        '
        'ColumnHeader4
        '
        Me.ColumnHeader4.Text = "Tel. 1 "
        Me.ColumnHeader4.Width = 195
        '
        'ColumnHeader5
        '
        Me.ColumnHeader5.Text = "Tel. 2"
        Me.ColumnHeader5.Width = 195
        '
        'ColumnHeader6
        '
        Me.ColumnHeader6.Text = "Adresse"
        Me.ColumnHeader6.Width = 321
        '
        'ContextMenuStrip1
        '
        Me.ContextMenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.RapportDesAchatsToolStripMenuItem})
        Me.ContextMenuStrip1.Name = "ContextMenuStrip1"
        Me.ContextMenuStrip1.Size = New System.Drawing.Size(178, 26)
        '
        'RapportDesAchatsToolStripMenuItem
        '
        Me.RapportDesAchatsToolStripMenuItem.Name = "RapportDesAchatsToolStripMenuItem"
        Me.RapportDesAchatsToolStripMenuItem.Size = New System.Drawing.Size(177, 22)
        Me.RapportDesAchatsToolStripMenuItem.Text = "Rapport des achats "
        '
        'btnDelete
        '
        Me.btnDelete.BackColor = System.Drawing.Color.FromArgb(CType(CType(90, Byte), Integer), CType(CType(252, Byte), Integer), CType(CType(253, Byte), Integer), CType(CType(255, Byte), Integer))
        Me.btnDelete.Cursor = System.Windows.Forms.Cursors.Hand
        Me.btnDelete.FlatAppearance.CheckedBackColor = System.Drawing.Color.FromArgb(CType(CType(38, Byte), Integer), CType(CType(72, Byte), Integer), CType(CType(84, Byte), Integer))
        Me.btnDelete.FlatAppearance.MouseDownBackColor = System.Drawing.Color.FromArgb(CType(CType(38, Byte), Integer), CType(CType(72, Byte), Integer), CType(CType(84, Byte), Integer))
        Me.btnDelete.FlatAppearance.MouseOverBackColor = System.Drawing.Color.FromArgb(CType(CType(38, Byte), Integer), CType(CType(72, Byte), Integer), CType(CType(84, Byte), Integer))
        Me.btnDelete.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.btnDelete.Font = New System.Drawing.Font("Consolas", 10.0!)
        Me.btnDelete.ForeColor = System.Drawing.Color.White
        Me.btnDelete.Image = CType(resources.GetObject("btnDelete.Image"), System.Drawing.Image)
        Me.btnDelete.Location = New System.Drawing.Point(676, 569)
        Me.btnDelete.Margin = New System.Windows.Forms.Padding(4, 5, 4, 5)
        Me.btnDelete.Name = "btnDelete"
        Me.btnDelete.Size = New System.Drawing.Size(181, 54)
        Me.btnDelete.TabIndex = 21
        Me.btnDelete.Text = "Supprimer"
        Me.btnDelete.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText
        Me.btnDelete.UseVisualStyleBackColor = False
        '
        'RadialPanel1
        '
        Me.RadialPanel1.Controls.Add(Me.Label1)
        Me.RadialPanel1.Location = New System.Drawing.Point(3, 0)
        Me.RadialPanel1.Name = "RadialPanel1"
        Me.RadialPanel1.Size = New System.Drawing.Size(1151, 53)
        Me.RadialPanel1.TabIndex = 68
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.BackColor = System.Drawing.Color.Transparent
        Me.Label1.Font = New System.Drawing.Font("Open Sans", 25.0!, System.Drawing.FontStyle.Bold)
        Me.Label1.ForeColor = System.Drawing.Color.White
        Me.Label1.Location = New System.Drawing.Point(13, 2)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(289, 46)
        Me.Label1.TabIndex = 60
        Me.Label1.Text = "Liste des clients"
        '
        'ListeClients
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(7.0!, 18.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.Turquoise
        Me.BackgroundImage = Global.HG.My.Resources.Resources.background
        Me.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch
        Me.ClientSize = New System.Drawing.Size(1189, 660)
        Me.Controls.Add(Me.PanelContainer)
        Me.Font = New System.Drawing.Font("Open Sans", 10.0!)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Margin = New System.Windows.Forms.Padding(3, 4, 3, 4)
        Me.Name = "ListeClients"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Liste des clients"
        Me.WindowState = System.Windows.Forms.FormWindowState.Maximized
        Me.PanelContainer.ResumeLayout(False)
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.ContextMenuStrip1.ResumeLayout(False)
        Me.RadialPanel1.ResumeLayout(False)
        Me.RadialPanel1.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents PanelContainer As System.Windows.Forms.Panel
    Friend WithEvents RadialPanel1 As HG.UI.RadialPanel
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents ListClient As System.Windows.Forms.ListView
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents labelProduitCF As System.Windows.Forms.Label
    Friend WithEvents txtNom As System.Windows.Forms.TextBox
    Friend WithEvents ColumnHeader1 As System.Windows.Forms.ColumnHeader
    Friend WithEvents ColumnHeader2 As System.Windows.Forms.ColumnHeader
    Friend WithEvents ColumnHeader3 As System.Windows.Forms.ColumnHeader
    Friend WithEvents ColumnHeader4 As System.Windows.Forms.ColumnHeader
    Friend WithEvents ColumnHeader5 As System.Windows.Forms.ColumnHeader
    Friend WithEvents ColumnHeader6 As System.Windows.Forms.ColumnHeader
    Friend WithEvents btnAdd As System.Windows.Forms.Button
    Friend WithEvents btnPrint As System.Windows.Forms.Button
    Friend WithEvents btnShow As System.Windows.Forms.Button
    Friend WithEvents btnEdit As System.Windows.Forms.Button
    Friend WithEvents btnDelete As System.Windows.Forms.Button
    Friend WithEvents ContextMenuStrip1 As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents RapportDesAchatsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
End Class
