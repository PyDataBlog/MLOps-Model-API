Namespace Reports
    <Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
    Partial Class DetteClients
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
            Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(DetteClients))
            Me.PanelContainer = New System.Windows.Forms.Panel()
            Me.Button1 = New System.Windows.Forms.Button()
            Me.labelTotal = New System.Windows.Forms.Label()
            Me.labelPayer = New System.Windows.Forms.Label()
            Me.labelReste = New System.Windows.Forms.Label()
            Me.ListDette = New System.Windows.Forms.ListView()
            Me.ColumnHeader5 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
            Me.ColumnHeader6 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
            Me.ColumnHeader7 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
            Me.ColumnHeader8 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
            Me.btnClose = New System.Windows.Forms.Button()
            Me.RadialPanel1 = New HG.UI.RadialPanel()
            Me.btnToutCF = New System.Windows.Forms.Button()
            Me.cmbClient = New System.Windows.Forms.ComboBox()
            Me.TransparentLabel1 = New HG.UI.TransparentLabel()
            Me.Label1 = New System.Windows.Forms.Label()
            Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
            Me.PanelContainer.SuspendLayout()
            Me.RadialPanel1.SuspendLayout()
            Me.SuspendLayout()
            '
            'PanelContainer
            '
            Me.PanelContainer.BackColor = System.Drawing.Color.Transparent
            Me.PanelContainer.BackgroundImage = Global.HG.My.Resources.Resources.bg_img
            Me.PanelContainer.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch
            Me.PanelContainer.Controls.Add(Me.Button1)
            Me.PanelContainer.Controls.Add(Me.labelTotal)
            Me.PanelContainer.Controls.Add(Me.labelPayer)
            Me.PanelContainer.Controls.Add(Me.labelReste)
            Me.PanelContainer.Controls.Add(Me.ListDette)
            Me.PanelContainer.Controls.Add(Me.btnClose)
            Me.PanelContainer.Controls.Add(Me.RadialPanel1)
            Me.PanelContainer.Location = New System.Drawing.Point(0, 0)
            Me.PanelContainer.Name = "PanelContainer"
            Me.PanelContainer.Size = New System.Drawing.Size(1137, 701)
            Me.PanelContainer.TabIndex = 0
            '
            'Button1
            '
            Me.Button1.BackColor = System.Drawing.Color.DeepSkyBlue
            Me.Button1.Cursor = System.Windows.Forms.Cursors.Hand
            Me.Button1.Font = New System.Drawing.Font("Open Sans", 11.25!)
            Me.Button1.Image = CType(resources.GetObject("Button1.Image"), System.Drawing.Image)
            Me.Button1.Location = New System.Drawing.Point(460, 607)
            Me.Button1.Name = "Button1"
            Me.Button1.Size = New System.Drawing.Size(114, 42)
            Me.Button1.TabIndex = 103
            Me.Button1.Text = "Imprimer"
            Me.Button1.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText
            Me.Button1.UseVisualStyleBackColor = False
            '
            'labelTotal
            '
            Me.labelTotal.BackColor = System.Drawing.Color.Honeydew
            Me.labelTotal.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
            Me.labelTotal.Font = New System.Drawing.Font("Microsoft Sans Serif", 11.25!, System.Drawing.FontStyle.Bold)
            Me.labelTotal.Location = New System.Drawing.Point(599, 566)
            Me.labelTotal.Name = "labelTotal"
            Me.labelTotal.Size = New System.Drawing.Size(168, 36)
            Me.labelTotal.TabIndex = 102
            Me.labelTotal.Text = "0 Fcfa"
            Me.labelTotal.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
            '
            'labelPayer
            '
            Me.labelPayer.BackColor = System.Drawing.Color.Honeydew
            Me.labelPayer.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
            Me.labelPayer.Font = New System.Drawing.Font("Microsoft Sans Serif", 11.25!, System.Drawing.FontStyle.Bold)
            Me.labelPayer.Location = New System.Drawing.Point(768, 566)
            Me.labelPayer.Name = "labelPayer"
            Me.labelPayer.Size = New System.Drawing.Size(168, 36)
            Me.labelPayer.TabIndex = 101
            Me.labelPayer.Text = "0 Fcfa"
            Me.labelPayer.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
            '
            'labelReste
            '
            Me.labelReste.BackColor = System.Drawing.Color.Honeydew
            Me.labelReste.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
            Me.labelReste.Font = New System.Drawing.Font("Microsoft Sans Serif", 11.25!, System.Drawing.FontStyle.Bold)
            Me.labelReste.Location = New System.Drawing.Point(937, 566)
            Me.labelReste.Name = "labelReste"
            Me.labelReste.Size = New System.Drawing.Size(168, 36)
            Me.labelReste.TabIndex = 101
            Me.labelReste.Text = "0 Fcfa"
            Me.labelReste.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
            '
            'ListDette
            '
            Me.ListDette.Activation = System.Windows.Forms.ItemActivation.TwoClick
            Me.ListDette.AllowColumnReorder = True
            Me.ListDette.BackColor = System.Drawing.Color.White
            Me.ListDette.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader5, Me.ColumnHeader6, Me.ColumnHeader7, Me.ColumnHeader8})
            Me.ListDette.Font = New System.Drawing.Font("Open Sans", 12.0!)
            Me.ListDette.FullRowSelect = True
            Me.ListDette.GridLines = True
            Me.ListDette.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable
            Me.ListDette.Location = New System.Drawing.Point(39, 105)
            Me.ListDette.MultiSelect = False
            Me.ListDette.Name = "ListDette"
            Me.ListDette.Size = New System.Drawing.Size(1071, 458)
            Me.ListDette.TabIndex = 100
            Me.ListDette.UseCompatibleStateImageBehavior = False
            Me.ListDette.View = System.Windows.Forms.View.Details
            '
            'ColumnHeader5
            '
            Me.ColumnHeader5.Text = "Client"
            Me.ColumnHeader5.Width = 559
            '
            'ColumnHeader6
            '
            Me.ColumnHeader6.Text = "Total"
            Me.ColumnHeader6.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
            Me.ColumnHeader6.Width = 168
            '
            'ColumnHeader7
            '
            Me.ColumnHeader7.Text = "Payer"
            Me.ColumnHeader7.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
            Me.ColumnHeader7.Width = 168
            '
            'ColumnHeader8
            '
            Me.ColumnHeader8.Text = "Reste"
            Me.ColumnHeader8.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
            Me.ColumnHeader8.Width = 168
            '
            'btnClose
            '
            Me.btnClose.BackColor = System.Drawing.Color.DeepSkyBlue
            Me.btnClose.Cursor = System.Windows.Forms.Cursors.Hand
            Me.btnClose.Font = New System.Drawing.Font("Open Sans", 11.25!)
            Me.btnClose.Image = CType(resources.GetObject("btnClose.Image"), System.Drawing.Image)
            Me.btnClose.Location = New System.Drawing.Point(577, 607)
            Me.btnClose.Name = "btnClose"
            Me.btnClose.Size = New System.Drawing.Size(103, 42)
            Me.btnClose.TabIndex = 96
            Me.btnClose.Text = "Fermer"
            Me.btnClose.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText
            Me.ToolTip1.SetToolTip(Me.btnClose, "Fermer cette fenetre")
            Me.btnClose.UseVisualStyleBackColor = False
            '
            'RadialPanel1
            '
            Me.RadialPanel1.Controls.Add(Me.btnToutCF)
            Me.RadialPanel1.Controls.Add(Me.cmbClient)
            Me.RadialPanel1.Controls.Add(Me.TransparentLabel1)
            Me.RadialPanel1.Controls.Add(Me.Label1)
            Me.RadialPanel1.Location = New System.Drawing.Point(10, 21)
            Me.RadialPanel1.Name = "RadialPanel1"
            Me.RadialPanel1.Size = New System.Drawing.Size(1122, 53)
            Me.RadialPanel1.TabIndex = 67
            '
            'btnToutCF
            '
            Me.btnToutCF.BackColor = System.Drawing.Color.FromArgb(CType(CType(120, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(171, Byte), Integer))
            Me.btnToutCF.Cursor = System.Windows.Forms.Cursors.Hand
            Me.btnToutCF.Font = New System.Drawing.Font("Consolas", 11.0!)
            Me.btnToutCF.Image = Global.HG.My.Resources.Resources.icon_tout
            Me.btnToutCF.Location = New System.Drawing.Point(907, 4)
            Me.btnToutCF.Name = "btnToutCF"
            Me.btnToutCF.Size = New System.Drawing.Size(49, 43)
            Me.btnToutCF.TabIndex = 63
            Me.ToolTip1.SetToolTip(Me.btnToutCF, "Tout afficher")
            Me.btnToutCF.UseVisualStyleBackColor = False
            '
            'cmbClient
            '
            Me.cmbClient.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.SuggestAppend
            Me.cmbClient.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.ListItems
            Me.cmbClient.Font = New System.Drawing.Font("Open Sans", 12.0!)
            Me.cmbClient.FormattingEnabled = True
            Me.cmbClient.Location = New System.Drawing.Point(427, 11)
            Me.cmbClient.Name = "cmbClient"
            Me.cmbClient.Size = New System.Drawing.Size(461, 30)
            Me.cmbClient.TabIndex = 62
            '
            'TransparentLabel1
            '
            Me.TransparentLabel1.AutoSize = True
            Me.TransparentLabel1.ForeColor = System.Drawing.Color.FromArgb(CType(CType(242, Byte), Integer), CType(CType(242, Byte), Integer), CType(CType(242, Byte), Integer))
            Me.TransparentLabel1.Location = New System.Drawing.Point(363, 17)
            Me.TransparentLabel1.Name = "TransparentLabel1"
            Me.TransparentLabel1.Size = New System.Drawing.Size(56, 19)
            Me.TransparentLabel1.TabIndex = 61
            Me.TransparentLabel1.Text = "Client :"
            '
            'Label1
            '
            Me.Label1.AutoSize = True
            Me.Label1.BackColor = System.Drawing.Color.Transparent
            Me.Label1.Font = New System.Drawing.Font("Open Sans", 25.0!, System.Drawing.FontStyle.Bold)
            Me.Label1.ForeColor = System.Drawing.Color.White
            Me.Label1.Location = New System.Drawing.Point(13, 2)
            Me.Label1.Name = "Label1"
            Me.Label1.Size = New System.Drawing.Size(252, 46)
            Me.Label1.TabIndex = 60
            Me.Label1.Text = "Dettes clients"
            '
            'ToolTip1
            '
            Me.ToolTip1.IsBalloon = True
            '
            'DetteClients
            '
            Me.AutoScaleDimensions = New System.Drawing.SizeF(7.0!, 18.0!)
            Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
            Me.BackColor = System.Drawing.SystemColors.Control
            Me.BackgroundImage = Global.HG.My.Resources.Resources.background
            Me.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch
            Me.ClientSize = New System.Drawing.Size(1179, 713)
            Me.Controls.Add(Me.PanelContainer)
            Me.Font = New System.Drawing.Font("Open Sans", 10.0!)
            Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
            Me.Margin = New System.Windows.Forms.Padding(3, 4, 3, 4)
            Me.Name = "DetteClients"
            Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
            Me.Text = "Dette Clients"
            Me.WindowState = System.Windows.Forms.FormWindowState.Maximized
            Me.PanelContainer.ResumeLayout(False)
            Me.RadialPanel1.ResumeLayout(False)
            Me.RadialPanel1.PerformLayout()
            Me.ResumeLayout(False)

        End Sub
        Friend WithEvents PanelContainer As System.Windows.Forms.Panel
        Friend WithEvents RadialPanel1 As HG.UI.RadialPanel
        Friend WithEvents TransparentLabel1 As HG.UI.TransparentLabel
        Friend WithEvents Label1 As System.Windows.Forms.Label
        Friend WithEvents cmbClient As System.Windows.Forms.ComboBox
        Friend WithEvents btnClose As System.Windows.Forms.Button
        Friend WithEvents labelReste As System.Windows.Forms.Label
        Friend WithEvents ListDette As System.Windows.Forms.ListView
        Friend WithEvents ColumnHeader5 As System.Windows.Forms.ColumnHeader
        Friend WithEvents ColumnHeader6 As System.Windows.Forms.ColumnHeader
        Friend WithEvents ColumnHeader7 As System.Windows.Forms.ColumnHeader
        Friend WithEvents ColumnHeader8 As System.Windows.Forms.ColumnHeader
        Friend WithEvents labelTotal As System.Windows.Forms.Label
        Friend WithEvents labelPayer As System.Windows.Forms.Label
        Friend WithEvents btnToutCF As System.Windows.Forms.Button
        Friend WithEvents ToolTip1 As System.Windows.Forms.ToolTip
        Friend WithEvents Button1 As System.Windows.Forms.Button
    End Class

End Namespace