Namespace Reports
    <Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
    Partial Class AnnulerFacture
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
            Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(AnnulerFacture))
            Me.Label1 = New System.Windows.Forms.Label()
            Me.labelNumero = New System.Windows.Forms.Label()
            Me.Label2 = New System.Windows.Forms.Label()
            Me.labelDate = New System.Windows.Forms.Label()
            Me.Label3 = New System.Windows.Forms.Label()
            Me.labelPoint = New System.Windows.Forms.Label()
            Me.Label5 = New System.Windows.Forms.Label()
            Me.labelUser = New System.Windows.Forms.Label()
            Me.Label7 = New System.Windows.Forms.Label()
            Me.txtMotif = New System.Windows.Forms.TextBox()
            Me.btnSave = New System.Windows.Forms.Button()
            Me.btnClose = New System.Windows.Forms.Button()
            Me.PanelContainer = New System.Windows.Forms.Panel()
            Me.Label6 = New System.Windows.Forms.Label()
            Me.labelTotal = New System.Windows.Forms.Label()
            Me.PanelContainer.SuspendLayout()
            Me.SuspendLayout()
            '
            'Label1
            '
            Me.Label1.AutoSize = True
            Me.Label1.BackColor = System.Drawing.Color.Transparent
            Me.Label1.Font = New System.Drawing.Font("Open Sans", 13.0!)
            Me.Label1.Location = New System.Drawing.Point(58, 30)
            Me.Label1.Name = "Label1"
            Me.Label1.Size = New System.Drawing.Size(106, 24)
            Me.Label1.TabIndex = 10
            Me.Label1.Text = "Facture n° :"
            '
            'labelNumero
            '
            Me.labelNumero.AutoSize = True
            Me.labelNumero.BackColor = System.Drawing.Color.Transparent
            Me.labelNumero.Font = New System.Drawing.Font("Open Sans", 13.0!, System.Drawing.FontStyle.Bold)
            Me.labelNumero.ForeColor = System.Drawing.Color.FromArgb(CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(255, Byte), Integer))
            Me.labelNumero.Location = New System.Drawing.Point(265, 31)
            Me.labelNumero.Name = "labelNumero"
            Me.labelNumero.Size = New System.Drawing.Size(60, 24)
            Me.labelNumero.TabIndex = 1
            Me.labelNumero.Text = "10005"
            '
            'Label2
            '
            Me.Label2.AutoSize = True
            Me.Label2.BackColor = System.Drawing.Color.Transparent
            Me.Label2.Font = New System.Drawing.Font("Open Sans", 13.0!)
            Me.Label2.Location = New System.Drawing.Point(58, 64)
            Me.Label2.Name = "Label2"
            Me.Label2.Size = New System.Drawing.Size(208, 24)
            Me.Label2.TabIndex = 0
            Me.Label2.Text = "Date d'enregistrement :"
            '
            'labelDate
            '
            Me.labelDate.AutoSize = True
            Me.labelDate.BackColor = System.Drawing.Color.Transparent
            Me.labelDate.Font = New System.Drawing.Font("Open Sans", 13.0!, System.Drawing.FontStyle.Bold)
            Me.labelDate.ForeColor = System.Drawing.Color.FromArgb(CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(255, Byte), Integer))
            Me.labelDate.Location = New System.Drawing.Point(265, 67)
            Me.labelDate.Name = "labelDate"
            Me.labelDate.Size = New System.Drawing.Size(247, 24)
            Me.labelDate.TabIndex = 1
            Me.labelDate.Text = "10 Semptembre 2016 10:30"
            '
            'Label3
            '
            Me.Label3.AutoSize = True
            Me.Label3.BackColor = System.Drawing.Color.Transparent
            Me.Label3.Font = New System.Drawing.Font("Open Sans", 13.0!)
            Me.Label3.Location = New System.Drawing.Point(58, 139)
            Me.Label3.Name = "Label3"
            Me.Label3.Size = New System.Drawing.Size(141, 24)
            Me.Label3.TabIndex = 0
            Me.Label3.Text = "Point de vente :"
            '
            'labelPoint
            '
            Me.labelPoint.AutoSize = True
            Me.labelPoint.BackColor = System.Drawing.Color.Transparent
            Me.labelPoint.Font = New System.Drawing.Font("Open Sans", 13.0!, System.Drawing.FontStyle.Bold)
            Me.labelPoint.ForeColor = System.Drawing.Color.FromArgb(CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(255, Byte), Integer))
            Me.labelPoint.Location = New System.Drawing.Point(265, 142)
            Me.labelPoint.Name = "labelPoint"
            Me.labelPoint.Size = New System.Drawing.Size(168, 24)
            Me.labelPoint.TabIndex = 1
            Me.labelPoint.Text = "CHAMBRE FROIDE"
            '
            'Label5
            '
            Me.Label5.AutoSize = True
            Me.Label5.BackColor = System.Drawing.Color.Transparent
            Me.Label5.Font = New System.Drawing.Font("Open Sans", 13.0!)
            Me.Label5.Location = New System.Drawing.Point(58, 100)
            Me.Label5.Name = "Label5"
            Me.Label5.Size = New System.Drawing.Size(138, 24)
            Me.Label5.TabIndex = 0
            Me.Label5.Text = "Enregistré par :"
            '
            'labelUser
            '
            Me.labelUser.AutoSize = True
            Me.labelUser.BackColor = System.Drawing.Color.Transparent
            Me.labelUser.Font = New System.Drawing.Font("Open Sans", 13.0!, System.Drawing.FontStyle.Bold)
            Me.labelUser.ForeColor = System.Drawing.Color.FromArgb(CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(255, Byte), Integer))
            Me.labelUser.Location = New System.Drawing.Point(265, 103)
            Me.labelUser.Name = "labelUser"
            Me.labelUser.Size = New System.Drawing.Size(68, 24)
            Me.labelUser.TabIndex = 1
            Me.labelUser.Text = "Admin"
            '
            'Label7
            '
            Me.Label7.AutoSize = True
            Me.Label7.BackColor = System.Drawing.Color.Transparent
            Me.Label7.Font = New System.Drawing.Font("Open Sans", 13.0!)
            Me.Label7.Location = New System.Drawing.Point(58, 224)
            Me.Label7.Name = "Label7"
            Me.Label7.Size = New System.Drawing.Size(170, 24)
            Me.Label7.TabIndex = 0
            Me.Label7.Text = "Motif d'annulation:"
            '
            'txtMotif
            '
            Me.txtMotif.Location = New System.Drawing.Point(234, 225)
            Me.txtMotif.Multiline = True
            Me.txtMotif.Name = "txtMotif"
            Me.txtMotif.Size = New System.Drawing.Size(387, 108)
            Me.txtMotif.TabIndex = 0
            '
            'btnSave
            '
            Me.btnSave.BackColor = System.Drawing.Color.FromArgb(CType(CType(76, Byte), Integer), CType(CType(175, Byte), Integer), CType(CType(80, Byte), Integer))
            Me.btnSave.Font = New System.Drawing.Font("Open Sans", 10.0!)
            Me.btnSave.Image = Global.HG.My.Resources.Resources.check
            Me.btnSave.Location = New System.Drawing.Point(335, 409)
            Me.btnSave.Margin = New System.Windows.Forms.Padding(4, 3, 4, 3)
            Me.btnSave.Name = "btnSave"
            Me.btnSave.Size = New System.Drawing.Size(128, 42)
            Me.btnSave.TabIndex = 1
            Me.btnSave.Text = "Enregistrer"
            Me.btnSave.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText
            Me.btnSave.UseVisualStyleBackColor = False
            '
            'btnClose
            '
            Me.btnClose.BackColor = System.Drawing.Color.Turquoise
            Me.btnClose.Font = New System.Drawing.Font("Open Sans", 10.0!)
            Me.btnClose.Image = Global.HG.My.Resources.Resources.fermer_icone_3776_32
            Me.btnClose.Location = New System.Drawing.Point(206, 409)
            Me.btnClose.Margin = New System.Windows.Forms.Padding(4, 3, 4, 3)
            Me.btnClose.Name = "btnClose"
            Me.btnClose.Size = New System.Drawing.Size(124, 42)
            Me.btnClose.TabIndex = 2
            Me.btnClose.Text = "Fermer"
            Me.btnClose.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText
            Me.btnClose.UseVisualStyleBackColor = False
            '
            'PanelContainer
            '
            Me.PanelContainer.BackColor = System.Drawing.Color.Transparent
            Me.PanelContainer.BackgroundImage = CType(resources.GetObject("PanelContainer.BackgroundImage"), System.Drawing.Image)
            Me.PanelContainer.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch
            Me.PanelContainer.Controls.Add(Me.Label1)
            Me.PanelContainer.Controls.Add(Me.Label2)
            Me.PanelContainer.Controls.Add(Me.labelNumero)
            Me.PanelContainer.Controls.Add(Me.txtMotif)
            Me.PanelContainer.Controls.Add(Me.Label6)
            Me.PanelContainer.Controls.Add(Me.Label3)
            Me.PanelContainer.Controls.Add(Me.labelUser)
            Me.PanelContainer.Controls.Add(Me.Label7)
            Me.PanelContainer.Controls.Add(Me.labelTotal)
            Me.PanelContainer.Controls.Add(Me.labelPoint)
            Me.PanelContainer.Controls.Add(Me.labelDate)
            Me.PanelContainer.Controls.Add(Me.Label5)
            Me.PanelContainer.Location = New System.Drawing.Point(13, 10)
            Me.PanelContainer.Name = "PanelContainer"
            Me.PanelContainer.Size = New System.Drawing.Size(642, 393)
            Me.PanelContainer.TabIndex = 11
            '
            'Label6
            '
            Me.Label6.AutoSize = True
            Me.Label6.BackColor = System.Drawing.Color.Transparent
            Me.Label6.Font = New System.Drawing.Font("Open Sans", 13.0!)
            Me.Label6.Location = New System.Drawing.Point(58, 179)
            Me.Label6.Name = "Label6"
            Me.Label6.Size = New System.Drawing.Size(98, 24)
            Me.Label6.TabIndex = 0
            Me.Label6.Text = "Total TTC :"
            '
            'labelTotal
            '
            Me.labelTotal.AutoSize = True
            Me.labelTotal.BackColor = System.Drawing.Color.Transparent
            Me.labelTotal.Font = New System.Drawing.Font("Open Sans", 13.0!, System.Drawing.FontStyle.Bold)
            Me.labelTotal.ForeColor = System.Drawing.Color.FromArgb(CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(255, Byte), Integer))
            Me.labelTotal.Location = New System.Drawing.Point(265, 182)
            Me.labelTotal.Name = "labelTotal"
            Me.labelTotal.Size = New System.Drawing.Size(102, 24)
            Me.labelTotal.TabIndex = 1
            Me.labelTotal.Text = "10000 Fcfa"
            '
            'AnnulerFacture
            '
            Me.AutoScaleDimensions = New System.Drawing.SizeF(7.0!, 18.0!)
            Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
            Me.BackColor = System.Drawing.Color.Turquoise
            Me.BackgroundImage = CType(resources.GetObject("$this.BackgroundImage"), System.Drawing.Image)
            Me.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch
            Me.ClientSize = New System.Drawing.Size(668, 463)
            Me.Controls.Add(Me.PanelContainer)
            Me.Controls.Add(Me.btnSave)
            Me.Controls.Add(Me.btnClose)
            Me.DoubleBuffered = True
            Me.Font = New System.Drawing.Font("Open Sans", 10.0!)
            Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
            Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
            Me.Margin = New System.Windows.Forms.Padding(3, 4, 3, 4)
            Me.Name = "AnnulerFacture"
            Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
            Me.Text = "AnnulerFacture"
            Me.PanelContainer.ResumeLayout(False)
            Me.PanelContainer.PerformLayout()
            Me.ResumeLayout(False)

        End Sub
        Friend WithEvents Label1 As System.Windows.Forms.Label
        Friend WithEvents labelNumero As System.Windows.Forms.Label
        Friend WithEvents Label2 As System.Windows.Forms.Label
        Friend WithEvents labelDate As System.Windows.Forms.Label
        Friend WithEvents Label3 As System.Windows.Forms.Label
        Friend WithEvents labelPoint As System.Windows.Forms.Label
        Friend WithEvents Label5 As System.Windows.Forms.Label
        Friend WithEvents labelUser As System.Windows.Forms.Label
        Friend WithEvents Label7 As System.Windows.Forms.Label
        Friend WithEvents txtMotif As System.Windows.Forms.TextBox
        Friend WithEvents btnSave As System.Windows.Forms.Button
        Friend WithEvents btnClose As System.Windows.Forms.Button
        Friend WithEvents PanelContainer As System.Windows.Forms.Panel
        Friend WithEvents Label6 As System.Windows.Forms.Label
        Friend WithEvents labelTotal As System.Windows.Forms.Label
    End Class

End Namespace