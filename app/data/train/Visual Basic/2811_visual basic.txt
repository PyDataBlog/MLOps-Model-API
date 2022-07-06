<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmConversion
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
        Me.lblMiles = New System.Windows.Forms.Label()
        Me.lblYards = New System.Windows.Forms.Label()
        Me.lblFeet = New System.Windows.Forms.Label()
        Me.lblInches = New System.Windows.Forms.Label()
        Me.txtMiles = New System.Windows.Forms.TextBox()
        Me.txtYards = New System.Windows.Forms.TextBox()
        Me.txtFeet = New System.Windows.Forms.TextBox()
        Me.txtInches = New System.Windows.Forms.TextBox()
        Me.btnConvert = New System.Windows.Forms.Button()
        Me.lstResults = New System.Windows.Forms.ListBox()
        Me.SuspendLayout()
        '
        'lblMiles
        '
        Me.lblMiles.AutoSize = True
        Me.lblMiles.Location = New System.Drawing.Point(16, 31)
        Me.lblMiles.Name = "lblMiles"
        Me.lblMiles.Size = New System.Drawing.Size(34, 13)
        Me.lblMiles.TabIndex = 0
        Me.lblMiles.Text = "Miles:"
        '
        'lblYards
        '
        Me.lblYards.AutoSize = True
        Me.lblYards.Location = New System.Drawing.Point(13, 57)
        Me.lblYards.Name = "lblYards"
        Me.lblYards.Size = New System.Drawing.Size(37, 13)
        Me.lblYards.TabIndex = 1
        Me.lblYards.Text = "Yards:"
        '
        'lblFeet
        '
        Me.lblFeet.AutoSize = True
        Me.lblFeet.Location = New System.Drawing.Point(19, 83)
        Me.lblFeet.Name = "lblFeet"
        Me.lblFeet.Size = New System.Drawing.Size(31, 13)
        Me.lblFeet.TabIndex = 2
        Me.lblFeet.Text = "Feet:"
        '
        'lblInches
        '
        Me.lblInches.AutoSize = True
        Me.lblInches.Location = New System.Drawing.Point(8, 109)
        Me.lblInches.Name = "lblInches"
        Me.lblInches.Size = New System.Drawing.Size(42, 13)
        Me.lblInches.TabIndex = 3
        Me.lblInches.Text = "Inches:"
        '
        'txtMiles
        '
        Me.txtMiles.Location = New System.Drawing.Point(56, 28)
        Me.txtMiles.Name = "txtMiles"
        Me.txtMiles.Size = New System.Drawing.Size(100, 20)
        Me.txtMiles.TabIndex = 4
        '
        'txtYards
        '
        Me.txtYards.Location = New System.Drawing.Point(56, 54)
        Me.txtYards.Name = "txtYards"
        Me.txtYards.Size = New System.Drawing.Size(100, 20)
        Me.txtYards.TabIndex = 5
        '
        'txtFeet
        '
        Me.txtFeet.Location = New System.Drawing.Point(56, 80)
        Me.txtFeet.Name = "txtFeet"
        Me.txtFeet.Size = New System.Drawing.Size(100, 20)
        Me.txtFeet.TabIndex = 6
        '
        'txtInches
        '
        Me.txtInches.Location = New System.Drawing.Point(56, 106)
        Me.txtInches.Name = "txtInches"
        Me.txtInches.Size = New System.Drawing.Size(100, 20)
        Me.txtInches.TabIndex = 7
        '
        'btnConvert
        '
        Me.btnConvert.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btnConvert.Location = New System.Drawing.Point(187, 12)
        Me.btnConvert.Name = "btnConvert"
        Me.btnConvert.Size = New System.Drawing.Size(150, 35)
        Me.btnConvert.TabIndex = 8
        Me.btnConvert.Text = "Convert to Metric"
        Me.btnConvert.UseVisualStyleBackColor = True
        '
        'lstResults
        '
        Me.lstResults.FormattingEnabled = True
        Me.lstResults.Location = New System.Drawing.Point(187, 57)
        Me.lstResults.Name = "lstResults"
        Me.lstResults.Size = New System.Drawing.Size(150, 69)
        Me.lstResults.TabIndex = 9
        '
        'frmConversion
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(354, 142)
        Me.Controls.Add(Me.lstResults)
        Me.Controls.Add(Me.btnConvert)
        Me.Controls.Add(Me.txtInches)
        Me.Controls.Add(Me.txtFeet)
        Me.Controls.Add(Me.txtYards)
        Me.Controls.Add(Me.txtMiles)
        Me.Controls.Add(Me.lblInches)
        Me.Controls.Add(Me.lblFeet)
        Me.Controls.Add(Me.lblYards)
        Me.Controls.Add(Me.lblMiles)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.MaximizeBox = False
        Me.Name = "frmConversion"
        Me.Text = "Length Conversion"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents lblMiles As System.Windows.Forms.Label
    Friend WithEvents lblYards As System.Windows.Forms.Label
    Friend WithEvents lblFeet As System.Windows.Forms.Label
    Friend WithEvents lblInches As System.Windows.Forms.Label
    Friend WithEvents txtMiles As System.Windows.Forms.TextBox
    Friend WithEvents txtYards As System.Windows.Forms.TextBox
    Friend WithEvents txtFeet As System.Windows.Forms.TextBox
    Friend WithEvents txtInches As System.Windows.Forms.TextBox
    Friend WithEvents btnConvert As System.Windows.Forms.Button
    Friend WithEvents lstResults As System.Windows.Forms.ListBox

End Class
