<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmRPT_Aging
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frmRPT_Aging))
        Me.GpRefAC = New System.Windows.Forms.GroupBox
        Me.rdbCredit = New System.Windows.Forms.RadioButton
        Me.rdbDebit = New System.Windows.Forms.RadioButton
        Me.dtpVDate = New System.Windows.Forms.DateTimePicker
        Me.Label3 = New System.Windows.Forms.Label
        Me.Label4 = New System.Windows.Forms.Label
        Me.lblEndName = New System.Windows.Forms.Label
        Me.mskEndCode = New System.Windows.Forms.MaskedTextBox
        Me.lblStartName = New System.Windows.Forms.Label
        Me.lblBrName = New System.Windows.Forms.Label
        Me.txtBrCode = New System.Windows.Forms.TextBox
        Me.mskStartCode = New System.Windows.Forms.MaskedTextBox
        Me.Label2 = New System.Windows.Forms.Label
        Me.Label1 = New System.Windows.Forms.Label
        Me.GpAnalysis = New System.Windows.Forms.GroupBox
        Me.Label24 = New System.Windows.Forms.Label
        Me.Label23 = New System.Windows.Forms.Label
        Me.Label22 = New System.Windows.Forms.Label
        Me.Label21 = New System.Windows.Forms.Label
        Me.Label20 = New System.Windows.Forms.Label
        Me.Label19 = New System.Windows.Forms.Label
        Me.Label17 = New System.Windows.Forms.Label
        Me.txtSeven = New System.Windows.Forms.TextBox
        Me.txtSix = New System.Windows.Forms.TextBox
        Me.txtFive = New System.Windows.Forms.TextBox
        Me.txtFour = New System.Windows.Forms.TextBox
        Me.txtThird = New System.Windows.Forms.TextBox
        Me.txtSecond = New System.Windows.Forms.TextBox
        Me.txtFirst = New System.Windows.Forms.TextBox
        Me.Label18 = New System.Windows.Forms.Label
        Me.Label15 = New System.Windows.Forms.Label
        Me.Label16 = New System.Windows.Forms.Label
        Me.Label13 = New System.Windows.Forms.Label
        Me.Label14 = New System.Windows.Forms.Label
        Me.Label11 = New System.Windows.Forms.Label
        Me.Label12 = New System.Windows.Forms.Label
        Me.Label9 = New System.Windows.Forms.Label
        Me.Label10 = New System.Windows.Forms.Label
        Me.Label8 = New System.Windows.Forms.Label
        Me.Label7 = New System.Windows.Forms.Label
        Me.Label6 = New System.Windows.Forms.Label
        Me.Label5 = New System.Windows.Forms.Label
        Me.btnPreview = New System.Windows.Forms.Button
        Me.btnCancel = New System.Windows.Forms.Button
        Me.btnPrint = New System.Windows.Forms.Button
        Me.Label25 = New System.Windows.Forms.Label
        Me.GpRefAC.SuspendLayout()
        Me.GpAnalysis.SuspendLayout()
        Me.SuspendLayout()
        '
        'GpRefAC
        '
        Me.GpRefAC.Controls.Add(Me.rdbCredit)
        Me.GpRefAC.Controls.Add(Me.rdbDebit)
        Me.GpRefAC.Controls.Add(Me.dtpVDate)
        Me.GpRefAC.Controls.Add(Me.Label3)
        Me.GpRefAC.Controls.Add(Me.Label4)
        Me.GpRefAC.Controls.Add(Me.lblEndName)
        Me.GpRefAC.Controls.Add(Me.mskEndCode)
        Me.GpRefAC.Controls.Add(Me.lblStartName)
        Me.GpRefAC.Controls.Add(Me.lblBrName)
        Me.GpRefAC.Controls.Add(Me.txtBrCode)
        Me.GpRefAC.Controls.Add(Me.mskStartCode)
        Me.GpRefAC.Controls.Add(Me.Label2)
        Me.GpRefAC.Controls.Add(Me.Label1)
        Me.GpRefAC.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.GpRefAC.Location = New System.Drawing.Point(8, 53)
        Me.GpRefAC.Name = "GpRefAC"
        Me.GpRefAC.Size = New System.Drawing.Size(452, 149)
        Me.GpRefAC.TabIndex = 1
        Me.GpRefAC.TabStop = False
        '
        'rdbCredit
        '
        Me.rdbCredit.AutoSize = True
        Me.rdbCredit.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.rdbCredit.Location = New System.Drawing.Point(275, 123)
        Me.rdbCredit.Name = "rdbCredit"
        Me.rdbCredit.Size = New System.Drawing.Size(114, 20)
        Me.rdbCredit.TabIndex = 6
        Me.rdbCredit.TabStop = True
        Me.rdbCredit.Text = "Credit Balance"
        Me.rdbCredit.UseVisualStyleBackColor = True
        '
        'rdbDebit
        '
        Me.rdbDebit.AutoSize = True
        Me.rdbDebit.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.rdbDebit.Location = New System.Drawing.Point(94, 123)
        Me.rdbDebit.Name = "rdbDebit"
        Me.rdbDebit.Size = New System.Drawing.Size(111, 20)
        Me.rdbDebit.TabIndex = 5
        Me.rdbDebit.TabStop = True
        Me.rdbDebit.Text = "Debit Balance"
        Me.rdbDebit.UseVisualStyleBackColor = True
        '
        'dtpVDate
        '
        Me.dtpVDate.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.dtpVDate.Format = System.Windows.Forms.DateTimePickerFormat.[Short]
        Me.dtpVDate.Location = New System.Drawing.Point(94, 95)
        Me.dtpVDate.Name = "dtpVDate"
        Me.dtpVDate.Size = New System.Drawing.Size(91, 22)
        Me.dtpVDate.TabIndex = 4
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label3.Location = New System.Drawing.Point(15, 100)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(71, 13)
        Me.Label3.TabIndex = 10
        Me.Label3.Text = "Analysis Date"
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label4.Location = New System.Drawing.Point(18, 74)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(68, 13)
        Me.Label4.TabIndex = 9
        Me.Label4.Text = "Ending Code"
        '
        'lblEndName
        '
        Me.lblEndName.BackColor = System.Drawing.SystemColors.ButtonHighlight
        Me.lblEndName.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.lblEndName.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblEndName.Location = New System.Drawing.Point(198, 70)
        Me.lblEndName.Name = "lblEndName"
        Me.lblEndName.Size = New System.Drawing.Size(248, 22)
        Me.lblEndName.TabIndex = 8
        '
        'mskEndCode
        '
        Me.mskEndCode.BackColor = System.Drawing.SystemColors.ButtonHighlight
        Me.mskEndCode.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.mskEndCode.Location = New System.Drawing.Point(94, 70)
        Me.mskEndCode.Mask = "##"
        Me.mskEndCode.Name = "mskEndCode"
        Me.mskEndCode.Size = New System.Drawing.Size(98, 22)
        Me.mskEndCode.TabIndex = 3
        Me.mskEndCode.Tag = "Enter Account Code"
        '
        'lblStartName
        '
        Me.lblStartName.BackColor = System.Drawing.SystemColors.ButtonHighlight
        Me.lblStartName.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.lblStartName.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblStartName.Location = New System.Drawing.Point(198, 45)
        Me.lblStartName.Name = "lblStartName"
        Me.lblStartName.Size = New System.Drawing.Size(248, 22)
        Me.lblStartName.TabIndex = 6
        '
        'lblBrName
        '
        Me.lblBrName.BackColor = System.Drawing.SystemColors.ButtonHighlight
        Me.lblBrName.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.lblBrName.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblBrName.Location = New System.Drawing.Point(142, 20)
        Me.lblBrName.Name = "lblBrName"
        Me.lblBrName.Size = New System.Drawing.Size(304, 22)
        Me.lblBrName.TabIndex = 5
        '
        'txtBrCode
        '
        Me.txtBrCode.BackColor = System.Drawing.SystemColors.ButtonHighlight
        Me.txtBrCode.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txtBrCode.Location = New System.Drawing.Point(94, 20)
        Me.txtBrCode.MaxLength = 3
        Me.txtBrCode.Name = "txtBrCode"
        Me.txtBrCode.Size = New System.Drawing.Size(46, 22)
        Me.txtBrCode.TabIndex = 1
        Me.txtBrCode.Tag = "Enter Description"
        '
        'mskStartCode
        '
        Me.mskStartCode.BackColor = System.Drawing.SystemColors.ButtonHighlight
        Me.mskStartCode.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.mskStartCode.Location = New System.Drawing.Point(94, 45)
        Me.mskStartCode.Mask = "##"
        Me.mskStartCode.Name = "mskStartCode"
        Me.mskStartCode.Size = New System.Drawing.Size(98, 22)
        Me.mskStartCode.TabIndex = 2
        Me.mskStartCode.Tag = "Enter Account Code"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label2.Location = New System.Drawing.Point(16, 49)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(71, 13)
        Me.Label2.TabIndex = 1
        Me.Label2.Text = "Starting Code"
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.Location = New System.Drawing.Point(18, 23)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(69, 13)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "Branch Code"
        '
        'GpAnalysis
        '
        Me.GpAnalysis.Controls.Add(Me.Label24)
        Me.GpAnalysis.Controls.Add(Me.Label23)
        Me.GpAnalysis.Controls.Add(Me.Label22)
        Me.GpAnalysis.Controls.Add(Me.Label21)
        Me.GpAnalysis.Controls.Add(Me.Label20)
        Me.GpAnalysis.Controls.Add(Me.Label19)
        Me.GpAnalysis.Controls.Add(Me.Label17)
        Me.GpAnalysis.Controls.Add(Me.txtSeven)
        Me.GpAnalysis.Controls.Add(Me.txtSix)
        Me.GpAnalysis.Controls.Add(Me.txtFive)
        Me.GpAnalysis.Controls.Add(Me.txtFour)
        Me.GpAnalysis.Controls.Add(Me.txtThird)
        Me.GpAnalysis.Controls.Add(Me.txtSecond)
        Me.GpAnalysis.Controls.Add(Me.txtFirst)
        Me.GpAnalysis.Controls.Add(Me.Label18)
        Me.GpAnalysis.Controls.Add(Me.Label15)
        Me.GpAnalysis.Controls.Add(Me.Label16)
        Me.GpAnalysis.Controls.Add(Me.Label13)
        Me.GpAnalysis.Controls.Add(Me.Label14)
        Me.GpAnalysis.Controls.Add(Me.Label11)
        Me.GpAnalysis.Controls.Add(Me.Label12)
        Me.GpAnalysis.Controls.Add(Me.Label9)
        Me.GpAnalysis.Controls.Add(Me.Label10)
        Me.GpAnalysis.Controls.Add(Me.Label8)
        Me.GpAnalysis.Controls.Add(Me.Label7)
        Me.GpAnalysis.Controls.Add(Me.Label6)
        Me.GpAnalysis.Controls.Add(Me.Label5)
        Me.GpAnalysis.Location = New System.Drawing.Point(8, 202)
        Me.GpAnalysis.Name = "GpAnalysis"
        Me.GpAnalysis.Size = New System.Drawing.Size(452, 173)
        Me.GpAnalysis.TabIndex = 2
        Me.GpAnalysis.TabStop = False
        '
        'Label24
        '
        Me.Label24.AutoSize = True
        Me.Label24.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label24.Location = New System.Drawing.Point(396, 151)
        Me.Label24.Name = "Label24"
        Me.Label24.Size = New System.Drawing.Size(31, 13)
        Me.Label24.TabIndex = 37
        Me.Label24.Text = "Days"
        '
        'Label23
        '
        Me.Label23.AutoSize = True
        Me.Label23.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label23.Location = New System.Drawing.Point(396, 130)
        Me.Label23.Name = "Label23"
        Me.Label23.Size = New System.Drawing.Size(31, 13)
        Me.Label23.TabIndex = 36
        Me.Label23.Text = "Days"
        '
        'Label22
        '
        Me.Label22.AutoSize = True
        Me.Label22.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label22.Location = New System.Drawing.Point(396, 108)
        Me.Label22.Name = "Label22"
        Me.Label22.Size = New System.Drawing.Size(31, 13)
        Me.Label22.TabIndex = 35
        Me.Label22.Text = "Days"
        '
        'Label21
        '
        Me.Label21.AutoSize = True
        Me.Label21.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label21.Location = New System.Drawing.Point(396, 85)
        Me.Label21.Name = "Label21"
        Me.Label21.Size = New System.Drawing.Size(31, 13)
        Me.Label21.TabIndex = 34
        Me.Label21.Text = "Days"
        '
        'Label20
        '
        Me.Label20.AutoSize = True
        Me.Label20.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label20.Location = New System.Drawing.Point(396, 62)
        Me.Label20.Name = "Label20"
        Me.Label20.Size = New System.Drawing.Size(31, 13)
        Me.Label20.TabIndex = 33
        Me.Label20.Text = "Days"
        '
        'Label19
        '
        Me.Label19.AutoSize = True
        Me.Label19.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label19.Location = New System.Drawing.Point(396, 38)
        Me.Label19.Name = "Label19"
        Me.Label19.Size = New System.Drawing.Size(31, 13)
        Me.Label19.TabIndex = 32
        Me.Label19.Text = "Days"
        '
        'Label17
        '
        Me.Label17.AutoSize = True
        Me.Label17.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label17.Location = New System.Drawing.Point(396, 16)
        Me.Label17.Name = "Label17"
        Me.Label17.Size = New System.Drawing.Size(31, 13)
        Me.Label17.TabIndex = 31
        Me.Label17.Text = "Days"
        '
        'txtSeven
        '
        Me.txtSeven.BackColor = System.Drawing.SystemColors.MenuBar
        Me.txtSeven.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.txtSeven.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txtSeven.Location = New System.Drawing.Point(335, 151)
        Me.txtSeven.MaxLength = 4
        Me.txtSeven.Name = "txtSeven"
        Me.txtSeven.Size = New System.Drawing.Size(46, 15)
        Me.txtSeven.TabIndex = 30
        Me.txtSeven.Tag = "Enter Description"
        Me.txtSeven.Text = "1095"
        Me.txtSeven.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'txtSix
        '
        Me.txtSix.BackColor = System.Drawing.SystemColors.MenuBar
        Me.txtSix.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.txtSix.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txtSix.Location = New System.Drawing.Point(335, 130)
        Me.txtSix.MaxLength = 4
        Me.txtSix.Name = "txtSix"
        Me.txtSix.Size = New System.Drawing.Size(46, 15)
        Me.txtSix.TabIndex = 29
        Me.txtSix.Tag = "Enter Description"
        Me.txtSix.Text = "1095"
        Me.txtSix.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'txtFive
        '
        Me.txtFive.BackColor = System.Drawing.SystemColors.MenuBar
        Me.txtFive.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.txtFive.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txtFive.Location = New System.Drawing.Point(335, 108)
        Me.txtFive.MaxLength = 3
        Me.txtFive.Name = "txtFive"
        Me.txtFive.Size = New System.Drawing.Size(46, 15)
        Me.txtFive.TabIndex = 28
        Me.txtFive.Tag = "Enter Description"
        Me.txtFive.Text = "730"
        Me.txtFive.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'txtFour
        '
        Me.txtFour.BackColor = System.Drawing.SystemColors.MenuBar
        Me.txtFour.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.txtFour.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txtFour.Location = New System.Drawing.Point(335, 85)
        Me.txtFour.MaxLength = 3
        Me.txtFour.Name = "txtFour"
        Me.txtFour.Size = New System.Drawing.Size(46, 15)
        Me.txtFour.TabIndex = 27
        Me.txtFour.Tag = "Enter Description"
        Me.txtFour.Text = "365"
        Me.txtFour.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'txtThird
        '
        Me.txtThird.BackColor = System.Drawing.SystemColors.MenuBar
        Me.txtThird.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.txtThird.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txtThird.Location = New System.Drawing.Point(335, 62)
        Me.txtThird.MaxLength = 3
        Me.txtThird.Name = "txtThird"
        Me.txtThird.Size = New System.Drawing.Size(46, 15)
        Me.txtThird.TabIndex = 26
        Me.txtThird.Tag = "Enter Description"
        Me.txtThird.Text = "180"
        Me.txtThird.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'txtSecond
        '
        Me.txtSecond.BackColor = System.Drawing.SystemColors.MenuBar
        Me.txtSecond.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.txtSecond.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txtSecond.Location = New System.Drawing.Point(335, 38)
        Me.txtSecond.MaxLength = 3
        Me.txtSecond.Name = "txtSecond"
        Me.txtSecond.Size = New System.Drawing.Size(46, 15)
        Me.txtSecond.TabIndex = 25
        Me.txtSecond.Tag = "Enter Description"
        Me.txtSecond.Text = "90"
        Me.txtSecond.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'txtFirst
        '
        Me.txtFirst.BackColor = System.Drawing.SystemColors.MenuBar
        Me.txtFirst.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.txtFirst.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txtFirst.Location = New System.Drawing.Point(335, 16)
        Me.txtFirst.MaxLength = 3
        Me.txtFirst.Name = "txtFirst"
        Me.txtFirst.Size = New System.Drawing.Size(46, 15)
        Me.txtFirst.TabIndex = 24
        Me.txtFirst.Tag = "Enter Description"
        Me.txtFirst.Text = "60"
        Me.txtFirst.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label18
        '
        Me.Label18.AutoSize = True
        Me.Label18.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label18.Location = New System.Drawing.Point(288, 151)
        Me.Label18.Name = "Label18"
        Me.Label18.Size = New System.Drawing.Size(30, 13)
        Me.Label18.TabIndex = 23
        Me.Label18.Text = "Over"
        '
        'Label15
        '
        Me.Label15.AutoSize = True
        Me.Label15.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label15.Location = New System.Drawing.Point(319, 130)
        Me.Label15.Name = "Label15"
        Me.Label15.Size = New System.Drawing.Size(10, 13)
        Me.Label15.TabIndex = 22
        Me.Label15.Text = "-"
        '
        'Label16
        '
        Me.Label16.AutoSize = True
        Me.Label16.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label16.Location = New System.Drawing.Point(293, 130)
        Me.Label16.Name = "Label16"
        Me.Label16.Size = New System.Drawing.Size(25, 13)
        Me.Label16.TabIndex = 21
        Me.Label16.Text = "731"
        '
        'Label13
        '
        Me.Label13.AutoSize = True
        Me.Label13.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label13.Location = New System.Drawing.Point(319, 108)
        Me.Label13.Name = "Label13"
        Me.Label13.Size = New System.Drawing.Size(10, 13)
        Me.Label13.TabIndex = 20
        Me.Label13.Text = "-"
        '
        'Label14
        '
        Me.Label14.AutoSize = True
        Me.Label14.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label14.Location = New System.Drawing.Point(293, 108)
        Me.Label14.Name = "Label14"
        Me.Label14.Size = New System.Drawing.Size(25, 13)
        Me.Label14.TabIndex = 19
        Me.Label14.Text = "366"
        '
        'Label11
        '
        Me.Label11.AutoSize = True
        Me.Label11.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label11.Location = New System.Drawing.Point(319, 85)
        Me.Label11.Name = "Label11"
        Me.Label11.Size = New System.Drawing.Size(10, 13)
        Me.Label11.TabIndex = 18
        Me.Label11.Text = "-"
        '
        'Label12
        '
        Me.Label12.AutoSize = True
        Me.Label12.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label12.Location = New System.Drawing.Point(293, 85)
        Me.Label12.Name = "Label12"
        Me.Label12.Size = New System.Drawing.Size(25, 13)
        Me.Label12.TabIndex = 17
        Me.Label12.Text = "181"
        '
        'Label9
        '
        Me.Label9.AutoSize = True
        Me.Label9.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label9.Location = New System.Drawing.Point(319, 62)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(10, 13)
        Me.Label9.TabIndex = 16
        Me.Label9.Text = "-"
        '
        'Label10
        '
        Me.Label10.AutoSize = True
        Me.Label10.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label10.Location = New System.Drawing.Point(299, 62)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(19, 13)
        Me.Label10.TabIndex = 15
        Me.Label10.Text = "91"
        '
        'Label8
        '
        Me.Label8.AutoSize = True
        Me.Label8.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label8.Location = New System.Drawing.Point(319, 38)
        Me.Label8.Name = "Label8"
        Me.Label8.Size = New System.Drawing.Size(10, 13)
        Me.Label8.TabIndex = 14
        Me.Label8.Text = "-"
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label7.Location = New System.Drawing.Point(299, 38)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(19, 13)
        Me.Label7.TabIndex = 13
        Me.Label7.Text = "61"
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label6.Location = New System.Drawing.Point(254, 16)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(75, 13)
        Me.Label6.TabIndex = 12
        Me.Label6.Text = "From  :      0   -"
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label5.Location = New System.Drawing.Point(15, 16)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(177, 13)
        Me.Label5.TabIndex = 11
        Me.Label5.Text = "Current Analysis Is Set As Follows  : "
        '
        'btnPreview
        '
        Me.btnPreview.Image = CType(resources.GetObject("btnPreview.Image"), System.Drawing.Image)
        Me.btnPreview.Location = New System.Drawing.Point(58, 391)
        Me.btnPreview.Name = "btnPreview"
        Me.btnPreview.Size = New System.Drawing.Size(108, 50)
        Me.btnPreview.TabIndex = 9
        Me.btnPreview.Text = "&Preview"
        Me.btnPreview.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        Me.btnPreview.UseVisualStyleBackColor = True
        '
        'btnCancel
        '
        Me.btnCancel.Image = CType(resources.GetObject("btnCancel.Image"), System.Drawing.Image)
        Me.btnCancel.Location = New System.Drawing.Point(303, 391)
        Me.btnCancel.Name = "btnCancel"
        Me.btnCancel.Size = New System.Drawing.Size(108, 50)
        Me.btnCancel.TabIndex = 11
        Me.btnCancel.Text = "&Close"
        Me.btnCancel.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        Me.btnCancel.UseVisualStyleBackColor = True
        '
        'btnPrint
        '
        Me.btnPrint.Image = CType(resources.GetObject("btnPrint.Image"), System.Drawing.Image)
        Me.btnPrint.Location = New System.Drawing.Point(182, 391)
        Me.btnPrint.Name = "btnPrint"
        Me.btnPrint.Size = New System.Drawing.Size(108, 50)
        Me.btnPrint.TabIndex = 10
        Me.btnPrint.Text = "Pr&int"
        Me.btnPrint.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        Me.btnPrint.UseVisualStyleBackColor = True
        '
        'Label25
        '
        Me.Label25.BackColor = System.Drawing.Color.PowderBlue
        Me.Label25.Font = New System.Drawing.Font("Nina", 24.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label25.ForeColor = System.Drawing.SystemColors.HotTrack
        Me.Label25.Location = New System.Drawing.Point(1, 0)
        Me.Label25.Name = "Label25"
        Me.Label25.Size = New System.Drawing.Size(469, 52)
        Me.Label25.TabIndex = 27
        Me.Label25.Text = "Aging Analysis"
        Me.Label25.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'frmRPT_Aging
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(466, 451)
        Me.Controls.Add(Me.Label25)
        Me.Controls.Add(Me.btnPreview)
        Me.Controls.Add(Me.btnCancel)
        Me.Controls.Add(Me.btnPrint)
        Me.Controls.Add(Me.GpAnalysis)
        Me.Controls.Add(Me.GpRefAC)
        Me.Name = "frmRPT_Aging"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Aging Analysis"
        Me.GpRefAC.ResumeLayout(False)
        Me.GpRefAC.PerformLayout()
        Me.GpAnalysis.ResumeLayout(False)
        Me.GpAnalysis.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents GpRefAC As System.Windows.Forms.GroupBox
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents lblEndName As System.Windows.Forms.Label
    Friend WithEvents mskEndCode As System.Windows.Forms.MaskedTextBox
    Friend WithEvents lblStartName As System.Windows.Forms.Label
    Friend WithEvents lblBrName As System.Windows.Forms.Label
    Friend WithEvents txtBrCode As System.Windows.Forms.TextBox
    Friend WithEvents mskStartCode As System.Windows.Forms.MaskedTextBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents dtpVDate As System.Windows.Forms.DateTimePicker
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents rdbCredit As System.Windows.Forms.RadioButton
    Friend WithEvents rdbDebit As System.Windows.Forms.RadioButton
    Friend WithEvents GpAnalysis As System.Windows.Forms.GroupBox
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents Label7 As System.Windows.Forms.Label
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents Label8 As System.Windows.Forms.Label
    Friend WithEvents Label18 As System.Windows.Forms.Label
    Friend WithEvents Label15 As System.Windows.Forms.Label
    Friend WithEvents Label16 As System.Windows.Forms.Label
    Friend WithEvents Label13 As System.Windows.Forms.Label
    Friend WithEvents Label14 As System.Windows.Forms.Label
    Friend WithEvents Label11 As System.Windows.Forms.Label
    Friend WithEvents Label12 As System.Windows.Forms.Label
    Friend WithEvents Label9 As System.Windows.Forms.Label
    Friend WithEvents Label10 As System.Windows.Forms.Label
    Friend WithEvents txtFirst As System.Windows.Forms.TextBox
    Friend WithEvents txtThird As System.Windows.Forms.TextBox
    Friend WithEvents txtSecond As System.Windows.Forms.TextBox
    Friend WithEvents txtFive As System.Windows.Forms.TextBox
    Friend WithEvents txtFour As System.Windows.Forms.TextBox
    Friend WithEvents Label17 As System.Windows.Forms.Label
    Friend WithEvents txtSeven As System.Windows.Forms.TextBox
    Friend WithEvents txtSix As System.Windows.Forms.TextBox
    Friend WithEvents Label20 As System.Windows.Forms.Label
    Friend WithEvents Label19 As System.Windows.Forms.Label
    Friend WithEvents Label24 As System.Windows.Forms.Label
    Friend WithEvents Label23 As System.Windows.Forms.Label
    Friend WithEvents Label22 As System.Windows.Forms.Label
    Friend WithEvents Label21 As System.Windows.Forms.Label
    Friend WithEvents btnPreview As System.Windows.Forms.Button
    Friend WithEvents btnCancel As System.Windows.Forms.Button
    Friend WithEvents btnPrint As System.Windows.Forms.Button
    Friend WithEvents Label25 As System.Windows.Forms.Label
End Class
