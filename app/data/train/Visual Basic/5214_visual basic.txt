Public Class FRMProgram

    Dim RATE_GST As Single = 5
    Dim RATE_PST As Single = 8.5

    Dim currCompany As Company = FRMChooseCompany.companies(FRMChooseCompany.currPosition)
    Dim currCar As Car
    Dim currTab As String = ""
    Dim carSelected As Boolean = False
    Dim newPoint As New System.Drawing.Point()
    Dim mouseX As Integer
    Dim mouseY As Integer
    Dim OriginalTitle As String
    Dim defName As String = Nothing
    Dim defPrice As String = Nothing
    Dim InactiveColor As Color = System.Drawing.Color.FromArgb(CType(CType(85, Byte), Integer), CType(CType(85, Byte), Integer), CType(CType(85, Byte), Integer))

    Dim optionsTotal, ACtax, tradein, subTotal, GST, PST, total, rate, downPayment, monthlyPayment, totalPayment As Double

    Private Sub FRMProgram_FormClosed(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
        FRMChooseCompany.Show()
    End Sub

    Private Sub FRMProgram_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Me.Width = 759
        Me.CenterToScreen()

        optionsTotal = 975
        ACtax = 0
        tradein = 0
        rate = 0.01

        OriginalTitle = LBLtitle.Text
        PICsummary.Parent = PICwheel
        PICoptions.Parent = PICwheel
        PICfinancing.Parent = PICwheel
        PICcolor.Parent = PICwheel
        PICsecurity.Parent = PICwheel
        PICtradein.Parent = PICwheel
        PICwheelLogo.Parent = PICwheel

        PNLsummary.Parent = PICwheel
        PNLtradein.Parent = PICwheel
        PNLoptions.Parent = PICwheel
        PNLsecurity.Parent = PICwheel
        PNLcolor.Parent = PICwheel
        PNLfinancing.Parent = PICwheel

        PICsummary.Left = 201
        PICsummary.Top = 28
        PICoptions.Left = 494
        PICoptions.Top = 94
        PICfinancing.Left = 472
        PICfinancing.Top = 346
        PICcolor.Left = 247
        PICcolor.Top = 589
        PICsecurity.Left = 23
        PICsecurity.Top = 366
        PICtradein.Left = 26
        PICtradein.Top = 85
        PICwheelLogo.Left = 315
        PICwheelLogo.Top = 321

        PNLsummary.Left = 213
        PNLsummary.Top = 140
        PNLtradein.Left = 213
        PNLtradein.Top = 140
        PNLoptions.Left = 213
        PNLoptions.Top = 140
        PNLsecurity.Left = 213
        PNLsecurity.Top = 140
        PNLcolor.Left = 213
        PNLcolor.Top = 140
        PNLfinancing.Left = 213
        PNLfinancing.Top = 140

        ChooseCompany()
    End Sub

    Sub Calculate()
        UpdateCheckAll()

        If CHKtradein.Checked = False Then tradein = 0
        subTotal = currCar.price + optionsTotal + ACtax - tradein
        GST = subTotal * RATE_GST / 100.0
        PST = (subTotal + GST) * RATE_PST / 100.0
        GST = Math.Floor(GST * 100) / 100
        PST = Math.Floor(PST * 100) / 100

        total = subTotal + GST + PST

        If CHKfinancing.Checked Then
            If CHKdownPayment.Checked Then
                total -= downPayment
                LBLdownPayment.Text = FormatCurrency(downPayment)
                LBLdownPaymentL.Visible = True
                LBLdownPayment.Visible = True
            Else
                LBLdownPaymentL.Visible = False
                LBLdownPayment.Visible = False
            End If
            monthlyPayment = Pmt(rate / 12, (TRBmonths.Value + TRByears.Value * 12), -total, 0, 1)
            totalPayment = monthlyPayment * (TRBmonths.Value + TRByears.Value * 12)

            defPrice = FormatCurrency(totalPayment, 0)
            LBLtotal.Text = FormatCurrency(total)
            LBLmonthly.Text = FormatCurrency(monthlyPayment)
            LBLfinal.Text = FormatCurrency(totalPayment)

            ' hide and show
            LBLmonthlyL.Visible = True
            LBLmonthly.Visible = True
            LBLtotalL.Visible = True
            LBLtotal.Visible = True
        Else
            LBLfinal.Text = FormatCurrency(total)
            defPrice = FormatCurrency(total, 0)

            ' hide and show
            LBLdownPaymentL.Visible = False
            LBLdownPayment.Visible = False
            LBLmonthlyL.Visible = False
            LBLmonthly.Visible = False
            LBLtotalL.Visible = False
            LBLtotal.Visible = False
        End If

        LBLoptionsPrice.Text = FormatCurrency(optionsTotal)
        LBLoptionsTotal.Text = FormatCurrency(optionsTotal)
        LBLoptionsTotal2.Text = FormatCurrency(optionsTotal)
        LBLACtax.Text = FormatCurrency(ACtax)
        LBLtradeinValue.Text = FormatCurrency(tradein)
        LBLGST.Text = FormatCurrency(GST)
        LBLPST.Text = FormatCurrency(PST)

        LBLcarPrice.Text = defPrice
    End Sub

    Sub ChooseCompany()
        PICcar0.Image = currCompany.cars(0).picture
        PICcar1.Image = currCompany.cars(1).picture
        PICcar2.Image = currCompany.cars(2).picture
        PICcar3.Image = currCompany.cars(3).picture
        PICcar4.Image = currCompany.cars(4).picture
        PICcar5.Image = currCompany.cars(5).picture
        PICcompany.Image = currCompany.logo

        If currCompany.name = "Audi" Then
            PICicon.Image = My.Resources.Audi_small
            Me.Icon = System.Drawing.Icon.FromHandle(My.Resources.Audi_small.GetHicon)
            PICwheelLogo.Image = My.Resources.Audi_small
        Else
            PICicon.Image = currCompany.logo
            Me.Icon = System.Drawing.Icon.FromHandle(My.Resources.ResourceManager.GetObject(currCompany.name).GetHicon)
            PICwheelLogo.Image = currCompany.logo
        End If

        LBLcompany.Text = currCompany.name
        SetTitle(currCompany.name)
        defName = Nothing
        defPrice = Nothing
        ResetCarLBL()
        ResetBorders()
        If carSelected Then ChooseTab(currTab)
        carSelected = False
        FRMChooseCompany.chooseCompany()
        FRMChooseCompany.DisplayAvailable()
    End Sub

    Sub ChooseCar(ByRef picture As System.Windows.Forms.PictureBox, ByVal position As Integer)
        If picture.BorderStyle = BorderStyle.Fixed3D Then  ' car already selected -> deselect it
            defName = Nothing
            defPrice = Nothing
            ResetBorders()
            ChooseTab(currTab)
            carSelected = False
        Else
            carSelected = True
            currCar = currCompany.cars(position)
            currTab = ""
            PICsummary.Image = My.Resources.summary_hover
            ChooseTab("summary")
            defName = LBLcarName.Text
            defPrice = LBLcarPrice.Text
            ResetBorders()
            picture.BorderStyle = BorderStyle.Fixed3D

            LBLbase.Text = FormatCurrency(currCar.price)
            NUDtradein.Maximum = currCar.price

            UncheckAll()

            Calculate()
        End If
    End Sub

    Sub ChooseTab(ByVal tab As String)
        If carSelected Then
            If tab = currTab Then ' tab already selected -> deselect it
                currTab = ""
                ResetTabs()
                PICwheel.Image = My.Resources.ResourceManager.GetObject("wheel")
                PICwheelLogo.Visible = True
            Else
                currTab = tab
                PICwheel.Image = My.Resources.ResourceManager.GetObject("wheel_dim")
                PICwheelLogo.Visible = False
                        If tab = "summary" Then
                            PNLsummary.Visible = True
                            PNLsummary.BringToFront()
                            PICsummary.Image = My.Resources.ResourceManager.GetObject("summary_hover")
                        Else
                            PICsummary.Image = My.Resources.ResourceManager.GetObject("summary")
                        End If
                        If tab = "options" Then
                            PNLoptions.Visible = True
                            PNLoptions.BringToFront()
                            PICoptions.Image = My.Resources.ResourceManager.GetObject("options_hover")
                        Else
                            PICoptions.Image = My.Resources.ResourceManager.GetObject("options")
                        End If
                        If tab = "financing" Then
                            PNLfinancing.Visible = True
                            PNLfinancing.BringToFront()
                            PICfinancing.Image = My.Resources.ResourceManager.GetObject("financing_hover")
                        Else
                            PICfinancing.Image = My.Resources.ResourceManager.GetObject("financing")
                        End If
                        If tab = "color" Then
                            PNLcolor.Visible = True
                            PNLcolor.BringToFront()
                            PICcolor.Image = My.Resources.ResourceManager.GetObject("color_hover")
                        Else
                            PICcolor.Image = My.Resources.ResourceManager.GetObject("color")
                        End If
                        If tab = "security" Then
                            PNLsecurity.Visible = True
                            PNLsecurity.BringToFront()
                            PICsecurity.Image = My.Resources.ResourceManager.GetObject("security_hover")
                        Else
                            PICsecurity.Image = My.Resources.ResourceManager.GetObject("security")
                        End If
                        If tab = "tradein" Then
                            PNLtradein.Visible = True
                    PNLtradein.BringToFront()
                    PICtradein.Image = My.Resources.ResourceManager.GetObject("tradein_hover")
                        Else
                            PICtradein.Image = My.Resources.ResourceManager.GetObject("tradein")
                        End If
                    End If
                Else
            MsgBox("Please select a car before trying to open a tab", MsgBoxStyle.Exclamation, "Warning")
                End If

    End Sub

    Sub ResetCarLBL()
        LBLcarName.Text = defName
        LBLcarPrice.Text = defPrice
    End Sub

    Sub ResetBorders()
        PICcar0.BorderStyle = BorderStyle.None
        PICcar1.BorderStyle = BorderStyle.None
        PICcar2.BorderStyle = BorderStyle.None
        PICcar3.BorderStyle = BorderStyle.None
        PICcar4.BorderStyle = BorderStyle.None
        PICcar5.BorderStyle = BorderStyle.None
    End Sub

    Sub ResetTabs()
        PICsummary.Image = My.Resources.ResourceManager.GetObject("summary")
        PICoptions.Image = My.Resources.ResourceManager.GetObject("options")
        PICfinancing.Image = My.Resources.ResourceManager.GetObject("financing")
        PICcolor.Image = My.Resources.ResourceManager.GetObject("color")
        PICsecurity.Image = My.Resources.ResourceManager.GetObject("security")
        PICtradein.Image = My.Resources.ResourceManager.GetObject("tradein")
        PNLsummary.Visible = False
        PNLoptions.Visible = False
        PNLfinancing.Visible = False
        PNLcolor.Visible = False
        PNLsecurity.Visible = False
        PNLtradein.Visible = False
    End Sub

    Sub UpdateCheckAll()
        If CHKAC.Checked And CHKairbags.Checked And CHKbulletproof.Checked And CHKcameras.Checked And CHKdeluxe.Checked And CHKDVD.Checked And CHKGPS.Checked And CHKheated.Checked And CHKonstar.Checked And CHKroof.Checked And CHKtires.Checked And CHKtinted.Checked And CHKmissile.Checked And CHKM60.Checked Then
            CHKall.CheckState = CheckState.Checked
            CHKall2.CheckState = CheckState.Checked
            CHKall.Text = "Uncheck All"
            CHKall2.Text = "Uncheck All"
        ElseIf CHKAC.Checked Or CHKairbags.Checked Or CHKbulletproof.Checked Or CHKcameras.Checked Or CHKdeluxe.Checked Or CHKDVD.Checked Or CHKGPS.Checked Or CHKheated.Checked Or CHKonstar.Checked Or CHKroof.Checked Or CHKtires.Checked Or CHKtinted.Checked Or CHKmissile.Checked Or CHKM60.Checked Then
            CHKall.CheckState = CheckState.Indeterminate
            CHKall2.CheckState = CheckState.Indeterminate
            CHKall.Text = "Check All"
            CHKall2.Text = "Check All"
        Else
            CHKall.CheckState = CheckState.Unchecked
            CHKall2.CheckState = CheckState.Unchecked
            CHKall.Text = "Check All"
            CHKall2.Text = "Check All"
        End If
    End Sub

    Sub CheckAll()
        optionsTotal = 82051
        CHKAC.Checked = True
        CHKairbags.Checked = True
        CHKbulletproof.Checked = True
        CHKcameras.Checked = True
        CHKdeluxe.Checked = True
        CHKDVD.Checked = True
        CHKGPS.Checked = True
        CHKheated.Checked = True
        CHKonstar.Checked = True
        CHKroof.Checked = True
        CHKtires.Checked = True
        CHKtinted.Checked = True
        CHKmissile.Checked = True
        CHKM60.Checked = True
        Calculate()
    End Sub

    Sub UncheckAll()
        optionsTotal = 975
        CHKAC.Checked = False
        CHKairbags.Checked = False
        CHKbulletproof.Checked = False
        CHKcameras.Checked = False
        CHKdeluxe.Checked = False
        CHKDVD.Checked = False
        CHKGPS.Checked = False
        CHKheated.Checked = False
        CHKonstar.Checked = False
        CHKroof.Checked = False
        CHKtires.Checked = False
        CHKtinted.Checked = False
        CHKmissile.Checked = False
        CHKM60.Checked = False
        Calculate()
    End Sub

    Private Sub Program_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles Me.KeyDown
        If e.KeyCode = Keys.Left Then
            PrevCompany()
        End If
        If e.KeyCode = Keys.Right Then
            NextCompany()
        End If
        If e.KeyCode = Keys.Escape Or e.KeyCode = Keys.Back Then
            Me.Close()
        End If
    End Sub

    Private Sub Panel1_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles Panel1.Click
        Me.Close()
    End Sub

    Private Sub LBLcompany_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles LBLcompany.Click
        Me.Close()
    End Sub

    Private Sub PICcompany_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICcompany.Click
        Me.Close()
    End Sub

    Private Sub PICcompany_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICcompany.MouseEnter
        LBLgoback.Visible = True
    End Sub

    Private Sub PICcompany_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICcompany.MouseLeave
        LBLgoback.Visible = False
    End Sub

    Sub SetTitle(ByVal title As String)
        title &= " - " & OriginalTitle
        LBLtitle.Text = title
        Me.Text = title
    End Sub

    Private Sub PNLtitlebar_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles PNLtitlebar.MouseDown
        StartMoving()
    End Sub

    Private Sub PNLtitlebar_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles PNLtitlebar.MouseMove
        MoveWindow(e)
    End Sub

    Private Sub LBLtitle_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles LBLtitle.MouseDown
        StartMoving()
    End Sub

    Private Sub LBLtitle_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles LBLtitle.MouseMove
        MoveWindow(e)
    End Sub

    Sub StartMoving()
        mouseX = Control.MousePosition.X - Me.Location.X
        mouseY = Control.MousePosition.Y - Me.Location.Y
    End Sub

    Sub MoveWindow(ByVal e As System.Windows.Forms.MouseEventArgs)
        If e.Button = MouseButtons.Left Then
            newPoint = Control.MousePosition
            newPoint.X = newPoint.X - (mouseX)
            newPoint.Y = newPoint.Y - (mouseY)
            Me.Location = newPoint
        End If
    End Sub

    Private Sub PICminimize_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICminimize.Click
        Me.WindowState = FormWindowState.Minimized
    End Sub

    Private Sub PICminimize_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICminimize.MouseEnter
        PICminimize.Image = My.Resources.minimize_hover
    End Sub

    Private Sub PICminimize_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICminimize.MouseLeave
        PICminimize.Image = My.Resources.minimize
    End Sub

    Private Sub PICclose_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICclose.Click
        End
    End Sub

    Private Sub PICclose_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICclose.MouseEnter
        PICclose.Image = My.Resources.close_hover
    End Sub

    Private Sub PICclose_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICclose.MouseLeave
        PICclose.Image = My.Resources.close
    End Sub

    Private Sub PICcar0_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICcar0.Click
        ChooseCar(PICcar0, 0)
    End Sub

    Private Sub PICcar1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICcar1.Click
        ChooseCar(PICcar1, 1)
    End Sub

    Private Sub PICcar2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICcar2.Click
        ChooseCar(PICcar2, 2)
    End Sub

    Private Sub PICcar3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICcar3.Click
        ChooseCar(PICcar3, 3)
    End Sub

    Private Sub PICcar4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICcar4.Click
        ChooseCar(PICcar4, 4)
    End Sub

    Private Sub PICcar5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICcar5.Click
        ChooseCar(PICcar5, 5)
    End Sub

    Private Sub PICcar0_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICcar0.MouseEnter
        LBLcarName.Text = currCompany.name & " " & currCompany.cars(0).name
        LBLcarPrice.Text = FormatCurrency(currCompany.cars(0).price, 0)
    End Sub

    Private Sub PICcar1_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICcar1.MouseEnter
        LBLcarName.Text = currCompany.name & " " & currCompany.cars(1).name
        LBLcarPrice.Text = FormatCurrency(currCompany.cars(1).price, 0)
    End Sub

    Private Sub PICcar2_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICcar2.MouseEnter
        LBLcarName.Text = currCompany.name & " " & currCompany.cars(2).name
        LBLcarPrice.Text = FormatCurrency(currCompany.cars(2).price, 0)
    End Sub

    Private Sub PICcar3_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICcar3.MouseEnter
        LBLcarName.Text = currCompany.name & " " & currCompany.cars(3).name
        LBLcarPrice.Text = FormatCurrency(currCompany.cars(3).price, 0)
    End Sub

    Private Sub PICcar4_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICcar4.MouseEnter
        LBLcarName.Text = currCompany.name & " " & currCompany.cars(4).name
        LBLcarPrice.Text = FormatCurrency(currCompany.cars(4).price, 0)
    End Sub

    Private Sub PICcar5_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICcar5.MouseEnter
        LBLcarName.Text = currCompany.name & " " & currCompany.cars(5).name
        LBLcarPrice.Text = FormatCurrency(currCompany.cars(5).price, 0)
    End Sub

    Private Sub PICcar0_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICcar0.MouseLeave
        ResetCarLBL()
    End Sub

    Private Sub PICcar1_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICcar1.MouseLeave
        ResetCarLBL()
    End Sub

    Private Sub PICcar2_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICcar2.MouseLeave
        ResetCarLBL()
    End Sub

    Private Sub PICcar3_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICcar3.MouseLeave
        ResetCarLBL()
    End Sub

    Private Sub PICcar4_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICcar4.MouseLeave
        ResetCarLBL()
    End Sub

    Private Sub PICcar5_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICcar5.MouseLeave
        ResetCarLBL()
    End Sub

    Function PrevPos(ByVal pos As Integer)
        pos -= 1
        If pos < 0 Then pos = 5
        Return pos
    End Function

    Function NextPos(ByVal pos As Integer)
        pos += 1
        If pos > 5 Then pos = 0
        Return pos
    End Function

    Function PrevPosComp(ByVal pos As Integer)
        pos -= 1
        If pos < 0 Then pos = 4
        Return pos
    End Function

    Function NextPosComp(ByVal pos As Integer)
        pos += 1
        If pos > 4 Then pos = 0
        Return pos
    End Function

    Private Sub PICprev_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICprev.Click
        PrevCompany()
    End Sub

    Private Sub PICprev_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICprev.MouseEnter
        PICprev.Image = My.Resources.prev_hover
    End Sub

    Private Sub PICprev_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICprev.MouseLeave
        PICprev.Image = My.Resources.prev
    End Sub

    Private Sub PICnext_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICnext.MouseEnter
        PICnext.Image = My.Resources.next_hover
    End Sub

    Private Sub PICnext_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICnext.MouseLeave
        PICnext.Image = My.Resources._next
    End Sub

    Private Sub PICnext_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICnext.Click
        NextCompany()
    End Sub
    Sub PrevCompany()
        currCompany = FRMChooseCompany.companies(PrevPosComp(FRMChooseCompany.currPosition))
        FRMChooseCompany.currPosition = PrevPosComp(FRMChooseCompany.currPosition)
        ChooseCompany()
    End Sub
    Sub NextCompany()
        currCompany = FRMChooseCompany.companies(NextPosComp(FRMChooseCompany.currPosition))
        FRMChooseCompany.currPosition = NextPosComp(FRMChooseCompany.currPosition)
        ChooseCompany()
    End Sub

    Private Sub PICsummary_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICsummary.Click
        ChooseTab("summary")
    End Sub
    Private Sub PICoptions_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICoptions.Click
        ChooseTab("options")
    End Sub
    Private Sub PICfinancing_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICfinancing.Click
        ChooseTab("financing")
    End Sub
    Private Sub PICcolor_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICcolor.Click
        ChooseTab("color")
    End Sub
    Private Sub PICsecurity_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICsecurity.Click
        ChooseTab("security")
    End Sub
    Private Sub PICtradeIn_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICtradein.Click
        ChooseTab("tradein")
    End Sub

    Private Sub PICsummary_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICsummary.MouseEnter
        PICsummary.Image = My.Resources.ResourceManager.GetObject("summary_hover")
    End Sub
    Private Sub PICoptions_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICoptions.MouseEnter
        PICoptions.Image = My.Resources.ResourceManager.GetObject("options_hover")
    End Sub
    Private Sub PICfinancing_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICfinancing.MouseEnter
        PICfinancing.Image = My.Resources.ResourceManager.GetObject("financing_hover")
    End Sub
    Private Sub PICcolor_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICcolor.MouseEnter
        PICcolor.Image = My.Resources.ResourceManager.GetObject("color_hover")
    End Sub
    Private Sub PICsecurity_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICsecurity.MouseEnter
        PICsecurity.Image = My.Resources.ResourceManager.GetObject("security_hover")
    End Sub
    Private Sub PICtradein_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICtradein.MouseEnter
        PICtradein.Image = My.Resources.ResourceManager.GetObject("tradein_hover")
    End Sub

    Private Sub PICsummary_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICsummary.MouseLeave
        If currTab <> "summary" Then PICsummary.Image = My.Resources.ResourceManager.GetObject("summary")
    End Sub
    Private Sub PICoptions_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICoptions.MouseLeave
        If currTab <> "options" Then PICoptions.Image = My.Resources.ResourceManager.GetObject("options")
    End Sub
    Private Sub PICfinancing_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICfinancing.MouseLeave
        If currTab <> "financing" Then PICfinancing.Image = My.Resources.ResourceManager.GetObject("financing")
    End Sub
    Private Sub PICcolor_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICcolor.MouseLeave
        If currTab <> "color" Then PICcolor.Image = My.Resources.ResourceManager.GetObject("color")
    End Sub
    Private Sub PICsecurity_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICsecurity.MouseLeave
        If currTab <> "security" Then PICsecurity.Image = My.Resources.ResourceManager.GetObject("security")
    End Sub
    Private Sub PICtradein_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICtradein.MouseLeave
        If currTab <> "tradein" Then PICtradein.Image = My.Resources.ResourceManager.GetObject("tradein")
    End Sub

    Private Sub CHKtradein_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKtradein.CheckedChanged
        If CHKtradein.Checked Then
            LBLtradinAmount.ForeColor = Color.White
            NUDtradein.Enabled = True
            tradein = NUDtradein.Value
        Else
            LBLtradinAmount.ForeColor = InactiveColor
            NUDtradein.Enabled = False
            tradein = 0
        End If
        Calculate()
    End Sub

    Private Sub NUDtradein_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NUDtradein.ValueChanged
        tradein = NUDtradein.Value
        Calculate()
    End Sub

    Private Sub CHKAC_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKAC.Click
        If CHKAC.Checked Then
            ACtax = 100
            optionsTotal += 1300
        Else
            ACtax = 0
            optionsTotal -= 1300
        End If
        Calculate()
    End Sub

    Private Sub CHKdeluxe_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKdeluxe.Click
        If CHKdeluxe.Checked Then
            optionsTotal += 1200
        Else
            optionsTotal -= 1200
        End If
        Calculate()
    End Sub

    Private Sub CHKheated_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKheated.Click
        If CHKheated.Checked Then
            optionsTotal += 700
        Else
            optionsTotal -= 700
        End If
        Calculate()
    End Sub

    Private Sub CHKDVD_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKDVD.Click
        If CHKDVD.Checked Then
            optionsTotal += 150
        Else
            optionsTotal -= 150
        End If
        Calculate()
    End Sub

    Private Sub CHKGPS_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKGPS.Click
        If CHKGPS.Checked Then
            optionsTotal += 400
        Else
            optionsTotal -= 400
        End If
        Calculate()
    End Sub

    Private Sub CHKroof_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKroof.Click
        If CHKroof.Checked Then
            optionsTotal += 1000
        Else
            optionsTotal -= 1000
        End If
        Calculate()
    End Sub

    Private Sub CHKtinted_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKtinted.Click
        If CHKtinted.Checked Then
            optionsTotal += 300
        Else
            optionsTotal -= 300
        End If
        Calculate()
    End Sub

    Private Sub CHKmissile_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKmissile.Click
        If CHKmissile.Checked Then
            optionsTotal += 14000
        Else
            optionsTotal -= 14000
        End If
        Calculate()
    End Sub

    Private Sub CHKM60_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKM60.Click
        If CHKM60.Checked Then
            optionsTotal += 6000
        Else
            optionsTotal -= 6000
        End If
        Calculate()
    End Sub

    Private Sub CHKairbags_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKairbags.Click
        If CHKairbags.Checked Then
            optionsTotal += 2000
        Else
            optionsTotal -= 2000
        End If
        Calculate()
    End Sub

    Private Sub CHKbulletproof_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKbulletproof.Click
        If CHKbulletproof.Checked Then
            optionsTotal += 50000
        Else
            optionsTotal -= 50000
        End If
        Calculate()
    End Sub

    Private Sub CHKonstar_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKonstar.Click
        If CHKonstar.Checked Then
            optionsTotal += 186
        Else
            optionsTotal -= 186
        End If
        Calculate()
    End Sub

    Private Sub CHKcameras_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKcameras.Click
        If CHKcameras.Checked Then
            optionsTotal += 340
        Else
            optionsTotal -= 340
        End If
        Calculate()
    End Sub

    Private Sub CHKtires_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKtires.Click
        If CHKtires.Checked Then
            optionsTotal += 3500
        Else
            optionsTotal -= 3500
        End If
        Calculate()
    End Sub

    Private Sub CHKall_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKall.Click
        If CHKall.CheckState = CheckState.Checked Then
            UncheckAll()
        Else
            CheckAll()
        End If
    End Sub

    Private Sub CHKall2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKall2.Click
        If CHKall2.CheckState = CheckState.Checked Then
            UncheckAll()
        Else
            CheckAll()
        End If
    End Sub

    Private Sub Label29_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        'Pmt(Rate / 100 / 12, years * 12, amount, 0, 1)
    End Sub

    Private Sub CHKfinancing_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKfinancing.CheckedChanged
        If CHKfinancing.Checked Then
            LBLrate.ForeColor = Color.White
            LBLrateL.ForeColor = Color.White
            LBLmonths.ForeColor = Color.White
            LBLmonthsL.ForeColor = Color.White
            LBLyears.ForeColor = Color.White
            LBLyearsL.ForeColor = Color.White
            CHKdownPayment.ForeColor = Color.White
            TRBrate.Enabled = True
            TRBmonths.Enabled = True
            TRByears.Enabled = True
            CHKdownPayment.Enabled = True

            If CHKdownPayment.Checked Then
                NUDdownPayment.Enabled = True
                downPayment = NUDdownPayment.Value
            Else
                NUDdownPayment.Enabled = False
                downPayment = 0
            End If
        Else
            LBLrate.ForeColor = InactiveColor
            LBLrateL.ForeColor = InactiveColor
            LBLmonths.ForeColor = InactiveColor
            LBLmonthsL.ForeColor = InactiveColor
            LBLyears.ForeColor = InactiveColor
            LBLyearsL.ForeColor = InactiveColor
            CHKdownPayment.ForeColor = InactiveColor
            TRBrate.Enabled = False
            TRBmonths.Enabled = False
            TRByears.Enabled = False

            CHKdownPayment.Enabled = False
            NUDdownPayment.Enabled = False
            downPayment = 0

        End If
        Calculate()
    End Sub

    Private Sub CHKdownPayment_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CHKdownPayment.CheckedChanged
        If CHKdownPayment.Checked Then
            NUDdownPayment.Enabled = True
            downPayment = NUDdownPayment.Value
        Else
            NUDdownPayment.Enabled = False
            downPayment = 0
        End If
        Calculate()
    End Sub

    Private Sub NUDdownPayment_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NUDdownPayment.ValueChanged
        downPayment = NUDdownPayment.Value
        Calculate()
    End Sub

    Private Sub TRBrate_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TRBrate.Scroll
        rate = TRBrate.Value / 1000
        LBLrate.Text = FormatPercent(rate, 1)
        Calculate()
    End Sub

    Private Sub TRBmonths_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TRBmonths.Scroll
        LBLmonths.Text = TRBmonths.Value
        Calculate()
    End Sub

    Private Sub TRByears_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TRByears.Scroll
        LBLyears.Text = TRByears.Value
        Calculate()
    End Sub

    Private Sub PICback_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICback.Click
        Me.Close()
    End Sub

    Private Sub PICreset_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PICreset.Click
        CHKtradein.Checked = False
        NUDtradein.Value = 1000
        CHKdownPayment.Checked = False
        NUDdownPayment.Value = 1000
        CHKfinancing.Checked = False
        TRBrate.Value = 30
        LBLrate.Text = "3.0%"
        TRBmonths.Value = 0
        TRByears.Value = 2
        ChooseCompany()
    End Sub

    Private Sub PICback_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICback.MouseEnter
        PICback.Image = My.Resources.back_hover
        LBLgoback.Visible = True
    End Sub

    Private Sub PICreset_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICreset.MouseEnter
        PICreset.Image = My.Resources.reset_hover
    End Sub

    Private Sub PICback_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICback.MouseLeave
        PICback.Image = My.Resources.back
        LBLgoback.Visible = False
    End Sub

    Private Sub PICreset_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PICreset.MouseLeave
        PICreset.Image = My.Resources.reset
    End Sub
End Class