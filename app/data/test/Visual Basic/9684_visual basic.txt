Public Class Form1

    Dim drizzleCakeFlour, cupCakeFlour, cupCakeSugar, drizzleCakeSugar, cupCakeButter, drizzleCakeButter, totalButter, totalSugar, totalFlour, smallFlourPack, mediumFlourPack, largeFlourPack As Integer
    Dim smallSugarPack, mediumSugarPack, largeSugarPack, smallButterPack, mediumButterPack, largeButterPack As Integer

    Sub ammountOfFlour()

        drizzleCakeFlour = drizzleCakeAmmount.Value * 240
        cupCakeFlour = cupCakeAmmount.Value * 12
        totalFlour = cupCakeFlour + drizzleCakeFlour

    End Sub

    Sub ammountOfSugar()

        drizzleCakeSugar = drizzleCakeAmmount.Value * 300
        cupCakeSugar = cupCakeAmmount.Value * 14
        totalSugar = cupCakeSugar + drizzleCakeSugar

    End Sub

    Sub ammountOfButter()

        drizzleCakeButter = drizzleCakeAmmount.Value * 240
        cupCakeButter = cupCakeAmmount.Value * 4
        totalButter = cupCakeButter + drizzleCakeButter

    End Sub

    Sub displayCustomerAmmount()

        plainFlourAmmount.Text = totalFlour
        sugarAmmount.Text = totalSugar
        butterAmmount.Text = totalButter

    End Sub

    Sub calculateFlour()

        smallFlourPack = 0
        mediumFlourPack = 0
        largeFlourPack = 0
        smallFlourPack = Math.Floor(totalFlour / 250)

        If totalFlour Mod 250 <> 0 Then
            smallFlourPack += 1
        End If

        While smallFlourPack > 2
            smallFlourPack -= 3
            largeFlourPack += 1
        End While

        While smallFlourPack > 1
            smallFlourPack -= 2
            mediumFlourPack += 1

        End While

    End Sub

    Sub calculateSugar()
        smallSugarPack = 0
        mediumSugarPack = 0
        largeSugarPack = 0
        smallSugarPack = Math.Floor(totalSugar / 200)
        If totalSugar Mod 200 <> 0 Then
            smallSugarPack += 1
        End If

        While smallSugarPack > 2
            smallSugarPack -= 3
            largeSugarPack += 1

        End While

        While smallSugarPack > 1
            smallSugarPack -= 2
            mediumSugarPack += 1
        End While



    End Sub

    Sub calculateButter()
        smallButterPack = 0
        mediumButterPack = 0
        largeButterPack = 0

        smallButterPack = Math.Floor(totalButter / 125)

        If totalButter Mod 125 <> 0 Then
            smallButterPack += 1
        End If

        While smallButterPack > 3
            smallButterPack -= 4
            largeButterPack += 1
        End While

        While smallButterPack > 1
            smallButterPack -= 2
            mediumButterPack += 1

        End While

    End Sub

    Sub displayTotal()

        smallPlainFlourBox.Text = smallFlourPack
        mediumFlourBox.Text = mediumFlourPack
        largeFlourBox.Text = largeFlourPack
        smallSugarBox.Text = smallSugarPack
        mediumSugarBox.Text = mediumSugarPack
        largeSugarBox.Text = largeSugarPack
        smallButterBox.Text = smallButterPack
        mediumButterBox.Text = mediumButterPack
        largeButterBox.Text = largeButterPack

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        ammountOfButter()
        ammountOfFlour()
        ammountOfSugar()
        displayCustomerAmmount()
        calculateButter()
        calculateFlour()
        calculateSugar()
        displayTotal()

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        drizzleCakeAmmount.Value = 0
        cupCakeAmmount.Value = 0
        Button1_Click(sender, New System.EventArgs())
    End Sub


End Class
