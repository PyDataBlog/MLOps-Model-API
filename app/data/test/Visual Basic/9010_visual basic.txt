Public Class Form1

    Private Sub ExitButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ExitButton.Click
        ' Exit the project

        Me.Close()
    End Sub

    Private Sub PrintButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PrintButton.Click
        ' Print the form

        PrintForm1.PrintAction = Printing.PrintAction.PrintToPreview
        PrintForm1.Print()

    End Sub

    Private Sub CalculateButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CalculateButton.Click
        '' Variables to used in calculating calories and total number of food.
        'Dim intFat As Integer
        'Dim intPro As Integer
        'Dim intCarb As Integer
        'Dim TotalCal As Integer
        'Static FoodSum As Integer
        'Static AccumCals As Integer

        '' Declares calorie quantities as constants
        'Const intFatCal As Integer = 9I
        'Const intProCal As Integer = 4I
        'Const intCarbCal As Integer = 4I

        'Try
        '    ' Assign user-inputted values to variables
        'intFat = CInt(FatTextBox.Text)
        'intPro = CInt(ProteinTextBox.Text)
        'intCarb = CInt(CarbohydrateTextBox.Text)

        '    ' Calculate totals from user information
        '    TotalCal = (intFat * intFatCal) + (intPro * intProCal) + (intCarb * intCarbCal)
        '    FoodSum += 1
        '    AccumCals += TotalCal

        '    ' Format totals for display on form
        '    TotalTextBox.Text = FormatNumber(TotalCal)
        '    AccumulatedTextBox.Text = FormatNumber(AccumCals)
        '    ItemTextBox.Text = FormatNumber(FoodSum)

        'Catch userErrors As InvalidCastException
        '    MessageBox.Show("Please enter numbers only.", "Data Entry Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        'Catch userErrors As Exception
        '    MessageBox.Show("Error:" & userErrors.Message)
        'End Try

        Dim CalculateNumbers As New Calculations(CInt(FatTextBox.Text), CInt(ProteinTextBox.Text), CInt(CarbohydrateTextBox.Text))

        TotalTextBox.Text = CalculateNumbers.Total.ToString("N")
        ItemTextBox.Text = Calculations.TotalSum.ToString("N")
        AccumulatedTextBox.Text = Calculations.TotalAccum.ToString("N")


    End Sub

    Private Sub ClearButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ClearButton.Click
        ' Clear previous information from the form

        With FatTextBox
            .Clear()
            .Focus()
        End With
        CarbohydrateTextBox.Clear()
        ProteinTextBox.Clear()
        TotalTextBox.Clear()
        AccumulatedTextBox.Clear()
        ItemTextBox.Clear()
    End Sub

    Private Sub Form1_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load

    End Sub
End Class
