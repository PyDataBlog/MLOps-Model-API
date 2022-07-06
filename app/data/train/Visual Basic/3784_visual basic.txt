Option Explicit On

'Title:     Running total & Avg
'Author:    Miguel Rodriguez
'Date:      2/16/2016
'Course:    CSC 162
'Section:   001
'Description:   This program demonstrates how to keep track
'               of a running total of numbers and it computes the avg
'
'Initial Algorithm:
'   1.  Get Number Entered by User
'   2.  Add to running total
'   3.  count by one each time the user enters a number
'   4.  Display running total
'   5.  Display the total average
'
'Data Requirements:
'   Input:
'       Number entered by user:  NumEntered
'   Output:
'       Running Total (sum of all numbers entered): RunTotal
'       Average (RunTotal / count): totalAvg
'   Additional:
'       None
'
'Formulas:
'   RunTotal = RunTotal + NumEntered
'   totalAvg = RunTotal / count
'
'Refined Algorithm:
'   1.  Get Number Entered by User
'   2.  Add to running total
'           RunTotal = RunTotal + NumEntered
'   3.  Compute the total average
'           totalAvg = RunTotal / count
'   4.  Display running total
'   5.  Display the total Average

Public Class frmRunningTotal
    Inherits System.Windows.Forms.Form

    'Global Variable Section
    Dim RunTotal As Integer 'Running Total

    Dim count As Integer 'counts everytime the user enters a number


    Private Sub cmdAdd_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdAdd.Click
        'This command button adds the number entered to the running total

        'Declare local variables
        Dim NumEntered As Integer 'Number entered by user

        'Get number entered into text box and put into variable
        NumEntered = Val(txtNumEntered.Text)

        'Calculate running total
        RunTotal = RunTotal + NumEntered

        'Place new total into label
        txtRunTotal.Text = RunTotal

        count = count + 1 'counts and increases by one


        'Clear out text box for next number
        txtNumEntered.Text = ""

        'Return cursor to text box for next number
        txtNumEntered.Focus()

    End Sub

    Private Sub cmdClear_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdClear.Click
        'This button clears out all text boxes and count
        txtNumEntered.Text = ""
        txtRunTotal.Text = ""
        txtDisplayAvg.Text = ""

        'Re-initialize RunTotal
        RunTotal = 0
        'reinitialize count
        count = 0
        'Set focus back to text box
        txtNumEntered.Focus()

    End Sub

    Private Sub cmdEnd_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdEnd.Click
        'This command button ends the program
        Me.Close() 'Remove active window
        End 'End program
    End Sub

    Private Sub frmRunningTotal_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Load
        'Initialize Text Boxes, labels, and variables to zero when form is loaded
        txtNumEntered.Text = ""
        txtRunTotal.Text = ""
        txtDisplayAvg.Text = ""
        RunTotal = 0
        lblInfo.Text = "Adds up numbers and Computes average"
    End Sub

    Private Sub cdmAvg_Click(sender As Object, e As EventArgs) Handles cdmAvg.Click

        Dim totalAvg As Double 'stores the total avg computed

        totalAvg = RunTotal / count 'computes the avg and stores it in totalAvg

        'Place average into the lbl
        txtDisplayAvg.Text = totalAvg



    End Sub
End Class
