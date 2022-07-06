
Namespace Scheduler
    ''' <summary>
    ''' A Class to Add Schedules to the Program just like we have in SQL Server
    ''' <para>This Class has Events also for Errors..... Do Use the Events</para>
    ''' <para>All Exceptions are thrown to The Event.</para>
    ''' <para>Use Contructuctor to Load the Class</para>
    ''' </summary>
    ''' <remarks>My Best Invention Till Date</remarks>
    Public Class Scheduler
        WithEvents Executer As System.Windows.Forms.Timer
        Dim Events As New ArrayList
        Public SetSchedule As New DataTable

        Public Event Errors(ByVal Sender As Object, ByVal e As String)
        Public Event EventsTriggered(ByVal sender As Object, ByVal e As String)

        ''' <summary>
        ''' Contructor
        ''' </summary>
        ''' <remarks></remarks>
        Public Sub New()
            Try
                Executer = New System.Windows.Forms.Timer
                Executer.Interval = 1000
                SetSchedule.Columns.Add(New DataColumn("SchType", System.Type.GetType("System.String")))
                SetSchedule.Columns.Add(New DataColumn("exeTime", System.Type.GetType("System.DateTime")))
                SetSchedule.Columns.Add(New DataColumn("active", System.Type.GetType("System.Boolean")))
                SetSchedule.Columns.Add(New DataColumn("NextexeTime", System.Type.GetType("System.DateTime")))
                SetSchedule.Columns.Add(New DataColumn("TimeDiff", System.Type.GetType("System.TimeSpan")))
                Application.DoEvents()
            Catch ex As Exception
                RaiseEvent Errors(Me, ex.Message)
            End Try
        End Sub

        ''' <summary>
        ''' Start The Scheduler
        ''' </summary>
        ''' <remarks></remarks>
        Public Sub Start()
            If SetSchedule.Rows.Count = 0 Then
                RaiseEvent Errors(Me, "No Schedules Set: Unable to Start")
            Else
                Executer.Start()
            End If
        End Sub

        ''' <summary>
        ''' Stop the Scheduler
        ''' </summary>
        ''' <remarks></remarks>
        Public Sub [Stop]()
            Executer.Stop()
        End Sub

        ''' <summary>
        ''' Add An Event To execute when the Scheduler is Ticked
        ''' <example><c><para>Dim msd As ShowMessage = AddressOf DisplayMessage</para>
        ''' <para>sch.AddEvent(msd)</para></c></example>
        ''' </summary>
        ''' <param name="DelegateFuntion">The Delegate Function Name</param>
        ''' <param name="ParametersToTheFunction">The Parameters to the Function passed as an Array of Objects</param>
        ''' <remarks></remarks>
        Public Sub AddEvent(ByVal DelegateFuntion As [Delegate], ByVal ParamArray ParametersToTheFunction() As Object)
            Try
                Dim xEvent As New [Event](DelegateFuntion, ParametersToTheFunction)
                Events.Add(xEvent)
            Catch ex As Exception
                RaiseEvent Errors(Me, ex.Message)
            End Try
        End Sub

        ''' <summary>
        ''' Add a Schedule Type denoting when to Execute the Events....
        ''' <para>If loading a Once Executable Schedule Check the Other Overload</para>
        ''' </summary>
        ''' <param name="Schedule">An Enum of Scheduled Type</param>
        ''' <param name="When">Date and Time when to Begin Execution of the Event</param>
        ''' <remarks>Check the Other Overloads as well</remarks>
        Public Sub AddSchedule(ByVal Schedule As Scheduled, ByVal [When] As DateTime)
            Dim SchType As String
            Dim exeTime As DateTime
            Dim active As Boolean
            Dim NextexeTime As DateTime
            Dim TimeDiff As TimeSpan

            SchType = "Regular"
            exeTime = [When]
            active = True
            TimeDiff = New TimeSpan(0, 0, 0)

            Select Case Schedule
                Case Scheduled.BySecond
                    SchType = "BySecond"
                    TimeDiff = New TimeSpan(0, 0, 1)
                Case Scheduled.ByMinute
                    SchType = "ByMinute"
                    TimeDiff = New TimeSpan(0, 1, 0)
                Case Scheduled.ByHour
                    SchType = "ByHour"
                    TimeDiff = New TimeSpan(1, 0, 0)
                Case Scheduled.Daily
                    SchType = "Daily"
                    TimeDiff = New TimeSpan(1, 0, 0, 0)
                Case Scheduled.Weekly
                    SchType = "Weekly"
                    TimeDiff = New TimeSpan(7, 0, 0, 0)
                Case Scheduled.Monthly
                    SchType = "Monthly"
                    Dim d As Integer
                    d = DateDiff(DateInterval.Day, [When].AddMonths(1), [When])
                    TimeDiff = New TimeSpan(d, 0, 0, 0)
                Case Scheduled.Yearly
                    SchType = "Yearly"
                    Dim d As Integer
                    d = DateDiff(DateInterval.Day, [When].AddYears(1), [When])
                    TimeDiff = New TimeSpan(1, 0, 0, 0)
            End Select
            NextexeTime = exeTime.Add(TimeDiff)
            SetSchedule.Rows.Add(SchType, exeTime, active, NextexeTime, TimeDiff)
        End Sub

        ''' <summary>
        ''' Add a Schedule that will be Executed JUST ONCE
        ''' </summary>
        ''' <param name="When">The Date/Time when the Execution will be done</param>
        ''' <remarks></remarks>
        Public Sub AddSchedule(ByVal [When] As DateTime)
            Dim SchType As String
            Dim exeTime As DateTime
            Dim active As Boolean
            Dim NextexeTime As DateTime
            Dim TimeDiff As TimeSpan

            SchType = "Once"
            exeTime = [When]
            active = True
            TimeDiff = New TimeSpan(0, 0, 0)
reChkExecutableTime:
            If DateDiff(DateInterval.Second, exeTime, Now) > 0 Then
                active = False
            End If
            SetSchedule.Rows.Add(SchType, exeTime, active, NextexeTime, TimeDiff)
        End Sub

        ''' <summary>
        ''' Add a Schedule, and Define a Perfect Time Slack between the Different Executes
        ''' <para>The First Execution will be NOW</para>
        ''' </summary>
        ''' <param name="TimeSpn">A System.Windows.Forms.TimeSpan Object</param>
        ''' <remarks></remarks>
        Public Sub AddSchedule(ByVal TimeSpn As TimeSpan)
            Dim SchType As String
            Dim exeTime As DateTime
            Dim active As Boolean
            Dim NextexeTime As DateTime
            Dim TimeDiff As TimeSpan

            SchType = "Regular"
            exeTime = Now
            active = True
            TimeDiff = TimeSpn

            NextexeTime = exeTime.Add(TimeDiff)
            SetSchedule.Rows.Add(SchType, exeTime, active, NextexeTime, TimeDiff)
        End Sub

        ''' <summary>
        ''' Add a Schedule, with Date/Time when to begin and The Perfect TimeSpan between Executions
        ''' </summary>
        ''' <param name="When">A Date/Time Object When to Begin Scheduling</param>
        ''' <param name="TimeSpn">A System.Windows.Forms.TimeSpan Object 
        ''' <para>Defining Spacing between the Calls</para></param>
        ''' <remarks></remarks>
        Public Sub AddSchedule(ByVal [When] As DateTime, ByVal TimeSpn As TimeSpan)
            Dim SchType As String
            Dim exeTime As DateTime
            Dim active As Boolean
            Dim NextexeTime As DateTime
            Dim TimeDiff As TimeSpan

            SchType = "Regular"
            exeTime = [When]
            active = True
            TimeDiff = TimeSpn
            NextexeTime = exeTime.Add(TimeDiff)
reChkExecutableTime:
            If DateDiff(DateInterval.Second, exeTime, Now) > 0 Then
                exeTime = NextexeTime
                NextexeTime = exeTime.Add(TimeDiff)
                GoTo reChkExecutableTime
            End If
            SetSchedule.Rows.Add(SchType, exeTime, active, NextexeTime, TimeDiff)
        End Sub

        ''' <summary>
        ''' Add a Schedule, Defining a Daily Execution Time Table
        ''' <example><c>AddSchedule("09:25", "21:25")</c> 
        ''' Two Scheduled Times set One for Morning and Another for Evening</example>
        ''' </summary>
        ''' <param name="Times"><example><c>AddSchedule("09:25", "21:25")</c> 
        ''' Two Scheduled Times set One for Morning and Another for Evening</example></param>
        ''' <remarks></remarks>
        Public Sub AddSchedule(ByVal ParamArray Times() As String)
            For Each t As String In Times
                Dim SchType As String
                Dim exeTime As DateTime
                Dim active As Boolean
                Dim NextexeTime As DateTime
                Dim TimeDiff As TimeSpan

                SchType = "Daily"
                exeTime = New DateTime(Today.Year, Today.Month, Today.Day, _
                                         Integer.Parse(t.Split(":")(0)), _
                                         Integer.Parse(t.Split(":")(1)), 0)
                active = True
                TimeDiff = New TimeSpan(1, 0, 0, 0)
                NextexeTime = exeTime.Add(TimeDiff)
                If DateDiff(DateInterval.Second, exeTime, Now) > 0 Then
                    exeTime = NextexeTime
                    NextexeTime = exeTime.Add(TimeDiff)
                End If
                SetSchedule.Rows.Add(SchType, exeTime, active, NextexeTime, TimeDiff)
            Next
        End Sub

        Private Sub Executer_Tick(ByVal sender As Object, ByVal e As System.EventArgs) Handles Executer.Tick
            For Each row As DataRow In SetSchedule.Rows
                If row.Item("active") = True Then
                    If DateDiff(DateInterval.Second, row.Item("exeTime"), Now) > 0 Then
                        If row.Item("SchType") = "Once" Then
                            row.Item("active") = False
                        Else
                            row.Item("exeTime") = row.Item("NextexeTime")
                            row.Item("NextexeTime") = row.Item("exeTime").Add(row.Item("TimeDiff"))
                        End If
                        For Each x As [Event] In Events
                            x.Func.DynamicInvoke(x.Parameters)
                        Next
                        RaiseEvent EventsTriggered(Me, "Done")
                    End If
                End If
            Next
        End Sub

        ''' <summary>
        ''' Event Structure for Storing Events
        ''' </summary>
        ''' <remarks></remarks>
        Public Structure [Event]
            Dim Func As [Delegate]
            Dim Parameters() As Object

            ''' <summary>
            ''' Constructor For Event Structure
            ''' </summary>
            ''' <param name="DelegateFunction">The Delegate Name for the Function</param>
            ''' <param name="ParametersForExecution">The Parameters for Execution of the Function</param>
            ''' <remarks></remarks>
            Public Sub New(ByVal DelegateFunction As [Delegate], ByVal ParamArray ParametersForExecution() As Object)
                Func = DelegateFunction
                Parameters = ParametersForExecution
            End Sub
        End Structure

        ''' <summary>
        ''' Enumeration of the Schedule Types
        ''' </summary>
        ''' <remarks></remarks>
        Public Enum Scheduled
            Once = 1
            BySecond = 2
            ByMinute = 4
            ByHour = 8
            Daily = 16
            Weekly = 32
            Monthly = 64
            Yearly = 128
            Regular = 256
        End Enum
    End Class

End Namespace
