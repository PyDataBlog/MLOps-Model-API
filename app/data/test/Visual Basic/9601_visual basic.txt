Namespace JetBrains.ReSharper.Koans.Refactoring
    ' Safe delete
    '
    ' Only delete code element if it's not being used
    '
    ' Ctrl+R, D (VS)
    ' Alt+Delete (IntelliJ)

    Public Class ClassWithUsedAndUnusedField
        ' 1. Delete unused field
        '    Place text caret on UnusedField And invoke Safe Delete
        '    Hit OK, field Is removed
        Public UnusedField As String = "hello world"

        ' 2. Safe delete used field
        '    Place text caret on UsedField And invoke Safe Delete
        '    Hit OK, ReSharper warns field Is used
        '    Click on the word "Usage" to navigate to the usage
        '    Edit/remove the usage, if required
        '    Click Next to remove field even with usages, Or Cancel
        Public UsedField As String = "hello world"

        ' 3. Safe delete unused parameter
        '    Place text caret on unusedParameter And invoke Safe Delete
        '    Parameter isn't used inside method, so is deleted
        '    Value passed as parameter when calling Method Is also deleted
        ' 3a. Safe delete used parameter
        '     Place text caret on usedParameter And invoke Safe Delete
        '     ReSharper warns parameter Is used, can navigate to usage
        '     Next will remove anyway, leaving broken code, Or Cancel
        Public Sub Method(usedParameter As String, unusedParameter As String)
            Console.WriteLine(usedParameter)
        End Sub

        Public Sub Method2()
            Method("hi")
        End Sub
    End Class

    ' 4. Safe Delete unused class
    '    Place text on UnusedClass And invoke Safe Delete
    '    ReSharper asks to remove empty files
    '    Next will analyse for usages, find none And delete type
    '    If file didn't contain any other definitions, the file would be deleted
    Public Class UnusedClass
    End Class

#Region "Implementation details"

    Public Class User
                Public Shared Sub Method()
                    ' Navigate back (Ctrl+-)
                    Console.WriteLine(New ClassWithUsedAndUnusedField().UsedField)
                End Sub
            End Class

#End Region
End Namespace