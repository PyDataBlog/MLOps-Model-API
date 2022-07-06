Namespace JetBrains.ReSharper.Koans.Refactoring
    ' Move Refactoring
    '
    ' Move a type to another file, folder Or namespace
    '
    ' Ctrl+R, O (VS)
    ' F6 (IntelliJ)

    ' 1. Move to another file
    '    Place text caret on type definition
    '    Invoke Move refactoring
    '    Select Move to another file
    '    ReSharper suggests file name
    '    Accepting moves file, existing code still works
    Public Class MoveToAnotherFile
        Public Sub Method()
        End Sub
    End Class

    ' 2. Move to another folder in the project
    '    Place text caret on type definition
    '    Invoke Move refactoring
    '    Select Move to another folder
    '    ReSharper suggests classes to move And folder location
    '    Start typing New location - ReSharper provides suggestions of existing folders And projects
    '    Add "\MoveTarget" to end of current location
    '    ReSharper shows a validation error as the folder does Not exist
    '    Click "Create this folder"
    '    Accept move, check type Is moved to New location
    Public Class MoveToAnotherFolderInProject
        Public Sub Method()
        End Sub
    End Class

    ' 3. Move to another folder in another project
    '    Place text caret on type definition
    '    Invoke Move refactoring
    '    Select Move to another folder
    '    ReSharper suggests classes to move And folder location
    '    Start typing New location - ReSharper provides suggestions of existing folders And projects
    '    Add "-MoveTarget" to end of current location
    '      (Target folder should be: "04 Refactoring\Refactoring-MoveTarget")
    '    Accept move, check type Is moved to the New project
    '    ReSharper detects there Is no reference between this project And Refactoring-MoveTarget
    '    Can Cancel the refactoring And add the reference
    '    Or continue, And fix up the reference by Alt+Enter on the broken usage below
    Public Class MoveToAnotherFolderInOtherProject
        Public Sub Method()
        End Sub
    End Class

    ' 4. Move to another namespace
    '    Place text caret on type definition
    '    Invoke Move Refactoring
    '    Select Move to another namespace
    '    ReSharper suggests current namespace - add ".MoveTarget" to end of namespace
    '      (namespace should be JetBrains.ReSharper.Koans.Refactoring.Target)
    '    ReSharper moves the type to that namespace, but keeps type in current file
    '    Type should be at bottom of file
    Public Class MoveToAnotherNamespace
        Public Sub Method()
        End Sub
    End Class

    Public Class UsesOtherClasses
        Public Sub CallsMethods()
            ' 5. Ensure all types are still referenced correctly
            Call New MoveToAnotherFile().Method()
            Call New MoveToAnotherFolderInProject().Method()
            Call New MoveToAnotherFolderInOtherProject().Method()
            Call New MoveToAnotherNamespace().Method()
        End Sub
    End Class
End Namespace

Namespace JetBrains.ReSharper.Koans.Refactoring.MoveTarget
End Namespace