Public Class nuevoEmpleado

    Private Sub EmpleadoBindingNavigatorSaveItem_Click(sender As Object, e As EventArgs) Handles EmpleadoBindingNavigatorSaveItem.Click
        Me.Validate()
        Me.EmpleadoBindingSource.EndEdit()
        Me.TableAdapterManager.UpdateAll(Me.Entradas)

    End Sub

    Private Sub nuevoEmpleado_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        'TODO: esta línea de código carga datos en la tabla 'Entradas.empleado' Puede moverla o quitarla según sea necesario.
        Me.EmpleadoTableAdapter.Fill(Me.Entradas.empleado)

    End Sub
End Class