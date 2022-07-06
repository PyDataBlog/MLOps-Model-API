Public Class nuevoProveedor

    Private Sub ProveedorBindingNavigatorSaveItem_Click(sender As Object, e As EventArgs) Handles ProveedorBindingNavigatorSaveItem.Click
        Me.Validate()
        Me.ProveedorBindingSource.EndEdit()
        Me.TableAdapterManager.UpdateAll(Me.Entradas)

    End Sub

    Private Sub nuevoProveedor_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        'TODO: esta línea de código carga datos en la tabla 'Entradas.proveedor' Puede moverla o quitarla según sea necesario.
        Me.ProveedorTableAdapter.Fill(Me.Entradas.proveedor)

    End Sub
End Class