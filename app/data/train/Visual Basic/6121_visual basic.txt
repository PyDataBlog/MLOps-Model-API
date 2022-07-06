<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class AuxiliarPedidosProveedores
    Inherits System.Windows.Forms.Form

    'Form reemplaza a Dispose para limpiar la lista de componentes.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Requerido por el Diseñador de Windows Forms
    Private components As System.ComponentModel.IContainer

    'NOTA: el Diseñador de Windows Forms necesita el siguiente procedimiento
    'Se puede modificar usando el Diseñador de Windows Forms.  
    'No lo modifique con el editor de código.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.NombreEmpresa = New System.Windows.Forms.Label()
        Me.BtnCancelar = New System.Windows.Forms.Button()
        Me.BtnAceptar = New System.Windows.Forms.Button()
        Me.CmbIDProveedores_P = New System.Windows.Forms.ComboBox()
        Me.SuspendLayout()
        '
        'NombreEmpresa
        '
        Me.NombreEmpresa.Location = New System.Drawing.Point(12, 21)
        Me.NombreEmpresa.Name = "NombreEmpresa"
        Me.NombreEmpresa.Size = New System.Drawing.Size(117, 13)
        Me.NombreEmpresa.TabIndex = 78
        Me.NombreEmpresa.Text = "ID del Proveedor:"
        '
        'BtnCancelar
        '
        Me.BtnCancelar.Location = New System.Drawing.Point(221, 64)
        Me.BtnCancelar.Name = "BtnCancelar"
        Me.BtnCancelar.Size = New System.Drawing.Size(85, 49)
        Me.BtnCancelar.TabIndex = 107
        Me.BtnCancelar.Text = "Cancelar"
        Me.BtnCancelar.UseVisualStyleBackColor = True
        '
        'BtnAceptar
        '
        Me.BtnAceptar.Location = New System.Drawing.Point(85, 64)
        Me.BtnAceptar.Name = "BtnAceptar"
        Me.BtnAceptar.Size = New System.Drawing.Size(85, 49)
        Me.BtnAceptar.TabIndex = 106
        Me.BtnAceptar.Text = "Aceptar"
        Me.BtnAceptar.UseVisualStyleBackColor = True
        '
        'CmbIDProveedores_P
        '
        Me.CmbIDProveedores_P.FormattingEnabled = True
        Me.CmbIDProveedores_P.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.CmbIDProveedores_P.Location = New System.Drawing.Point(150, 21)
        Me.CmbIDProveedores_P.Name = "CmbIDProveedores_P"
        Me.CmbIDProveedores_P.Size = New System.Drawing.Size(232, 21)
        Me.CmbIDProveedores_P.TabIndex = 129
        '
        'AuxiliarPedidosProveedores
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(403, 135)
        Me.Controls.Add(Me.CmbIDProveedores_P)
        Me.Controls.Add(Me.BtnCancelar)
        Me.Controls.Add(Me.BtnAceptar)
        Me.Controls.Add(Me.NombreEmpresa)
        Me.Name = "AuxiliarPedidosProveedores"
        Me.Text = "AuxiliarPedidosProveedores"
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents NombreEmpresa As System.Windows.Forms.Label
    Friend WithEvents BtnCancelar As System.Windows.Forms.Button
    Friend WithEvents BtnAceptar As System.Windows.Forms.Button
    Friend WithEvents CmbIDProveedores_P As System.Windows.Forms.ComboBox
End Class
