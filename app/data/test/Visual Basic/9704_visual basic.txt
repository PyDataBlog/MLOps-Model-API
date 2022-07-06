'
' Criado por SharpDevelop.
' Usuário: Isaac
' Data: 12/03/2013
' Hora: 19:27
' 
' Para alterar este modelo use Ferramentas | Opções | Codificação | Editar Cabeçalhos Padrão.
'
Partial Class Form2
	Inherits System.Windows.Forms.Form
	
	''' <summary>
	''' Designer variable used to keep track of non-visual components.
	''' </summary>
	Private components As System.ComponentModel.IContainer
	
	''' <summary>
	''' Disposes resources used by the form.
	''' </summary>
	''' <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
	Protected Overrides Sub Dispose(ByVal disposing As Boolean)
		If disposing Then
			If components IsNot Nothing Then
				components.Dispose()
			End If
		End If
		MyBase.Dispose(disposing)
	End Sub
	
	''' <summary>
	''' This method is required for Windows Forms designer support.
	''' Do not change the method contents inside the source code editor. The Forms designer might
	''' not be able to load this method if it was changed manually.
	''' </summary>
	Private Sub InitializeComponent()
		Me.SuspendLayout
		'
		'Form2
		'
		Me.AutoScaleDimensions = New System.Drawing.SizeF(6!, 13!)
		Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
		Me.ClientSize = New System.Drawing.Size(284, 264)
		Me.IsMdiContainer = true
		Me.Name = "Form2"
		Me.Text = "Form2"
		AddHandler Load, AddressOf Me.Form2Load
		Me.ResumeLayout(false)
	End Sub
End Class
