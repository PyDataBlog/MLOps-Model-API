'
' Criado por SharpDevelop.
' Usuário: isaac.lopes
' Data: 11/03/2013
' Hora: 20:20
' 
' Para alterar este modelo use Ferramentas | Opções | Codificação | Editar Cabeçalhos Padrão.
'
Public Partial Class MainForm
	
	
	
	Public Sub New()
		' The Me.InitializeComponent call is required for Windows Forms designer support.
		Me.InitializeComponent()
		
		'
		' TODO : Add constructor code after InitializeComponents
		'
		
	End Sub
	

	



	'Carregamento dos dados, propriedades das classes, informações de configurações e XML
	Sub MainFormLoad(sender As Object, e As EventArgs)
		lblautor.Text = Application.ProductName
		lblversao.Text = Application.ProductVersion
		lbldireitos.Text = "Isaac Yung lopes e a Guyn Labs, Todos os Direitos Reservados. Copyright 2013"
		lblCompania.Text = "Guyn Labs"
		
		
		

		'Carregamento de dados **
		'Carregameno das instancias (reservando espaços na memória)
		
		
		'Carregamento de dados e informação
		splash.User = Environment.UserName
		splash.Computador = Environment.MachineName
		
		
		'Encaminhar para identificação
		formularioBv.Show()
		Me.Finalize()
		
		
	End Sub
	


	
	Sub LbllinkdevelopClick(sender As Object, e As EventArgs)
		
	End Sub
	
	Sub LblversaoClick(sender As Object, e As EventArgs)
		
	End Sub
	
	Sub ProgressBar1Click(sender As Object, e As EventArgs)
		
	End Sub
	
	Sub LblautorClick(sender As Object, e As EventArgs)
		
	End Sub
	
	Sub Button1Click(sender As Object, e As EventArgs)
		
	End Sub
End Class
