package br.ifrn.meutcc.visao;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import br.ifrn.meutcc.modelo.Aluno;
import br.ifrn.meutcc.modelo.Orientador;

@WebServlet("/ViewAlunoCandidatou")
public class ViewAlunoCandidatou extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    public ViewAlunoCandidatou() {
        super();
    }

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		String id = request.getParameter("id");
		int idCandidato = 2;
		int idTema = -1;
		try {
			idTema = Integer.parseInt(id);
		} catch (NumberFormatException nfex) {
			nfex.printStackTrace();
		}
		
		Orientador a = new Orientador();
		Orientador orientador = a.getOrientadorPorTema(idTema);	
		
		Aluno logic = new Aluno();
		logic.registraObserver(orientador);
		boolean aluno = logic.addCandidato(idTema, idCandidato);
		
		request.setAttribute("candidatou", logic.getStatus());
		request.setAttribute("aluno", aluno);
		request.getRequestDispatcher("viewAluno.jsp").forward(request, response);
	}

}
