package org.oguz.servlet;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebInitParam;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;


public class XMLServlet extends HttpServlet
{


	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */

	protected void doGet(HttpServletRequest request, HttpServletResponse response)
		throws ServletException, IOException
	{
		response.setContentType("text/html");

		PrintWriter out = response.getWriter();
		String userName = request.getParameter("username");
		String fullName = request.getParameter("fullname");
		String profession = request.getParameter("profession");
		// HttpSession session =request.getSession();
		ServletContext context = request.getServletContext();
		if (userName != "" && userName != null)
		{
			// session.setAttribute("savedUser",userName);
			context.setAttribute("savedUser", userName);
			out.println("<p>Hello context parameter " + (String)context.getAttribute("savedUser") +
				" from GET method</p>");
		}
		else
		{
			out.println("<p>Hello default user " +
				this.getServletConfig().getInitParameter("username") + " from GET method</p>");
		}
		if (fullName != "" && fullName != null)
		{
			// session.setAttribute("savedFull", fullName);
			context.setAttribute("savedFull", fullName);
			out.println("<p> your full name is: " + (String)context.getAttribute("savedFull") +
				"</p>");
		}
		else
		{
			out.println("<p>Hello default fullname " +
				this.getServletConfig().getInitParameter("fullname") + " from GET method</p>");
		}


	}

	protected void doPost(HttpServletRequest request, HttpServletResponse response)
		throws ServletException, IOException
	{
		response.setContentType("text/html");

		PrintWriter out = response.getWriter();
		String userName = request.getParameter("username");
		String fullName = request.getParameter("fullname");
		String profession = request.getParameter("profession");
		// String location = request.getParameter("location");
		String[] location = request.getParameterValues("location");
		out.println("<p>Hello " + userName + " from POST method in XMLSERVLET response</p>");
		out.println("<p> your full name is: " + fullName + "</p>");
		out.println("<p>your profession is: " + profession + "</p>");

		for (int i = 0; i < location.length; i++)
		{
			out.println("<p>your location is: " + location[i].toUpperCase() + "</p>");
		}
	}

}
