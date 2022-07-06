//Problem 15. Replace tags

//Write a program that replaces in a HTML document given as string all the tags <a href="…">…</a> with corresponding tags [URL=…]…/URL].
//Example:

//input	
//<p>Please visit <a href="http://academy.telerik. com">our site</a> to choose a training course.
//Also visit <a href="www.devbg.org">our forum</a> to discuss the courses.</p>
//output
//<p>Please visit [URL=http://academy.telerik. com]our site[/URL] to choose a training course.
//Also visit [URL=www.devbg.org]our forum[/URL] to discuss the courses.</p>

using System;

class ReplaceTags
{
	static void Main()
	{
		string task = "Problem 15. Replace tags\n\nWrite a program that replaces in a HTML document given as string all the tags\n<a href=\"…\">…</a> with corresponding tags [URL=…]…/URL].\nExample:\ninput\n<p>Please visit <a href=\"http://academy.telerik. com\">our site</a> to choose a training course. Also visit <a href=\"www.devbg.org\">our forum</a> to discuss the courses.</p>\noutput\n<p>Please visit [URL=http://academy.telerik. com]our site[/URL] to choose a training course. Also visit [URL=www.devbg.org]our forum[/URL] to discuss the courses.</p>\n";
		string separator = new string('*', Console.WindowWidth);

		Console.WriteLine(task);
		Console.WriteLine(separator);

	}
}
