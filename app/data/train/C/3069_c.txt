/*
** functions.c for functions.c in /home/kiwi/CPool_evalexpr
** 
** Made by Lyes Kaïdi
** Login   <kiwi@epitech.net>
** 
** Started on  Fri Oct 28 17:44:44 2016 Lyes Kaïdi
** Last update Fri Oct 28 17:47:02 2016 Lyes Kaïdi
*/

#include "./include/calc.h"
#include "./include/my_functs.h"

int     next_token_index = 0;

int     term_helper(int factor_1, char **tokens,
		    int factor_2, char *next_token)
{
  int	term;

  term = factor_1;
  while (next_token != NULL && (*next_token == '*'
				|| *next_token == '/' || *next_token == '%'))
    {
      next_token_index = next_token_index + 1;
      factor_2 = factor(tokens);
      if (*next_token == '*')
	  term = factor_1 * factor_2;
      else if (*next_token == '/')
	  term = factor_1 / factor_2;
      else
	  term = factor_1 % factor_2;
      factor_1 = term;
      next_token = tokens[next_token_index];
    }
  return term;
}

char    *ope_finder(char *token, int i, char c, char *str)
{
  if (*str == '\0')
    token[i] = '\0';
  else
    {
      while (*str == ' ')
	(str)++;
      if (*str == '+' || *str == '-' || *str == '*' || *str == '/'
	  || *str == '%' || *str == '(' || *str == ')')
	{
	  token[i] = *str;
	  token[i + 1] = '\0';
	}
      else
	{
	  c = *str;
	  while (c >= '0' && c <= '9')
	    {
	      token[i++] = c;
	      (str)++;
	      c = *str;
	    }
	  token[i] = '\0';
	}
    }
  return (token);
}

int     eval_expr(char *str)
{
  int   value;
  char  **tokens = all_tokens(str);

  value = expr(tokens);
  while (*tokens != NULL)
    {
      free(*tokens);
      tokens++;
    }
  return value;
}

int     expr(char **tokens)
{
  int   term_1;
  int   value;
  int   term_2;
  char  *next_token;

  term_1 = term(tokens);
  value = term_1;
  next_token = tokens[next_token_index];
  while (next_token != NULL && (*next_token == '+' || *next_token == '-'))
    {
      next_token_index = next_token_index + 1;
      term_2 = term(tokens);
      if (*next_token == '+')
	{
	  value = term_1 + term_2;
	  term_1 = value;
	}
      else
	{
	  value = term_1 - term_2;
	  term_1 = value;
	}
      next_token = tokens[next_token_index];
    }
  return value;
}
