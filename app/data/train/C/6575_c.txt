/*
** use.h for epiDatabase in /epiEngine
**
** Made by Thomas Moulard
** Login   moular_j
**
** Started on  02/01/2005 moular_t
** Last update 24/02/2005 moular_t
*/

#ifndef USE_H_
# define USE_H_

# include <stdio.h>

# include "utilsdb.h"

# include "../types.h"
# include "../locale/msg_index.h"

void usedb(struct s_sql_query *query, struct s_sql_result *result);

#endif /* !USE_H_ */
