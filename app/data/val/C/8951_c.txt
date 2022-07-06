/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*   ft_arrlen.c                                        :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By: npineau <npineau@student.42.fr>            +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created: 2016/06/10 10:38:59 by npineau           #+#    #+#             */
/*   Updated: 2016/06/10 10:39:28 by npineau          ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */

#include "arr.h"

size_t	ft_arrcapacity(const t_arr *arr)
{
	size_t	l;

	l = 0;
	while (arr[l].cell)
	{
		l += 1;
	}
	return (l);
}

size_t	ft_arrused(const t_arr *arr)
{
	size_t	l;

	l = 0;
	while (arr[l].cell && arr[l].size)
	{
		l += 1;
	}
	return (l);
}
