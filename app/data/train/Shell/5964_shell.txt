#!/bin/bash
if [ $# -lt 1 ]
then
	echo "get-products.sh [ORG_ID]"
	exit 1
fi

## List all of the hammer products
hammer content-view list --organization-id $1
