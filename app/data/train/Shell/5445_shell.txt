#!/bin/bash

CMD="aws ecs describe-clusters --region $AWS_REGION --clusters $AWS_CLUSTER"
echo $CMD
$CMD
