#!/usr/bin/env bash

# more bash-friendly output for jq
JQ="jq --raw-output --exit-status"

configure_aws_cli(){
	aws --version
	aws configure set default.region $AWS_DEFAULT_REGION
	aws configure set default.output json
}

push_ecr_image(){
    # see https://github.com/aws/aws-cli/issues/1926
    eval $(aws ecr get-login --region $AWS_DEFAULT_REGION | sed -e 's/-e none//g')
    docker push $AWS_ACCOUNT_ID.dkr.ecr.$AWS_DEFAULT_REGION.amazonaws.com/swift-ecs-ecr:$CIRCLE_SHA1
}

deploy_cluster() {

    family="swift-ecs-ecr-task"

    make_task_def
    register_definition
    if [[ $(aws ecs update-service --cluster swift-ecs-ecr-cluster --service swift-ecs-ecr-service --task-definition $revision | \
                   $JQ '.service.taskDefinition') != $revision ]]; then
        echo "Error updating service."
        return 1
    fi

    # wait for older revisions to disappear
    # not really necessary, but nice for demos
    for attempt in {1..30}; do
        if stale=$(aws ecs describe-services --cluster swift-ecs-ecr-cluster --services swift-ecs-ecr-service | \
                       $JQ ".services[0].deployments | .[] | select(.taskDefinition != \"$revision\") | .taskDefinition"); then
            echo "Waiting for stale deployments:"
            echo "$stale"
            sleep 5
        else
            echo "Deployed!"
            return 0
        fi
    done
    echo "Service update took too long."
    return 1
}

make_task_def(){
	task_template='[
		{
			"name": "swift-ecs-ecr",
			"image": "%s.dkr.ecr.%s.amazonaws.com/swift-ecs-ecr:%s",
			"essential": true,
			"memory": 300,
			"cpu": 1,
			"portMappings": [
				{
					"containerPort": 8080,
					"hostPort": 80
				}
			]
		}
	]'
	
	task_def=$(printf "$task_template" $AWS_ACCOUNT_ID $AWS_DEFAULT_REGION $CIRCLE_SHA1)
}

register_definition() {
    if revision=$(aws ecs register-task-definition --container-definitions "$task_def" --family $family | $JQ '.taskDefinition.taskDefinitionArn'); then
        echo "Revision: $revision"
    else
        echo "Failed to register task definition"
        return 1
    fi
}

configure_aws_cli
push_ecr_image
deploy_cluster
