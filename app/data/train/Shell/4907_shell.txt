#!/bin/bash
curl -v -H "Content-type: application/json" -H "Authorization: Token token=MY_TOKEN_HERE" -X POST \
    -d '{    
      "role": "admin",
      "name": "Bart Simpson",
      "email": "bart@example.com",
      "requester_id": "PP1565R",
      "job_title": "Developer"
    }' \
    "https://funboarding.pagerduty.com/api/v1/users"
