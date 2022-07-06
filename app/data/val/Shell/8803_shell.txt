aws ec2 run-instances --image-id ami-d05e75b8 --count $1 --instance-type t2.micro --key-name 1878KeyPairVM --security-group-ids sg-dfd9c3b8 --subnet-id subnet-65500d4e --iam-instance-profile $2  --associate-public-ip-address 

aws ec2 wait instance-running

