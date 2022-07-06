docker stop dev-cow
docker rm dev-cow
docker rmi dev-cow
docker build -t dev-cow ./docker
$direct=Get-Location
$direct=$direct.Path.Replace('C:\', '/c/').Replace('\', '/')
echo $pwd
docker run -d -P -v ${direct}:/var/www/html --name dev-cow dev-cow
