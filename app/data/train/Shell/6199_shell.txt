#shell path will be passed by $1
cd /home/ackfruit-server/jackfruit-server
git pull origin develop
forever restart server.js
echo 'deploy success!'
