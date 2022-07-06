@ECHO OFF
SET LOCALDIR=C:\WEB\WEBFILES\IYKEDAIRO\PTNigeria
CD %LOCALDIR%
git init
git config --global credential.helper wincred
git config --local user.name PTNigeria
git config --local user.password iykesMan22
git config --local user.email PTNigeriaplc@gmail.com
git remote add Local https://PTNigeria@github.com/PTNigeria/CDN.git
git remote set-url --add Local https://PTNigeria@github.com/PTNigeria/CDN.git
git pull Local master --allow-unrelated-histories
git remote -v

PAUSE

