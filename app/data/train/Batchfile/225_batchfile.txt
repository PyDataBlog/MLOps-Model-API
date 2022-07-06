@ECHO OFF
cmdkey /delete:git:https//github.com
CD PTNigeria
git init
git config --local user.name PTNigeria
git config --local user.password iykesMan22
git config --local user.email PTNigeriaplc@gmail.com
git remote add Local https://github.com/PTNigeria/CDN
git remote set-url --add Local https://github.com/PTNigeria/CDN
git pull Local master --allow-unrelated-histories
git remote -v
git add --all ./
git commit -m "PT NG Auto commits By Date:- %Date% %TIME%" ./
git push Local master

PAUSE
