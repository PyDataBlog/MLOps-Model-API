REM Install Chocolatey
@powershell -NoProfile -ExecutionPolicy Bypass -Command "iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1'))" && SET PATH=%PATH%;%ALLUSERSPROFILE%\chocolatey\bin

REM install git
c:\programdata\chocolatey\bin\choco.exe install git wget puppet -y

REM install r10k
"c:\Program Files\Puppet Labs\Puppet\sys\ruby\bin\gem" install r10k

REM retrieve Puppetfile
cd c:\ProgramData\PuppetLabs\puppet\etc\

c:\programdata\chocolatey\bin\wget.exe -cv https://raw.githubusercontent.com/ppouliot/puppet-Puppetfile_Env/master/Puppetfile.windows -o Puppetfile

REM Execute Puppet
"c:\Program Files\Puppet Labs\Puppet\bin\puppet.bat" apply --debug --trace --verbose --modulepath=c:\ProgramData\PuppetLabs\puppet\etc\modules c:\ProgramData\PuppetLabs\puppet\etc\manifests

