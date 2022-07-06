file nexe_loader
set args hello.nexe
set env NACLLOG=run.log
set env NACLVERBOSITY=99
break main
break NaClAppThreadLauncher
break MySyscallInvoke
break NotImplementedDecoder
break NaClHostDescMap
