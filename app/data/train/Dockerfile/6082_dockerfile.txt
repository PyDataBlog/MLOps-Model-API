FROM snoyberg/haskell-scratch:integer-gmp

ADD .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/haskellweb-exe/haskellweb-exe /usr/bin/haskellweb-exe
CMD haskellweb-exe