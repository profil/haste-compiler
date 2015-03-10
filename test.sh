cd Tests
echo -en "Haste says:\t"
hastec -fforce-recomp --opt-whole-program -DO2 --onexec --with-js=TestData.js Test.hs -main-is Tests.Test.main && node Test.js
cd ..
echo -en "GHC says:\t"
runghc -no-user-package-db -package-db=.cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d/ Tests/Test.hs
