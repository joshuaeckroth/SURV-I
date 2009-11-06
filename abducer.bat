mkdir build

ghc -O -threaded --make -package HaXml -o build\abducer.exe -isrc\abducer src\abducer\abducer.hs && build\abducer.exe +RTS -N2 -H256M -M512M -B -RTS
