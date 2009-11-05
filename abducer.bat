mkdir build
ghc --make -package HaXml -o build\abducer.exe -isrc\abducer src\abducer\abducer.hs
build\abducer.exe