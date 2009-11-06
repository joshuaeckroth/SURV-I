:begin
mkdir build

ghc -O -threaded --make -package HaXml -o build\abducer.exe -isrc\abducer src\abducer\abducer.hs

if errorlevel 1 goto run else goto end

:run
cmd /c build\abducer.exe +RTS -N2 -H256M -M512M -B -RTS

:end

