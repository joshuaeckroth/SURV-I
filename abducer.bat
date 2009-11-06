:begin
mkdir build

ghc -cpp -O -threaded --make -o build\abducer.exe -isrc\abducer src\abducer\abducer.hs

if errorlevel 1 goto run else goto end

:run
cmd /c build\abducer.exe +RTS -N2 -RTS

:end

