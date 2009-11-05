mkdir api
mkdir api\abducer
mkdir api\gui

haddock -o api\abducer -h -t "SURV-I" src\abducer\*.hs src\abducer\Reasoner\*.hs src\abducer\WrappedInts\*.hs

"c:\program files\doxygen\bin\doxygen.exe" doxyfile