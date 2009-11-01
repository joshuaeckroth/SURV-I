
QT += xml \
    network
TARGET = build/runsurv
OBJECTS_DIR = build
RCC_DIR = build
UI_DIR = build
MOC_DIR = build
RESOURCES += src/gui/surv-1.qrc
INCLUDEPATH += "C:\Program Files\OpenCV1.2\include\opencv"
LIBS += -L"C:\Program Files\OpenCV1.2\lib" \
    -lcv120 \
    -lcxcore120 \
    -lhighgui120 \
    -lcvaux120
HEADERS += src/gui/abducerthread.h \
    src/gui/cameramodel.h \
    src/gui/capturethread.h \
    src/gui/decoder.h \
    src/gui/detection.h \
    src/gui/entities.h \
    src/gui/frame.h \
    src/gui/mainwindow.h \
    src/gui/noise.h \
    src/gui/processingcontroller.h \
    src/gui/renderarea.h \
    src/gui/resultsreader.h \
    src/gui/settings.h \
    src/gui/track.h \
    src/gui/tracksreader.h \
    src/gui/movement.h
FORMS += src/gui/mainwindow.ui \
    src/gui/settings.ui
SOURCES += src/gui/abducerthread.cpp \
    src/gui/cameramodel.cpp \
    src/gui/capturethread.cpp \
    src/gui/decoder.cpp \
    src/gui/detection.cpp \
    src/gui/entities.cpp \
    src/gui/frame.cpp \
    src/gui/main.cpp \
    src/gui/mainwindow.cpp \
    src/gui/noise.cpp \
    src/gui/processingcontroller.cpp \
    src/gui/renderarea.cpp \
    src/gui/resultsreader.cpp \
    src/gui/settings.cpp \
    src/gui/track.cpp \
    src/gui/tracksreader.cpp \
    src/gui/movement.cpp
