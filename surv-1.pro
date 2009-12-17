QT += xml \
    network
TARGET = build/runsurv
OBJECTS_DIR = build
RCC_DIR = build
UI_DIR = build
MOC_DIR = build
RESOURCES += src/gui/surv-1.qrc
INCLUDEPATH += "C:\OpenCV2.0\include\opencv"
LIBS += -L"C:\OpenCV2.0\bin" \
    -lcv200 \
    -lcxcore200 \
    -lhighgui200 \
    -lcvaux200
HEADERS += src/gui/cameramodel.h \
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
    src/gui/movement.h \
    src/gui/abducerwriter.h \
    src/gui/abducerreader.h \
    src/gui/path.h \
    src/gui/entitiestree.h \
    src/gui/entity.h \
    src/gui/entitiestreeitem.h \
    src/gui/behavior.h \
    src/gui/infobox.h \
    src/gui/agent.h
FORMS += src/gui/mainwindow.ui \
    src/gui/settings.ui \
    src/gui/entitiesTree.ui \
    src/gui/infobox.ui
SOURCES += src/gui/cameramodel.cpp \
    src/gui/capturethread.cpp \
    src/gui/decoder.cpp \
    src/gui/detection.cpp \
    src/gui/entities.cpp \
    src/gui/frame.cpp \
    src/gui/main.cpp \
    src/gui/mainwindow.cpp \
    src/gui/processingcontroller.cpp \
    src/gui/renderarea.cpp \
    src/gui/resultsreader.cpp \
    src/gui/settings.cpp \
    src/gui/movement.cpp \
    src/gui/abducerwriter.cpp \
    src/gui/abducerreader.cpp \
    src/gui/path.cpp \
    src/gui/entitiestree.cpp \
    src/gui/entity.cpp \
    src/gui/entitiestreeitem.cpp \
    src/gui/behavior.cpp \
    src/gui/infobox.cpp \
    src/gui/agent.cpp
OTHER_FILES += 
