
CONFIG += qt debug
QT += xml

TARGET = build/runsurv

OBJECTS_DIR = build
RCC_DIR = build
UI_DIR = build
MOC_DIR = build

RESOURCES += src/gui/surv-1.qrc

unix {
  LIBS += -L"/home/josh/school/surveillance/surv-1/libs/opencv/lib"
  INCLUDEPATH += "/home/josh/school/surveillance/surv-1/libs/opencv/include"
}
LIBS += -lcv -lcxcore -lhighgui -lcvaux

HEADERS += \
  src/gui/abducerthread.h \
  src/gui/capturethread.h \
  src/gui/decoder.h \
  src/gui/imagebuffer.h \
  src/gui/mainwindow.h \
  src/gui/processingcontroller.h \
  src/gui/renderarea.h \
  src/gui/renderthread.h \
  src/gui/settings.h \
  src/gui/tracksreader.h

FORMS += \
  src/gui/mainwindow.ui \
  src/gui/settings.ui

SOURCES += \
  src/gui/abducerthread.cpp \
  src/gui/capturethread.cpp \
  src/gui/decoder.cpp \
  src/gui/imagebuffer.cpp \
  src/gui/main.cpp \
  src/gui/mainwindow.cpp \
  src/gui/processingcontroller.cpp \
  src/gui/renderarea.cpp \
  src/gui/renderthread.cpp \
  src/gui/settings.cpp \
  src/gui/tracksreader.cpp


