
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
  src/gui/detection.h \
  src/gui/frame.h \
  src/gui/frames.h \
  src/gui/mainwindow.h \
  src/gui/noise.h \
  src/gui/processingcontroller.h \
  src/gui/renderarea.h \
  src/gui/settings.h \
  src/gui/track.h \
  src/gui/tracksreader.h

FORMS += \
  src/gui/mainwindow.ui \
  src/gui/settings.ui

SOURCES += \
  src/gui/abducerthread.cpp \
  src/gui/capturethread.cpp \
  src/gui/decoder.cpp \
  src/gui/detection.cpp \
  src/gui/frame.cpp \
  src/gui/frames.cpp \
  src/gui/main.cpp \
  src/gui/mainwindow.cpp \
  src/gui/noise.cpp \
  src/gui/processingcontroller.cpp \
  src/gui/renderarea.cpp \
  src/gui/settings.cpp \
  src/gui/track.cpp \
  src/gui/tracksreader.cpp


