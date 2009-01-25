
CONFIG += qt debug

TARGET = build/runsurv

DEPENDPATH += .

RESOURCES += src/gui/surv-1.qrc

unix {
  LIBS += -L"/home/josh/school/surveillance/surv-1/libs/opencv/lib"
  INCLUDEPATH += "/home/josh/school/surveillance/surv-1/libs/opencv/include"
}
LIBS += -lcv -lcxcore -lhighgui -lcvaux

HEADERS += \
  src/gui/capturethread.h \
  src/gui/decoder.h \
  src/gui/imagebuffer.h \
  src/gui/mainwindow.h \
  src/gui/processingcontroller.h \
  src/gui/renderarea.h \
  src/gui/renderthread.h \
  src/gui/settings.h

FORMS += \
  src/gui/mainwindow.ui \
  src/gui/settings.ui

SOURCES += \
  src/gui/capturethread.cpp \
  src/gui/decoder.cpp \
  src/gui/imagebuffer.cpp \
  src/gui/main.cpp \
  src/gui/mainwindow.cpp \
  src/gui/processingcontroller.cpp \
  src/gui/renderarea.cpp \
  src/gui/renderthread.cpp \
  src/gui/settings.cpp


