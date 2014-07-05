# This QMake file is complex, as it usese
# boolean operators and function calls

QT += core gui
greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

# We could use some OpenGL right now
contains(QT_CONFIG, opengl) | contains(QT_CONFIG, opengles2) {
   QT += opengl
} else {
   DEFINES += QT_NO_OPENGL
}

TEMPLATE = app
win32 {
    TARGET = BlahApp
    RC_FILE = Resources/winres.rc
}
!win32 { TARGET = blahapp }

# Let's add a PRI file!
include(functions.pri)

SOURCES += file.cpp

HEADERS  += file.h

FORMS    += file.ui

RESOURCES += res.qrc
