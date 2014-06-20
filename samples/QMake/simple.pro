# Simple QMake file

CONFIG += qt
QT += core gui
TEMPLATE = app
TARGET = simpleapp

SOURCES += file.cpp \
    file2.c \
    This/Is/Folder/file3.cpp

HEADERS += file.h \
    file2.h \
    This/Is/Folder/file3.h

FORMS += This/Is/Folder/file3.ui \
    Test.ui
