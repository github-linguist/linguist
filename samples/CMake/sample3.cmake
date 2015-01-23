# Specifications for building user and development documentation.
#
# ====================================================================
# Copyright (c) 2009 Ian Blumel.  All rights reserved.
# 
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.  
# ====================================================================

CMAKE_MINIMUM_REQUIRED(VERSION 2.6)

FIND_FILE( SPHINX sphinx-build.exe) 

# If we are windows call to the make.bat file, otherwise rely on the Makefile
# to handle the processing.
IF(WIN32)
   SET(SPHINX_MAKE make.bat)
ELSE(WIN32)
   SET(SPHINX_MAKE make)
ENDIF(WIN32)

ADD_CUSTOM_TARGET(
   doc_usr
   COMMAND ${SPHINX_MAKE} html
   WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/usr
)

ADD_CUSTOM_TARGET(
   doc_dev
   COMMAND ${SPHINX_MAKE} html
   WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/dev
)

