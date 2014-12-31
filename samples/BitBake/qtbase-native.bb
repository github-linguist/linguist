require qt5-git.inc
require ${PN}.inc

do_install_append() {
    # for modules which are still using syncqt and call qtPrepareTool(QMAKE_SYNCQT, syncqt)
    # e.g. qt3d, qtwayland
    ln -sf syncqt.pl ${D}${OE_QMAKE_PATH_QT_BINS}/syncqt
}

QT_MODULE_BRANCH = "release"
# v5.2.1 + 168 commits
SRCREV = "08cbbde61778276ccdda73d89fd64d02c623779f"

