/****************************************************************************
**
** Copyright (C) 2015 The Qt Company Ltd.
** Contact: http://www.qt.io/contact-us
**
** This file is part of the Enginio Qt Client Library.
**
** $QT_BEGIN_LICENSE:LGPL21$
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and The Qt Company. For licensing terms
** and conditions see http://www.qt.io/terms-conditions. For further
** information use the contact form at http://www.qt.io/contact-us.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 or version 3 as published by the Free
** Software Foundation and appearing in the file LICENSE.LGPLv21 and
** LICENSE.LGPLv3 included in the packaging of this file. Please review the
** following information to ensure the GNU Lesser General Public License
** requirements will be met: https://www.gnu.org/licenses/lgpl.html and
** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** As a special exception, The Qt Company gives you certain additional
** rights. These rights are described in The Qt Company LGPL Exception
** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
**
** $QT_END_LICENSE$
**
****************************************************************************/

function Component()
{
    installer.addWizardPage(component, "QtSelectionPage", QInstaller.ComponentSelection);
    component.userInterface("QtSelectionPage").complete = false;
    if (installer.value("os") == "win") {
        component.userInterface("QtSelectionPage").qmakePathLineEdit.text = "C:\\Qt\\";
    } else {
        component.userInterface("QtSelectionPage").qmakePathLineEdit.text = "/";
    }
    component.userInterface("QtSelectionPage").qmakePathLineEdit.textChanged.connect(checkQmakePath);
    component.userInterface("QtSelectionPage").browseButton.clicked.connect(showFileDialog);
}

checkQmakePath = function()
{
    try {
        qtpath = component.userInterface("QtSelectionPage").qmakePathLineEdit.text;
        if (installer.fileExists(qtpath)) {
            if (installer.fileExists(qtpath)) {
                component.userInterface("QtSelectionPage").complete = true;
                return;
            }
        }
        component.userInterface("QtSelectionPage").complete = false;
    } catch (e) {
        QMessageBox.warning("", "Error Installing Enginio", e);
    }
}

showFileDialog = function()
{
    try {
        if (installer.value("os") == "win") {
            path = QFileDialog.getOpenFileName("Select qmake.exe (in the bin directory of your Qt installation)", "c:\\qt\\", "qmake (qmake.exe)");
            path = path.replace(/\//g, "\\");
            component.userInterface("QtSelectionPage").qmakePathLineEdit.text = path;
        } else {
            // FIXME on linux the prefixes might be different (bin/lib etc in different places)
            path = QFileDialog.getOpenFileName("Select qmake (in the bin directory of your Qt installation)", "", "qmake (*qmake)");
            component.userInterface("QtSelectionPage").qmakePathLineEdit.text = path;
        }
    } catch (e) {
        QMessageBox.warning("", "Error Installing Enginio", e);
    }
}

Component.prototype.createOperationsForArchive = function(archive)
{
    if (component.name === "com.digia.enginio") {
        path = component.userInterface("QtSelectionPage").qmakePathLineEdit.text;
        // remove bin/qmake* from path
        path = path.replace(/bin.*/, "");
        if (installer.value("os") === "win") {
            // On windows Qt is often installed in c:\qt which needs elevated privs
            component.addElevatedOperation("Extract", archive, path);
        } else {
            component.addOperation("Extract", archive, path);
        }
    } else {
        component.addOperation("Extract", archive, "@TargetDir@");
    }
}

Component.prototype.createOperations = function()
{
    component.createOperations();

    if (installer.value("os") === "win") {
        component.addOperation("CreateShortcut", "@TargetDir@/examples/examples.pro", "@StartMenuDir@/Enignio Examples.lnk",
            "workingDirectory=@TargetDir@", "iconPath=%SystemRoot%/system32/SHELL32.dll",
            "iconId=2");
    }
    if (installer.value("os") === "mac" || installer.value("os") === "x11") {
        try {
            // patch Qt binaries
            var script = path + "patcher.py";
            print(script)
            component.addOperation("Execute", "{0}", "/usr/bin/python", script);
            component.addOperation("Execute", "{0}", "/bin/rm", script);
        } catch( e ) {
            print( e );
        }
    }
}