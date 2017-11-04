/****************************************************************************
**
** Copyright (C) 2015 The Qt Company Ltd.
** Contact: http://www.qt.io/licensing
**
** This file is part of the Qt Build Suite.
**
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and The Qt Company. For licensing terms and
** conditions see http://www.qt.io/terms-conditions. For further information
** use the contact form at http://www.qt.io/contact-us.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 or version 3 as published by the Free
** Software Foundation and appearing in the file LICENSE.LGPLv21 and
** LICENSE.LGPLv3 included in the packaging of this file.  Please review the
** following information to ensure the GNU Lesser General Public License
** requirements will be met: https://www.gnu.org/licenses/lgpl.html and
** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** In addition, as a special exception, The Qt Company gives you certain additional
** rights.  These rights are described in The Qt Company LGPL Exception
** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
**
****************************************************************************/

import qbs 1.0
import qbs.FileInfo
import qbs.ModUtils

Module {
    property string buildVariant: "debug"
    property bool enableDebugCode: buildVariant == "debug"
    property bool debugInformation: (buildVariant == "debug")
    property string optimization: (buildVariant == "debug" ? "none" : "fast")
    readonly property stringList hostOS: undefined // set internally
    property string hostOSVersion: {
        if (hostOS && hostOS.contains("osx")) {
            return getNativeSetting("/System/Library/CoreServices/ServerVersion.plist", "ProductVersion") ||
                   getNativeSetting("/System/Library/CoreServices/SystemVersion.plist", "ProductVersion");
        } else if (hostOS && hostOS.contains("windows")) {
            var version = getNativeSetting("HKEY_LOCAL_MACHINE\\Software\\Microsoft\\Windows NT\\CurrentVersion", "CurrentVersion");
            return version + "." + hostOSBuildVersion;
        }
    }

    property string hostOSBuildVersion: {
        if (hostOS.contains("osx")) {
            return getNativeSetting("/System/Library/CoreServices/ServerVersion.plist", "ProductBuildVersion") ||
                   getNativeSetting("/System/Library/CoreServices/SystemVersion.plist", "ProductBuildVersion");
        } else if (hostOS.contains("windows")) {
            return getNativeSetting("HKEY_LOCAL_MACHINE\\Software\\Microsoft\\Windows NT\\CurrentVersion", "CurrentBuildNumber");
        }
    }

    readonly property var hostOSVersionParts: hostOSVersion ? hostOSVersion.split('.').map(function(item) { return parseInt(item, 10); }) : []
    readonly property int hostOSVersionMajor: hostOSVersionParts[0] || 0
    readonly property int hostOSVersionMinor: hostOSVersionParts[1] || 0
    readonly property int hostOSVersionPatch: hostOSVersionParts[2] || 0

    property stringList targetOS: hostOS
    property string pathListSeparator: hostOS.contains("windows") ? ";" : ":"
    property string pathSeparator: hostOS.contains("windows") ? "\\" : "/"
    property string profile
    property stringList toolchain
    property string architecture
    property bool install: false
    property string installSourceBase
    readonly property string installRoot: undefined
    property string installDir
    property string installPrefix: ""
    property path sysroot

    PropertyOptions {
        name: "buildVariant"
        allowedValues: ['debug', 'release']
        description: "name of the build variant"
    }

    PropertyOptions {
        name: "optimization"
        allowedValues: ['none', 'fast', 'small']
        description: "optimization level"
    }

    validate: {
        var validator = new ModUtils.PropertyValidator("qbs");
        validator.setRequiredProperty("architecture", architecture,
                                      "you might want to re-run 'qbs-setup-toolchains'");
        validator.setRequiredProperty("hostOS", hostOS);
        validator.setRequiredProperty("targetOS", targetOS);
        if (hostOS && (hostOS.contains("windows") || hostOS.contains("osx"))) {
            validator.setRequiredProperty("hostOSVersion", hostOSVersion,
                                          "could not detect host operating system version; " +
                                          "verify that system files and registry keys have not " +
                                          "been modified.");
            if (hostOSVersion)
                validator.addVersionValidator("hostOSVersion", hostOSVersion, 2, 4);

            validator.setRequiredProperty("hostOSBuildVersion", hostOSBuildVersion,
                                          "could not detect host operating system build version; " +
                                          "verify that system files or registry have not been " +
                                          "tampered with.");
        }

        validator.addCustomValidator("architecture", architecture, function (value) {
            return architecture === canonicalArchitecture(architecture);
        }, "'" + architecture + "' is invalid. You must use the canonical name '" +
        canonicalArchitecture(architecture) + "'");

        validator.validate();
    }

    // private properties
    property var commonRunEnvironment: {
        var env = {};
        if (targetOS.contains("windows")) {
            env["PATH"] = [
                FileInfo.joinPaths(installRoot, installPrefix)
            ];
        } else if (hostOS.contains("darwin") && targetOS.contains("darwin")) {
            env["DYLD_FRAMEWORK_PATH"] = [
                FileInfo.joinPaths(installRoot, installPrefix, "Library", "Frameworks"),
                FileInfo.joinPaths(installRoot, installPrefix, "lib"),
                FileInfo.joinPaths(installRoot, installPrefix)
            ].join(pathListSeparator);

            env["DYLD_LIBRARY_PATH"] = [
                FileInfo.joinPaths(installRoot, installPrefix, "lib"),
                FileInfo.joinPaths(installRoot, installPrefix, "Library", "Frameworks"),
                FileInfo.joinPaths(installRoot, installPrefix)
            ].join(pathListSeparator);

            if (targetOS.contains("ios-simulator") && sysroot) {
                env["DYLD_ROOT_PATH"] = [sysroot];
            }
        } else if (hostOS.contains("unix") && targetOS.contains("unix")) {
            env["LD_LIBRARY_PATH"] = [
                FileInfo.joinPaths(installRoot, installPrefix, "lib")
            ];
        }

        return env;
    }

    // internal properties
    readonly property string version: [versionMajor, versionMinor, versionPatch].join(".")
    readonly property int versionMajor: undefined // set internally
    readonly property int versionMinor: undefined // set internally
    readonly property int versionPatch: undefined // set internally
}
