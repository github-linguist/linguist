@if "%DEBUG%" == "" @echo off
@rem
@rem The Maven Wrapper 0.1.0 (https://github.com/shyiko/mvnw).
@rem Based on https://github.com/gradle/gradle/blob/62925785791e2487c43d19d234c2fced6d750412/gradlew.bat.
@rem

@rem Set local scope for the variables with windows NT shell
if "%OS%"=="Windows_NT" setlocal

@rem Add default JVM options here. You can also use JAVA_OPTS and MAVEN_OPTS to pass JVM options to this script.
set DEFAULT_JVM_OPTS=-Dfile.encoding=UTF-8

set DIRNAME=%~dp0
if "%DIRNAME%" == "" set DIRNAME=.
set APP_BASE_NAME=%~n0
set APP_HOME=%DIRNAME%

@rem Find java.exe
if defined JAVA_HOME goto findJavaFromJavaHome

set JAVA_EXE=java.exe
%JAVA_EXE% -version >NUL 2>&1
if "%ERRORLEVEL%" == "0" goto init

echo.
echo ERROR: JAVA_HOME is not set and no 'java' command could be found in your PATH.
echo.
echo Please set the JAVA_HOME variable in your environment to match the
echo location of your Java installation.

goto fail

:findJavaFromJavaHome
set JAVA_HOME=%JAVA_HOME:"=%
set JAVA_EXE=%JAVA_HOME%/bin/java.exe

if exist "%JAVA_EXE%" goto init

echo.
echo ERROR: JAVA_HOME is set to an invalid directory: %JAVA_HOME%
echo.
echo Please set the JAVA_HOME variable in your environment to match the
echo location of your Java installation.

goto fail

:init
@rem Get command-line arguments, handling Windowz variants

if not "%OS%" == "Windows_NT" goto win9xME_args
if "%@eval[2+2]" == "4" goto 4NT_args

:win9xME_args
@rem Slurp the command line arguments.
set CMD_LINE_ARGS=
set _SKIP=2

:win9xME_args_slurp
if "x%~1" == "x" goto execute

set CMD_LINE_ARGS=%*
goto execute

:4NT_args
@rem Get arguments from the 4NT Shell from JP Software
set CMD_LINE_ARGS=%$

:execute

@rem taken from https://github.com/takari/maven-wrapper/blob/69f3c6dd1b07620f28c1fc8cb20e392afcd9e95b/mvnw

IF NOT EXIST "%APP_HOME%\.mvn\jvm.config" goto endReadJvmConfig

@setlocal EnableExtensions EnableDelayedExpansion
for /F "usebackq delims=" %%a in ("%APP_HOME%\.mvn\jvm.config") do set JVM_CONFIG=!JVM_CONFIG! %%a
@endlocal & set JVM_CONFIG=%JVM_CONFIG%

:endReadJvmConfig

IF NOT EXIST "%APP_HOME%\.mvn\maven.config" goto endReadMavenConfig

@setlocal EnableExtensions EnableDelayedExpansion
for /F "usebackq delims=" %%a in ("%APP_HOME%\.mvn\maven.config") do set MAVEN_CONFIG=!MAVEN_CONFIG! %%a
@endlocal & set MAVEN_CONFIG=%MAVEN_CONFIG%

:endReadMavenConfig

@rem Setup the command line

set CLASSPATH=%APP_HOME%\.mvn\wrapper\maven-wrapper.jar

@rem Execute Maven
"%JAVA_EXE%" %DEFAULT_JVM_OPTS% %JVM_CONFIG% %JAVA_OPTS% %MAVEN_OPTS% -classpath "%CLASSPATH%" -Dmaven.multiModuleProjectDirectory="%APP_HOME%" org.apache.maven.wrapper.MavenWrapperMain %MAVEN_CONFIG% %CMD_LINE_ARGS%

:end
@rem End local scope for the variables with windows NT shell
if "%ERRORLEVEL%"=="0" goto mainEnd

:fail
rem Set variable MAVEN_EXIT_CONSOLE if you need the _script_ return code instead of
rem the _cmd.exe /c_ return code!
if  not "" == "%MAVEN_EXIT_CONSOLE%" exit 1
exit /b 1

:mainEnd
if "%OS%"=="Windows_NT" endlocal

:omega