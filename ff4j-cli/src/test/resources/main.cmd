@REM ----------------------------------------------------------------------------
@REM Licensed to the Apache Software Foundation (ASF) under one
@REM or more contributor license agreements.  See the NOTICE file
@REM distributed with this work for additional information
@REM regarding copyright ownership.  The ASF licenses this file
@REM to you under the Apache License, Version 2.0 (the
@REM "License"); you may not use this file except in compliance
@REM with the License.  You may obtain a copy of the License at
@REM
@REM    http://www.apache.org/licenses/LICENSE-2.0
@REM
@REM Unless required by applicable law or agreed to in writing,
@REM software distributed under the License is distributed on an
@REM "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
@REM KIND, either express or implied.  See the License for the
@REM specific language governing permissions and limitations
@REM under the License.
@REM ----------------------------------------------------------------------------

@REM ----------------------------------------------------------------------------
@REM FF4J Start Up Batch script
@REM
@REM Required ENV vars:
@REM JAVA_HOME - location of a JDK home dir
@REM ----------------------------------------------------------------------------

@echo off
set ERROR_CODE=0

@REM ==== START VALIDATION ====
if not "%JAVA_HOME%" == "" goto OkJHome

echo.
echo Error: JAVA_HOME not found in your environment. >&2
echo Please set the JAVA_HOME variable in your environment to match the >&2
echo location of your Java installation. >&2
echo.
goto error

:OkJHome
if exist "%JAVA_HOME%\bin\java.exe" goto chkFF4JHome

echo.
echo Error: JAVA_HOME is set to an invalid directory. >&2
echo JAVA_HOME = "%JAVA_HOME%" >&2
echo Please set the JAVA_HOME variable in your environment to match the >&2
echo location of your Java installation. >&2
echo.
goto error

:chkFF4JHome
if not "%FF4J_HOME%"=="" goto OkFF4JHome

echo.
echo Error: FF4J_HOME not found in your environment. >&2
echo Please set the FF4J_HOME variable in your environment to match the >&2
echo location of the ff4j installation. >&2
echo.
goto error

:OkFF4JHome
set CP=./conf
set CP=%CP%;%FF4J_HOME%\lib\aopalliance-1.0.jar
set CP=%CP%;%FF4J_HOME%\lib\commons-cli-1.3.1.jar
set CP=%CP%;%FF4J_HOME%\lib\ff4j-aop-1.4.jar
set CP=%CP%;%FF4J_HOME%\lib\ff4j-core-1.4.jar
set CP=%CP%;%FF4J_HOME%\lib\ff4j-cli-1.4.jar
set CP=%CP%;%FF4J_HOME%\lib\ff4j-store-jcache-1.4.jar
set CP=%CP%;%FF4J_HOME%\lib\jansi-1.11.jar
set CP=%CP%;%FF4J_HOME%\lib\slf4j-api-1.7.7.jar
set CP=%CP%;%FF4J_HOME%\lib\spring-aop-4.2.3.RELEASE.jar
set CP=%CP%;%FF4J_HOME%\lib\spring-beans-4.2.3.RELEASE.jar
set CP=%CP%;%FF4J_HOME%\lib\spring-context-4.2.3.RELEASE.jar
set CP=%CP%;%FF4J_HOME%\lib\spring-core-4.2.3.RELEASE.jar
set CP=%CP%;%FF4J_HOME%\lib\spring-expression-4.2.3.RELEASE.jar

@REM for /r %%i in (%FF4J_HOME%\lib\*) do echo %%i
SET FF4J_JAVA_EXE="%JAVA_HOME%\bin\java.exe"
%FF4J_JAVA_EXE% -cp %CP% org.ff4j.cli.MainCli

@REM if ERRORLEVEL 1 goto error
@REM goto end

@REM :error
@REM set ERROR_CODE=1

@REM :end
@REM @endlocal & set ERROR_CODE=%ERROR_CODE%

@REM exit /B %ERROR_CODE%