@ECHO OFF
TITLE = FCS Volume and Biomass Compiler

SET SHOWMSG=%1

IF EXIST "%APPDATA%"\FIABiosum\fcs_error_msg.txt DEL "%APPDATA%"\FIABiosum\fcs_error_msg.txt

CALL "%JAVA_HOME%"\bin\JAVA.EXE -jar "%APPDATA%"/FIABiosum/BiosumComps.jar -tc "%APPDATA%"/FIABiosum/BiosumSpeciesConfig.db ^
               -td "%APPDATA%"/FIABiosum/fcs_tree.db -tdt biosum_calc

SET ERRNUM=%ERRORLEVEL%

IF "%ERRNUM%"=="1" GOTO tryagain
IF "%ERRNUM%"=="-1" GOTO errdb
IF "%ERRNUM%"=="-2" GOTO errtree
IF "%ERRNUM%"=="0" GOTO success

:tryagain
IF NOT EXIST C:\JDK1.8\BIN\JAVA.EXE GOTO errjava
CALL C:\JDK1.8\BIN\JAVA.EXE -jar "%APPDATA%"/FIABiosum/BiosumComps.jar "%APPDATA%"/FIABiosum/FCS_TREE.DB
SET ERRNUM=%ERRORLEVEL%


IF "%ERRNUM%"=="1" GOTO errjava
IF "%ERRNUM%"=="-1" GOTO errdb
IF "%ERRNUM%"=="-2" GOTO errtree
IF "%ERRNUM%"=="0" GOTO success

GOTO errunk

:errdb
echo msgbox "SQLite database file not found",0,"FCS Volume and Biomass Compiler" > %tmp%\biosum_err_msg.vbs
IF "%SHOWMSG%"=="Y" wscript %tmp%\biosum_err_msg.vbs
del %tmp%\biosum_err_msg.vbs
GOTO exit

:errtree
echo msgbox "Not a valid tree count",0,"FCS Volume and Biomass Compiler" > %tmp%\biosum_err_msg.vbs
IF "%SHOWMSG%"=="Y" wscript %tmp%\biosum_err_msg.vbs
del %tmp%\biosum_err_msg.vbs
GOTO exit

:errjava
Rem This section requires a specific version/location for the JDK
echo msgbox "Problem detected running JAVA.EXE" + CHR(13) + "64-bit requirements" + CHR(13) + "-----------------------" + CHR(13) + "Install Java 8 Update 251" + CHR(13) + "Install Oracle 64bit Client" + CHR(13) + "Install JDK1.8 (optional)",0,"FCS Volume and Biomass Compiler" > %tmp%\biosum_err_msg.vbs
IF "%SHOWMSG%"=="Y" wscript %tmp%\biosum_err_msg.vbs
del %tmp%\biosum_err_msg.vbs
echo Problem detected running JAVA.EXE\r\n64-bit requirements\r\n-----------------------\r\nInstall Java 8 Update 251\r\nInstall Oracle 64bit Client\r\nInstall JDK1.8 (optional)>"%APPDATA%"\FIABiosum\fcs_error_msg.txt
GOTO exit

:errunk
SET /P ERRMSG=<"%APPDATA%"/FIABiosum/FCS_ERROR_MSG.TXT
echo msgbox "%ERRMSG%",0,"FCS Volume and Biomass Compiler" > %tmp%\biosum_err_msg.vbs
IF "%SHOWMSG%"=="Y" wscript %tmp%\biosum_err_msg.vbs
del %tmp%\biosum_err_msg.vbs
GOTO exit

:success
echo msgbox "Done with with no errors detected",0,"FCS Volume and Biomass Compiler" > %tmp%\biosum_msg.vbs
IF "%SHOWMSG%"=="Y" wscript %tmp%\biosum_msg.vbs
del %tmp%\biosum_msg.vbs

:exit
IF "%SHOWMSG%"=="" EXIT
IF "%SHOWMSG%"=="N" EXIT
