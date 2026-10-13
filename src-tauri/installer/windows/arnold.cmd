@echo off
rem The `arnold` command (also installed as `ace`): opens a folder in Arnold from a terminal.
rem   arnold [folder]    without a folder, the current one
rem The installers put this folder on PATH.
if "%~1"=="" (start "" "%~dp0..\arnold.exe" .) else start "" "%~dp0..\arnold.exe" %*
