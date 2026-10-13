; Hooks for the setup.exe installer: put the folder holding the `arnold` and `ace` commands on
; the user's PATH, and take it off again on uninstall.
;
; The PATH is edited by PowerShell because NSIS cuts strings off at 1024 characters, which a PATH
; can exceed. The folder is handed over in an environment variable, so it needs no quoting.
; Terminals opened afterwards see the change.

!macro ARNOLD_PATH ACTION
  Push $0
  System::Call 'kernel32::SetEnvironmentVariable(t "ARNOLD_PATH_ENTRY", t "$INSTDIR\bin")'
  System::Call 'kernel32::SetEnvironmentVariable(t "ARNOLD_PATH_ACTION", t "${ACTION}")'
  nsExec::ExecToLog `powershell.exe -NoProfile -NonInteractive -ExecutionPolicy Bypass -Command "\
    $$ErrorActionPreference = 'Stop'; \
    $$key = [Microsoft.Win32.Registry]::CurrentUser.OpenSubKey('Environment', $$true); \
    $$kind = 'ExpandString'; \
    if ($$key.GetValueNames() -contains 'Path') { $$kind = $$key.GetValueKind('Path') }; \
    $$entries = @($$key.GetValue('Path', '', 'DoNotExpandEnvironmentNames') -split ';' | Where-Object { $$_ -and $$_ -ne $$env:ARNOLD_PATH_ENTRY }); \
    if ($$env:ARNOLD_PATH_ACTION -eq 'add') { $$entries += $$env:ARNOLD_PATH_ENTRY }; \
    $$key.SetValue('Path', ($$entries -join ';'), $$kind)"`
  Pop $0 ; exit code
  Pop $0
  SendMessage ${HWND_BROADCAST} ${WM_SETTINGCHANGE} 0 "STR:Environment" /TIMEOUT=5000
!macroend

!macro NSIS_HOOK_POSTINSTALL
  !insertmacro ARNOLD_PATH add
!macroend

!macro NSIS_HOOK_PREUNINSTALL
  ; An update runs the old uninstaller first; the entry stays for the new version.
  ${If} $UpdateMode <> 1
    !insertmacro ARNOLD_PATH remove
  ${EndIf}
!macroend
