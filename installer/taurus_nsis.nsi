[% extends "pyapp_w_pylauncher.nsi" %]

[% block install_commands %]
[[ super() ]]
  IfFileExists "C:\etc\bzt.d\99-installID.yml" file_found file_not_found
  file_not_found:
  nsExec::ExecToStack '[[ python ]] -c "import uuid; print uuid.uuid4()"'
  Pop $0  ; return code
  Pop $1  ; output
  FileOpen $9 C:\etc\bzt.d\99-installID.yml w
  FileWrite $9 "---$\r$\n"
  FileWrite $9 "install-id: "
  FileWrite $9 $1
  FileWrite $9 "$\r$\n"
  FileClose $9
  file_found:

[% endblock %]
