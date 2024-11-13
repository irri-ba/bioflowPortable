Rexe           = "R-Portable\R-4.4.1\bin\Rscript.exe"
Ropts          = "--no-save --no-environ --no-init-file --no-restore --no-Rconsole"
RScriptFile    = "runBioflowApp.R"
Outfile        = "ShinyAppBioflow.log" 
strCommand     = Rexe & " " & Ropts & " " & RScriptFile & " 1> " & Outfile & " 2>&1"
intWindowStyle = 7     ' Hide the window and activate another window.'
bWaitOnReturn  = true ' continue running script after launching R   '
' the following is a Sub call, so no parentheses around arguments'
CreateObject("Wscript.Shell").Run strCommand, intWindowStyle, bWaitOnReturn