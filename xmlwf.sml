(* for testing: canonicalize an XML file using xmlwf tool from expat *)
structure XMLWF = struct
  local 
    fun readAll fileName =
          let
            val ins = TextIO.openIn fileName
          in
            TextIO.inputAll ins
            before TextIO.closeIn ins
            handle e => (TextIO.closeIn ins; raise e)
          end
  in
    fun canonicalizeFile tmpDir fileName =
          let
            val basename = OS.Path.file fileName
            val tmpFile = OS.Path.joinDirFile {dir=tmpDir, file=basename}
            val cmd = "xmlwf -d " ^ tmpDir ^ " " ^ fileName
          in
            case OS.Process.isSuccess (OS.Process.system cmd) of
                 false => raise Fail "xmlwf failed"
               | true => readAll tmpFile
            before OS.FileSys.remove tmpFile
            handle e => (OS.FileSys.remove tmpFile handle _ => (); raise e)
          end
  end
end
