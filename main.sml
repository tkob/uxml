structure Main = struct
  fun main (_, arguments) =
       let
         val fileName = case arguments of [] => NONE | a::_ => SOME a
         val ins = case fileName of
                        NONE => TextIO.stdIn
                      | SOME name => TextIO.openIn name
         fun release () =
               if Option.isSome fileName then TextIO.closeIn ins else ()
       in
         let
           val sourcemap = case fileName of
                                NONE => AntlrStreamPos.mkSourcemap ()
                              | SOME n => AntlrStreamPos.mkSourcemap' n
           val uxmls =
                 UXML.parse TextIO.StreamIO.input1 (TextIO.getInstream ins)
           val numParses = length uxmls
           fun println s = print (s ^ "\n")
         in
           print (Int.toString numParses ^ " parse(s)\n");
           List.app (println o UXML.showDocument) uxmls;
           release ();
           OS.Process.success
         end
         handle e => (release (); raise e)
       end
end

fun main () =
  let
    val name = CommandLine.name ()
    val arguments = CommandLine.arguments ()
  in
      OS.Process.exit (Main.main (name, arguments))
  end

val _ = main ()
