use "CryptoLib.sml";

open CryptoLib

fun main () =
  let val s = TextIO.inputAll TextIO.stdIn
  in TextIO.output(TextIO.stdOut,unhex_string s) end
