use "CryptoLib.sml";

open CryptoLib

fun main () =
  let val s1 = getLine()
      val s2 = getLine()
  in TextIO.output(TextIO.stdOut,hex_xor_hex_strings s1 s2) end
