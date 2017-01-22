structure CryptoLib = struct

fun groupBy n ls =
  let
    fun f 0 curr chars acc =
          f n [] chars ((List.rev curr)::acc)
      | f n curr [] acc = List.rev acc
      | f n curr (c::chars) acc =
        f (n-1) (c::curr) chars acc
  in f n [] ls [] end

fun unhex_to_ints s =
  let
    val chars = String.explode s
    val charpairs = groupBy 2 chars
  in
    List.map
      (Option.valOf
       o StringCvt.scanString (Int.scan StringCvt.HEX)
       o String.implode) charpairs
  end

fun unhex_string s =
  String.implode (List.map Char.chr (unhex_to_ints s))

val hex_char = Int.fmt StringCvt.HEX o Char.ord

val hex_chars = List.map hex_char

val hex_string =
  String.concat o hex_chars o String.explode

fun xor_hex_strings_and func s1 s2 =
  let
    val words1 = List.map Word8.fromInt (unhex_to_ints s1)
    val words2 = List.map Word8.fromInt (unhex_to_ints s2)
    val xord = ListPair.map Word8.xorb (words1,words2)
    val xchars = List.map (func o Word8.toInt) xord
  in
    String.concat xchars
  end

val hex_xor_hex_strings = xor_hex_strings_and (Int.fmt StringCvt.HEX)
val xor_hex_strings = xor_hex_strings_and (String.str o Char.chr)

fun getLine() =
  let
    val line = TextIO.inputLine TextIO.stdIn
  in
    Substring.string (Substring.trimr 1 (Substring.full (Option.valOf line)))
  end

fun IntArray_inc(a,i) =
  IntArray.update(a,i,IntArray.sub(a,i)+1)

fun char_freq_array s =
  let
    val freq = IntArray.tabulate(256,(fn _ => 0))
    val () = Substring.foldl(fn(c,()) => IntArray_inc(freq,Char.ord c))()(Substring.full s)
  in freq end

end
