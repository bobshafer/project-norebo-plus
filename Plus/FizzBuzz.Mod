MODULE FizzBuzz;

IMPORT Out;

(* Extra credit: word-wrapping! *)
CONST WordWrap = TRUE; MaxColumns = 73;
  UsingOBNC = FALSE;

VAR 
  fizzBuzzIndex: INTEGER;
  neitherFizzNorBuzz: BOOLEAN;
  separator: CHAR;
  columns: INTEGER;

PROCEDURE Need(len: INTEGER);
BEGIN
  (* We add 1 to len in the following to account for commas *)
  IF columns + len + 1 > MaxColumns THEN
    IF WordWrap THEN Out.Ln END;
    columns := len 
  ELSE
    INC(columns, len + 1)
  END
END Need;

PROCEDURE First100*;
BEGIN
  columns := 0;
  separator := 0X;
  FOR fizzBuzzIndex := 1 TO 100 DO
    neitherFizzNorBuzz := TRUE;
    IF separator = 0X THEN separator := "," ELSE Out.Char(separator) END;
    IF fizzBuzzIndex MOD 3 = 0 THEN
      Need(4);
      Out.String("Fizz");
      neitherFizzNorBuzz := FALSE
    END;
    IF fizzBuzzIndex MOD 5 = 0 THEN
      Need(4);
      Out.String("Buzz");
      neitherFizzNorBuzz := FALSE
    END;
    IF neitherFizzNorBuzz THEN 
      (* We know by inspection that 5 divides 100, so is a Buzz *)
      IF fizzBuzzIndex < 10 THEN Need(1) ELSE
        Need(2);
        Out.Char(CHR(ORD("0") + fizzBuzzIndex DIV 10))
      END;
      Out.Char(CHR(ORD("0") + fizzBuzzIndex MOD 10))
    END;
  END;
  Out.Ln;
END First100;

BEGIN 
  Out.Open;
  IF UsingOBNC THEN First100 END
END FizzBuzz.

Making it run (on linux; obnc is at http://miasap.se/obnc/):

$ obnc FizzBuzz.Mod
$ ./FizzBuzz
1,2,Fizz,4,Buzz,Fizz,7,8,Fizz,Buzz,11,Fizz,13,14,FizzBuzz,16,17,Fizz,19,
Buzz,Fizz,22,23,Fizz,Buzz,26,Fizz,28,29,FizzBuzz,31,32,Fizz,34,Buzz,Fizz,
37,38,Fizz,Buzz,41,Fizz,43,44,FizzBuzz,46,47,Fizz,49,Buzz,Fizz,52,53,
Fizz,Buzz,56,Fizz,58,59,FizzBuzz,61,62,Fizz,64,Buzz,Fizz,67,68,Fizz,Buzz,
71,Fizz,73,74,FizzBuzz,76,77,Fizz,79,Buzz,Fizz,82,83,Fizz,Buzz,86,Fizz,
88,89,FizzBuzz,91,92,Fizz,94,Buzz,Fizz,97,98,Fizz,Buzz
