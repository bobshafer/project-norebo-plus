MODULE IO;

IMPORT SYSTEM, RS232, Texts;

CONST NL* = CHR(10);
      CR* = CHR(13);
	CliLen = 1024;

VAR EOF*: BOOLEAN;
    Overflow*: BOOLEAN;

PROCEDURE Read*(VAR ch: CHAR);
VAR i : INTEGER;
BEGIN 
  RS232.Rec(i);
  EOF := (i < 0);
  IF ~EOF THEN ch := CHR(i) END
END Read;

PROCEDURE ReadLine*(VAR str: ARRAY OF CHAR);
VAR i : INTEGER;
    ch : CHAR;
BEGIN
  i := 0;
  Overflow := FALSE;
  Read(ch);
  WHILE ~EOF & (ch # NL) & (i < LEN(str)) DO
    IF ch # CR THEN str[i] := ch; INC(i) END;
    IF i < LEN(str) THEN Read(ch) END
  END;
  str[i] := 0X;
  IF ~EOF & (i = LEN(str)) THEN
    Overflow := TRUE (* too many chars on line to fit in array *)
  END
END ReadLine;

PROCEDURE Write*(ch: CHAR);
BEGIN
  RS232.Send(ORD(ch))
END Write;

PROCEDURE WriteString*(str: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN
  i := 0;
  WHILE (i < LEN(str)) & (str[i] # 0X) DO Write(str[i]); INC(i) END
END WriteString;

PROCEDURE WriteLn*;
BEGIN Write(NL) END WriteLn;

PROCEDURE WriteEOL*;
BEGIN Write(CR); Write(NL) END WriteEOL;

PROCEDURE Ignore(T: Texts.Text; op: INTEGER; beg, end: INTEGER);
END Ignore;

PROCEDURE AppendBuffer*(B: Texts.Buffer);
VAR R: Texts.Reader; ch: CHAR; T: Texts.Text;
BEGIN
	NEW(T); Texts.Open(T, ""); T.notify := Ignore;
	Texts.Append(T, B); 
	Texts.OpenReader(R, T, 0);
	Texts.Read(R, ch); 
	WHILE ~R.eot DO
		IF ch = 0DX THEN ch := 0AX END; (* Not necessarily so *)
		Write(ch); 
		Texts.Read(R, ch) 
	END;
	T := NIL
END AppendBuffer;

PROCEDURE Decode*(VAR percentEncodedString: ARRAY OF CHAR);
  VAR ch: CHAR;
    c: ARRAY 2 OF CHAR;
    i, j: INTEGER;

  PROCEDURE HexOrd(c: CHAR): INTEGER;
  VAR i: INTEGER;
  BEGIN
      IF    c >= "a" THEN i := ORD(c) - ORD("a") + 10
      ELSIF c >= "A" THEN i := ORD(c) - ORD("A") + 10
      ELSE                i := ORD(c) - ORD("0")
      END
      RETURN i
  END HexOrd;

  BEGIN
    i := 0; j := 0;
    WHILE (j < LEN(percentEncodedString)) & (percentEncodedString[j] # 0X) DO
	ch := percentEncodedString[j];
	IF ch = "%" THEN
	    INC(j); c[0] := percentEncodedString[j];
	    INC(j); c[1] := percentEncodedString[j];
	    ch := CHR(HexOrd(c[0]) * 16 + HexOrd(c[1]))
	END;
	IF percentEncodedString[i] # ch THEN percentEncodedString[i] := ch END;
	INC(i); INC(j)
    END;
    percentEncodedString[i] := 0X
END Decode;

PROCEDURE Hello*;
BEGIN
	WriteString("Hello, World!");
	WriteLn;
END Hello;

BEGIN
	EOF := FALSE;
	Overflow := FALSE
END IO.
