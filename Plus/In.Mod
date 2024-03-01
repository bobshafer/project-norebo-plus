MODULE In; (*AP 12.4.21 / Extended Oberon*)
  IMPORT IO;

  VAR Done*: BOOLEAN;

  PROCEDURE Char*(VAR ch: CHAR);
  BEGIN
    IF Done THEN IO.Read(ch); Done := ~IO.EOF END
  END Char;

  PROCEDURE Line*(VAR x: ARRAY OF CHAR);
  BEGIN
    IF Done THEN IO.ReadLine(x); Done := ~IO.Overflow END
  END Line;

BEGIN Done := TRUE
END In.
