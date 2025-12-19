MODULE Shell;

IMPORT Texts, Oberon, IO, Norebo, S := SYSTEM;

CONST
  Prompt = "(*nw*) ";
  LineLen = 1024;

PROCEDURE SkipSpaces(VAR s: ARRAY OF CHAR; VAR i: INTEGER);
BEGIN
  WHILE (i < LEN(s)) & (s[i] = " ") DO INC(i) END
END SkipSpaces;

PROCEDURE IsEndCommand(VAR s: ARRAY OF CHAR; i: INTEGER): BOOLEAN;
BEGIN
  RETURN (i + 2 < LEN(s)) & (s[i] = "e") & (s[i + 1] = "n") & (s[i + 2] = "d") &
    ((i + 3 >= LEN(s)) OR (s[i + 3] = 0X))
END IsEndCommand;

PROCEDURE RunHost(VAR s: ARRAY OF CHAR; i: INTEGER);
  VAR len: INTEGER;
BEGIN
  len := 0;
  WHILE (i + len < LEN(s)) & (s[i + len] # 0X) DO INC(len) END;
  IF len > 0 THEN
    Norebo.System(S.ADR(s[i]), len)
  END
END RunHost;

PROCEDURE Ignore(T: Texts.Text; op: INTEGER; beg, end: INTEGER);
END Ignore;

PROCEDURE ParamCall(VAR cli: ARRAY OF CHAR; VAR res: INTEGER);
  VAR W: Texts.Writer; T: Texts.Text;
    i: INTEGER;
BEGIN
  NEW(T); Texts.Open(T, ""); T.notify := Ignore;
  i := 0;
  WHILE (i < LEN(cli)) & (cli[i] = " ") DO INC(i) END;
  WHILE (i < LEN(cli)) & (cli[i] > " ") DO INC(i) END;
  cli[i] := 0X; INC(i);
  Texts.OpenWriter(W);
  WHILE (i < LEN(cli)) & (cli[i] >= " ") DO Texts.Write(W, cli[i]); INC(i) END;
  Texts.Append(T, W.buf);
  Oberon.SetPar(T, 0);
  Oberon.Call(cli, res)
END ParamCall;

PROCEDURE Step*;
  VAR line: ARRAY LineLen OF CHAR;
    res, i: INTEGER;
    done: BOOLEAN;
BEGIN
  IO.WriteString(Prompt);
  IO.ReadLine(line);
  IF IO.EOF THEN Norebo.Halt(0) END;
  i := 0;
  SkipSpaces(line, i);
  done := FALSE;
  IF line[i] = 0X THEN done := TRUE END;
  IF ~done & (line[i] = "!") THEN
    INC(i); SkipSpaces(line, i);
    RunHost(line, i);
    done := TRUE
  END;
  IF ~done & IsEndCommand(line, i) THEN Norebo.Halt(0) END;
  IF ~done THEN
    ParamCall(line, res);
    IF res # 0 THEN
      IO.WriteString("???"); IO.WriteLn
    ELSE
      Oberon.Collect(0)
    END
  END
END Step;

BEGIN
END Shell.
