MODULE ErrPos;

IMPORT Files, Texts, Oberon;

CONST
  CR = 0DX;
  LF = 0AX;
  MaxLineLen = 1024;

VAR W: Texts.Writer;

PROCEDURE CopyString(src: ARRAY OF CHAR; VAR dst: ARRAY OF CHAR);
  VAR i: LONGINT;
BEGIN
  i := 0;
  WHILE (i < LEN(dst) - 1) & (src[i] # 0X) DO dst[i] := src[i]; INC(i) END;
  dst[i] := 0X
END CopyString;

PROCEDURE CopyLineAtPos(T: Texts.Text; pos: LONGINT;
  VAR line: ARRAY OF CHAR; VAR lineNo, col: LONGINT; VAR truncated: BOOLEAN);
  VAR R: Texts.Reader; ch: CHAR;
    p, lineStart: LONGINT;
    prevCR: BOOLEAN;
    i: LONGINT;
BEGIN
  lineNo := 1; col := 1; lineStart := 0; prevCR := FALSE;
  Texts.OpenReader(R, T, 0); Texts.Read(R, ch); p := 0;
  WHILE (p < pos) & ~R.eot DO
    IF (ch = CR) OR (ch = LF) THEN
      IF ~((ch = LF) & prevCR) THEN INC(lineNo) END;
      col := 1; lineStart := p + 1;
      prevCR := ch = CR
    ELSE
      INC(col); prevCR := FALSE
    END;
    Texts.Read(R, ch); INC(p)
  END;
  Texts.OpenReader(R, T, lineStart); Texts.Read(R, ch);
  i := 0; truncated := FALSE;
  WHILE ~R.eot & (ch # CR) & (ch # LF) DO
    IF i < LEN(line) - 1 THEN line[i] := ch; INC(i) ELSE truncated := TRUE END;
    Texts.Read(R, ch)
  END;
  line[i] := 0X
END CopyLineAtPos;

PROCEDURE Show*;
  VAR S: Texts.Scanner;
    fname: ARRAY 64 OF CHAR;
    pos: LONGINT;
    T: Texts.Text;
    f: Files.File;
    line: ARRAY MaxLineLen OF CHAR;
    lineNo, col: LONGINT;
    truncated: BOOLEAN;
    i: LONGINT;
BEGIN
  Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S);
  IF S.class = Texts.Name THEN
    CopyString(S.s, fname); Texts.Scan(S);
    IF S.class = Texts.Int THEN
      pos := S.i;
      f := Files.Old(fname);
      IF f = NIL THEN
        Texts.WriteString(W, "file not found: "); Texts.WriteString(W, fname);
        Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf)
      ELSE
        NEW(T); Texts.Open(T, fname);
        IF pos < 0 THEN pos := 0 END;
        IF pos > T.len THEN
          Texts.WriteString(W, "pos out of range: "); Texts.WriteInt(W, pos, 1);
          Texts.WriteString(W, " (len "); Texts.WriteInt(W, T.len, 1); Texts.Write(W, ")");
          Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf)
        ELSE
          CopyLineAtPos(T, pos, line, lineNo, col, truncated);
          Texts.WriteString(W, fname); Texts.WriteString(W, ": ");
          Texts.WriteString(W, "line "); Texts.WriteInt(W, lineNo, 1);
          Texts.WriteString(W, " col "); Texts.WriteInt(W, col, 1);
          Texts.WriteLn(W);
          Texts.WriteString(W, line);
          IF truncated THEN Texts.WriteString(W, " ...") END;
          Texts.WriteLn(W);
          i := 1;
          WHILE i < col DO Texts.Write(W, " "); INC(i) END;
          Texts.Write(W, "^");
          Texts.WriteLn(W);
          Texts.Append(Oberon.Log, W.buf)
        END
      END
    ELSE
      Texts.WriteString(W, "usage: ErrPos.Show filename pos"); Texts.WriteLn(W);
      Texts.Append(Oberon.Log, W.buf)
    END
  ELSE
    Texts.WriteString(W, "usage: ErrPos.Show filename pos"); Texts.WriteLn(W);
    Texts.Append(Oberon.Log, W.buf)
  END
END Show;

BEGIN
  Texts.OpenWriter(W);
END ErrPos.
