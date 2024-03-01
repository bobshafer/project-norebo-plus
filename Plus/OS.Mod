MODULE OS;

  IMPORT SYSTEM, Norebo;

  CONST osGetenv = 41;

  PROCEDURE Getenv*(env: ARRAY OF CHAR; VAR v: ARRAY OF CHAR);
  BEGIN Norebo.SysReq(osGetenv, SYSTEM.ADR(env), SYSTEM.ADR(v), LEN(v));
  END Getenv;

END OS.
