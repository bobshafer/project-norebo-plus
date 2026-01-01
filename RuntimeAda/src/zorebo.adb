with Ada.Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;
with Ada.Directories;
with Ada.Direct_IO;
with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;

procedure Zorebo is
   use Interfaces;
   use Ada.Calendar;
   use type Ada.Streams.Stream_Element_Offset;
   use Interfaces.C.Strings;
   use type Interfaces.C.int;

   Path_Env  : constant String := "NOREBO_PATH";
   InnerCore : constant String := "InnerCore";

   Mem_Bytes   : constant Unsigned_32 := 8 * 1024 * 1024;
   Stack_Org   : constant Unsigned_32 := 16#0008_0000#;
   Max_Files   : constant := 500;
   Name_Length : constant := 32;
   Enable_Checks : constant Boolean := False;
   Fast_Word_Access : constant Boolean := True;
   Assume_Little_Endian : constant Boolean := True;

   Pbit : constant Unsigned_32 := 16#8000_0000#;
   Qbit : constant Unsigned_32 := 16#4000_0000#;
   Ubit : constant Unsigned_32 := 16#2000_0000#;
   Vbit : constant Unsigned_32 := 16#1000_0000#;

   type Op_Code is (MOV, LSL, ASR, ROR, ANDD, ANN, IOR, XORR, ADD, SUB, MUL, DIV, FAD, FSB, FML, FDV);

   subtype U4 is Integer range 0 .. 15;

   type Flag_State is record
      Z : Boolean := False;
      N : Boolean := False;
      C : Boolean := False;
      V : Boolean := False;
   end record;

   type Reg_Array is array (U4) of Unsigned_32;

   type Vm_State is record
      PC    : Unsigned_32 := 0;
      R     : Reg_Array := (others => 0);
      H     : Unsigned_32 := 0;
      Flags : Flag_State := (others => False);
   end record;

   type Mem_Array is array (Unsigned_32 range 0 .. Mem_Bytes - 1) of Unsigned_8;

   package Byte_IO is new Ada.Direct_IO (Unsigned_8);
   use type Byte_IO.Count;
   package U32_Access is new System.Address_To_Access_Conversions (Unsigned_32);
   use type System.Storage_Elements.Storage_Offset;

   type File_Entry is record
      File       : Byte_IO.File_Type;
      Opened     : Boolean := False;
      Name       : String (1 .. Name_Length) := (others => Character'Val (0));
      Registered : Boolean := False;
   end record;

   type File_Table is array (Natural range 0 .. Max_Files - 1) of File_Entry;
   type Sysarg_Array is array (Natural range 0 .. 2) of Unsigned_32;

   type Runtime is record
      Mem       : Mem_Array := (others => 0);
      Sysarg    : Sysarg_Array := (others => 0);
      Sysres    : Unsigned_32 := 0;
      Argc      : Unsigned_32 := 0;
      Argv_Count : Natural := 0;
      VM        : Vm_State;
      Files     : File_Table;
      Searching : Boolean := False;
      Search    : Ada.Directories.Search_Type;
   end record;

   RT_Ptr : constant access Runtime := new Runtime;
   RT : Runtime renames RT_Ptr.all;

   type I32 is new Interfaces.Integer_32;
   type I64 is new Interfaces.Integer_64;
   type U64 is new Interfaces.Unsigned_64;

   function U32_To_I32 is new Ada.Unchecked_Conversion (Unsigned_32, I32);
   function I32_To_U32 is new Ada.Unchecked_Conversion (I32, Unsigned_32);
   function I64_To_U64 is new Ada.Unchecked_Conversion (I64, U64);

   function C_System (Cmd : chars_ptr) return Interfaces.C.int;
   pragma Import (C, C_System, "system");
   function C_Getchar return Interfaces.C.int;
   pragma Import (C, C_Getchar, "getchar");
   procedure C_Exit (Code : Interfaces.C.int);
   pragma Import (C, C_Exit, "exit");

   function ShiftL (V : Unsigned_32; N : Natural) return Unsigned_32 is (Shift_Left (V, N));
   function ShiftR (V : Unsigned_32; N : Natural) return Unsigned_32 is (Shift_Right (V, N));

   function U4_From (V : Unsigned_32) return U4 is
   begin
      return U4 (Integer (V and 16#F#));
   end U4_From;
   pragma Inline (U4_From);

   function Op_From (V : Unsigned_32) return Op_Code is
   begin
      return Op_Code'Val (Integer (V and 16#F#));
   end Op_From;
   pragma Inline (Op_From);

   function Sign_Extend (V : Unsigned_32; Bits : Natural) return I32 is
      Mask : constant Unsigned_32 := Shift_Left (1, Bits) - 1;
      Sign : constant Unsigned_32 := Shift_Left (1, Bits - 1);
      Val  : constant Unsigned_32 := V and Mask;
   begin
      if (Val and Sign) /= 0 then
         return U32_To_I32 (Val or not Mask);
      end if;
      return U32_To_I32 (Val);
   end Sign_Extend;
   pragma Inline (Sign_Extend);

   function SHL_I32 (V : I32; N : Natural) return I32 is
   begin
      return U32_To_I32 (Shift_Left (I32_To_U32 (V), N));
   end SHL_I32;
   pragma Inline (SHL_I32);

   function ASR_I32 (V : I32; N : Natural) return I32 is
      U : Unsigned_32 := I32_To_U32 (V);
   begin
      if N = 0 then
         return V;
      end if;
      if V < 0 then
         U := Shift_Right (U, N) or Shift_Left (Unsigned_32'Last, 32 - N);
      else
         U := Shift_Right (U, N);
      end if;
      return U32_To_I32 (U);
   end ASR_I32;
   pragma Inline (ASR_I32);

   function Trim_Image (N : Natural) return String is
      Img : constant String := Integer'Image (Integer (N));
   begin
      return Ada.Strings.Fixed.Trim (Img, Ada.Strings.Left);
   end Trim_Image;

   function Path_Separator (Env : String) return Character is
   begin
      if Ada.Strings.Fixed.Index (Env, ";") /= 0 then
         return ';';
      end if;
      return ':';
   end Path_Separator;

   function Find_In_Path (Env, Filename : String) return String is
      Sep   : constant Character := Path_Separator (Env);
      Start : Positive := Env'First;
      I     : Natural := Env'First;
   begin
      if Env'Length = 0 then
         return "";
      end if;
      while I <= Env'Last + 1 loop
         if I > Env'Last or else Env (I) = Sep then
            declare
               Part : constant String := (if I = Start then "" else Env (Start .. I - 1));
               Full : constant String := (if Part'Length = 0 then Filename else Ada.Directories.Compose (Part, Filename));
            begin
               if Ada.Directories.Exists (Full) then
                  return Full;
               end if;
            end;
            Start := I + 1;
         end if;
         I := I + 1;
      end loop;
      return "";
   end Find_In_Path;

   procedure Exit_With (Code : Integer) is
   begin
      -- Temp cleanup omitted for brevity in this translation.
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status (Code));
      C_Exit (Interfaces.C.int (Code));
   end Exit_With;

   function Le32_To_Host (B0, B1, B2, B3 : Unsigned_8) return Unsigned_32 is
   begin
      return Unsigned_32 (B0) or
        ShiftL (Unsigned_32 (B1), 8) or
        ShiftL (Unsigned_32 (B2), 16) or
        ShiftL (Unsigned_32 (B3), 24);
   end Le32_To_Host;

   procedure Mem_Check_Range (Adr, Siz : Unsigned_32; Proc : String) is
   begin
      if Enable_Checks then
         if Adr >= Mem_Bytes or else Mem_Bytes - Adr < Siz then
            Ada.Text_IO.Put_Line (Proc & ": Memory access out of bounds");
            Exit_With (1);
         end if;
      end if;
   end Mem_Check_Range;

   function Mem_Read_Word (Adr : Unsigned_32) return Unsigned_32 is
   begin
      if Enable_Checks then
         if Adr >= Mem_Bytes - 3 then
            Ada.Text_IO.Put_Line ("Memory read out of bounds");
            Exit_With (1);
         end if;
      end if;
      if Fast_Word_Access and then Assume_Little_Endian and then (Adr and 3) = 0 then
         declare
            Ptr : constant U32_Access.Object_Pointer := U32_Access.To_Pointer (RT.Mem (Adr)'Address);
         begin
            return Ptr.all;
         end;
      end if;
      return Le32_To_Host (
        RT.Mem (Adr),
        RT.Mem (Adr + 1),
        RT.Mem (Adr + 2),
        RT.Mem (Adr + 3));
   end Mem_Read_Word;
   pragma Inline (Mem_Read_Word);

   function Mem_Read_Byte (Adr : Unsigned_32) return Unsigned_8 is
   begin
      if Enable_Checks then
         if Adr >= Mem_Bytes then
            Ada.Text_IO.Put_Line ("Memory read out of bounds");
            Exit_With (1);
         end if;
      end if;
      return RT.Mem (Adr);
   end Mem_Read_Byte;
   pragma Inline (Mem_Read_Byte);

   procedure Mem_Write_Word (Adr : Unsigned_32; Val : Unsigned_32) is
   begin
      if Enable_Checks then
         if Adr >= Mem_Bytes - 3 then
            Ada.Text_IO.Put_Line ("Memory write out of bounds");
            Exit_With (1);
         end if;
      end if;
      if Fast_Word_Access and then Assume_Little_Endian and then (Adr and 3) = 0 then
         declare
            Ptr : constant U32_Access.Object_Pointer := U32_Access.To_Pointer (RT.Mem (Adr)'Address);
         begin
            Ptr.all := Val;
            return;
         end;
      end if;
      RT.Mem (Adr)     := Unsigned_8 (Val and 16#FF#);
      RT.Mem (Adr + 1) := Unsigned_8 (ShiftR (Val, 8) and 16#FF#);
      RT.Mem (Adr + 2) := Unsigned_8 (ShiftR (Val, 16) and 16#FF#);
      RT.Mem (Adr + 3) := Unsigned_8 (ShiftR (Val, 24) and 16#FF#);
   end Mem_Write_Word;
   pragma Inline (Mem_Write_Word);

   procedure Mem_Write_Byte (Adr : Unsigned_32; Val : Unsigned_8) is
   begin
      if Enable_Checks then
         if Adr >= Mem_Bytes then
            Ada.Text_IO.Put_Line ("Memory write out of bounds");
            Exit_With (1);
         end if;
      end if;
      RT.Mem (Adr) := Val;
   end Mem_Write_Byte;
   pragma Inline (Mem_Write_Byte);

   procedure Set_Register (Reg : U4; Value : Unsigned_32) is
   begin
      RT.VM.R (Reg) := Value;
      RT.VM.Flags.Z := (Value = 0);
      RT.VM.Flags.N := (U32_To_I32 (Value) < 0);
   end Set_Register;
   pragma Inline (Set_Register);

   function CPU_Read_Program (Adr : Unsigned_32) return Unsigned_32 is
   begin
      return Mem_Read_Word (Adr * 4);
   end CPU_Read_Program;
   pragma Inline (CPU_Read_Program);

   function Sysreq_Exec (Nval : Unsigned_32) return Unsigned_32;

   function IO_Read_Word (Adr : Unsigned_32) return Unsigned_32 is
      Slot : constant I32 := I32 (U32_To_I32 (Adr));
   begin
      case Integer (-Slot / 4) is
         when 16 / 4 => return RT.Sysarg (2);
         when 12 / 4 => return RT.Sysarg (1);
         when 8 / 4  => return RT.Sysarg (0);
         when 4 / 4  => return RT.Sysres;
         when 52 / 4 => return 3;
         when 56 / 4 =>
            declare
               Ch : Interfaces.C.int;
            begin
               begin
                  --Ada.Text_IO.Get (Ch);
                  Ch := C_Getchar;
                  return Unsigned_32 (Ch);
               exception
                  when Ada.Text_IO.End_Error =>
                     return 16#FFFF_FFFF#;
               end;
            end;
         when 64 / 4 =>
            declare
               Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
               Epoch : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (1970, 1, 1);
               D : constant Duration := Now - Epoch;
            begin
               return Unsigned_32 (Integer (D * 1000.0));
            end;
        when others =>
           Ada.Text_IO.Put_Line ("Unimplemented read of I/O address " & Integer'Image (Integer (Slot)));
           Exit_With (1);
      end case;
      return 0;
   end IO_Read_Word;

   procedure IO_Write_Word (Adr : Unsigned_32; Val : Unsigned_32) is
      Slot : constant I32 := I32 (U32_To_I32 (Adr));
   begin
      case Integer (-Slot / 4) is
         when 4 / 4  => RT.Sysres := Sysreq_Exec (Val);
         when 8 / 4  => RT.Sysarg (0) := Val;
         when 12 / 4 => RT.Sysarg (1) := Val;
         when 16 / 4 => RT.Sysarg (2) := Val;
         when 56 / 4 =>
            declare
               Out_Ch : constant Unsigned_32 := Val and 16#FF#;
            begin
               Ada.Text_IO.Put (Character'Val (Integer (Out_Ch)));
               if Out_Ch = 10 or else Out_Ch = 13 then
                  Ada.Text_IO.Flush;
               end if;
            end;
         when 60 / 4 =>
            declare
               Buf : String (1 .. 16) := "[LEDs: 76543210]";
               I   : Natural := 0;
            begin
               while I < 8 loop
                  declare
                     Idx  : constant Natural := 14 - I;
                     Mask : constant Unsigned_32 := ShiftL (1, I);
                  begin
                     if (Val and Mask) /= 0 then
                        Buf (Idx) := Character'Val (Character'Pos ('0') + I);
                     else
                        Buf (Idx) := '-';
                     end if;
                  end;
                  I := I + 1;
               end loop;
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Buf);
            end;
         when others =>
            Ada.Text_IO.Put_Line ("Unimplemented write of I/O address " & Integer'Image (Integer (Slot)));
            Exit_With (1);
      end case;
   end IO_Write_Word;

   function CPU_Read_Word (Adr : Unsigned_32) return Unsigned_32 is
   begin
      if I32 (U32_To_I32 (Adr)) >= 0 then
         return Mem_Read_Word (Adr);
      else
         return IO_Read_Word (Adr);
      end if;
   end CPU_Read_Word;
   pragma Inline (CPU_Read_Word);

   function CPU_Read_Byte (Adr : Unsigned_32) return Unsigned_32 is
   begin
      if I32 (U32_To_I32 (Adr)) >= 0 then
         return Unsigned_32 (Mem_Read_Byte (Adr));
      else
         return IO_Read_Word (Adr);
      end if;
   end CPU_Read_Byte;
   pragma Inline (CPU_Read_Byte);

   procedure CPU_Write_Word (Adr : Unsigned_32; Val : Unsigned_32) is
   begin
      if I32 (U32_To_I32 (Adr)) >= 0 then
         Mem_Write_Word (Adr, Val);
      else
         IO_Write_Word (Adr, Val);
      end if;
   end CPU_Write_Word;
   pragma Inline (CPU_Write_Word);

   procedure CPU_Write_Byte (Adr : Unsigned_32; Val : Unsigned_8) is
   begin
      if I32 (U32_To_I32 (Adr)) >= 0 then
         Mem_Write_Byte (Adr, Val);
      else
         IO_Write_Word (Adr, Unsigned_32 (Val));
      end if;
   end CPU_Write_Byte;
   pragma Inline (CPU_Write_Byte);

   function Bool_U32 (B : Boolean) return Unsigned_32 is
   begin
      return (if B then 1 else 0);
   end Bool_U32;
   pragma Inline (Bool_U32);

   function FP_Add (X, Y : Unsigned_32; U, Vflag : Boolean) return Unsigned_32 is
      Xs : constant Boolean := (X and 16#8000_0000#) /= 0;
      Xe : Unsigned_32;
      X0 : I32;
   begin
      if not U then
         Xe := ShiftR (X, 23) and 16#FF#;
         declare
            Xm : constant Unsigned_32 := ShiftL (X and 16#7FFFFF#, 1) or 16#1000000#;
         begin
            X0 := I32 (U32_To_I32 (Xm));
            if Xs then
               X0 := -X0;
            end if;
         end;
      else
         Xe := 150;
         X0 := ASR_I32 (SHL_I32 (I32 (U32_To_I32 (X and 16#00FF_FFFF#)), 8), 7);
      end if;

      declare
         Ys : constant Boolean := (Y and 16#8000_0000#) /= 0;
         Ye : constant Unsigned_32 := ShiftR (Y, 23) and 16#FF#;
         Ym : Unsigned_32 := ShiftL (Y and 16#7FFFFF#, 1);
         Y0 : I32;
         E0 : Unsigned_32;
         X3 : I32;
         Y3 : I32;
      begin
         if (not U) and (not Vflag) then
            Ym := Ym or 16#1000000#;
         end if;
         Y0 := I32 (U32_To_I32 (Ym));
         if Ys then
            Y0 := -Y0;
         end if;

         if Ye > Xe then
            declare
               Shift : constant Unsigned_32 := Ye - Xe;
            begin
               E0 := Ye;
               if Shift > 31 then
                  X3 := ASR_I32 (X0, 31);
               else
                  X3 := ASR_I32 (X0, Integer (Shift));
               end if;
               Y3 := Y0;
            end;
         else
            declare
               Shift : constant Unsigned_32 := Xe - Ye;
            begin
               E0 := Xe;
               X3 := X0;
               if Shift > 31 then
                  Y3 := ASR_I32 (Y0, 31);
               else
                  Y3 := ASR_I32 (Y0, Integer (Shift));
               end if;
            end;
         end if;

         declare
            Sum : Unsigned_32 :=
              (ShiftL (Bool_U32 (Xs), 26) or ShiftL (Bool_U32 (Xs), 25) or
               (I32_To_U32 (X3) and 16#01FF_FFFF#)) +
              (ShiftL (Bool_U32 (Ys), 26) or ShiftL (Bool_U32 (Ys), 25) or
               (I32_To_U32 (Y3) and 16#01FF_FFFF#));
            S : Unsigned_32;
            E1 : Unsigned_32;
            T3 : Unsigned_32;
         begin
            if (Sum and ShiftL (1, 26)) /= 0 then
               S := 0 - Sum;
            else
               S := Sum;
            end if;
            S := (S + 1) and 16#07FF_FFFF#;

            E1 := E0 + 1;
            T3 := ShiftR (S, 1);
            if (S and 16#03FF_FFFC#) /= 0 then
               while (T3 and ShiftL (1, 24)) = 0 loop
                  T3 := ShiftL (T3, 1);
                  E1 := E1 - 1;
               end loop;
            else
               T3 := ShiftL (T3, 24);
               E1 := E1 - 24;
            end if;

            if Vflag then
               return I32_To_U32 (ASR_I32 (I32 (U32_To_I32 (ShiftL (Sum, 5))), 6));
            elsif (X and 16#7FFF_FFFF#) = 0 then
               return (if not U then Y else 0);
            elsif (Y and 16#7FFF_FFFF#) = 0 then
               return X;
            elsif (T3 and 16#01FF_FFFF#) = 0 or else (E1 and 16#100#) /= 0 then
               return 0;
            else
               return (ShiftL (Sum and 16#0400_0000#, 5)) or
                 ShiftL (E1, 23) or
                 (ShiftR (T3, 1) and 16#7FFFFF#);
            end if;
         end;
      end;
   end FP_Add;

   function FP_Mul (X, Y : Unsigned_32) return Unsigned_32 is
      Sign : constant Unsigned_32 := (X xor Y) and 16#8000_0000#;
      Xe   : constant Unsigned_32 := ShiftR (X, 23) and 16#FF#;
      Ye   : constant Unsigned_32 := ShiftR (Y, 23) and 16#FF#;
      Xm   : constant Unsigned_32 := (X and 16#7FFFFF#) or 16#800000#;
      Ym   : constant Unsigned_32 := (Y and 16#7FFFFF#) or 16#800000#;
      M    : constant U64 := U64 (Xm) * U64 (Ym);
      E1   : Unsigned_32 := (Xe + Ye) - 127;
      Z0   : Unsigned_32;
   begin
      if (M and Shift_Left (U64 (1), 47)) /= 0 then
         E1 := E1 + 1;
         Z0 := Unsigned_32 (Shift_Right (M, 23) + 1) and 16#00FF_FFFF#;
      else
         Z0 := Unsigned_32 (Shift_Right (M, 22) + 1) and 16#00FF_FFFF#;
      end if;

      if Xe = 0 or else Ye = 0 then
         return 0;
      elsif (E1 and 16#100#) = 0 then
         return Sign or ShiftL (E1 and 16#FF#, 23) or ShiftR (Z0, 1);
      elsif (E1 and 16#80#) = 0 then
         return Sign or ShiftL (16#FF#, 23) or ShiftR (Z0, 1);
      else
         return 0;
      end if;
   end FP_Mul;

   function FP_Div (X, Y : Unsigned_32) return Unsigned_32 is
      Sign : constant Unsigned_32 := (X xor Y) and 16#8000_0000#;
      Xe   : constant Unsigned_32 := ShiftR (X, 23) and 16#FF#;
      Ye   : constant Unsigned_32 := ShiftR (Y, 23) and 16#FF#;
      Xm   : constant Unsigned_32 := (X and 16#7FFFFF#) or 16#800000#;
      Ym   : constant Unsigned_32 := (Y and 16#7FFFFF#) or 16#800000#;
      Q1   : constant Unsigned_32 := Unsigned_32 (Shift_Left (U64 (Xm), 25) / U64 (Ym));
      E1   : Unsigned_32 := (Xe - Ye) + 126;
      Q2   : Unsigned_32;
      Q3   : Unsigned_32;
   begin
      if (Q1 and ShiftL (1, 25)) /= 0 then
         E1 := E1 + 1;
         Q2 := ShiftR (Q1, 1) and 16#00FF_FFFF#;
      else
         Q2 := Q1 and 16#00FF_FFFF#;
      end if;
      Q3 := Q2 + 1;

      if Xe = 0 then
         return 0;
      elsif Ye = 0 then
         return Sign or ShiftL (16#FF#, 23);
      elsif (E1 and 16#100#) = 0 then
         return Sign or ShiftL (E1 and 16#FF#, 23) or ShiftR (Q3, 1);
      elsif (E1 and 16#80#) = 0 then
         return Sign or ShiftL (16#FF#, 23) or ShiftR (Q2, 1);
      else
         return 0;
      end if;
   end FP_Div;

   type Idiv_Result is record
      Quot : Unsigned_32;
      Remainder  : Unsigned_32;
   end record;

   function Idiv (X, Y : Unsigned_32; Signed_Div : Boolean) return Idiv_Result is
      Sign : constant Boolean := (U32_To_I32 (X) < 0) and Signed_Div;
      X0   : Unsigned_32 := (if Sign then (0 - X) else X);
      Rq   : U64 := U64 (X0);
   begin
      for S in 0 .. 31 loop
         declare
            W0 : constant Unsigned_32 := Unsigned_32 (Shift_Right (Rq, 31));
            W1 : constant Unsigned_32 := W0 - Y;
         begin
            if U32_To_I32 (W1) < 0 then
               Rq := Shift_Left (U64 (W0), 32) or Shift_Left (Rq and 16#7FFF_FFFF#, 1);
            else
               Rq := Shift_Left (U64 (W1), 32) or Shift_Left (Rq and 16#7FFF_FFFF#, 1) or 1;
            end if;
         end;
      end loop;

      declare
         D : Idiv_Result;
      begin
         D.Quot := Unsigned_32 (Rq and 16#FFFF_FFFF#);
         D.Remainder  := Unsigned_32 (Shift_Right (Rq, 32));
         if Sign then
            D.Quot := 0 - D.Quot;
            if D.Remainder /= 0 then
               D.Quot := D.Quot - 1;
               D.Remainder := Y - D.Remainder;
            end if;
         end if;
         return D;
      end;
   end Idiv;

   function Files_Check_Name (Name : String) return Boolean is
      I : Natural := 1;
   begin
      while I <= Name_Length loop
         declare
            Ch : constant Character := Name (I);
         begin
            if Ch = Character'Val (0) then
               return True;
            elsif not ((Ch >= 'A' and Ch <= 'Z') or else
              (Ch >= 'a' and Ch <= 'z') or else
              (I > 1 and then (Ch = '.' or else (Ch >= '0' and Ch <= '9'))))
            then
               return False;
            end if;
         end;
         I := I + 1;
      end loop;
      return False;
   end Files_Check_Name;

   function Get_Name (Adr : Unsigned_32; Name : out String) return Boolean is
      Buf : String (1 .. Name_Length);
   begin
      Mem_Check_Range (Adr, Name_Length, "Files.GetName");
      for I in 0 .. Name_Length - 1 loop
         Buf (I + 1) := Character'Val (Integer (RT.Mem (Adr + Unsigned_32 (I))));
      end loop;
      if Files_Check_Name (Buf) then
         Name := Buf;
         return True;
      end if;
      return False;
   end Get_Name;

   function Name_To_String (Buf : String) return String is
      Len : Natural := 0;
   begin
      for I in Buf'Range loop
         exit when Buf (I) = Character'Val (0);
         Len := Len + 1;
      end loop;
      if Len = 0 then
         return "";
      end if;
      return Buf (Buf'First .. Buf'First + Len - 1);
   end Name_To_String;

   function Files_Allocate (Name : String; Registered : Boolean) return Unsigned_32 is
   begin
      for I in RT.Files'Range loop
         if not RT.Files (I).Opened then
            RT.Files (I).Name := (others => Character'Val (0));
            RT.Files (I).Name (1 .. Name'Length) := Name;
            RT.Files (I).Registered := Registered;
            return Unsigned_32 (I);
         end if;
      end loop;
      Ada.Text_IO.Put_Line ("Files.Allocate: Too many open files");
      Exit_With (1);
      return 16#FFFF_FFFF#;
   end Files_Allocate;

   procedure Files_Check_Handle (H : Unsigned_32; Proc : String) is
   begin
      if H >= Unsigned_32 (Max_Files) or else not RT.Files (Natural (H)).Opened then
         Ada.Text_IO.Put_Line (Proc & ": Invalid file handle");
         Exit_With (1);
      end if;
   end Files_Check_Handle;

   function Files_New (Name_Adr : Unsigned_32) return Unsigned_32 is
      Name_Buf : String (1 .. Name_Length);
   begin
      if not Get_Name (Name_Adr, Name_Buf) then
         return 16#FFFF_FFFF#;
      end if;
      declare
         Name_Str : constant String := Name_To_String (Name_Buf);
         Hidx     : constant Unsigned_32 := Files_Allocate (Name_Str, False);
         Tmp_Name : constant String := ".norebo_tmp_" & Trim_Image (Natural (Hidx));
      begin
         Byte_IO.Create (RT.Files (Natural (Hidx)).File,
           Byte_IO.Inout_File, Tmp_Name);
         RT.Files (Natural (Hidx)).Opened := True;
         return Hidx;
      exception
         when others =>
            Ada.Text_IO.Put_Line ("Files.New: " & Name_Str);
            Exit_With (1);
            return 16#FFFF_FFFF#;
      end;
   end Files_New;

   function Files_Old (Name_Adr : Unsigned_32) return Unsigned_32 is
      Name_Buf : String (1 .. Name_Length);
   begin
      if not Get_Name (Name_Adr, Name_Buf) then
         return 16#FFFF_FFFF#;
      end if;
      declare
         Name_Str : constant String := Name_To_String (Name_Buf);
         Hidx     : constant Unsigned_32 := Files_Allocate (Name_Str, True);
      begin
         begin
            Byte_IO.Open (RT.Files (Natural (Hidx)).File,
              Byte_IO.Inout_File, Name_Str);
            RT.Files (Natural (Hidx)).Opened := True;
            return Hidx;
         exception
            when others =>
               null;
         end;

         declare
            Env  : constant String := Ada.Environment_Variables.Value (Path_Env, "");
            Path : constant String := Find_In_Path (Env, Name_Str);
         begin
            if Path'Length > 0 then
               Byte_IO.Open (RT.Files (Natural (Hidx)).File,
                 Byte_IO.Inout_File, Path);
               RT.Files (Natural (Hidx)).Opened := True;
               return Hidx;
            end if;
         exception
            when others =>
               null;
         end;

         RT.Files (Natural (Hidx)).Opened := False;
         RT.Files (Natural (Hidx)).Registered := False;
         RT.Files (Natural (Hidx)).Name := (others => Character'Val (0));
         return 16#FFFF_FFFF#;
      end;
   end Files_Old;

   function Files_Register (Hidx : Unsigned_32) return Unsigned_32 is
   begin
      Files_Check_Handle (Hidx, "Files.Register");
      declare
         File_Ent : File_Entry renames RT.Files (Natural (Hidx));
      begin
         if (not File_Ent.Registered) and then File_Ent.Name (1) /= Character'Val (0) then
            declare
               Name_Str : constant String := Name_To_String (File_Ent.Name);
               New_File : Byte_IO.File_Type;
               Tmp_File : Byte_IO.File_Type;
               B        : Unsigned_8;
            begin
               Byte_IO.Close (File_Ent.File);
               Byte_IO.Open (Tmp_File, Byte_IO.Inout_File, ".norebo_tmp_" & Trim_Image (Natural (Hidx)));
               Byte_IO.Create (New_File, Byte_IO.Inout_File, Name_Str);
               Byte_IO.Set_Index (Tmp_File, 1);
               loop
                  exit when Byte_IO.End_Of_File (Tmp_File);
                  Byte_IO.Read (Tmp_File, B);
                  Byte_IO.Write (New_File, B);
               end loop;
               Byte_IO.Close (Tmp_File);
               Byte_IO.Close (New_File);
               Byte_IO.Open (File_Ent.File, Byte_IO.Inout_File, Name_Str);
               File_Ent.Registered := True;
            exception
               when others =>
                  Ada.Text_IO.Put_Line ("Can't create/write file " & Name_Str);
                  Exit_With (1);
                  return 16#FFFF_FFFF#;
            end;
         end if;
      end;
      return 0;
   end Files_Register;

   function Files_Close (Hidx : Unsigned_32) return Unsigned_32 is
   begin
      Files_Check_Handle (Hidx, "Files.Close");
      Byte_IO.Close (RT.Files (Natural (Hidx)).File);
      RT.Files (Natural (Hidx)).Opened := False;
      RT.Files (Natural (Hidx)).Registered := False;
      RT.Files (Natural (Hidx)).Name := (others => Character'Val (0));
      return 0;
   end Files_Close;

   function Files_Seek (Hidx, Pos : Unsigned_32) return Unsigned_32 is
   begin
      Files_Check_Handle (Hidx, "Files.Seek");
      Byte_IO.Set_Index (RT.Files (Natural (Hidx)).File, Byte_IO.Count (Pos) + 1);
      return 0;
   exception
      when others =>
         return 1;
   end Files_Seek;

   function Files_Tell (Hidx : Unsigned_32) return Unsigned_32 is
   begin
      Files_Check_Handle (Hidx, "Files.Tell");
      return Unsigned_32 (Byte_IO.Index (RT.Files (Natural (Hidx)).File) - 1);
   end Files_Tell;

   function Files_Read (Hidx, Adr, Siz : Unsigned_32) return Unsigned_32 is
   begin
      Files_Check_Handle (Hidx, "Files.Read");
      Mem_Check_Range (Adr, Siz, "Files.Read");
      if Siz = 0 then
         return 0;
      end if;
      declare
         Count : Unsigned_32 := 0;
         B     : Unsigned_8 := 0;
      begin
         for I in 0 .. Integer (Siz) - 1 loop
            if Byte_IO.End_Of_File (RT.Files (Natural (Hidx)).File) then
               RT.Mem (Adr + Unsigned_32 (I)) := 0;
            else
               Byte_IO.Read (RT.Files (Natural (Hidx)).File, B);
               RT.Mem (Adr + Unsigned_32 (I)) := B;
               Count := Count + 1;
            end if;
         end loop;
         return Count;
      end;
   end Files_Read;

   function Files_Write (Hidx, Adr, Siz : Unsigned_32) return Unsigned_32 is
   begin
      Files_Check_Handle (Hidx, "Files.Write");
      Mem_Check_Range (Adr, Siz, "Files.Write");
      if Siz = 0 then
         return 0;
      end if;
      declare
         B : Unsigned_8;
      begin
         for I in 0 .. Integer (Siz) - 1 loop
            B := RT.Mem (Adr + Unsigned_32 (I));
            Byte_IO.Write (RT.Files (Natural (Hidx)).File, B);
         end loop;
      end;
      return Siz;
   end Files_Write;

   function Files_Length (Hidx : Unsigned_32) return Unsigned_32 is
   begin
      Files_Check_Handle (Hidx, "Files.Length");
      return Unsigned_32 (Byte_IO.Size (RT.Files (Natural (Hidx)).File));
   end Files_Length;

   function Time_To_Oberon (T : Ada.Calendar.Time) return Unsigned_32 is
      Year  : Ada.Calendar.Year_Number;
      Month : Ada.Calendar.Month_Number;
      Day   : Ada.Calendar.Day_Number;
      Secs  : Ada.Calendar.Day_Duration;
      Hour  : Unsigned_32;
      Minute: Unsigned_32;
      Second: Unsigned_32;
   begin
      Ada.Calendar.Split (T, Year, Month, Day, Secs);
      Hour := Unsigned_32 (Integer (Secs) / 3600);
      Minute := Unsigned_32 ((Integer (Secs) mod 3600) / 60);
      Second := Unsigned_32 (Integer (Secs) mod 60);
      return (Unsigned_32 (Year mod 100) * 16#0400_0000#) or
        (Unsigned_32 (Month) * 16#0040_0000#) or
        (Unsigned_32 (Day) * 16#0002_0000#) or
        (Hour * 16#0000_1000#) or
        (Minute * 16#0000_0040#) or
        Second;
   end Time_To_Oberon;

   function Files_Date (Hidx : Unsigned_32) return Unsigned_32 is
   begin
      Files_Check_Handle (Hidx, "Files.Date");
      if RT.Files (Natural (Hidx)).Registered then
         declare
            Name_Str : constant String := Name_To_String (RT.Files (Natural (Hidx)).Name);
            T : constant Ada.Calendar.Time := Ada.Directories.Modification_Time (Name_Str);
         begin
            return Time_To_Oberon (T);
         end;
      else
         return Time_To_Oberon (Ada.Calendar.Clock);
      end if;
   end Files_Date;

   function Files_Delete (Name_Adr : Unsigned_32) return Unsigned_32 is
      Name_Buf : String (1 .. Name_Length);
   begin
      if not Get_Name (Name_Adr, Name_Buf) or else Name_Buf (1) = Character'Val (0) then
         return 16#FFFF_FFFF#;
      end if;
      declare
         Name_Str : constant String := Name_To_String (Name_Buf);
      begin
         Ada.Directories.Delete_File (Name_Str);
      exception
         when others => return 16#FFFF_FFFF#;
      end;
      return 0;
   end Files_Delete;

   function Files_Rename (Old_Adr, New_Adr : Unsigned_32) return Unsigned_32 is
      Old_Buf : String (1 .. Name_Length);
      New_Buf : String (1 .. Name_Length);
   begin
      if not Get_Name (Old_Adr, Old_Buf) or else Old_Buf (1) = Character'Val (0) then
         return 16#FFFF_FFFF#;
      end if;
      if not Get_Name (New_Adr, New_Buf) or else New_Buf (1) = Character'Val (0) then
         return 16#FFFF_FFFF#;
      end if;
      declare
         Old_Name : constant String := Name_To_String (Old_Buf);
         New_Name : constant String := Name_To_String (New_Buf);
      begin
         Ada.Directories.Rename (Old_Name, New_Name);
      exception
         when others => return 16#FFFF_FFFF#;
      end;
      return 0;
   end Files_Rename;

   function FileDir_Enumerate_Begin return Unsigned_32 is
   begin
      RT.Searching := True;
      Ada.Directories.Start_Search (RT.Search, ".", "*");
      return 0;
   end FileDir_Enumerate_Begin;

   function FileDir_Enumerate_Next (Adr : Unsigned_32) return Unsigned_32 is
   begin
      Mem_Check_Range (Adr, Name_Length, "FileDir.EnumerateNext");
      if not RT.Searching then
         Mem_Write_Byte (Adr, 0);
         return 16#FFFF_FFFF#;
      end if;
      while Ada.Directories.More_Entries (RT.Search) loop
         declare
            Ent : Ada.Directories.Directory_Entry_Type;
         begin
            Ada.Directories.Get_Next_Entry (RT.Search, Ent);
            declare
               Nam : constant String := Ada.Directories.Simple_Name (Ent);
               Buf : String (1 .. Name_Length) := (others => Character'Val (0));
               Copy_Len : constant Natural := Natural'Min (Nam'Length, Name_Length);
            begin
            if Copy_Len > 0 then
               Buf (1 .. Copy_Len) := Nam (Nam'First .. Nam'First + Copy_Len - 1);
            end if;
            if Files_Check_Name (Buf) then
               for I in 0 .. Name_Length - 1 loop
                  RT.Mem (Adr + Unsigned_32 (I)) := Unsigned_8 (Character'Pos (Buf (Buf'First + I)));
               end loop;
               return 0;
            end if;
            end;
         end;
      end loop;
      RT.Searching := False;
      Mem_Write_Byte (Adr, 0);
      return 16#FFFF_FFFF#;
   end FileDir_Enumerate_Next;

   function FileDir_Enumerate_End return Unsigned_32 is
   begin
      if RT.Searching then
         Ada.Directories.End_Search (RT.Search);
      end if;
      RT.Searching := False;
      return 0;
   end FileDir_Enumerate_End;

   function Mem_To_String (Adr : Unsigned_32; Max_Len : Natural) return String is
      Len : Natural := 0;
   begin
      for I in 0 .. Max_Len - 1 loop
         exit when RT.Mem (Adr + Unsigned_32 (I)) = 0;
         Len := Len + 1;
      end loop;
      if Len = 0 then
         return "";
      end if;
      declare
         S : String (1 .. Len);
      begin
         for I in 0 .. Len - 1 loop
            S (I + 1) := Character'Val (Integer (RT.Mem (Adr + Unsigned_32 (I))));
         end loop;
         return S;
      end;
   end Mem_To_String;

   function Norebo_Halt (Code : Unsigned_32) return Unsigned_32 is
   begin
      Exit_With (Integer (Code and 16#FF#));
      return 0;
   end Norebo_Halt;

   function Norebo_Argv (Idx, Adr, Siz : Unsigned_32) return Unsigned_32 is
   begin
      Mem_Check_Range (Adr, Siz, "Norebo.Argv");
      if Idx < RT.Argc then
         declare
            Arg : constant String := Ada.Command_Line.Argument (Integer (Idx) + 1);
            Len : constant Unsigned_32 := Unsigned_32 (Arg'Length);
            Copy_Len : Natural := 0;
         begin
            if Siz > 0 then
               for I in 0 .. Integer (Siz) - 1 loop
                  RT.Mem (Adr + Unsigned_32 (I)) := 0;
               end loop;
               Copy_Len := Integer (Unsigned_32'Min (Len, Siz - 1));
               if Copy_Len > 0 then
                  for I in 0 .. Copy_Len - 1 loop
                     RT.Mem (Adr + Unsigned_32 (I)) :=
                       Unsigned_8 (Character'Pos (Arg (Arg'First + I)));
                  end loop;
               end if;
            end if;
            return Len;
         end;
      end if;
      return 16#FFFF_FFFF#;
   end Norebo_Argv;

   function Norebo_System (Adr, Len : Unsigned_32) return Unsigned_32 is
   begin
      if Len = 0 then
         return 0;
      end if;
      Mem_Check_Range (Adr, Len, "Norebo.System");
      declare
         Cmd : constant String := Mem_To_String (Adr, Natural (Len));
         C_Cmd : chars_ptr := New_String (Cmd);
         Res : Interfaces.C.int;
      begin
         Res := C_System (C_Cmd);
         Free (C_Cmd);
         return Unsigned_32 (Res);
      exception
         when others =>
            if C_Cmd /= Null_Ptr then
               Free (C_Cmd);
            end if;
            return 16#FFFF_FFFF#;
      end;
   end Norebo_System;

   function OS_Getenv (Env_Adr, Adr, Siz : Unsigned_32) return Unsigned_32 is
      Name : constant String := Mem_To_String (Env_Adr, Name_Length);
      Val  : constant String := Ada.Environment_Variables.Value (Name, "");
   begin
      Mem_Check_Range (Adr, Siz, "OS.Getenv");
      if Val'Length = 0 then
         return 0;
      end if;
      if Siz > 0 then
         for I in 0 .. Integer (Siz) - 1 loop
            RT.Mem (Adr + Unsigned_32 (I)) := 0;
         end loop;
         declare
            Copy_Len : Natural := Integer (Unsigned_32'Min (Unsigned_32 (Val'Length), Siz - 1));
         begin
            if Copy_Len > 0 then
               for I in 0 .. Copy_Len - 1 loop
                  RT.Mem (Adr + Unsigned_32 (I)) :=
                    Unsigned_8 (Character'Pos (Val (Val'First + I)));
               end loop;
            end if;
         end;
      end if;
      return 1;
   end OS_Getenv;

   function Trap_Message (Trap_Id : Unsigned_32) return String is
   begin
      case Trap_Id is
         when 1 => return "array index out of range";
         when 2 => return "type guard failure";
         when 3 => return "array or string copy overflow";
         when 4 => return "access via NIL pointer";
         when 5 => return "illegal procedure call";
         when 6 => return "integer division by zero";
         when 7 => return "assertion violated";
         when others => return "unknown trap";
      end case;
   end Trap_Message;

   function Norebo_Trap (Trap_Id, Name_Adr, Pos : Unsigned_32) return Unsigned_32 is
      Msg : constant String := Trap_Message (Trap_Id);
   begin
      declare
         Name_Str : constant String := Mem_To_String (Name_Adr, Name_Length);
      begin
         Ada.Text_IO.Put_Line (Msg & " at " & (if Name_Str'Length = 0 then "(unknown)" else Name_Str) &
           " pos " & Integer'Image (Integer (Pos)));
      end;
      Exit_With (Integer (100 + Trap_Id));
      return 0;
   end Norebo_Trap;

   function Sysreq_Exec (Nval : Unsigned_32) return Unsigned_32 is
      Arg0 : constant Unsigned_32 := RT.Sysarg (0);
      Arg1 : constant Unsigned_32 := RT.Sysarg (1);
      Arg2 : constant Unsigned_32 := RT.Sysarg (2);
   begin
      case Nval is
         when 1  => return Norebo_Halt (Arg0);
         when 2  => return RT.Argc;
         when 3  => return Norebo_Argv (Arg0, Arg1, Arg2);
         when 4  => return Norebo_Trap (Arg0, Arg1, Arg2);
         when 11 => return Files_New (Arg0);
         when 12 => return Files_Old (Arg0);
         when 13 => return Files_Register (Arg0);
         when 14 => return Files_Close (Arg0);
         when 15 => return Files_Seek (Arg0, Arg1);
         when 16 => return Files_Tell (Arg0);
         when 17 => return Files_Read (Arg0, Arg1, Arg2);
         when 18 => return Files_Write (Arg0, Arg1, Arg2);
         when 19 => return Files_Length (Arg0);
         when 20 => return Files_Date (Arg0);
         when 21 => return Files_Delete (Arg0);
        when 22 =>
           Ada.Text_IO.Put_Line ("Files.Purge not implemented");
           Exit_With (1);
           return 0;
         when 23 => return Files_Rename (Arg0, Arg1);
         when 31 => return FileDir_Enumerate_Begin;
         when 32 => return FileDir_Enumerate_Next (Arg0);
         when 33 => return FileDir_Enumerate_End;
         when 41 => return OS_Getenv (Arg0, Arg1, Arg2);
         when 42 => return Norebo_System (Arg0, Arg1);
        when others =>
           Ada.Text_IO.Put_Line ("Unimplemented sysreq");
           Exit_With (1);
           return 0;
      end case;
   end Sysreq_Exec;

   procedure Risc_Run is
      Ir : Unsigned_32;
      VM : Vm_State renames RT.VM;
   begin
      loop
         Ir := CPU_Read_Program (VM.PC);
         VM.PC := VM.PC + 1;

         if (Ir and Pbit) = 0 then
            declare
               A    : constant U4 := U4_From (ShiftR (Ir, 24));
               B    : constant U4 := U4_From (ShiftR (Ir, 20));
               Op   : constant Op_Code := Op_From (ShiftR (Ir, 16));
               Im   : constant Unsigned_32 := Ir and 16#FFFF#;
               Creg : constant U4 := U4_From (Ir);
               Bval : constant Unsigned_32 := VM.R (B);
               Cval : Unsigned_32;
               Aval : Unsigned_32 := 0;
            begin
               if (Ir and Qbit) = 0 then
                  Cval := VM.R (Creg);
               elsif (Ir and Vbit) = 0 then
                  Cval := Im;
               else
                  Cval := 16#FFFF_0000# or Im;
               end if;

               case Op is
                  when MOV =>
                     if (Ir and Ubit) = 0 then
                        Aval := Cval;
                     elsif (Ir and Qbit) /= 0 then
                        Aval := ShiftL (Cval, 16);
                     elsif (Ir and Vbit) /= 0 then
                        Aval := 16#D0# or
                          ShiftL (Bool_U32 (VM.Flags.N), 31) or
                          ShiftL (Bool_U32 (VM.Flags.Z), 30) or
                          ShiftL (Bool_U32 (VM.Flags.C), 29) or
                          ShiftL (Bool_U32 (VM.Flags.V), 28);
                     else
                        Aval := VM.H;
                     end if;
                  when LSL =>
                     Aval := ShiftL (Bval, Integer (Cval and 31));
                  when ASR =>
                     Aval := I32_To_U32 (ASR_I32 (I32 (U32_To_I32 (Bval)), Integer (Cval and 31)));
                  when ROR =>
                     declare
                        Sh  : constant Natural := Integer (Cval and 31);
                        Rsh : constant Natural := Integer ((32 - Unsigned_32 (Sh)) and 31);
                     begin
                        Aval := ShiftR (Bval, Sh) or ShiftL (Bval, Rsh);
                     end;
                  when ANDD =>
                     Aval := Bval and Cval;
                  when ANN =>
                     Aval := Bval and (not Cval);
                  when IOR =>
                     Aval := Bval or Cval;
                  when XORR =>
                     Aval := Bval xor Cval;
                  when ADD =>
                     declare
                        Tmp : Unsigned_32 := Bval + Cval;
                     begin
                        if (Ir and Ubit) /= 0 then
                           Tmp := Tmp + Bool_U32 (RT.VM.Flags.C);
                        end if;
                        VM.Flags.C := Tmp < Bval;
                        VM.Flags.V := (ShiftR ((Tmp xor Cval) and (Tmp xor Bval), 31) /= 0);
                        Aval := Tmp;
                     end;
                  when SUB =>
                     declare
                        Tmp : Unsigned_32 := Bval - Cval;
                     begin
                        if (Ir and Ubit) /= 0 then
                           Tmp := Tmp - Bool_U32 (RT.VM.Flags.C);
                        end if;
                        VM.Flags.C := Tmp > Bval;
                        VM.Flags.V := (ShiftR ((Bval xor Cval) and (Tmp xor Bval), 31) /= 0);
                        Aval := Tmp;
                     end;
                  when MUL =>
                     if (Ir and Ubit) = 0 then
                        declare
                           Tmp : I64 := I64 (U32_To_I32 (Bval)) * I64 (U32_To_I32 (Cval));
                           UT  : constant U64 := I64_To_U64 (Tmp);
                        begin
                           Aval := Unsigned_32 (UT and 16#FFFF_FFFF#);
                           VM.H := Unsigned_32 (Shift_Right (UT, 32));
                        end;
                     else
                        declare
                           Tmp : constant U64 := U64 (Bval) * U64 (Cval);
                        begin
                           Aval := Unsigned_32 (Tmp and 16#FFFF_FFFF#);
                           VM.H := Unsigned_32 (Shift_Right (Tmp, 32));
                        end;
                     end if;
                  when DIV =>
                     if U32_To_I32 (Cval) > 0 then
                        if (Ir and Ubit) = 0 then
                           declare
                              Bsi : constant I32 := U32_To_I32 (Bval);
                              Csi : constant I32 := U32_To_I32 (Cval);
                              Q   : I32 := Bsi / Csi;
                              R   : I32 := Bsi rem Csi;
                           begin
                              Aval := I32_To_U32 (Q);
                              VM.H := I32_To_U32 (R);
                              if U32_To_I32 (VM.H) < 0 then
                                 Aval := Aval - 1;
                                 VM.H := VM.H + Cval;
                              end if;
                           end;
                        else
                           Aval := Bval / Cval;
                           VM.H := Bval mod Cval;
                        end if;
                     else
                        declare
                           Q : constant Idiv_Result := Idiv (Bval, Cval, (Ir and Ubit) /= 0);
                        begin
                           Aval := Q.Quot;
                           VM.H := Q.Remainder;
                        end;
                     end if;
                  when FAD =>
                     Aval := FP_Add (Bval, Cval, (Ir and Ubit) /= 0, (Ir and Vbit) /= 0);
                  when FSB =>
                     Aval := FP_Add (Bval, Cval xor 16#8000_0000#, (Ir and Ubit) /= 0, (Ir and Vbit) /= 0);
                  when FML =>
                     Aval := FP_Mul (Bval, Cval);
                  when FDV =>
                     Aval := FP_Div (Bval, Cval);
               end case;

               Set_Register (A, Aval);
            end;
         elsif (Ir and Qbit) = 0 then
            declare
               A    : constant U4 := U4_From (ShiftR (Ir, 24));
               B    : constant U4 := U4_From (ShiftR (Ir, 20));
               Off  : constant I32 := Sign_Extend (Ir and 16#000F_FFFF#, 20);
               Addr : constant Unsigned_32 := VM.R (B) + I32_To_U32 (Off);
            begin
               if (Ir and Ubit) = 0 then
                  declare
                     Aval : Unsigned_32 := (if (Ir and Vbit) = 0 then CPU_Read_Word (Addr) else CPU_Read_Byte (Addr));
                  begin
                     Set_Register (A, Aval);
                  end;
               else
                  if (Ir and Vbit) = 0 then
                     CPU_Write_Word (Addr, VM.R (A));
                  else
                     CPU_Write_Byte (Addr, Unsigned_8 (VM.R (A) and 16#FF#));
                  end if;
               end if;
            end;
         else
            declare
               T : Boolean := (ShiftR (Ir, 27) and 1) /= 0;
            begin
               case Integer ((ShiftR (Ir, 24)) and 7) is
                  when 0 => T := T xor VM.Flags.N;
                  when 1 => T := T xor VM.Flags.Z;
                  when 2 => T := T xor VM.Flags.C;
                  when 3 => T := T xor VM.Flags.V;
                  when 4 => T := T xor (VM.Flags.C or VM.Flags.Z);
                  when 5 => T := T xor (VM.Flags.N xor VM.Flags.V);
                  when 6 => T := T xor ((VM.Flags.N xor VM.Flags.V) or VM.Flags.Z);
                  when 7 => T := T xor True;
                  when others => null;
               end case;
               if T then
                  if (Ir and Vbit) /= 0 then
                     Set_Register (15, VM.PC * 4);
                  end if;
                  if (Ir and Ubit) = 0 then
                     declare
                        Creg : constant U4 := U4_From (Ir);
                     begin
                        VM.PC := VM.R (Creg) / 4;
                     end;
                  else
                     declare
                        Off : constant I32 := Sign_Extend (Ir and 16#00FF_FFFF#, 24);
                     begin
                        VM.PC := VM.PC + I32_To_U32 (Off);
                     end;
                  end if;
               end if;
            end;
         end if;
      end loop;
   end Risc_Run;

   procedure Load_InnerCore is
      File : Ada.Streams.Stream_IO.File_Type;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (4));
      Last : Ada.Streams.Stream_Element_Offset;

      function Read_U32 return Unsigned_32 is
      begin
         Ada.Streams.Stream_IO.Read (File, Buffer, Last);
         if Last /= Ada.Streams.Stream_Element_Offset (4) then
            return 0;
         end if;
         return Le32_To_Host (
           Unsigned_8 (Buffer (1)),
           Unsigned_8 (Buffer (2)),
           Unsigned_8 (Buffer (3)),
           Unsigned_8 (Buffer (4)));
      end Read_U32;
   begin
      begin
         Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, InnerCore);
      exception
         when others =>
            declare
               Env  : constant String := Ada.Environment_Variables.Value (Path_Env, "");
               Path : constant String := Find_In_Path (Env, InnerCore);
            begin
               if Path'Length > 0 then
                  Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Path);
               else
                  raise;
               end if;
            end;
      end;
      declare
         Buffer : Ada.Streams.Stream_Element_Array (1 .. 4096);
      begin
         loop
            declare
               Siz : Unsigned_32 := Read_U32;
            begin
               exit when Siz = 0;
               declare
                  Adr : constant Unsigned_32 := Read_U32;
                  Remaining : Unsigned_32 := Siz;
                  Offset : Unsigned_32 := 0;
               begin
                  Mem_Check_Range (Adr, Siz, InnerCore);
                  while Remaining > 0 loop
                     declare
                        Chunk : constant Ada.Streams.Stream_Element_Offset :=
                          Ada.Streams.Stream_Element_Offset
                            (Natural'Min (Natural (Remaining), Buffer'Length));
                     begin
                        Ada.Streams.Stream_IO.Read (File, Buffer (1 .. Chunk), Last);
                        if Last = 0 then
                           Ada.Text_IO.Put_Line ("Error while reading " & InnerCore);
                           Exit_With (1);
                        end if;
                        for I in 1 .. Integer (Last) loop
                           RT.Mem (Adr + Offset + Unsigned_32 (I - 1)) :=
                             Unsigned_8 (Buffer (Ada.Streams.Stream_Element_Offset (I)));
                        end loop;
                        Remaining := Remaining - Unsigned_32 (Last);
                        Offset := Offset + Unsigned_32 (Last);
                     end;
                  end loop;
               end;
            end;
         end loop;
      end;
      Ada.Streams.Stream_IO.Close (File);
   exception
      when others =>
         Ada.Text_IO.Put_Line ("Can't load " & InnerCore);
         Exit_With (1);
   end Load_InnerCore;

begin
   RT.Argc := Unsigned_32 (Ada.Command_Line.Argument_Count);
   RT.Argv_Count := Ada.Command_Line.Argument_Count;

   Load_InnerCore;
   Mem_Write_Word (12, Mem_Bytes);
   Mem_Write_Word (24, Stack_Org);
   RT.VM.PC := 0;
   RT.VM.R (12) := 16#20#;
   RT.VM.R (14) := Stack_Org;

   Risc_Run;
end Zorebo;
