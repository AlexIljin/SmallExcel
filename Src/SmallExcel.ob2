<*+main*>
MODULE SmallExcel;

(* ------------------------------------------------------------------------
 * (C) 2011 by Alexander Iljin
 * ------------------------------------------------------------------------ *)

IMPORT
   SYSTEM, In, Out;

(* SmallExcel is a program that reads a table from StdIn, calculates it on the
 * fly and outputs it to StdOut.
 * Note: can't dump data directly to output and forget it: later we can find a
 * link to a previously discarded cell. How about two-pass work: 1st pass
 * gathers all link data and remembers error positions. Possible errors:
 * - can't calculate due to string reference (#string);
 * - circular reference (#loop);
 * - division by zero (#zero);
 * - empty cell or contains an error (#empty).
 * Links can be backward, forward and self (#loop error). If we know all the
 * links, then we can output data on the second stage as soon as all the
 * preconditions for a next cell is read from the input stream.

 Sample data:
6	4
12	=C2	3	'Sample
=A1+B1*C1/5	=A2*B1	=B3-C3	'Spread
'Test	=4-3	5	'Sheet
""	=A9	=1/0	=A5
=B5	=1+A5+1	=5A	=A1++A1
=1+	x	=A5

12	-4	3	Sample
4	-16	-4	Spread
Test	1	5	Sheet
#Parse	#Cell	#Inf	#Cycle
#Cycle	#Cycle	#NaN	#StrOp on Numbers
#Expr	#Parse	#Cycle
 *)

CONST
   MaxCellDataLength = 256; (* Max length of a cell input text *)

TYPE
   (* Abstract cell of the table *)
   CellDesc = RECORD
   END;
   Cell = POINTER TO CellDesc;

   (* Table is a Width by Height array of Cells *)
   Table = POINTER TO ARRAY OF ARRAY OF Cell;

   (* Common type for dynamic-length strings *)
   PStr = POINTER TO ARRAY OF CHAR;

   (* Literal string of characters *)
   StringCell = POINTER TO RECORD (CellDesc)
      string: PStr;
   END;

   (* Cell with an integer value *)
   ValueCell = POINTER TO RECORD (CellDesc)
      value: LONGINT;
   END;

   (* Cell with an expression to be calculated *)
   ExpressionCell = POINTER TO RECORD (CellDesc)
      marked: BOOLEAN; (* used for expression loop detection *)
      expression: PStr;
   END;

   (* Cell with an error *)
   ErrorCell = POINTER TO RECORD (CellDesc)
   END;

VAR
   (* Global error identifiers *)
   errParsing: ErrorCell;

PROCEDURE MakeErrorCell (VAR error: ErrorCell): ErrorCell;
(* Error cells are all marked by the same special 'ErrorCell' type. To
 * distinguish between errors there are global variables, which play the role
 * of error codes. E.g. to check if a cell error is a parsing error, you
 * should compare the cell value against the 'errParsing' global variable.
 * This way we can use a fixed number of ErrorCell instances, not more than
 * the number of error types we actually encountered in a given table. But we
 * can't just assign 'errParsing' to a table cell, because we don't know if
 * 'errParsing' was ever initialized or if it is = NIL (which would mean it is
 * a valid empty cell). So, we must assign table[w,h] := MakeErrorCell and
 * pass it one of global variable instances, e.g. 'errParsing'. *)
BEGIN
   IF error = NIL THEN
      NEW (error);
   END;
   RETURN error
END MakeErrorCell;

PROCEDURE Length (VAR str: ARRAY OF CHAR): LONGINT;
(* Return length of a string in the 'str' array, excluding 0X terminator. *)
VAR
   res: LONGINT;
BEGIN
   res := 0;
   WHILE str [res] # 0X DO
      INC (res);
   END;
   RETURN res
END Length;

PROCEDURE StrToInt (VAR str: ARRAY OF CHAR; VAR value: LONGINT): BOOLEAN;
(* Convert 'str' to 'value' >= 0, return TRUE on success. *)
CONST
   MaxLength = 10; (* bound by the LONGINT type *)
VAR
   res: BOOLEAN;
   i, power10: LONGINT;
BEGIN
   value := 0;
   i := 0;
   power10 := 1;
   WHILE (i < MaxLength) & ('0' <= str [i]) & (str [i] <= '9') DO
      value := value * power10 + ORD (str [i]) - ORD ('0');
      power10 := power10 * 10;
      INC (i);
   END;
   RETURN str [i] = 0X
END StrToInt;

PROCEDURE StrToCell (VAR str: ARRAY OF CHAR): Cell;
(* Interpret the contents of 'str' and return the appropriate Cell object.
 * This procedure is used for initial filling of the table, 'str' is read from
 * the input stream. If 'str' is empty, return NIL. *)
VAR
   stringCell: StringCell;
   valueCell: ValueCell;
   pstr: PStr;
   value, len: LONGINT;
   res: Cell;
BEGIN
   CASE str [0] OF
   | '0'..'9': (* value cell *)
      IF StrToInt (str, value) THEN
         NEW (valueCell);
         valueCell.value := value;
         res := valueCell;
      ELSE
         res := MakeErrorCell (errParsing);
      END;
   | '=': (* expression cell *)
   | "'": (* string cell *)
      len := Length (str); (* no +1, since we are reducing the length by 1 *)
      NEW (pstr, len);
      SYSTEM.MOVE (SYSTEM.ADR (str [1]), SYSTEM.ADR (pstr^ [0]), len);
      NEW (stringCell);
      stringCell.string := pstr;
      res := stringCell;
   | 0X: (* empty cell *)
      res := NIL;
   ELSE (* parsing error *)
      res := MakeErrorCell (errParsing);
   END;
   RETURN res
END StrToCell;

PROCEDURE LoadTable (): Table;
(* Read table from module In into the global 'table'. *)
CONST
   MaxWidth = ORD('Z') - ORD ('A') + 1;
   MaxHeight = ORD('9') - ORD ('0') + 1;
VAR
   width, height, w, h: LONGINT;
   str: ARRAY MaxCellDataLength OF CHAR;
   res: Table;
BEGIN
   In.Open;
   In.Int (height);
   ASSERT (In.Done, 20);
   ASSERT ((1 <= height) & (height <= MaxHeight), 21);
   In.Int (width);
   ASSERT (In.Done, 22);
   ASSERT ((1 <= width) & (width <= MaxWidth), 23);
   NEW (res, width, height);
   h := 0;
   WHILE h < height DO
      w := 0;
      WHILE w < width DO
         In.String (str);
         res [w, h] := StrToCell (str);
         INC (w);
      END;
      INC (h);
   END;
   RETURN res
END LoadTable;

PROCEDURE Do;
VAR
   table: Table;
BEGIN
   table := LoadTable ();
END Do;

BEGIN
   Do;
END SmallExcel.
