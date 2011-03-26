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
   (* Error codes are sequential to be used as array indices. *)
   errParsing = 0;
   errReading = 1;
   errCycle   = 2;
   errOutOfRange = 3;
   errEmpty = 4;
   errStringOp = 5;
   errRefError = 6;
   errDivByZero = 7;
   NumErrors = 8; (* Total number of error codes *)

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
      code: INTEGER; (* Error code *)
   END;

VAR
   (* Global error identifiers *)
   errors: ARRAY NumErrors OF ErrorCell;
   errorTexts: ARRAY NumErrors OF ARRAY 12 OF CHAR;

PROCEDURE MakeErrorCell (errorCode: INTEGER): ErrorCell;
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
   IF errors [errorCode] = NIL THEN
      NEW (errors [errorCode]);
      errors [errorCode].code := errorCode;
   END;
   RETURN errors [errorCode]
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

PROCEDURE StrToInt (VAR str: ARRAY OF CHAR; VAR i: LONGINT; VAR value: LONGINT): BOOLEAN;
(* Convert 'str [i...]' to 'value' >= 0, return TRUE on success. *)
CONST
   MaxLength = 10; (* bound by the LONGINT type *)
VAR
   maxIndex, power10: LONGINT;
BEGIN
   WHILE str [i] = '0' DO (* skip leading zeroes *)
      INC (i);
   END;
   maxIndex := i + MaxLength;
   value := 0;
   power10 := 1;
   WHILE (i < maxIndex) & ('0' <= str [i]) & (str [i] <= '9') DO
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
   expressionCell: ExpressionCell;
   stringCell: StringCell;
   valueCell: ValueCell;
   pstr: PStr;
   value, len, i: LONGINT;
   res: Cell;
BEGIN
   CASE str [0] OF
   | '0'..'9': (* value cell *)
      i := 0;
      IF StrToInt (str, i, value) THEN
         NEW (valueCell);
         valueCell.value := value;
         res := valueCell;
      ELSE
         res := MakeErrorCell (errParsing);
      END;
   | '=': (* expression cell *)
      len := Length (str); (* no +1, since we are reducing the length by 1 *)
      NEW (pstr, len);
      SYSTEM.MOVE (SYSTEM.ADR (str [1]), SYSTEM.ADR (pstr^ [0]), len);
      NEW (expressionCell);
      expressionCell.marked := FALSE;
      expressionCell.expression := pstr;
      res := expressionCell;
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

   PROCEDURE SkipEol;
   VAR
      ch: CHAR;
   BEGIN
      In.Char (ch);
   END SkipEol;

   PROCEDURE ReadCellStr (VAR str: ARRAY OF CHAR): BOOLEAN;
   (* Read data from In module upto the next Tab or EOL character. Return TRUE
    * on success, or FALSE if input is too long for the 'str' buffer. *)
   VAR
      ch: CHAR;
      i: LONGINT;
   BEGIN
      i := 0;
      In.Char (ch);
      WHILE In.Done & (ch >= ' ') & (i < LEN (str) - 1) DO
         str [i] := ch;
         INC (i);
         In.Char (ch);
      END;
      str [i] := 0X;
      RETURN (i > 0) OR In.Done & (ch < ' ')
   END ReadCellStr;

BEGIN
   In.Open;
   In.LongInt (height);
   ASSERT (In.Done, 20);
   ASSERT ((1 <= height) & (height <= MaxHeight), 21);
   In.LongInt (width);
   ASSERT (In.Done, 22);
   ASSERT ((1 <= width) & (width <= MaxWidth), 23);
   SkipEol;
   NEW (res, width, height);
   h := 0;
   WHILE h < height DO
      w := 0;
      WHILE w < width DO
         IF ReadCellStr (str) THEN
            res [w, h] := StrToCell (str);
         ELSE
            res [w, h] := MakeErrorCell (errReading);
         END;
         INC (w);
      END;
      INC (h);
   END;
   RETURN res
END LoadTable;

PROCEDURE OutputTable (table: Table);
(* Output contents of the 'table' using the Out module. *)
CONST
   Tab = 09X;
VAR
   w, h: LONGINT;

   PROCEDURE OutputCell (cell: Cell);
   BEGIN
      IF cell # NIL THEN
         WITH cell: StringCell DO
            Out.String (cell.string^);
         | cell: ValueCell DO
            Out.Int (cell.value, 0);
         | cell: ErrorCell DO
            Out.String (errorTexts [cell.code]);
         END;
      END;
   END OutputCell;

BEGIN (* OutputTable *)
   Out.Open;
   h := 0;
   WHILE h < LEN (table^, 1) DO
      w := 0;
      WHILE w < LEN (table^, 0) - 1 DO
         OutputCell (table [w, h]);
         Out.Char (Tab);
         INC (w);
      END;
      OutputCell (table [w, h]);
      Out.Ln;
      INC (h);
   END;
END OutputTable;

PROCEDURE CalculateTable (table: Table);
(* Calculate all expressions in the table replacing ExpressionCells with
 * ValueCells or ErrorCells. Cells being used in evaluation are marked using
 * the 'mark' flag (initially all flags are FALSE), this is used to detect
 * circular references. *)
VAR
   w, h: LONGINT;
   cell: Cell;

   PROCEDURE ^ CalcCell (cell: ExpressionCell): Cell;

   PROCEDURE CalcExpression (VAR str: ARRAY OF CHAR): Cell;
   (* Calculate the expression in 'str' and return the result either as a
    * ValueCell on success, or one of the global ErrorCell instances on
    * error. *)
   CONST
      opAssign = 0;
      opPlus = 1;
      opMinus = 2;
      opDivide = 3;
      opMultiply = 4;
   VAR
      valueCell: ValueCell;
      i, w, h, value, integer, operation: LONGINT;
      res, cell: Cell;

      PROCEDURE DoOperation (VAR value: LONGINT; operation, operand: LONGINT): BOOLEAN;
      (* Perform operation (one of opXXX constants) and put result in 'value'.
       * 'value' initially contains left operand, 'operand' contains right
       * operand. Return FALSE on division by zero error. *)
      VAR
         res: BOOLEAN;
      BEGIN
         res := TRUE;
         CASE operation OF
         | opAssign: value := operand;
         | opPlus: value := value + operand;
         | opMinus: value := value - operand;
         | opDivide:
            IF operand = 0 THEN
               res := FALSE;
            ELSE
               value := value DIV operand;
            END;
         | opMultiply: value := value * operand;
         END;
         RETURN res
      END DoOperation;

   BEGIN
      res := NIL;
      value := 0;
      operation := opAssign;
      i := 0;
      WHILE res = NIL DO
         CASE str [i] OF
         | 0X:
            res := MakeErrorCell (errParsing);
         | '0'..'9':
            IF StrToInt (str, i, integer) OR ~(('0' <= str [i]) & (str [i] <= '9')) THEN
               IF ~DoOperation (value, operation, integer) THEN
                  res := MakeErrorCell (errDivByZero);
               END;
            END;
         | 'a'..'z', 'A'..'Z':
            w := ORD (CAP (str [i])) - ORD ('A');
            INC (i);
            IF w < LEN (table^, 0) THEN
               IF ('1' <= str [i]) & (str [i] <= '9') THEN
                  h := ORD (str [i]) - ORD ('1');
                  INC (i);
                  IF h < LEN (table^, 1) THEN
                     cell := table [w, h];
                     IF cell # NIL THEN
                        IF cell IS ExpressionCell THEN
                           cell := CalcCell (cell (ExpressionCell));
                        END;
                        WITH cell: ValueCell DO
                           IF ~DoOperation (value, operation, cell.value) THEN
                              res := MakeErrorCell (errDivByZero);
                           END;
                        | cell: StringCell DO
                           res := MakeErrorCell (errStringOp);
                        | cell: ErrorCell DO
                           IF cell.code = errCycle THEN
                              res := cell
                           ELSE
                              res := MakeErrorCell (errRefError);
                           END;
                        END;
                     ELSE
                        res := MakeErrorCell (errEmpty);
                     END;
                  ELSE
                     res := MakeErrorCell (errOutOfRange);
                  END;
               ELSE
                  res := MakeErrorCell (errParsing);
               END;
            ELSE
               res := MakeErrorCell (errOutOfRange);
            END;
         ELSE
            res := MakeErrorCell (errParsing);
         END;
         IF res = NIL THEN
            CASE str [i] OF
            | 0X: (* end of expression = success *)
               NEW (valueCell);
               valueCell.value := value;
               res := valueCell;
            | '+': operation := opPlus;
            | '-': operation := opMinus;
            | '/': operation := opDivide;
            | '*': operation := opMultiply;
            ELSE
               res := MakeErrorCell (errParsing);
            END;
            INC (i);
         END;
      END;
      RETURN res
   END CalcExpression;

   PROCEDURE CalcCell (cell: ExpressionCell): Cell;
   VAR
      res: Cell;
   BEGIN
      IF cell.marked THEN
         res := MakeErrorCell (errCycle);
      ELSE
         cell.marked := TRUE;
         res := CalcExpression (cell.expression^);
         cell.marked := FALSE;
      END;
      RETURN res
   END CalcCell;

BEGIN
   h := 0;
   WHILE h < LEN (table^, 1) DO
      w := 0;
      WHILE w < LEN (table^, 0) DO
         cell := table [w, h];
         IF (cell # NIL) & (cell IS ExpressionCell) THEN
            table [w, h] := CalcCell (cell (ExpressionCell));
         END;
         INC (w);
      END;
      INC (h);
   END;
END CalculateTable;

PROCEDURE Do;
VAR
   table: Table;
BEGIN
   table := LoadTable ();
   CalculateTable (table);
   OutputTable (table);
END Do;

PROCEDURE Init;
VAR
   i: INTEGER;
BEGIN
   FOR i := 0 TO LEN (errorTexts) - 1 DO
      errorTexts [i] := '';
   END;
   errorTexts [errParsing   ] := '#Parsing';
   errorTexts [errReading   ] := '#Reading';
   errorTexts [errCycle     ] := '#Cycle';
   errorTexts [errOutOfRange] := '#OutOfRange';
   errorTexts [errEmpty     ] := '#Empty';
   errorTexts [errStringOp  ] := '#StringOp';
   errorTexts [errRefError  ] := '#RefErr';
   errorTexts [errDivByZero ] := '#DivByZero';
   FOR i := 0 TO LEN (errorTexts) - 1 DO
      ASSERT (errorTexts [i] # '', 100);
   END;
END Init;

BEGIN
   Init;
   Do;
END SmallExcel.
