program Metrology2;

{$APPTYPE CONSOLE}

uses
  SysUtils;

const
  FileAddress = 'Analyzed.txt';

type
  TPointerToElementsInListOfLines = ^TListOfLinesRecord;

  TListOfLinesRecord = record
    LineText: string;
    NextLine: TPointerToElementsInListOfLines;
  end;

  TStruct = ^TStructRecord;

  TStructRecord = record
    CurrentStruct: (HEADER_MODE, STRUCT_MODE, PROCEDURE_MODE, OPERATOR_MODE, IF_ELSE_MODE, UNITTEST_MODE);
    Next: TStruct;
    Prev: TStruct;
  end;

  TSetOfTypes = array[1..25] of string;

const
  Types: TSetOfTypes = ('void', 'bool', 'byte', 'ubyte', 'string', 'short', 'ushort', 'int', 'uint', 'long', 'ulong', 'cent', 'ucent', 'float', 'double', 'real', 'ifloat', 'idouble', 'ireal', 'cfloat', 'cdouble', 'creal', 'char', 'wchar', 'dchar');
  NumberOfTypes = 25;

var
  AnalyzedFile: TextFile;
  PointerFirstInListOfLinesOfCode: TPointerToElementsInListOfLines;
  ExpectedStruct: (EXP_STRUCT, EXP_PROCEDURE, EXP_SWITCH, EXP_CASE, EXP_IF, EXP_ELSE, EXP_CYCLE, EXP_UNITTEST);
  NumberOfOperators, NumberOfConditionalOperators: Integer;
  MaximumNestingLevel, CurrentNestingLevel: Integer;
  NewConditionalOperatorFound, NewCompoundOperatorFound: Boolean;
  PointerLineOfCode: TPointerToElementsInListOfLines;
  Struct: TStruct;

function AddNewStruct(var Current: TStruct): TStruct;
var
  Temp: TStruct;
begin
  New(Temp);
  Current^.Next := Temp;
  Temp := Current;
  Current := Current^.Next;
  Current^.Prev := Temp;
  Current^.Next := nil;
  Result := Current;
end;

function DeleteStruct(var CurrentStruct: TStruct): TStruct;
begin
  CurrentStruct^.Prev^.Next := CurrentStruct^.Next;
  CurrentStruct := CurrentStruct^.Prev;
  Result := CurrentStruct;
end;

procedure IndicateNextStruct(var PreviousStruct: TStruct);
begin
  case (ExpectedStruct) of
    EXP_STRUCT:
      PreviousStruct^.Next^.CurrentStruct := STRUCT_MODE;
    EXP_PROCEDURE:
      PreviousStruct^.Next^.CurrentStruct := PROCEDURE_MODE;
    EXP_SWITCH, EXP_CYCLE, EXP_CASE:
      begin
        PreviousStruct^.Next^.CurrentStruct := OPERATOR_MODE;
        NewCompoundOperatorFound := True;
      end;
    EXP_IF:
      begin
        PreviousStruct^.Next^.CurrentStruct := IF_ELSE_MODE;
        NewConditionalOperatorFound := True;
        NewCompoundOperatorFound := True;
      end;
    EXP_ELSE:
      begin
        PreviousStruct^.Next^.CurrentStruct := IF_ELSE_MODE;
        NewConditionalOperatorFound := True;
        NewCompoundOperatorFound := True;
      end;
    EXP_UNITTEST:
      PreviousStruct^.Next^.CurrentStruct := UNITTEST_MODE;
  end;

end;

procedure IndicateStruct(const CurrentLine: String; var Struct: TStruct);
begin

  if (CurrentLine = '{') then
  begin
    Struct := AddNewStruct(Struct);
    IndicateNextStruct(Struct^.Prev);
  end
  else if (CurrentLine = '}') then
  begin
    if Struct^.CurrentStruct = IF_ELSE_MODE then
    begin
      Dec(CurrentNestingLevel);
    end;
    Struct := DeleteStruct(Struct);
  end;
end;

procedure FindProcedure(var CurrentLine: string);
var
  StartPosition, ConstantSetCounter, BreacketsPosition, Position: Integer;
  IsReturnTypePresent, AreBracketsPresent, FlagContinue: Boolean;
begin
  IsReturnTypePresent := False;
  AreBracketsPresent := False;

  BreacketsPosition := AnsiPos('(', CurrentLine);
  if BreacketsPosition <> 0 then
  begin
    AreBracketsPresent := True;
  end;

  ConstantSetCounter := 0;
  StartPosition := 0;
  FlagContinue := True;
  while ((ConstantSetCounter < NumberOfTypes) and (FlagContinue = true)) do
  begin
    Inc(ConstantSetCounter);
    Position := AnsiPos(Types[ConstantSetCounter], CurrentLine);
    if ((Position < BreacketsPosition) and (Position <> 0)) then
    begin
      StartPosition := Position;
      FlagContinue := false;
    end;
  end;
  if ((StartPosition = 1) and (CurrentLine[StartPosition + Length(Types[ConstantSetCounter])] = ' ')) then
  begin
    IsReturnTypePresent := True;
  end;

  if ((IsReturnTypePresent) and (AreBracketsPresent)) then
  begin
    ExpectedStruct := EXP_PROCEDURE;
  end;
end;

procedure IfElseOperator();
begin
  if ((ExpectedStruct = EXP_IF) or (ExpectedStruct = EXP_ELSE)) then
  begin
    if (AnsiPos('{', Trim(PointerLineOfCode^.NextLine^.LineText)) = 0) then
    begin
      Struct := AddNewStruct(Struct);
      Struct^.Prev^.Next^.CurrentStruct := IF_ELSE_MODE;
      Inc(CurrentNestingLevel);
      if (CurrentNestingLevel > MaximumNestingLevel) then
      begin
        MaximumNestingLevel := CurrentNestingLevel;
      end;
      if ((AnsiPos('?', Trim(PointerLineOfCode^.NextLine^.LineText)) = 0) and (AnsiPos('if', Trim(PointerLineOfCode^.NextLine^.LineText)) = 0)) then
      begin
        Dec(CurrentNestingLevel);
      end;  
      Struct := DeleteStruct(Struct);
    end
  end;
end;

procedure FindIfElseSwicthCaseUnittestStructCycle(CurrentLine: string);
type
  TSetOfKeyWords = array[1..9] of string;
const
  KeyWordArray: TSetOfKeyWords = ('unittest', 'struct', 'swicth', 'case', 'if', 'else', 'foreach', 'for', 'while');
var
  KeyWordCounter, FirstPosition: Integer;
begin
  CurrentLine := Trim(CurrentLine);
  for KeyWordCounter := 1 to 9 do
  begin
    FirstPosition := AnsiPos(KeyWordArray[KeyWordCounter], CurrentLine);
    if ((FirstPosition = 1) and ((FirstPosition + Length(KeyWordArray[KeyWordCounter]) -1 = Length(CurrentLine)) or (CurrentLine[FirstPosition + Length(KeyWordArray[KeyWordCounter])] = ' ') or (CurrentLine[FirstPosition + Length(KeyWordArray[KeyWordCounter])] = '('))) then
    begin
      if ((KeyWordArray[KeyWordCounter] <> 'unittest') and (KeyWordArray[KeyWordCounter] <> 'struct')) then
      begin
        Inc(NumberOfOperators);
      end;
      case (KeyWordCounter) of
        1:
          ExpectedStruct := EXP_UNITTEST;
        2:
          ExpectedStruct := EXP_STRUCT;
        3:
          ExpectedStruct := EXP_SWITCH;
        4:
          ExpectedStruct := EXP_CASE;
        5:
          begin
            ExpectedStruct := EXP_IF;
            Writeln(Trim(PointerLineOfCode^.LineText));
            Inc(NumberOfConditionalOperators);
            IfElseOperator();
          end;
        6:
          begin
            ExpectedStruct := EXP_ELSE;
            IfElseOperator();
          end;
        7, 8, 9:
          ExpectedStruct := EXP_CYCLE;
      end;
    end;
  end;
end;

procedure FindExpectedStruct(CurrentLine: string);
begin
  FindProcedure(CurrentLine);
  FindIfElseSwicthCaseUnittestStructCycle(CurrentLine);
end;

procedure FindAllOperators(CurrentLine: string);
type
  TSetOfOtherOperators = array[1..4] of string;

  TSetOfLogicalOperators = array[1..3] of string;

  TSetOfAssignmentOperators = array[1..11] of string;

  TSetOfBitwiseOperators = array[1..6] of string;

  TSetOfArithmeticOperators = array[1..7] of string;

  TSetOfRelationalOperators = array[1..7] of string;

  TTernaryOperationOperators = array[1..2] of string;
const
  OtherOpeators: TSetOfOtherOperators = (',', '.', 'sizeof', 'return');
  LogicalOperators: TSetOfLogicalOperators = ('&&', '||', '!');
  AssignmentOperators: TSetOfAssignmentOperators = ('-=', '+=', '/=', '*=', '%=', '<<=', '>>=', '|=', '^=', '&=', '=');
  BitwiseOperators: TSetOfBitwiseOperators = ('&', '|', '^', '~', '<<', '>>');
  ArithmeticOperators: TSetOfArithmeticOperators = ('++', '--', '+', '-', '*', '/', '%');
  RelationalOperators: TSetOfRelationalOperators = ('==', '!=', '>=', '<=', '>', '<', '~=');
  TernaryOperationComponents: TTernaryOperationOperators = ('?', ':');
  NumberOfOtherOpeators = 4;
  NumberOfLogicalOperators = 3;
  NumberOfAssignmentOperators = 11;
  NumberOfBitwiseOperators = 6;
  NumberOfArithmeticOperators = 7;
  NumberOfRelationalOperators = 7;
var
  CounterInOperatorList, FoundPosition: Integer;
  BufferLine: string;
  TernaryOperationComponent1, TernaryOperationComponent2: Boolean;
begin
  BufferLine := Trim(CurrentLine);
  FoundPosition := AnsiPos('..', BufferLine);
  if (FoundPosition <> 0) then
  begin
    Delete(BufferLine, FoundPosition, Length('..'));
  end;
  for CounterInOperatorList := 1 to NumberOfOtherOpeators do             //Other Operators
  begin
    FoundPosition := AnsiPos(OtherOpeators[CounterInOperatorList], BufferLine);
    while (FoundPosition <> 0) do
    begin
      if ((BufferLine[FoundPosition + 1] <> BufferLine[FoundPosition]) and (BufferLine[FoundPosition - 1] <> BufferLine[FoundPosition])) then
      begin
        Inc(NumberOfOperators);
        Delete(BufferLine, FoundPosition, Length(OtherOpeators[CounterInOperatorList]));
      end;
      FoundPosition := AnsiPos(OtherOpeators[CounterInOperatorList], BufferLine);
    end;
  end;
  for CounterInOperatorList := 1 to NumberOfLogicalOperators do         //Logical Operators
  begin
    FoundPosition := AnsiPos(LogicalOperators[CounterInOperatorList], BufferLine);
    if (BufferLine[FoundPosition + Length(LogicalOperators[CounterInOperatorList])] <> '=') then
    begin
      Inc(NumberOfOperators);
      Delete(BufferLine, FoundPosition, Length(LogicalOperators[CounterInOperatorList]));
    end;
  end;
  for CounterInOperatorList := 1 to NumberOfAssignmentOperators do       //Assignment Operators
  begin
    FoundPosition := AnsiPos(AssignmentOperators[CounterInOperatorList], BufferLine);
    if ((BufferLine[FoundPosition + Length(AssignmentOperators[CounterInOperatorList])] <> '=') and ((BufferLine[FoundPosition - 1] <> '='))) then
    begin
      Inc(NumberOfOperators);
      Delete(BufferLine, FoundPosition, Length(AssignmentOperators[CounterInOperatorList]));
    end;
  end;
  for CounterInOperatorList := 1 to NumberOfBitwiseOperators do         //Bitwise Operators
  begin
    FoundPosition := AnsiPos(BitwiseOperators[CounterInOperatorList], BufferLine);
    while (FoundPosition <> 0) do
    begin
      Inc(NumberOfOperators);
      Delete(BufferLine, FoundPosition, Length(BitwiseOperators[CounterInOperatorList]));
      FoundPosition := AnsiPos(BitwiseOperators[CounterInOperatorList], BufferLine);
    end;
  end;
  for CounterInOperatorList := 1 to NumberOfArithmeticOperators do         //Arithmetic Operators
  begin
    FoundPosition := AnsiPos(ArithmeticOperators[CounterInOperatorList], BufferLine);
    while (FoundPosition <> 0) do
    begin
      Inc(NumberOfOperators);
      Delete(BufferLine, FoundPosition, Length(ArithmeticOperators[CounterInOperatorList]));
      FoundPosition := AnsiPos(ArithmeticOperators[CounterInOperatorList], BufferLine);
    end;
  end;
  for CounterInOperatorList := 1 to NumberOfRelationalOperators do         //Relational Operators
  begin
    FoundPosition := AnsiPos(RelationalOperators[CounterInOperatorList], BufferLine);
    while (FoundPosition <> 0) do
    begin
      Inc(NumberOfOperators);
      Delete(BufferLine, FoundPosition, Length(RelationalOperators[CounterInOperatorList]));
      FoundPosition := AnsiPos(RelationalOperators[CounterInOperatorList], BufferLine);
    end;
  end;
  TernaryOperationComponent1 := False;      //Ternary Operators
  TernaryOperationComponent2 := False;
  FoundPosition := AnsiPos(TernaryOperationComponents[1], BufferLine);
  if (FoundPosition <> 0) then
  begin
    TernaryOperationComponent1 := True;
  end;
  FoundPosition := AnsiPos(TernaryOperationComponents[2], BufferLine);
  if (FoundPosition <> 0) then
  begin
    TernaryOperationComponent2 := True;
  end;
  if ((TernaryOperationComponent1) and (TernaryOperationComponent2)) then
  begin
    Writeln(Trim(PointerLineOfCode^.LineText));
    Inc(CurrentNestingLevel);
    If (CurrentNestingLevel > MaximumNestingLevel) then
    begin
       MaximumNestingLevel:= CurrentNestingLevel;
    end;
    Dec(CurrentNestingLevel);
    Inc(NumberOfConditionalOperators);
  end;
end;                                                             

procedure AnalyzeCodeOnOperators(const PointerFirstLineOfCode: TPointerToElementsInListOfLines);
var
  State: (ST_COMMENT, ST_STRING, ST_CODE);
  SymbolsCounter: Integer;
  BooleanTwoSlashes: Boolean;
  CodeLine: string;
begin
  New(Struct);
  Writeln('List of conditions:');
  Writeln;
  Struct^.CurrentStruct := HEADER_MODE;
  Struct^.Prev := nil;
  Struct^.Next := nil;
  PointerLineOfCode := PointerFirstLineOfCode^.NextLine;
  State := ST_CODE;
  NumberOfConditionalOperators := 0;
  MaximumNestingLevel := 0;
  CurrentNestingLevel := 0;
  NumberOfOperators := 0;
  NewConditionalOperatorFound := False;
  while (PointerLineOfCode <> nil) do
  begin
    SymbolsCounter := 1;
    BooleanTwoSlashes := false;
    CodeLine := '';
    while (SymbolsCounter <> (Length(PointerLineOfCode^.LineText) + 1)) do
    begin
      if (BooleanTwoSlashes = false) then
      begin
        case State of
          ST_COMMENT:
            begin
              if (((PointerLineOfCode^.LineText[SymbolsCounter] = '*') or (PointerLineOfCode^.LineText[SymbolsCounter] = '+')) and (PointerLineOfCode^.LineText[SymbolsCounter + 1] = '/')) then
              begin
                State := ST_CODE;
                Inc(SymbolsCounter);
              end
              else
                Inc(SymbolsCounter);
            end;
          ST_STRING:
            begin
              if (PointerLineOfCode^.LineText[SymbolsCounter] = '"') then
              begin
                State := ST_CODE;
                Inc(SymbolsCounter);
              end
              else
                Inc(SymbolsCounter);
            end;
          ST_CODE:
            begin
              if ((PointerLineOfCode^.LineText[SymbolsCounter] = '/') and ((PointerLineOfCode^.LineText[SymbolsCounter + 1] = '*') or (PointerLineOfCode^.LineText[SymbolsCounter] = '+'))) then
              begin
                State := ST_COMMENT;
                Inc(SymbolsCounter);
              end
              else if (PointerLineOfCode^.LineText[SymbolsCounter] = '"') then
              begin
                State := ST_STRING;
                Inc(SymbolsCounter);
              end
              else if ((PointerLineOfCode^.LineText[SymbolsCounter] = '/') and (PointerLineOfCode^.LineText[SymbolsCounter + 1] = '/')) then
              begin
                State := ST_CODE;
                BooleanTwoSlashes := true;
              end;

              if (State = ST_CODE) and (BooleanTwoSlashes = false) then
              begin
                CodeLine := CodeLine + PointerLineOfCode^.LineText[SymbolsCounter];
                Inc(SymbolsCounter);
              end;
            end;
        end;
      end
      else
        SymbolsCounter := (Length(PointerLineOfCode^.LineText) + 1);
    end;

    IndicateStruct(Trim(CodeLine), Struct);
    FindExpectedStruct(CodeLine);
    if ((Struct^.CurrentStruct <> HEADER_MODE) and (Trim(CodeLine) <> '')) then
    begin
      FindAllOperators(CodeLine);
    end;
    if (NewConditionalOperatorFound = True) then
    begin
      Inc(CurrentNestingLevel);
      NewConditionalOperatorFound := False;
      if MaximumNestingLevel < CurrentNestingLevel then
      begin
        MaximumNestingLevel := CurrentNestingLevel;
      end;
    end;
    if (((Struct^.CurrentStruct = IF_ELSE_MODE) or (Struct^.CurrentStruct = OPERATOR_MODE)) and (NewCompoundOperatorFound = True)) then
    begin
      Inc(NumberOfOperators);
      NewCompoundOperatorFound := False;
    end;
    PointerLineOfCode := PointerLineOfCode^.NextLine;
  end;
end;

function ReadFromFile(): TPointerToElementsInListOfLines;
var
  TempLine: string;
  PointerToNewLine, TempPointer: TPointerToElementsInListOfLines;
begin
  Assignfile(AnalyzedFile, FileAddress);
  Reset(AnalyzedFile);
  New(PointerToNewLine);
  Result := PointerToNewLine;
  while not EOf(AnalyzedFile) do
  begin
    TempPointer := PointerToNewLine;
    Readln(AnalyzedFile, TempLine);
    GetMem(PointerToNewLine, SizeOf(TempLine));
    PointerToNewLine^.LineText := TempLine;
    TempPointer^.NextLine := PointerToNewLine;
  end;
  PointerToNewLine^.NextLine := nil;
  CloseFile(AnalyzedFile);
end;

begin
  PointerFirstInListOfLinesOfCode := ReadFromFile();
  AnalyzeCodeOnOperators(PointerFirstInListOfLinesOfCode);
  Writeln('*************************************************************');
  Writeln('Absolute complexity of the program: ', NumberOfConditionalOperators);
  Writeln('Maximum nesting level: ',MaximumNestingLevel);
  Writeln('Relative complexity of the program: ', (NumberOfConditionalOperators / NumberOfOperators):0:6);
  Readln;
end.
