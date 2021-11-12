using System;
using System.Collections.Generic;
using System.Diagnostics;
using GrammlatorRuntime;

namespace grammlator;

// The terminal descriptions are analysed EvalDescription (handwritten code)
// The grammar driven implementation (method Analyze) is not yet tested or used

internal partial class P1aParser
{

   #region grammar
   //|
   //| // Grammlator settings
   //| TerminalSymbolEnum:            "E";
   //| InputExpression:               "Peek()";
   //| InputAcceptInstruction:        "Accepted = true;";
   //| ErrorHaltInstruction:          "return false;";
   //| GenerateFlagTestStartingLevel:  2;
   //| NameOfFlagTestMethod:          "_is";
   //|
   //| // Terminal definitions:
   enum E
   {
      Space = 1, Letter = 2, Digit = 4, QuotationMark = 8, OpenParenthesis = 16,
      CloseParenthesis = 32, Percent = 64, Comma = 128, Other = 512, EOL = 1024
   };

   #endregion grammar
   private Boolean Analyze(ReadOnlyMemory<char> description, int textPos)
   {
      int i = -1; // Index of the last accepted char
      Boolean Accepted = true;
      int NumberFirstPos = 0;
      int NumberLastPos = 0;
      int IdentifierFirstPos = 0;
      int IdentifierLastPos = 0;
      long Weight = GlobalSettings.TerminalDefaultWeight.Value;
      TerminalSymbol? Terminal = null;
      Boolean TerminalHasBeenDefined = false;
      List<ReadOnlyMemory<char>> AttributeTypes = new(20);
      List<ReadOnlyMemory<char>> AttributeIdentifiers = new(20);
      var _s = new Stack<int>(50); // using GrammlatorRuntime !!!

      E Peek()
      {
         if (Accepted)
            i++;
         if (i + 1 >= description.Length)
            return E.EOL;
         if (char.IsLetter(description.Span[i + 1]))
            return E.Letter;
         if (char.IsWhiteSpace(description.Span[i + 1]))
            return E.Space;
         if (description.Span[i + 1] >= '0' && description.Span[i + 1] <= '9')
            return E.Digit;
         return description.Span[i + 1] switch
         {
            '(' => E.OpenParenthesis,
            ')' => E.CloseParenthesis,
            '"' => E.QuotationMark,
            '%' => E.Percent,
            ',' => E.Comma,
            _ => E.Other
         };
      }

      #region grammar
      //|
      //| *= QuotationMark, Space*,
      //|    TerminalIdentifier, Space*,
      //|    OpenParenthesis, Space*,
      //|    Attributes?,
      //|    CloseParenthesis, Space*
      //|    OptionalWeight
      void AssignAttributes()
      {
         if (!TerminalHasBeenDefined)
         {
            var TypeStrings = new UnifiedString[AttributeTypes.Count];
            var NameStrings = new UnifiedString[AttributeIdentifiers.Count];
            for (int i = 0; i < AttributeTypes.Count; i++)
            {
               TypeStrings[i] = new UnifiedString(AttributeTypes[i]);
               NameStrings[i] = new UnifiedString(AttributeIdentifiers[i]);
            }
            Terminal!.AttributetypeStrings = TypeStrings;
            Terminal!.AttributenameStrings = NameStrings;
         }
      }

      //|
      //| TerminalIdentifier= Identifier
      void TerminalIdentifier()
      {

         Debug.Assert(false, "implementation not yet complete: value ... ?");


         ReadOnlyMemory<char> TerminalIdentifierMemory = description[IdentifierFirstPos..(IdentifierLastPos + 1)];
         UnifiedString TerminalString = new(TerminalIdentifierMemory);
         TerminalHasBeenDefined = SymbolDictionary.TryGetValue(TerminalString, out Symbol? s);
         if (TerminalHasBeenDefined)
            Terminal = (TerminalSymbol)s!;
         else
         {
            SymbolDictionary[TerminalString] =
               new TerminalSymbol(TerminalString, textPos, symbolNumber: SymbolDictionary.Count, enumValue: SymbolDictionary.Count)
               {
                  Weight = Weight
                     //,
                     //AttributetypeStringIndexList = TypeStringIndexes,
                     //AttributenameStringIndexList = NameStringIndexes
                  };
         }
      }

      //|
      //| Identifier=
      //|    Letter ??-1??
      void IdentifierFirstChar()
      {
         IdentifierFirstPos = i;
         IdentifierLastPos = i;
      }
      //|   | Identifier, (LetterOrDigit= Letter | Digit ) 
      void IdentifierNextChar() => IdentifierLastPos = i;
      //|
      //| 
      //|
      //| Digits=
      //|    Digit ??-2??
      void FirstDigit()
      {
         NumberFirstPos = i;
         NumberLastPos = i;
      }
      //|   | Digits, Digit
      void NextDigit() => NumberLastPos = i;

      //|
      //| Attributes=
      //|   Attribute, Space*
      //|   | Attributes, Comma, Space*, Attribute, Space*;
      //|
      //| Attribute=
      //|   AttributeType, Space*, Identifier
      void AddAtributeIdentifier()
         => AttributeIdentifiers.Add(description[IdentifierFirstPos..(IdentifierLastPos + 1)]);

      //|
      //| AttributeType= Identifier ??-10??
      void AddAttributeType()
         => AttributeTypes.Add(description[IdentifierFirstPos..(IdentifierLastPos + 1)]);

      //|
      //| OptionalWeight= /* empty */ ??-20??
      //| | Percent, Digits ??-21??
      void ReAssignWeight()
      {
         if (!long.TryParse(description[NumberFirstPos..(NumberLastPos + 1)].Span, out Terminal!.Weight))
            ;
      }
      //|
      #endregion grammar
      #region grammlator generated 26 Okt 2020 (grammlator file version/date 2020.10.18.0/26 Okt 2020)
      Int32 _StateStackInitialCount = _s.Count;
      Boolean _is(E flags) => ((Peek()) & flags) != 0;

      // State1:
      /* *Startsymbol= ►QuotationMark, Space*, TerminalIdentifier, Space*, OpenParenthesis, Space*, Attributes?, CloseParenthesis, Space*, OptionalWeight; */
      if (Peek() != E.QuotationMark)
         goto EndWithError;
      Debug.Assert(Peek() == E.QuotationMark);
   AcceptState3:
      Accepted = true;
      // State3:
      /* *Startsymbol= QuotationMark, Space*, ►TerminalIdentifier, Space*, OpenParenthesis, Space*, Attributes?, CloseParenthesis, Space*, OptionalWeight;
       * Space*= Space*, ►Space; */
      if (Peek() == E.Space)
         goto AcceptState3;
      if (Peek() != E.Letter)
         goto EndWithError;
      Debug.Assert(Peek() == E.Letter);
      Accepted = true;
      // Reduce1:
      /* Identifier= Letter;◄ */

      IdentifierFirstChar();

   State4:
      /* TerminalIdentifier= Identifier●;
       * Identifier= Identifier, ►LetterOrDigit; */
      if (_is(E.Space | E.OpenParenthesis))
      // Reduce2:
      {
         /* TerminalIdentifier= Identifier;◄ */

         TerminalIdentifier();

         goto State6;
      }
      if (Peek() >= E.QuotationMark)
         goto EndWithError;
      Debug.Assert(_is(E.Letter | E.Digit));
      Accepted = true;
      // Reduce3:
      /* Identifier= Identifier, LetterOrDigit;◄ */

      IdentifierNextChar();

      goto State4;

   Reduce5:
      /* Identifier= Letter;◄ */

      IdentifierFirstChar();

   State14:
      /* Identifier= Identifier, ►LetterOrDigit;
       * AttributeType= Identifier●; */
      if (Peek() == E.Space)
      // Reduce9:
      {
         /* AttributeType= Identifier;◄ */

         AddAttributeType();

         goto State10;
      }
      if (Peek() >= E.QuotationMark)
         goto EndWithError;
      Debug.Assert(_is(E.Letter | E.Digit));
      Accepted = true;
      // Reduce10:
      /* Identifier= Identifier, LetterOrDigit;◄ */

      IdentifierNextChar();

      goto State14;

   State6:
      /* *Startsymbol= QuotationMark, Space*, TerminalIdentifier, Space*, ►OpenParenthesis, Space*, Attributes?, CloseParenthesis, Space*, OptionalWeight;
       * Space*= Space*, ►Space; */
      if (Peek() == E.Space)
      {
         Accepted = true;
         goto State6;
      }
      if (Peek() != E.OpenParenthesis)
         goto EndWithError;
      Debug.Assert(Peek() == E.OpenParenthesis);
      Accepted = true;
   State8:
      /* *Startsymbol= QuotationMark, Space*, TerminalIdentifier, Space*, OpenParenthesis, Space*, ►Attributes?, CloseParenthesis, Space*, OptionalWeight;
       * Space*= Space*, ►Space; */
      _s.Push(0);
      if (Peek() == E.CloseParenthesis)
      // State15:
      {
         /* *Startsymbol= QuotationMark, Space*, TerminalIdentifier, Space*, OpenParenthesis, Space*, Attributes?, ►CloseParenthesis, Space*, OptionalWeight; */
         Debug.Assert(Peek() == E.CloseParenthesis);
         Debug.Assert(Peek() == E.CloseParenthesis);
         goto AcceptState17;
      }
      if (Peek() == E.Space)
      {
         Accepted = true;
         // Reduce4:
         /* sAdjust: -1
          * Space*= Space*, Space;◄ */
         _s.Pop();
         goto State8;
      }
      if (Peek() != E.Letter)
         goto EndWithError;
      Debug.Assert(Peek() == E.Letter);
      Accepted = true;
      goto Reduce5;

   State10:
      /* Space*= Space*, ►Space;
       * Attribute= AttributeType, Space*, ►Identifier; */
      if (Peek() == E.Space)
      {
         Accepted = true;
         goto State10;
      }
      if (Peek() != E.Letter)
         goto EndWithError;
      Debug.Assert(Peek() == E.Letter);
      Accepted = true;
      // Reduce6:
      /* Identifier= Letter;◄ */

      IdentifierFirstChar();

   State11:
      /* Identifier= Identifier, ►LetterOrDigit;
       * Attribute= AttributeType, Space*, Identifier●; */
      if (_is(E.Letter | E.Digit))
      {
         Accepted = true;
         // Reduce8:
         /* Identifier= Identifier, LetterOrDigit;◄ */

         IdentifierNextChar();

         goto State11;
      }
      if (!_is(E.Space | E.CloseParenthesis | E.Comma))
         goto EndWithError;
      Debug.Assert(_is(E.Space | E.CloseParenthesis | E.Comma));
      // Reduce7:
      /* Attribute= AttributeType, Space*, Identifier;◄ */

      AddAtributeIdentifier();

      // Branch1:
      if (_s.Peek() == 0)
         goto State13;
      State24:
      /* Space*= Space*, ►Space;
       * Attributes= Attributes, Comma, Space*, Attribute, Space*●; */
      if (Peek() == E.Space)
      {
         Accepted = true;
         goto State24;
      }
      if (!_is(E.CloseParenthesis | E.Comma))
         goto EndWithError;
      Debug.Assert(_is(E.CloseParenthesis | E.Comma));
      // Reduce16:
      /* sAdjust: -1
       * Attributes= Attributes, Comma, Space*, Attribute, Space*;◄ */
      _s.Pop();
   State20:
      /* *Startsymbol= QuotationMark, Space*, TerminalIdentifier, Space*, OpenParenthesis, Space*, Attributes?, ►CloseParenthesis, Space*, OptionalWeight;
       * Attributes= Attributes, ►Comma, Space*, Attribute, Space*; */
      Debug.Assert(_is(E.CloseParenthesis | E.Comma));
      if (Peek() == E.Comma)
      {
         Accepted = true;
         goto State22;
      }
      Debug.Assert(Peek() == E.CloseParenthesis);
   AcceptState17:
      Accepted = true;
      // State17:
      /* *Startsymbol= QuotationMark, Space*, TerminalIdentifier, Space*, OpenParenthesis, Space*, Attributes?, CloseParenthesis, Space*, ►OptionalWeight;
       * Space*= Space*, ►Space; */
      if (Peek() == E.Space)
         goto AcceptState17;
      if (Peek() == E.Percent)
      {
         Accepted = true;
         // State18:
         /* OptionalWeight= Percent, ►Digits; */
         if (Peek() != E.Digit)
            goto EndWithError;
         Debug.Assert(Peek() == E.Digit);
         Accepted = true;
         // Reduce12:
         /* Digits= Digit;◄ */

         FirstDigit();

         goto State19;
      }
      Debug.Assert(!_is(E.Space | E.Percent));
   Reduce11:
      /* sAdjust: -1
       * *Startsymbol= QuotationMark, Space*, TerminalIdentifier, Space*, OpenParenthesis, Space*, Attributes?, CloseParenthesis, Space*, OptionalWeight;◄ */
      _s.Pop();

      AssignAttributes();

      goto EndOfGeneratedCode;

   State13:
      /* Space*= Space*, ►Space;
       * Attributes= Attribute, Space*●; */
      if (Peek() == E.Space)
      {
         Accepted = true;
         goto State13;
      }
      if (!_is(E.CloseParenthesis | E.Comma))
         goto EndWithError;
      Debug.Assert(_is(E.CloseParenthesis | E.Comma));
      goto State20;

   State19:
      /* OptionalWeight= Percent, Digits●;
       * Digits= Digits, ►Digit; */
      if (Peek() != E.Digit)
      // Reduce13:
      {
         /* OptionalWeight= Percent, Digits;◄ */

         ReAssignWeight();

         goto Reduce11;
      }
      Debug.Assert(Peek() == E.Digit);
      Accepted = true;
      // Reduce14:
      /* Digits= Digits, Digit;◄ */

      NextDigit();

      goto State19;

   State22:
      /* Space*= Space*, ►Space;
       * Attributes= Attributes, Comma, Space*, ►Attribute, Space*; */// *Push(1)
      if (Peek() >= E.Digit)
      // PushState2:
      {
         _s.Push(1);
         goto EndWithError;
      }
      if (Peek() == E.Space)
      {
         Accepted = true;
         // Reduce15:
         /* Space*= Space*, Space;◄ */
         goto State22;
      }
      Debug.Assert(Peek() == E.Letter);
      Accepted = true;
      // PushState1:
      _s.Push(1);
      goto Reduce5;

   EndWithError:
      // This point is reached after an input error has been found
      _s.Remove(_s.Count - _StateStackInitialCount);
      return false;

   EndOfGeneratedCode:
      ;

      #endregion grammlator generated 26 Okt 2020 (grammlator file version/date 2020.10.18.0/26 Okt 2020)
      return true;
   }

   private Boolean EvalDescription(UnifiedString enumElementUString, ReadOnlySpan<Char> Description, List<String> ArgumentTypes, List<String> ArgumentNames, Int64 enumElementValue)
   {
      // expect "EnumElementIdentifier" "(" typeIdentifier  argumentidentifier , ...")"
      String EnumElementIdentifier = enumElementUString.ToString();
      Int32 Position = 0;

      if (!(Description[Position++] == '"'))
         return false;
      if (Description.Length < 2 || !(Description[^1] == '"')) // "" is used as end marker => do not need to check Position>=description.length
         return false;
      Description.SkipWhiteSpace(ref Position);
      if (!Description.StartsWithAndSkip(ref Position, EnumElementIdentifier))
         return false;

      Description.SkipWhiteSpace(ref Position);
      if (!(Description[Position++] == '('))
         return false;

      Boolean Is1st = true;
      Description.SkipWhiteSpace(ref Position);

      while (Description[Position] != ')')
      {
         if (!Is1st)
         {
            if (Description[Position++] != ',')
               return false;
            Description.SkipWhiteSpace(ref Position);
         }
         Is1st = false;
         if (!Description.IsIdentifier(ref Position, out ReadOnlySpan<char> ArgumentType))
            return false;
         Description.SkipWhiteSpace(ref Position);

         if (!Description.IsIdentifier(ref Position, out ReadOnlySpan<char> ArgumentName))
            return false;

         ArgumentTypes.Add(ArgumentType.ToString());
         ArgumentNames.Add(ArgumentName.ToString());

         Description.SkipWhiteSpace(ref Position);
      }

      Position++; // Skip ')'
      Description.SkipWhiteSpace(ref Position);

      // *************************
      TerminalSymbol t;
      if (SymbolDictionary.TryGetValue(enumElementUString, out Symbol? s))
      {
         t = (s as TerminalSymbol)!;
         for (int i = 0; i < ArgumentTypes.Count; i++)
            if (t.AttributetypeStrings[i].ToString() != ArgumentTypes[i])
            {
               String TypeIofT = t.AttributetypeStrings[i].ToString();
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
@$"Error in enum: the terminal symbol {EnumElementIdentifier} has already been defined with a different argumenttype {TypeIofT} / {ArgumentTypes[i]} ");
            }
      }
      else
      {
         var TypeStringIndexes = new UnifiedString[ArgumentTypes.Count];
         var NameStringIndexes = new UnifiedString[ArgumentNames.Count];
         for (int i = 0; i < ArgumentTypes.Count; i++)
         {
            TypeStringIndexes[i] = new UnifiedString(ArgumentTypes[i]);
            NameStringIndexes[i] = new UnifiedString(ArgumentNames[i]);
         }
         SymbolDictionary[enumElementUString] =
            t = new TerminalSymbol(enumElementUString, Lexer.LexerTextPos, symbolNumber: SymbolDictionary.Count, enumValue: enumElementValue)
            {
               Weight = GlobalSettings.TerminalDefaultWeight.Value,
               AttributetypeStrings = TypeStringIndexes,
               AttributenameStrings = NameStringIndexes
            };
      }

      // Optional weight
      Int64 Weight = GlobalSettings.TerminalDefaultWeight.Value;
      if (Description.IsCharacter(ref Position, '%'))
      {
         Description.SkipWhiteSpace(ref Position);
         Description.IsInt64(ref Position, out t.Weight);
      }

      return true;
   }

}
