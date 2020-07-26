using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;

namespace Grammlator {
   internal sealed partial class P5GenerateCode {
      /// <summary>
      /// list of blocks of symbols inside TerminalSymbols to be reused in <see cref="P5GenerateCode"/>
      /// </summary>
      private readonly List<BlockOfEqualBits> blockList = new List<BlockOfEqualBits>(GlobalVariables.NumberOfTerminalSymbols);

      private void GenerateOneConditionalAction(ConditionalAction a, BitArray relevantSymbols, Int32 nestingLevel)
         {
         codegen.IndentExactly();
         codegen.AppendWithOptionalLinebreak("if (");

         codegen.IncrementIndentationLevel();
         // codegen.Indent(nestingLevel + 1);
         GenerateCondition(a.TerminalSymbols, relevantSymbols);
         codegen.AppendWithOptionalLinebreak(") ");

         GenerateCodeSequence(a, labelMustBeGenerated: false, nestingLevel + 1);

         // All elements (value==true) in conditions of remaining actions remain relevant.
         // Only bits representing not allowed elements may become irrelevant.
         relevantSymbols.ExceptWith(a.TerminalSymbols);

         codegen.DecrementIndentationLevel();
         }

      private void GenerateConditionAsComment(BitArray Condition)
      {
         codegen.IndentExactly();
         codegen.AppendWithOptionalLinebreak("Debug.Assert(");
         codegen.IncrementIndentationLevel();
         // codegen.Indent(nestingLevel + 1);
         // force complete check, e.g. /* MyCharacterInput.Symbol >= CharGroupEnum.Digit */
         /* MyCharacterInput.Symbol == CharGroupEnum.Digit || MyCharacterInput.Symbol == CharGroupEnum.Letter */
         GenerateCondition(Condition, GlobalVariables.AllTerminalSymbols);

         // The restriction on relevant symbols would produce a optimized condition resulting on preceding checks, e.g.
         /* MyCharacterInput.Symbol >= CharGroupEnum.Digit */
         // GenerateCondition(Condition, RelevantSymbols); 

         codegen.AppendWithOptionalLinebreak(");");
         codegen.DecrementIndentationLevel();
         }

      internal struct BlockOfEqualBits {
         /// <summary>
         /// false if the relevant bits of the block are false, true if the relevant bits are true
         /// </summary>
         internal Boolean blockType;

         /// <summary>
         /// blockStart is the index of the first bit of the block
         /// </summary>
         internal Int32 blockStart;

         /// <summary>
         /// blockLength is the number of relevant bits
         /// </summary>
         internal Int32 blockLength;

         /// <summary>
         /// blockEnd is the indexc of the last bit of the block
         /// </summary>
         internal Int32 blockEnd;
         }

      private void GenerateCondition(BitArray Condition, BitArray Relevant)
         {
         // In the worst case all symbols are relevant and the condition changes at each index
         // so that each symbol needs an own block

         // Reuse the blockList            blockList.Clear();
         if (blockList.Capacity != Condition.Count)
            blockList.Capacity = Condition.Count;

         // determine consecutive blocks (indexes) of equal values of Condition ignoring not relevant symbols (indexes)
         ComputeBlocklist(Condition, Relevant, blockList);

         Boolean test1sRecommended = AnalyseBlockList(blockList);

         if (test1sRecommended)
            {
            GenerateTest1s(blockList);
            }
         else
            {
            GenerateTest0s(blockList);
            }

         blockList.Clear(); // keep capacity for reuse
         return;
         }

      /// <summary>
      /// Creates a list of blocks each of which desribes a sequence of contiguous 0s resp. 1s of the given condition.
      /// Bits which are not relevant are interpreted as 0s or as 1s arbitrarily to get large blocks.
      /// </summary>
      /// <param name="Condition">A <see cref="BitArray"/> with the terminal symbols to be checked</param>
      /// <param name="Relevant">A <see cref="BitArray"/> with the terminal symbols which are not yet checked</param>
      /// <param name="BlockList">The blocklist to be filled with information</param>
      private static void ComputeBlocklist(
          BitArray Condition,
          BitArray Relevant,
          List<BlockOfEqualBits> BlockList)
         {
         (Int32 firstRelevant, Int32 lastRelevant) = Relevant.IndexOfFirstAndLastTrueElement();

         if (firstRelevant == -1)
            {
            return; // no relevant elements, blocklist remains empty
            }

         Boolean blockType;
         Int32 blockStart;
         Int32 blockLength; // counts the relevant bits
         Int32 blockEnd;

         Int32 nextRelevant = firstRelevant; // first relevant

         while (true)
            { // loop over all blocks
            Debug.Assert(nextRelevant <= lastRelevant);

            blockStart = nextRelevant;
            blockType = Condition[blockStart];
            blockLength = 1;
            blockEnd = blockStart;

            // increment blockEnd and blockLength until end of block or end of relevant part of condition
            while (++nextRelevant <= lastRelevant)
               {
               // ignore all elements which are not relevant
               nextRelevant = Relevant.FindNextTrue(nextRelevant - 1);
               // Note: Relevant.FindNextTrue(LastRelevant) == LastRelevant

               Debug.Assert(nextRelevant <= lastRelevant && Relevant[nextRelevant]);

               // beyond end of block?
               if (Condition[nextRelevant] != blockType)
                  break; // yes

               // assume this is the last relevant bit 
               blockEnd = nextRelevant;
               // count the relevant bits
               blockLength++;
               }

            // found a block
            BlockList.Add(new BlockOfEqualBits {
               blockType = blockType,
               blockStart = blockStart,
               blockLength = blockLength,
               blockEnd = blockEnd
               });

            if (nextRelevant > lastRelevant)
               break; // no more blocks

            Debug.Assert(nextRelevant <= lastRelevant && Relevant[nextRelevant] && Condition[nextRelevant] != blockType); // = blockStart of next block
            }
         }

      /// <summary>
      /// returns true if GenerateTest1s will create less or equal number of comparisions than GenerateTest0s
      /// </summary>
      /// <param name="BlockList">the <paramref name="BlockList"/> will be cleared and filled with information for code generation</param>
      /// <returns>returns true if testing 1 s will create less comparisions</returns>
      private static Boolean AnalyseBlockList(List<BlockOfEqualBits> BlockList)
         {
         if (BlockList.Count <= 1)
            return true; // all true elements (if any) must be testet for true (may occur in conditions generated as comment)

         var complexity = new Int32[2] { 0, 0 };

         Int32 first = BlockList[0].blockStart;
         Int32 last = BlockList[^1].blockEnd;

         BlockOfEqualBits block;

         // default: start the loop below with the first block
         Int32 blockIndex = 0;

         //  1st block may need special handling
         block = BlockList[0];

         if (blockIndex == 0 && block.blockLength >= 2)
            {
            // special handlings of first block if it is type true and contains more than 1 element
            Debug.Assert(blockIndex == 0 && block.blockStart == first);

            if (block.blockEnd == last)
               {
               // special case: all elements are equal, condition returns true
               // This should not happen if conflicts are solved properly.
               // This will have the effect that parts of the generated code can never be reached
               ////               codegen.AppendWithOptionalLinebreak("true");

               // complexity[block.blockType ? 1 : 0] += 0; // contains no operator
               return complexity[0] >= complexity[1]; // returns true if testing 1 s will create less comparisions
               }

            // special case: at beginning of relevant symbols test of start may be ommitted
            ////var endTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.blockEnd);
            ////codegen.AppendWithOptionalLinebreakAndPrefix(endTerminalSymbol.InputClass, "Symbol <= ");
            ////codegen.AppendWithPrefix(endTerminalSymbol.SymbolEnum, endTerminalSymbol.Bezeichner);
            complexity[block.blockType ? 1 : 0] += 100;

            blockIndex++; // special case has been handled here, start the loop below with next block
            }

         // handle all (remaining) blocks with type==true
         for (; blockIndex < BlockList.Count; blockIndex++)
            {
            block = BlockList[blockIndex];

            if (blockIndex >= 2)
               {
               // concatenate code for not the first block etc. with " || " 
               ////codegen.AppendLineAndIndent();
               ////codegen.Append(" || ");
               complexity[block.blockType ? 1 : 0] += 100;
               }

            switch (block.blockLength)
               {
            // prefer == and != (by using complexity 99) to <=, <, > and >= (with complexity 100) 
            // The codegen examples are taken from GenerateTest1s.
            // The results are also applicable for GenerateTest0s where only the comparision operators differ.
            case 1: // generate: check of equality of one allowed symbol
                  {
                  ////codegen.AppendWithOptionalLinebreakAndPrefix(startTerminalSymbol.InputClass, "Symbol == ");
                  ////codegen.AppendWithPrefix(startTerminalSymbol.SymbolEnum, startTerminalSymbol.Bezeichner);
                  complexity[block.blockType ? 1 : 0] += 99; // == 
                  break;
                  }

            case 2: // generate: check of two allowed symbols or special case
                  {
                  if (block.blockEnd == last)
                     {
                     // special case: at the end of relevant symbols test of end may be ommitted
                     ////codegen.AppendWithOptionalLinebreakAndPrefix(startTerminalSymbol.InputClass, "Symbol >= ");
                     ////codegen.AppendWithPrefix(startTerminalSymbol.SymbolEnum, startTerminalSymbol.Bezeichner);
                     complexity[block.blockType ? 1 : 0] += 100; // >=
                     }
                  else
                     { // compare two symbols: same complexity as test of interval but better readability
                       ////codegen.AppendWithOptionalLinebreakAndPrefix(endTerminalSymbol.InputClass, "Symbol == ");
                       ////codegen.AppendWithPrefix(endTerminalSymbol.SymbolEnum, endTerminalSymbol.Bezeichner);
                       ////codegen.Append(" || ");
                       ////codegen.AppendWithOptionalLinebreakAndPrefix(startTerminalSymbol.InputClass, "Symbol == ");
                       ////codegen.AppendWithPrefix(startTerminalSymbol.SymbolEnum, startTerminalSymbol.Bezeichner);
                     complexity[block.blockType ? 1 : 0] += 100 + 99 + 99; // == || ==
                     }
                  break;
                  }

            default:// generate: check a sequence of three or more allowed symbols
                  {
                  if (block.blockEnd == last)
                     {
                     // special case: at end the of relevant symbols test of end may be ommitted
                     ////codegen.AppendWithOptionalLinebreakAndPrefix(startTerminalSymbol.InputClass, "Symbol >= ");
                     ////codegen.AppendWithPrefix(startTerminalSymbol.SymbolEnum, startTerminalSymbol.Bezeichner);
                     complexity[block.blockType ? 1 : 0] += 100; // >=
                     }
                  else
                     { // generate: test closed interval of three or more symbols
                       ////codegen.AppendWithOptionalLinebreakAndPrefix(startTerminalSymbol.InputClass, "(Symbol >= ");
                       ////codegen.AppendWithPrefix(startTerminalSymbol.SymbolEnum, startTerminalSymbol.Bezeichner);
                       ////codegen.Append(" && ");
                       ////codegen.AppendWithOptionalLinebreakAndPrefix(endTerminalSymbol.InputClass, "Symbol <= ");
                       ////codegen.AppendWithPrefix(endTerminalSymbol.SymbolEnum, endTerminalSymbol.Bezeichner);
                     complexity[block.blockType ? 1 : 0] += 300; // >= && <=
                     }
                  break;
                  }
               }
            }

         return complexity[0] >= complexity[1]; // returns true if testing 1 s will create less comparisions;
         }

      /// <summary>
      /// Code is generated for all blocks with BlockType == true
      /// </summary>
      /// <param name="BlockList"></param>
      private void GenerateTest1s(List<BlockOfEqualBits> BlockList)
         {
         /* Code is generated for all blocks of type == true, typically  "(symbol >= 'StartOfBlock' && symbol <= 'EndOfBlock") || ..." 
          * Code for different blocks is concatenated by " || "
          * special cases:
          *    blockstart == blockend:  "Symbol == StartOfBlock"
          *    blockstart == 1st relevant elements index: "symbol <= EndOfBlock"
          *    blockend   == last relevant elements index: "Symbol >= StartOfBlock"
          *    blockLength==2: "Symbol == StartOfBlock || Symbol == EndOfBlock"
          *    blocklength >2: "(Symbol >= StartOfBlock && Symbol <= EndOfBlock)" with paranthesis
          *  some more special cases see below
          */

         if (BlockList.Count == 0)
            {
            // no relevant symbol, should not occur
            codegen.AppendWithOptionalLinebreak("true"); // "false" would also be ok
            return;
            }

         Int32 first = BlockList[0].blockStart;
         Int32 last = BlockList[^1].blockEnd;

         /// blockType==0 if the block contains 0s else 1
         BlockOfEqualBits block;

         // default: start the loop below with the first block of type==true
         Int32 blockIndex = BlockList[0].blockType ? 0 : 1;
         // Enclose condition in parentheses if there is one more block of the same type
         Boolean useParentheses = BlockList.Count > blockIndex + 2; // example: Count==3, blockIndex=1 => no parentheses

         // 1st block may need special handling
         block = BlockList[0];
         if (blockIndex == 0) //  && block.blockLength >= 2 && blockList.Count > 1)
            {
            // special handlings of first block if its type is true and contains more than 1 element
            Debug.Assert(blockIndex == 0 && block.blockStart == first);

            if (block.blockEnd == last)
               {
               // Must not occur here, because blockList.Count<=1 has been excluded in If(...
               // so that conditions generated as comment are not only the text "true"
               codegen.AppendWithOptionalLinebreak("true");
               return;
               }

            // special case: at beginning of relevant symbols test of start may be ommitted;
            // resp. must be omitted if the first terminal is intended to include all lower values
            TerminalSymbol endTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.blockEnd);
            codegen.AppendWithOptionalLinebreak(GlobalVariables.VariableNameSymbol, " <= ");
            codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum, endTerminalSymbol.Identifier);

            blockIndex += 2; // special case has been handled here, start the loop below with next block of same type 
            }

         // handle all (remaining) blocks with type==true
         for (; blockIndex < BlockList.Count; blockIndex += 2)
            {
            // by increment 2 all blocks of blockType==false are skipped
            block = BlockList[blockIndex];
            Debug.Assert(block.blockType);

            TerminalSymbol startTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.blockStart);
            TerminalSymbol endTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.blockEnd);

            if (blockIndex >= 2)
               {
               // concatenate code for not the first block etc. with " || " 
               codegen.AppendLineAndIndent();
               codegen.Append("|| ");
               }

            switch (block.blockLength)
               {
            case 1: // generate: check of equality of one allowed symbol (except block with last terminal symbol)
                  {
                  if (block.blockEnd == last)
                     goto default;
                  codegen.AppendWithOptionalLinebreak(GlobalVariables.VariableNameSymbol, " == ");
                  codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum, startTerminalSymbol.Identifier);
                  break;
                  }
            case 2: // generate: check of two allowed symbols or special case
                  {
                  if (block.blockEnd == last)
                     goto default;
                  // compare two symbols: same complexity as test of interval but better readability
                  // no parantheses necessary
                  codegen.AppendWithOptionalLinebreak(GlobalVariables.VariableNameSymbol, " == ");
                  codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum, startTerminalSymbol.Identifier);
                  codegen.Append(" || ");
                  codegen.AppendWithOptionalLinebreak(GlobalVariables.VariableNameSymbol, " == ");
                  codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum, endTerminalSymbol.Identifier);
                  break;
                  }

            default:// generate: check a sequence of three or more allowed symbols
                  {
                  if (block.blockEnd == last)
                     {
                     // special case: at end the of relevant symbols test of end may be ommitted
                     codegen.AppendWithOptionalLinebreak(GlobalVariables.VariableNameSymbol, " >= ");
                     codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum, startTerminalSymbol.Identifier);
                     }
                  else
                     { // generate: test closed interval of three or more symbols
                     if (useParentheses)
                        codegen.Append('(');
                     codegen.AppendWithOptionalLinebreak(GlobalVariables.VariableNameSymbol, " >= ");
                     codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum, startTerminalSymbol.Identifier);
                     codegen.Append(" && ");
                     codegen.AppendWithOptionalLinebreak(GlobalVariables.VariableNameSymbol, " <= ");
                     codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum, endTerminalSymbol.Identifier);
                     if (useParentheses)
                        codegen.Append(')');
                     }
                  break;
                  }
               }
            }
         }

      /// <summary>
      /// Code is generated for all blocks with BlockType == false
      /// </summary>
      /// <param name="BlockList"></param>
      private void GenerateTest0s(List<BlockOfEqualBits> BlockList)
         {
         /* Code is generated for all blocks  of type == false, typically  "(symbol < 'StartIndexOfBlock' || symbol > EndIndexOfBlock) && ..."  
          * Code for different blocks is concatenated by " && "
          * special cases:
          *    blockstart == blockend:  "Symbol != StartOfBlock"
          *    blockstart == 1st relevant elements index: "symbol > EndOfBlock"
          *    blockend   == last relevant elements index: "Symbol < StartOfBlock"
          *    blockLength==2: "Symbol != StartOfBlock && Symbol != EndOfBlock"
          *    blocklength >2: "(Symbol < StartOfBlock || Symbol > EndOfBlock)" with paranthesis

          *  some more special cases see below
          */

         if (BlockList.Count == 0)
            {
            // no relevant symbol
            codegen.AppendWithOptionalLinebreak("true"); // "false" would also be ok
            return;
            }

         Int32 first = BlockList[0].blockStart;
         Int32 last = BlockList[^1].blockEnd;

         BlockOfEqualBits block;

         // default: start the loop below with the first block of type==false
         Int32 blockIndex = BlockList[0].blockType ? 1 : 0;
         // Enclose condition in parentheses if there is one more block of the same type
         Boolean useParentheses = BlockList.Count > blockIndex + 2; // example: Count==3, blockIndex=1 => no parentheses

         // 1st block may need special handling
         block = BlockList[0];

         if (blockIndex == 0) // && block.blockLength >= 2 && blockList.Count > 1)
            {
            // special handlings of first block if its type is false and contains more than 1 element
            Debug.Assert(blockIndex == 0 && block.blockStart == first);

            if (block.blockEnd == last)
               {
               // special case: all elements are false, condition returns false
               // This should not happen if conflicts are solved properly.
               // This will have the effect that parts of the generated code can never be reached
               codegen.AppendWithOptionalLinebreak("false");

               return;
               }

            // special case: at beginning of relevant symbols test of start may be ommitted
            TerminalSymbol endTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.blockEnd);
            codegen.AppendWithOptionalLinebreak(GlobalVariables.VariableNameSymbol, " > ");
            codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum, endTerminalSymbol.Identifier);

            blockIndex += 2; // special case has been handled here, start the loop below with next block of same type 
            }

         // handle all (remaining) blocks with type==true
         for (; blockIndex < BlockList.Count; blockIndex += 2)
            {
            // by increment 2 all blocks of blockType==false are skipped
            block = BlockList[blockIndex];
            Debug.Assert(!block.blockType);

            TerminalSymbol startTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.blockStart);
            TerminalSymbol endTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.blockEnd);

            if (blockIndex >= 2)
               {
               // concatenate code for not the first block etc. with " && " 
               codegen.AppendLineAndIndent();
               codegen.Append("&& ");
               }

            switch (block.blockLength)
               {
            case 1: // generate: check of equality of one allowed symbol
                  {
                  if (block.blockEnd == last)
                     goto default;
                  codegen.AppendWithOptionalLinebreak(GlobalVariables.VariableNameSymbol, " != ");
                  codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum, startTerminalSymbol.Identifier);
                  break;
                  }
            case 2: // generate: check of two allowed symbols or special case
                  {
                  if (block.blockEnd == last)
                     goto default;
                  // compare two symbols: same complexity as test of interval but better readability
                  // no parantheses necessary
                  codegen.AppendWithOptionalLinebreak(GlobalVariables.VariableNameSymbol, " != ");
                  codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum, startTerminalSymbol.Identifier);
                  codegen.Append(" && ");
                  codegen.AppendWithOptionalLinebreak(GlobalVariables.VariableNameSymbol, " != ");
                  codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum, endTerminalSymbol.Identifier);
                  break;
                  }
            default:// generate: check a sequence of three or more allowed symbols
                  {
                  if (block.blockEnd == last)
                     {
                     // special case: at end the of relevant symbols test of end must be ommitted
                     // because the last terminal symbol represents al fllowing symbols
                     codegen.AppendWithOptionalLinebreak(GlobalVariables.VariableNameSymbol, " < ");
                     codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum, startTerminalSymbol.Identifier);
                     }
                  else
                     { // generate: test closed interval of three or more symbols
                     if (useParentheses)
                        codegen.Append('(');
                     codegen.AppendWithOptionalLinebreak(GlobalVariables.VariableNameSymbol, " < ");
                     codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum, startTerminalSymbol.Identifier);
                     codegen.Append(" || ");
                     codegen.AppendWithOptionalLinebreak(GlobalVariables.VariableNameSymbol, " > ");
                     codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum, endTerminalSymbol.Identifier);
                     if (useParentheses)
                        codegen.Append(')');
                     }
                  break;
                  }
               }
            }
         }
      }
   }

