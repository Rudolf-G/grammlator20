using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Text;

namespace Grammlator
    {
    /// <summary>
    /// The <see cref="SubStringStruct"/> references a <see cref="BaseString"/> and stores the <see cref="StartIndex"/> 
    /// and the <see cref="Length"/> of a substring. It provides methods to derive new substrings without
    /// modifying the origin and without allocations on the heap.
    /// </summary>
    internal struct SubStringStruct
        {
        /// <summary>
        /// The <see cref="BaseString"/> is the string from which the substring is taken
        /// </summary>
        public String BaseString
            {
            get; private set;
            } // the referenced string: may be empty but not null

#pragma warning disable RCS1213 // Remove unused member declaration. This property is typically used in the debugger !!
#pragma warning disable IDE0052 // Ungelesene private Member entfernen
        /// <summary>
        /// The property <see cref="SubString"/> returns the selected substring  as a String
        /// </summary>
        private string SubString => BaseString.Substring(StartIndex, Length);
#pragma warning restore IDE0052 // Ungelesene private Member entfernen
#pragma warning restore RCS1213 // Remove unused member declaration.

        /// <summary>
        /// The property <see cref="BaseLength"/> returns the length of the referenced string
        /// </summary>
        public Int32 BaseLength => BaseString.Length;

        /// <summary>
        /// <para>The property <see cref="StartIndex"/> is the index of the first character of the substring in <see cref="BaseString"/>,</para> 
        /// 0 &lt;= <see cref="StartIndex"/>+<see cref="Length"/> &lt;= <see cref="BaseLength"/>
        /// </summary>
        public Int32 StartIndex
            {
            get; private set;
            }

        /// <summary>
        /// <para>The property <see cref="Length"/> is the length of the substring, </para> 
        /// 0 &lt;= <see cref="StartIndex"/>+<see cref="Length"/> &lt;= <see cref="BaseLength"/>
        /// </summary>
        public Int32 Length
            {
            get; private set;
            }

        /// <summary>
        /// Constructs a <see cref="SubStringStruct"/> which denotes the total string <see cref="BaseString"/> given by the parameter.
        /// </summary>
        /// <param name="BaseString">The String to be used as <see cref="BaseString"/></param>
        public SubStringStruct(String BaseString)
            {
            this.BaseString = BaseString;
            StartIndex = 0;
            Length = BaseString.Length;
            }

        /// <summary>
        /// Makes a new <see cref="SubStringStruct"/> representing the substring of the original substring up
        /// to the first occurend of the given character or - if not found - to the end of the origin
        /// </summary>
        /// <param name="c">character which denotes the end of the new substring</param>
        /// <returns><see cref="SubStringStruct"/> ending with c or if not found with the last character of origin</returns>
        public SubStringStruct SelectUpTo(Char c)
            {
            Int32 newEnd = StartIndex;
            for (; newEnd < StartIndex + Length; newEnd++)
                {
                if (BaseString[newEnd] == c)
                    {
                    newEnd++; // make newEnd to point behind the found character so that newEnd-StartIndex == newLength
                    break;
                    }
                }
            // if not found newEnd points behind the last character of BaseString and newEnd-StartIndex == newLength
            return new SubStringStruct {
                BaseString = BaseString,
                StartIndex = StartIndex,
                Length = newEnd - StartIndex
                };
            }

        /// <summary>
        /// Checks whether the beginning of the substring is equal to <paramref name="Pattern"/>
        /// </summary>
        /// <param name="Pattern">a String which </param>
        /// <returns>true if the substring starts with the given <paramref name="Pattern"/></returns>
        public Boolean StartsWith(String Pattern)
            {
            Int32 position = StartIndex;
            return BaseString.StartsWithAndSkip(ref position, Pattern);
            }

        public SubStringStruct IgnoreLeadingSeparators()
            {
            Int32 newStart = StartIndex;
            for (; newStart < StartIndex + Length; newStart++)
                {
                if (!Char.IsSeparator(BaseString[newStart]))
                    break;
                }

            return new SubStringStruct {
                BaseString = BaseString,
                StartIndex = newStart,
                Length = Length - newStart + StartIndex
                };
            }

        /// <summary>
        /// If the first character equals the given character the returned substring will not include this character.
        /// </summary>
        /// <param name="c">returns the substring without the leading char or - if not present - the unchanged substring</param>
        /// <returns>The 0 or 1 characters shorter SubStringStruct</returns>
        public SubStringStruct IgnoreLeadingChar(Char c)
            {
            Int32 newStart = StartIndex;
            if (StartIndex < BaseLength && BaseString[newStart] == c)
                newStart++;
            return new SubStringStruct {
                BaseString = BaseString,
                StartIndex = newStart,
                Length = Length - newStart + StartIndex
                };
            }

        /// <summary>
        /// If the last character equals the given character the returned substring will not include this character.
        /// </summary>
        /// <param name="c">returns the substring without the leading char or - if not present - the unchanged substring</param>
        /// <returns>The 0 or 1 characters shorter SubStringStruct</returns>
        public SubStringStruct IgnoreTrailingChar(Char c)
            {
            if ((StartIndex + Length) >= BaseLength) // empty substring
                return this; // creates a copy because this is a structure!
            Int32 newLength = Length;
            if (BaseString[StartIndex + newLength - 1] == c)
                newLength--;
            return new SubStringStruct {
                BaseString = BaseString,
                StartIndex = StartIndex,
                Length = newLength
                };
            }

        public SubStringStruct IgnoreTrailingSeparators()
            {
            if ((StartIndex + Length) >= BaseLength) // empty substring
                return this; // creates a copy because this is a structure!
            Int32 newLength = Length;
            for (; newLength > 0; newLength--)
                {
                if (!Char.IsSeparator(BaseString[StartIndex + newLength - 1]))
                    break;
                }

            return new SubStringStruct {
                BaseString = BaseString,
                StartIndex = StartIndex,
                Length = newLength
                };
            }

        /// <summary>
        /// Compares the substrings character by character
        /// </summary>
        /// <param name="s1">Substring to compare</param>
        /// <param name="s2">Substring to compare</param>
        /// <returns>false if different length or at least one different character</returns>
        public static Boolean operator ==(SubStringStruct s1, SubStringStruct s2)
            {
            if (s1.Length != s2.Length)
                return false;

            Int32 i1 = s1.StartIndex, i2 = s2.StartIndex;
            while (i1 < s1.StartIndex + s1.Length)
                {
                if (s1.BaseString[i1] != s2.BaseString[i2])
                    return false;
                i1++;
                i2++;
                }
            return true;
            }

        public override Boolean Equals(Object obj)
            {
            if (!(obj is SubStringStruct objAsSubstringStruct))
                return false;
            return this == objAsSubstringStruct;
            }

        public override Int32 GetHashCode()
            {
            Int64 Hash = 0;
            const Int32 Primzahl = 1073741827; // >2^30, <2^31, nicht 2**n -1
            for (Int32 i = 0; i < Length; i++)
                {
                Hash = (Hash << 16 + BaseString[StartIndex + i]) % Primzahl;
                }
            return (Int32)Hash;
            }

        /// <summary>
        /// Compares the substrings character by character
        /// </summary>
        /// <param name="s1">Substring to compare</param>
        /// <param name="s2">Substring to compare</param>
        /// <returns>true if different length or all character equal</returns>
        public static Boolean operator !=(SubStringStruct s1, SubStringStruct s2)
            => !(s1 == s2);

        public SubStringStruct Skip(Int32 NumberOfChars)
            {
            return (NumberOfChars < Length)
                ? new SubStringStruct {
                    BaseString = BaseString,
                    StartIndex = StartIndex + NumberOfChars,
                    Length = Length - NumberOfChars
                    }
                : new SubStringStruct {
                    BaseString = BaseString,
                    StartIndex = StartIndex + Length - 1,
                    Length = 0
                    };
            }
        }
    }


