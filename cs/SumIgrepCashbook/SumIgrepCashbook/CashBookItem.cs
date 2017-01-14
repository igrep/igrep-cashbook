using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace SumIgrepCashbook
{
    class CashBookItem
    {
        private static readonly Regex itemLineRegex = new Regex(@"^ [\S ]+  (\+?)(\d[\d,_]*)  (\S+)");
        private static readonly Regex priceDelimiterRegex = new Regex(@"[,_]");
        public int Price { get; set; }
        public string Category { get; set; }
        public bool IsIncome { get; set; }

        public static bool IsItemLine( string line )
        {
            return itemLineRegex.IsMatch(line);
        }

        public static CashBookItem ParseLine(string line)
        {
            var match = itemLineRegex.Match(line);
            if (match.Success)
            {
                var groups = match.Groups;
                return new CashBookItem
                {
                    IsIncome = !( groups[1].Value.Equals(String.Empty) ),
                    Price = ParsePrice( groups[2].Value ),
                    Category = groups[3].Value
                };
            }
            else
            {
                throw new InvalidItemException(String.Format("Invalid line: {0}", line));
            }
        }

        private static int ParsePrice(string priceField)
        {
            return int.Parse(priceDelimiterRegex.Replace(priceField, ""));
        }
    }

    class InvalidItemException : Exception
    {
        public InvalidItemException( string message ): base( message )
        {
        }
    }
}
