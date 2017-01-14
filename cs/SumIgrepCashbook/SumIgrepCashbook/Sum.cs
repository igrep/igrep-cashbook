using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace SumIgrepCashbook
{
    class Sum
    {
        static void Main(string[] args)
        {
            foreach ( var fileName in args )
            {
                var itemsForSum = from line in File.ReadLines(fileName)
                                  where CashBookItem.IsItemLine(line)
                                  select CashBookItem.ParseLine(line);
                var categorySums = from item in itemsForSum
                                   group item by item.Category into byCategory
                                   select new
                                   {
                                       Category = byCategory.Key,
                                       Sum = byCategory.Sum( g => g.Price )
                                   };

                var wholeSum = (from c in categorySums select c.Sum).Sum();
                foreach (var c in categorySums.OrderBy( c => c.Sum ))
                {
                    Console.WriteLine("{0}\t{1,8:D}", c.Category, c.Sum);
                }
                Console.WriteLine("合計\t{0,8:D}", wholeSum);
            }
        }
    }
}
