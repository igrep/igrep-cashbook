extern crate combine;

use combine::stream::easy::{ParseError};

#[derive(Debug, PartialEq)]
pub struct Summary {
    incomes: SubSummary,
    expenditures: SubSummary,
}

impl Summary {
    pub fn from_file(filename: &String, contents: &String) -> Summary {
        let mut summary = Summary {
            incomes: 0,
            expenditures: 0,
        };

        for line in contents.lines() {
            if line.contains('+') {
                summary.incomes += 1
            } else {
                summary.expenditures += 1
            }
        }

        summary
    }
}

// Currently, just count lines
type SubSummary = u64;

#[derive(Debug, PartialEq)]
struct Entry<'a> {
    is_income: bool,
    amount: u64,
    group: &'a str,
}

impl Entry<'_> {
    pub fn parse_line(line: &str) -> Result<Entry, ParseError<&str>> {
        Ok(Entry { is_income: false, amount: 0, group: "" })
    }
}

#[test]
fn test_collect_file_lines() {
    let contents = concat!(
        " 漫画  500  娯楽\n",
        " 給料  +30,000  給料\n",
        " 油そば  800  外食\n",
        " 本  6,600  勉強\n",
        " CD 売却  +10  その他\n"
    );
    assert_eq!(
        Summary::from_file(&"input.txt".to_string(), &contents.to_string()),
        Summary {
            incomes: 30_000 + 10,
            expenditures: 500 + 800 + 6600,
        }
    );
}
