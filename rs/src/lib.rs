extern crate combine;

use combine::parser::char::{char, digit};
use combine::stream::easy::ParseError;
use combine::{many, many1, none_of, not_followed_by, one_of, skip_many, skip_many1, value};
use combine::{EasyParser, Parser};

#[derive(Debug, PartialEq)]
pub struct Summary<'a> {
    incomes: SubSummary,
    expenditures: SubSummary,
    errors: Vec<ParseError<&'a str>>,
}

impl<'a> Summary<'a> {
    pub fn from_file(contents: &'a String) -> Summary<'a> {
        let mut summary = Summary {
            incomes: 0,
            expenditures: 0,
            errors: Vec::new(),
        };

        for line in contents.lines() {
            let eent = Entry::parse_line(line);
            match eent {
                Ok(ent) => {
                    if ent.is_income {
                        summary.incomes += ent.amount
                    } else {
                        summary.expenditures += ent.amount
                    }
                }
                Err(err) => summary.errors.push(err),
            }
        }

        summary
    }
}

// Currently, just count total price
type SubSummary = u64;

#[derive(Debug, PartialEq)]
struct Entry {
    is_income: bool,
    amount: u64,
    group: String,
}

impl Entry {
    pub fn parse_line(line: &str) -> Result<Entry, ParseError<&str>> {
        let not_space = || none_of(" ".chars());
        let column_left_chars = || not_space().or(char(' ').skip(not_followed_by(char(' '))));

        // consumes a trailing space (the first space of the separator)
        let name_column = not_space().with(skip_many(column_left_chars()));
        let price_column = {
            let is_income = char('+').map(|_| true).or(value(false));
            static NUM_SEPARATOR: &'static str = ",_";
            let num = many1(digit().or(one_of(NUM_SEPARATOR.chars())))
                .map(|string: String| string.replace(NUM_SEPARATOR, "").parse::<u64>().unwrap());
            is_income.and(num)
        };
        let group_column = not_space().then(|first| {
            many(column_left_chars()).map(move |left: String| format!("{}{}", first, left))
        });
        let mut parser = char(' ')
            .with(name_column)
            .with(skip_many1(char(' ')))
            .with(price_column)
            .skip(skip_many1(char(' ')))
            .and(group_column)
            .map(|((is_income, amount), group)| Entry {
                is_income,
                amount,
                group,
            });
        parser.easy_parse(line).map(|x| x.0)
    }
}

#[test]
fn test_collect_file_lines() {
    let contents = concat!(
        " 漫画  500  娯楽  # comment\n",
        " 給料  +30,000  給料\n",
        " Book Store マンガ  400  娯楽\n",
        " 油そば  600  外食\n",
        " 本  6,600  勉強\n",
        " CD 売却  +10  その他\n"
    );
    assert_eq!(
        Summary::from_file(&contents.to_string()),
        Summary {
            incomes: 30_000 + 10,
            expenditures: 500 + 400 + 600 + 6600,
            errors: Vec::new(),
        }
    );
}
