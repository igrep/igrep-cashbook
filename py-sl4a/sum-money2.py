#!/usr/bin/python
# -*- coding: utf-8 -*-

import re
import fileinput
import os.path

class CashbookItem(object):

  #to parse content of a file
  COMMENT_TOKEN = '#'
  DATE_RE       = re.compile( r'^(\d\d/)?\d\d/\d\d$' )
  SEP           = re.compile( r' {2,}' )

  def __init__(self, name, price, group, date = None):
    self._name  = name
    self._price = price
    self._group = group
    self._date  = date

  @property
  def name(self): return self._name

  @property
  def group(self): return self._group

  @property
  def date(self): return self._date

  @property
  def price(self): return self._price.value

  def is_income(self): return self._price.income

  @classmethod
  def parse_line(klass, line):
    """parse a line"""
    if line.startswith( klass.COMMENT_TOKEN ) or klass.DATE_RE.match( line ):
      return None

    columns = klass.SEP.split( line.strip() )
    try:
      name      = columns[0]
      price_str = columns[1]
      price     = Price( price_str )
      group     = columns[2]
    except IndexError:
      klass.raise_with_line( "Invalid line: Some fields are missing", line )
    except InvalidPriceError:
      klass.raise_with_line( "Invalid line: The price fileld is not valid", line )
    else:
      return klass(name, price, group)

  @classmethod
  def raise_with_line(klass, cause, line):
    """raise for a malformed line"""
    msg = cause + ": \"" + line + "\""
    raise MalformedItemError( msg )

class MalformedItemError(Exception):
  """representing invalid input"""
  def __init__(self, cause):
    self.cause = cause

class Price(object):
  """representing the price field of the CashbookItem."""

  PRICE_RE = re.compile( r"""
      ^(\+?)
      [1-9]
      [_,\d]
      *$
      """, re.VERBOSE )

  def __init__(self, signed_price):
    mdat = self.PRICE_RE.match(signed_price)
    if mdat == None:
      raise InvalidPriceError
    self._income = 1 == len( mdat.group(1) )
    self._value = int( filter( lambda x: x in '1234567890', signed_price ) )

  @property
  def value(self): return self._value

  @property
  def income(self): return self._income

class InvalidPriceError(Exception):
  """representing invalid price"""
  pass

try:
  import android
except ImportError:
  by_android = False
else:
  by_android = True
  droid = android.Android()

#input/output
if by_android:
  import StringIO
  result_out = StringIO.StringIO(u'')

  # choose files to calculate
  import glob
  money_dirs = [ '/storage/sdcard1/My/money/', '/mnt/sdcard/My/money/' ]
  default_pattern = '[0-9][0-9]-[0-9][0-9].txt'

  ## collect cash book file paths.
  cash_books = [
    glob.glob( d + default_pattern ) for d in money_dirs
  ]

  ## select a directory which contains cash book files.
  present_cash_books, money_dir = [
    ( cash_books[i], d ) for i, d in enumerate( money_dirs ) if cash_books[i]
  ][0]

  default_file = os.path.basename( max( present_cash_books ) )
  file_pattern = droid.dialogGetInput('ARGV:', 'Enter file pattern.', default_file ).result
  file_list = glob.glob( money_dir + file_pattern )

else:
  import sys
  result_out = sys.stdout
  file_list = sys.argv[1:]

#to output a result
## Use counter module instead!
incomes     = dict()
expenses    = dict()
income_sum  = 0
expense_sum = 0

utf8_hook = fileinput.hook_encoded("utf-8")

for line in fileinput.input( file_list, openhook=utf8_hook ):
  try:
    item = CashbookItem.parse_line(line)
  except MalformedItemError as err:
    print >>result_out, \
        u"[WARNING] {0} at {1} of {2}.".format(
            err.cause, fileinput.filelineno(), fileinput.filename() )
    continue

  if item == None: continue

  group = item.group
  price = item.price
  if item.is_income():
    incomes[ group ] = \
        incomes.get( group, 0 ) + price
    income_sum +=  price
  else:
    expenses[ group ] = \
        expenses.get( group, 0 ) + price
    expense_sum += price

total_str = str( income_sum - expense_sum )

#To format the result
income_str    = str( income_sum )
expense_str   = str( expense_sum )
income_digit  = len( income_str )
expense_digit = len( expense_str )
digit = expense_digit if expense_digit > income_digit else income_digit
JP_TOKEN = re.compile(u"[一-龠]|[ぁ-ん]|[ァ-ヴ]")
def format_result_line( group, price ):
  #日本語の文字数分だけ寄せる幅が減る。
  #(日本語1文字毎に1つ余計にスペースを使うので)
  extra_width = len( JP_TOKEN.findall( group ) )
  max_width = 10
  width = (max_width - extra_width) if extra_width <= max_width else max_width
  return u"{0:<{1}}{2:>{3}}".format( group, width, price, digit )

def print_sums( header, sums, whole_sum ):
  print >>result_out, header
  sorted_groups = sorted( sums.keys(), key=lambda group: sums[ group ] )
  for group in sorted_groups:
    print >>result_out, \
      format_result_line( group, sums[ group ] )
  print >>result_out, format_result_line( u"小計", whole_sum )
  print >>result_out, "\n",

print_sums( '## EXPENSES ##', expenses, expense_sum )
print_sums( '## INCOMES ##', incomes, income_sum )
print >>result_out, format_result_line( u"合計", total_str )

if by_android:
  droid.dialogCreateAlert(
    " ".join(
        map( os.path.basename, file_list ) ),
    result_out.getvalue()
  )
  droid.dialogSetPositiveButtonText('OK')
  droid.dialogShow()
  result_out.close()
