#!/usr/bin/python
# -*- coding: utf-8 -*-

import re
import fileinput
import os.path

class CashbookItem:
  def __init__(self, name, price, income, group, date = None):
    self.name = name
    self.price = price
    self.income = income
    self.group = group
    self.date = date

  @classmethod
  def parse_no_date(klass, lines):
    """parse lines"""
    [ parse_line( line, index + 1 ) for index, line in enumerate(lines) if not comment_re.match( line ) ]

  @classmethod
  def parse_line(klass, line):
    """parse a line"""
    try:
      price_str = columns[2]
      price     = _in( price_str )
      category  = columns[3] if len( columns ) >= 4 else ''
      file_name = fileinput.filename()
      line_no   = fileinput.filelineno()
    except IndexError, ValueError:
      warn_file_format( result_out, line, file_name, line_no )
      continue

def int_ruby_style(string):
  """parse a string into an integer like String#to_s on Ruby"""
  try:
    return int( string.replace( '_', '' ) )
  except ValueError:
    return 0

def warn_file_format( out, line, file_name, line_no ):
  print >>out, "Invalid line: {0!r} at {1}: {2}".format(
    line, file_name, line_no )

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
  money_dir = '/mnt/sdcard/My/money/'
  default_pattern = '[0-9][0-9]-[0-9][0-9].txt'
  default_file = os.path.basename( max( glob.glob( money_dir + default_pattern ) ) )
  file_pattern = droid.dialogGetInput('ARGV:', 'Enter file pattern.', default_file ).result
  file_list = glob.glob( money_dir + file_pattern )

else:
  import sys
  result_out = sys.stdout
  file_list = sys.argv[1:]

#to parse content of a file
comment_re = re.compile( r'^\s*#' )
sep        = re.compile( r' {2,}|\t' )

#to output a result
sums_by_categories = dict()
income             = 0
expense            = 0

utf8_hook = fileinput.hook_encoded("utf-8")
for line in fileinput.input( file_list, openhook=utf8_hook ):
  if comment_re.match( line ): continue

  columns = sep.split( line.rstrip("\n") )
  try:
    price_str = columns[2]
    price     = int_ruby_style( price_str )
    category  = columns[3] if len( columns ) >= 4 else ''
    file_name = fileinput.filename()
    line_no   = fileinput.filelineno()
  except IndexError, ValueError:
    warn_file_format( result_out, line, file_name, line_no )
    continue
    
  if price <= 0:
    warn_file_format( result_out, line, file_name, line_no )

  if price_str.startswith('+'):
    income += price
  else:
    sums_by_categories[ category ] = \
        sums_by_categories.get( category, 0 ) + price
    expense += price

total_str = str( income - expense )

#To format the result
income_str    = str( income )
expense_str   = str( expense )
income_digit  = len( income_str )
expense_digit = len( expense_str )
digit = expense_digit if expense_digit > income_digit else income_digit
JP_TOKEN = re.compile(u"[一-龠]|[ぁ-ん]|[ァ-ヴ]")
def format_result_line( category, price ):
  #日本語の文字数分だけ寄せる幅が減る。
  #(日本語1文字毎に1つ余計にスペースを使うので)
  width = 10 - len( JP_TOKEN.findall( category ) )
  return u"{0:<{1}}{2:>{3}}".format( category, width, price, digit )

sorted_categories = sorted( sums_by_categories.keys(),
    key=lambda category: sums_by_categories[ category ] )
for category in sorted_categories:
  print >>result_out, \
    format_result_line( category, sums_by_categories[ category ] )

print >>result_out, format_result_line( u"支出", expense_str )
if income > 0:
  print >>result_out, format_result_line( u"収入", income_str )
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
