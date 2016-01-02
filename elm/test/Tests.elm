module Tests where

import Graphics.Element exposing (Element)

import ElmTest exposing
  ( test
  , Test
  , suite
  , assert
  , assertEqual
  , elementRunner
  )

import IgrepCashbook.FileList as FileList
import IgrepCashbook.File as File
import IgrepCashbook.Line as Line exposing (SuccessLine, WrongLine)

import Dict


all : Test
all =
  suite "IgrepCashbook"
    [ suite ".FileList"
      [ suite ".extractFromHtml"
        [ test "parses html returned by wai-app-static server" <|
          assertEqual expectedPaths (FileList.extractFromHtml htmlFromWarp)
        , test "parses html returned by elm-reactor" <|
          assertEqual expectedPaths (FileList.extractFromHtml htmlFromElmReactor)
        , test "parses html returned by lighttpd" <|
          assertEqual expectedPaths (FileList.extractFromHtml htmlFromLighttpd)
        ]
      , suite ".latestFileNameOf"
        [ test
            "given a file list containing only non-dash-ending file, returns the latest file name."
            <|
              let model = FileList.fromPaths ["15-06.txt", "15-07.txt", "15-08.txt"]
              in
                  assertEqual "15-08.txt" (FileList.latestFileNameOf model)
        , test
            "given a file list containing dash-ending file, returns the latest non-dash-ending file name."
            <|
              let model = FileList.fromPaths ["15-12.txt", "15-11.txt", "15-10.txt", "15-13-.txt"]
              in
                  assertEqual "15-12.txt" (FileList.latestFileNameOf model)
        , test "given an empty file list, returns an empty file name." <|
          let model = Ok { files = Dict.empty }
          in
              assertEqual "" (FileList.latestFileNameOf model)
        , test "given an error file list, returns an empty file name." <|
          let model = Err "error"
          in
              assertEqual "" (FileList.latestFileNameOf model)
        ]
      ]

    , suite ".File"
      [ suite ".parse"
        [ test "given lines representing cashbook lines, returns parsed items" <|
            assertEqual
              (File.Model "name" expectedLines)
              (File.parse "name" exampleCashbookData)
        ]
      ]
    ]


main : Element
main = elementRunner all


expectedPaths : List String
expectedPaths =
  [ "15-05.txt"
  , "15-06.txt"
  , "15-07.txt"
  , "15-08.txt"
  , "15-09.txt"
  , "15-10.txt"
  , "15-11-.txt"
  ]


exampleCashbookData : String
exampleCashbookData = """
# Comment
# The line below is date line formatted as YY/MM/DD or MM/DD
01/02/03
02/03
# price field can be separated by comma or underline
 Salary  +12_300,000  Group1 # plus sign represents an income.
01/02/03
 What I bought  2000  Group1 # no sign represents an expense.

# empty lines should be ignored!

01/02/04
 Other income  +100  Group2
 Another things I bought  10_000  Group2
 Yet another income  +10  Group2
 Yet another things I bought  1000  Group2
 Wrong line (only 1 space between name and price) 1000  Group2
 Wrong line (only 1 space between price and group)  1000 Group2
 Wrong line (no group1)  1000
 Wrong line (no group2)  1000  
   1000  NoName
 Wrong line (price is 0)  0  Group2
 Wrong line (malformed price)  -0  Group2
 Wrong line (malformed price)  0+  Group2
"""


expectedLines : List Line.Model
expectedLines =
  [ Ok <| SuccessLine 12300000 "Group1"
  , Ok <| SuccessLine -2000 "Group1"
  , Ok <| SuccessLine 100 "Group2"
  , Ok <| SuccessLine -10000 "Group2"
  , Ok <| SuccessLine 10 "Group2"
  , Ok <| SuccessLine -1000 "Group2"
  , Err <| WrongLine Line.errorInvalidPrice          " Wrong line (only 1 space between name and price) 1000  Group2"
  , Err <| WrongLine Line.errorNoSeparatorAfterPrice " Wrong line (only 1 space between price and group)  1000 Group2"
  , Err <| WrongLine Line.errorNoSeparatorAfterPrice " Wrong line (no group1)  1000"
  , Err <| WrongLine Line.errorNoGroup               " Wrong line (no group2)  1000  "
  , Err <| WrongLine Line.errorNoName                "   1000  NoName"
  , Err <| WrongLine Line.errorInvalidPrice          " Wrong line (price is 0)  0  Group2"
  , Err <| WrongLine Line.errorInvalidPrice          " Wrong line (malformed price)  -0  Group2"
  , Err <| WrongLine Line.errorInvalidPrice          " Wrong line (malformed price)  0+  Group2"
  ]


htmlFromWarp : String
htmlFromWarp = """
<html><head><title>root folder</title><style>table { margin: 0 auto; width: 760px; border-collapse: collapse; font-family: 'sans-serif'; }
table, th, td { border: 1px solid #353948; }
td.size { text-align: right; font-size: 0.7em; width: 50px }
td.date { text-align: right; font-size: 0.7em; width: 130px }
td { padding-right: 1em; padding-left: 1em; }
th.first { background-color: white; width: 24px }
td.first { padding-right: 0; padding-left: 0; text-align: center }
tr { background-color: white; }
tr.alt { background-color: #A3B5BA}
th { background-color: #3C4569; color: white; font-size: 1.125em; }
h1 { width: 760px; margin: 1em auto; font-size: 1em; font-family: sans-serif }
img { width: 20px }
a { text-decoration: none }
</style></head><body><h1>root</h1><table><thead><th class="first"><img src=".hidden/haskell.png"></th><th>Name</th><th>Modified</th><th>Size</th></thead><tbody><tr><td class="first"><img src=".hidden/folder.png" alt="Folder"></td><td><a href="10">10</a></td><td class="date"></td><td class="size"></td></tr><tr class="alt"><td class="first"><img src=".hidden/folder.png" alt="Folder"></td><td><a href="11">11</a></td><td class="date"></td><td class="size"></td></tr><tr><td class="first"><img src=".hidden/folder.png" alt="Folder"></td><td><a href="12">12</a></td><td class="date"></td><td class="size"></td></tr><tr class="alt"><td class="first"><img src=".hidden/folder.png" alt="Folder"></td><td><a href="13">13</a></td><td class="date"></td><td class="size"></td></tr><tr><td class="first"><img src=".hidden/folder.png" alt="Folder"></td><td><a href="14">14</a></td><td class="date"></td><td class="size"></td></tr><tr class="alt"><td class="first"><img src=".hidden/folder.png" alt="Folder"></td><td><a href="old">old</a></td><td class="date"></td><td class="size"></td></tr><tr><td class="first"><img src=".hidden/folder.png" alt="Folder"></td><td><a href="payslips">payslips</a></td><td class="date"></td><td class="size"></td></tr><tr class="alt"><td class="first"></td><td><a href="15-05.txt">15-05.txt</a></td><td class="date">16-Jun-2015 14:26:47</td><td class="size">5 KB</td></tr><tr><td class="first"></td><td><a href="15-06.txt">15-06.txt</a></td><td class="date">15-Jul-2015 14:25:40</td><td class="size">3 KB</td></tr><tr class="alt"><td class="first"></td><td><a href="15-07.txt">15-07.txt</a></td><td class="date">27-Sep-2015 10:04:42</td><td class="size">3 KB</td></tr><tr><td class="first"></td><td><a href="15-08.txt">15-08.txt</a></td><td class="date">14-Sep-2015 12:33:41</td><td class="size">3 KB</td></tr><tr class="alt"><td class="first"></td><td><a href="15-09.txt">15-09.txt</a></td><td class="date">13-Oct-2015 12:16:17</td><td class="size">2 KB</td></tr><tr><td class="first"></td><td><a href="15-10.txt">15-10.txt</a></td><td class="date">25-Oct-2015 02:50:44</td><td class="size">2 KB</td></tr><tr class="alt"><td class="first"></td><td><a href="15-11-.txt">15-11-.txt</a></td><td class="date">23-Oct-2015 05:03:09</td><td class="size">961 B</td></tr><tr><td class="first"></td><td><a href="animate-point.txt">animate-point.txt</a></td><td class="date">29-Sep-2013 05:00:14</td><td class="size">2 B</td></tr><tr class="alt"><td class="first"></td><td><a href="hasmoku.txt">hasmoku.txt</a></td><td class="date">31-Jan-2015 14:01:13</td><td class="size">1 KB</td></tr><tr><td class="first"></td><td><a href="pc.txt">pc.txt</a></td><td class="date">30-Mar-2015 13:01:38</td><td class="size">488 B</td></tr><tr class="alt"><td class="first"></td><td><a href="psc.tix">psc.tix</a></td><td class="date">07-Oct-2015 01:41:32</td><td class="size">63 KB</td></tr><tr><td class="first"></td><td><a href="sum">sum</a></td><td class="date">25-May-2014 13:18:27</td><td class="size">2 MB</td></tr><tr class="alt"><td class="first"></td><td><a href="sum-money2.py">sum-money2.py</a></td><td class="date">27-Apr-2014 03:07:49</td><td class="size">5 KB</td></tr><tr><td class="first"></td><td><a href="template.txt">template.txt</a></td><td class="date">23-Oct-2015 04:58:08</td><td class="size">1,004 B</td></tr></tbody></table></body></html>
"""


htmlFromElmReactor : String
htmlFromElmReactor = """
<!DOCTYPE html>
<html>
<head><title>~</title><style type='text/css'>body {
    margin: 0;
    background: rgb(253,253,253);
    font-family: 'Lucida Grande','Trebuchet MS','Bitstream Vera Sans',Verdana,Helvetica,sans-serif;
}
div.topbar {
    height: 6px;
    background-color: rgb(96,181,204);
}
div.header {
    padding: 20px 50px;
    font-size: 30px;
}
div.content { padding: 0 40px }
table {
    width:100%;
    border-collapse: collapse;
    margin-bottom: 40px;
    float: left
}
a { text-decoration: none; color:rgb(96,181,204) }
td { padding: 8px 10px; color:rgb(136,136,136) }
tr { border-bottom: solid rgb(245,245,245) 1px }
th {
    text-align: left;
    padding: 6px 10px;
    font-weight: normal;
    font-size: 24px;
}</style></head><body><div class="topbar"></div><div class="header"><a href="/">~</a> / </div><div class="content"><table><tr><th>Directories</th></tr><tr><td><a href="10/">10</a></td></tr><tr><td><a href="11/">11</a></td></tr><tr><td><a href="12/">12</a></td></tr><tr><td><a href="13/">13</a></td></tr><tr><td><a href="14/">14</a></td></tr><tr><td><a href="old/">old</a></td></tr><tr><td><a href="payslips/">payslips</a></td></tr></table><table><tr><th>Other Files</th><th></th></tr><tr><td><a href="15-05.txt">15-05.txt</a></td><td style="text-align:right;">131 days ago</td></tr><tr><td><a href="15-06.txt">15-06.txt</a></td><td style="text-align:right;">102 days ago</td></tr><tr><td><a href="15-07.txt">15-07.txt</a></td><td style="text-align:right;">28 days ago</td></tr><tr><td><a href="15-08.txt">15-08.txt</a></td><td style="text-align:right;">41 days ago</td></tr><tr><td><a href="15-09.txt">15-09.txt</a></td><td style="text-align:right;">12 days ago</td></tr><tr><td><a href="15-10.txt">15-10.txt</a></td><td style="text-align:right;">10 hours ago</td></tr><tr><td><a href="15-11-.txt">15-11-.txt</a></td><td style="text-align:right;">2 days ago</td></tr><tr><td><a href="animate-point.txt">animate-point.txt</a></td><td style="text-align:right;">2 years ago</td></tr><tr><td><a href="hasmoku.txt">hasmoku.txt</a></td><td style="text-align:right;">267 days ago</td></tr><tr><td><a href="pc.txt">pc.txt</a></td><td style="text-align:right;">209 days ago</td></tr><tr><td><a href="psc.tix">psc.tix</a></td><td style="text-align:right;">18 days ago</td></tr><tr><td><a href="sum">sum</a></td><td style="text-align:right;">1 year ago</td></tr><tr><td><a href="sum-money2.py">sum-money2.py</a></td><td style="text-align:right;">1 year ago</td></tr><tr><td><a href="template.txt">template.txt</a></td><td style="text-align:right;">2 days ago</td></tr></table></div></body></html>
"""


htmlFromLighttpd : String
htmlFromLighttpd = """
<!--?xml version="1.0" encoding="iso-8859-1"?-->
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en"><head>
<title>Index of /</title>
<style type="text/css">
a, a:active {text-decoration: none; color: blue;}
a:visited {color: #48468F;}
a:hover, a:focus {text-decoration: underline; color: red;}
body {background-color: #F5F5F5;}
h2 {margin-bottom: 12px;}
table {margin-left: 12px;}
th, td { font: 90% monospace; text-align: left;}
th { font-weight: bold; padding-right: 14px; padding-bottom: 3px;}
td {padding-right: 14px;}
td.s, th.s {text-align: right;}
div.list { background-color: white; border-top: 1px solid #646464; border-bottom: 1px solid #646464; padding-top: 10px; padding-bottom: 14px;}
div.foot { font: 90% monospace; color: #787878; padding-top: 4px;}
</style>
</head>
<body>
<h2>Index of /</h2>
<div class="list">
<table summary="Directory Listing" cellpadding="0" cellspacing="0">
<thead><tr><th class="n">Name</th><th class="m">Last Modified</th><th class="s">Size</th><th class="t">Type</th></tr></thead>
<tbody>
<tr><td class="n"><a href="../">Parent Directory</a>/</td><td class="m">&nbsp;</td><td class="s">- &nbsp;</td><td class="t">Directory</td></tr>
<tr><td class="n"><a href="10/">10</a>/</td><td class="m">2014-Apr-09 09:54:23</td><td class="s">- &nbsp;</td><td class="t">Directory</td></tr>
<tr><td class="n"><a href="11/">11</a>/</td><td class="m">2014-Apr-09 09:54:23</td><td class="s">- &nbsp;</td><td class="t">Directory</td></tr>
<tr><td class="n"><a href="12/">12</a>/</td><td class="m">2014-Apr-09 09:54:24</td><td class="s">- &nbsp;</td><td class="t">Directory</td></tr>
<tr><td class="n"><a href="13/">13</a>/</td><td class="m">2014-May-18 10:43:10</td><td class="s">- &nbsp;</td><td class="t">Directory</td></tr>
<tr><td class="n"><a href="14/">14</a>/</td><td class="m">2015-May-16 09:26:26</td><td class="s">- &nbsp;</td><td class="t">Directory</td></tr>
<tr><td class="n"><a href="old/">old</a>/</td><td class="m">2014-Apr-09 09:54:24</td><td class="s">- &nbsp;</td><td class="t">Directory</td></tr>
<tr><td class="n"><a href="payslips/">payslips</a>/</td><td class="m">2015-Oct-23 10:03:13</td><td class="s">- &nbsp;</td><td class="t">Directory</td></tr>
<tr><td class="n"><a href=".local.text.vimrc">.local.text.vimrc</a></td><td class="m">2013-Oct-07 22:37:33</td><td class="s">0.1K</td><td class="t">application/octet-stream</td></tr>
<tr><td class="n"><a href="15-05.txt">15-05.txt</a></td><td class="m">2015-Jun-16 21:52:18</td><td class="s">5.2K</td><td class="t">text/plain</td></tr>
<tr><td class="n"><a href="15-06.txt">15-06.txt</a></td><td class="m">2015-Jul-15 15:50:49</td><td class="s">3.6K</td><td class="t">text/plain</td></tr>
<tr><td class="n"><a href="15-07.txt">15-07.txt</a></td><td class="m">2015-Sep-27 10:04:28</td><td class="s">3.9K</td><td class="t">text/plain</td></tr>
<tr><td class="n"><a href="15-08.txt">15-08.txt</a></td><td class="m">2015-Sep-14 21:56:12</td><td class="s">3.3K</td><td class="t">text/plain</td></tr>
<tr><td class="n"><a href="15-09.txt">15-09.txt</a></td><td class="m">2015-Oct-13 21:49:09</td><td class="s">2.9K</td><td class="t">text/plain</td></tr>
<tr><td class="n"><a href="15-10.txt">15-10.txt</a></td><td class="m">2015-Oct-23 14:17:26</td><td class="s">2.8K</td><td class="t">text/plain</td></tr>
<tr><td class="n"><a href="15-11-.txt">15-11-.txt</a></td><td class="m">2015-Oct-23 10:03:00</td><td class="s">0.9K</td><td class="t">text/plain</td></tr>
<tr><td class="n"><a href="animate-point.txt">animate-point.txt</a></td><td class="m">2013-Oct-07 22:37:53</td><td class="s">0.1K</td><td class="t">text/plain</td></tr>
<tr><td class="n"><a href="hasmoku.txt">hasmoku.txt</a></td><td class="m">2015-Jan-31 14:01:01</td><td class="s">1.7K</td><td class="t">text/plain</td></tr>
<tr><td class="n"><a href="pc.txt">pc.txt</a></td><td class="m">2015-Mar-30 15:06:03</td><td class="s">0.4K</td><td class="t">text/plain</td></tr>
<tr><td class="n"><a href="psc.tix">psc.tix</a></td><td class="m">2015-Oct-07 10:08:44</td><td class="s">63.1K</td><td class="t">application/octet-stream</td></tr>
<tr><td class="n"><a href="sum">sum</a></td><td class="m">2014-May-25 16:47:20</td><td class="s">2.1M</td><td class="t">application/octet-stream</td></tr>
<tr><td class="n"><a href="sum-money2.py">sum-money2.py</a></td><td class="m">2014-Apr-28 03:03:05</td><td class="s">5.2K</td><td class="t">application/octet-stream</td></tr>
<tr><td class="n"><a href="template.txt">template.txt</a></td><td class="m">2015-Oct-23 10:03:02</td><td class="s">0.9K</td><td class="t">text/plain</td></tr>
</tbody>
</table>
</div>
<div class="foot">lighttpd/1.4.35</div>


</body></html>
"""
