module IgrepCashbook.Line
  ( Model
  , SuccessLine
  , WrongLine
  , parse
  , parseList
  , errorNoName
  , errorNoGroup
  , errorInvalidPrice
  , errorNoSeparatorAfterPrice
  , errorNoSeparatorAfterName
  ) where

import Combine exposing (many1)
import Combine.Infix exposing (..)
import Combine.Char exposing (char, space)
import Debug
import String
import List
import Regex exposing (Regex)
import Result

type alias Model = Result WrongLine SuccessLine

type alias SuccessLine =
  { price : Int
  , group : String
  }

type alias WrongLine =
  { errorMessage : String
  , content : String
  }


parseList : List String -> List Model
parseList =
  List.map eraseComment
    >> List.filter (not << isIgnored)
    >> List.map parse


parse : String -> Model
parse line =
  let nameField = String.trimLeft <$> Combine.regex "(\\S| (?! ))+" <?> errorNoName
      groupField = String.trimRight <$> Combine.regex "\\S+" <?> errorNoGroup
      sign = Combine.optional -1 (1 <$ char '+')
      priceParser =
        (*)
          <$> sign
          <*> (
            (toInt << noSeparator)
              <$> (Combine.regex "[1-9][_,\\d]*" <?> errorInvalidPrice)
          )
      parser =
        SuccessLine
          <$  nameField
          <*  (twoOrMoreSpaces <?> errorNoSeparatorAfterName)
          <*> priceParser
          <*  (twoOrMoreSpaces <?> errorNoSeparatorAfterPrice)
          <*> groupField
      (result, _) = Combine.parse parser line
  in
      case result of
        Combine.Done successLine ->
          Ok successLine
        Combine.Fail errorMessages ->
          Err <| WrongLine (String.join ", " errorMessages) line


twoOrMoreSpaces : Combine.Parser ()
twoOrMoreSpaces =
  () <$ Combine.regex " {2,}"


isIgnored : String -> Bool
isIgnored line =
  String.isEmpty line || Regex.contains dateRegex line


eraseComment : String -> String
eraseComment =
  Regex.replace (Regex.AtMost 1) commentRegex (always "")


recordRegex : Regex
recordRegex =
  Regex.regex "^(\\S+) {2,}(\\S*) {2,}(\\S*)"


dateRegex : Regex
dateRegex =
  Regex.regex "^(\\d\\d/)?\\d\\d/\\d\\d"


commentRegex : Regex
commentRegex =
  Regex.regex "#.*"


priceSeparatorRegex : Regex
priceSeparatorRegex =
  Regex.regex "[,_]"


toInt : String -> Int
toInt =
  String.toInt
    >> Result.withDefault 0


noSeparator : String -> String
noSeparator = Regex.replace Regex.All priceSeparatorRegex (always "")

errorInvalidPrice : String
errorInvalidPrice = "Invalid price field"


errorNoSeparatorAfterName : String
errorNoSeparatorAfterName = "No separator between name and price"


errorNoSeparatorAfterPrice : String
errorNoSeparatorAfterPrice = "No separator between price and group"


errorNoName : String
errorNoName = "No name field"


errorNoGroup : String
errorNoGroup = "No group field"
