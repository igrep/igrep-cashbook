module IgrepCashbook.Line exposing
  ( Model
  , Success
  , Wrong
  , liWrong
  , parse
  , parseList
  , errorNoName
  , errorNoGroup
  , errorInvalidPrice
  , errorNoSeparatorAfterPrice
  , errorNoSeparatorAfterName
  )

import Combine exposing (many1)
import Combine.Infix exposing (..)
import Combine.Char exposing (char, space)
import Debug
import Html exposing (..)
import String
import List
import Regex exposing (Regex)
import Result

type alias Model = Result Wrong Success

type alias Success =
  { price : Int
  , group : String
  }

type alias Wrong =
  { at : Int
  , errorMessage : String
  , content : String
  }


liWrong : Wrong -> Html msg
liWrong wl =
  li [] [text <| wl.errorMessage ++ " at line " ++ toString wl.at ++ ": " ++ toString wl.content]

parseList : List String -> List Model
parseList =
  List.indexedMap (\i l -> (i + 1, eraseComment l))
    >> List.filter (not << isIgnored << snd)
    >> List.map (uncurry parse)


parse : Int -> String -> Model
parse lineNumber line =
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
        Success
          <$  nameField
          <*  (twoOrMoreSpaces <?> errorNoSeparatorAfterName)
          <*> priceParser
          <*  (twoOrMoreSpaces <?> errorNoSeparatorAfterPrice)
          <*> groupField
      (result, _) = Combine.parse parser line
  in
      case result of
        Ok successLine ->
          Ok successLine
        Err errorMessages ->
          Err <| Wrong lineNumber (String.join ", " errorMessages) line


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
