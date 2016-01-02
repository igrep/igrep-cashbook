module Result.Extra where

import List


partitionList : List (Result error value) -> (List error, List value)
partitionList results =
  let partitioner result (errors, values) =
        case result of
          Ok  value -> (       errors, value::values)
          Err error -> (error::errors,        values)
  in
  List.foldr partitioner ([], []) results
