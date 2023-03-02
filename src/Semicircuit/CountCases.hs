{-# LANGUAGE OverloadedStrings #-}


module Semicircuit.CountCases
  ( countCases
  ) where


import Die (die)
import OSL.Types.ErrorMessage (ErrorMessage)
import Semicircuit.Types.Semicircuit (Semicircuit)
import Trace.Types (NumberOfCases)


countCases :: Semicircuit -> Either (ErrorMessage ()) NumberOfCases
countCases = todo


todo :: a
todo = die "todo"
