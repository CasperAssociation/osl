-- This compiler stage takes an arithmetic circuit and removes the gates from the
-- lookup arguments (i.e., it sets them to zero).
--
-- CAUTION: This stage assumes (without checking it) that all lookup argument
-- gate polynomials can only evaluate to zero or one in a satisfying assignment
-- of values.
--
-- NOTICE: This stage assumes (and checks) that all polynomial variables have a
-- relative row offset of zero.
--
-- This stage works by adding to the circuit:
--  * One dummy row, which contains all zeroes in the pre-existing fixed columns.
--  * One dummy row indicator fixed column, which contains all zeroes in the pre-existing
--    rows and one in the dummy row.
--  * A term in each gate constraint which sets the gate value to zero in the dummy row
--    and does not change the gate value in any other row.
--  * A term in each lookup argument input expression which sets the value to zero in
--    the dummy row and does not change the value in any other row.
--
-- Instances and witnesses are compiled into the instances and witnesses for the new
-- circuit by adding zeroes in the new dummy row.


{-# LANGUAGE OverloadedStrings #-}


module Halo2.RemoveLookupGates
  ( removeLookupGates,
    removeLookupGatesArgumentConversion
  ) where


import Die (die)
import Halo2.Types.Argument (Argument)
import Halo2.Types.Circuit (ArithmeticCircuit)
import OSL.Types.ErrorMessage (ErrorMessage)


removeLookupGates ::
  ArithmeticCircuit ->
  Either (ErrorMessage ()) ArithmeticCircuit
removeLookupGates = todo


removeLookupGatesArgumentConversion ::
  ArithmeticCircuit ->
  Argument ->
  Either (ErrorMessage ()) Argument
removeLookupGatesArgumentConversion = todo


todo :: a
todo = die "todo"
