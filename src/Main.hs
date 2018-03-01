{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Main where

import           Data.List
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as M
import           Data.Ord           (comparing)
import           System.Environment (getArgs)

main :: IO ()
main = do
  [file          ] <- getArgs
  (constants:rest) <- lines <$> readFile file
  let [rows, cols, vehicles, numRides, rideBonus, steps] =
        read <$> words constants
      rides = fmap parseRide . zip [0 ..] $ take numRides rest
  putStrLn . printResult $ go rows cols vehicles rideBonus steps rides
 where
  parseRide (rideId, ride) =
    let [a, b, x, y, s, f] = read <$> words ride in Ride rideId a b x y s f

data Ride = Ride
  { rideId   :: Int
  , startRow :: Int
  , startCol :: Int
  , endRow   :: Int
  , endCol   :: Int
  , start    :: Int
  , finish   :: Int
  } deriving (Eq, Show)

printResult :: Map Int [Int] -> String
printResult m = unlines $ printSingleResult <$> M.toAscList m

printSingleResult :: (Int, [Int]) -> String
printSingleResult (k, rides) = unwords $ show (length rides) : map show rides

go :: Int -> Int -> Int -> Int -> Int -> [Ride] -> Map Int [Int]
go rows cols vehicles rideBonus steps rides = assignment
  where (assignment, _, _) = foldl step initialState [0..steps]
        initialState = (M.empty, M.fromList $ map (, (0, (0, 0))) [1..vehicles], rides)

type State = (Map Int [Int], Map Int (Int, (Int, Int)), [Ride])

step :: State -> Int -> State
step state@(_, vehicleState, _) time =
  let freeVehicles = filter ((<= time) . fst . snd) (M.toList vehicleState)
  in foldl (flip (assignRide time)) state freeVehicles

assignRide :: Int -> (Int, (Int, (Int, Int))) -> State -> State
assignRide time (vehicleId, (_, currentCoord)) state@(assignedRides, vehicleState, remaining)
  = maybe
    state
    ( \ride@Ride {..} ->
      ( M.insertWith (flip (++)) vehicleId [rideId] assignedRides
      , M.adjust
        ( \(_, currentCoord) ->
          ( startTime currentCoord time ride
            + coordDiff (startRow, startCol) (endRow, endCol)
          , (endRow, endCol)
          )
        )
        vehicleId
        vehicleState
      , remaining'
      )
    )
    ride
  where (ride, remaining') = pickValidRide currentCoord time remaining

pickValidRide :: (Int, Int) -> Int -> [Ride] -> (Maybe Ride, [Ride])
pickValidRide currentCoord time rides = (ride, invalidRides ++ rest)
 where
  (validRides, invalidRides) = partition
    (\ride -> canCompleteRide currentCoord time ride && rideLessThan 17000 ride)
    rides
  (ride, rest) = case validRides of
    [] -> (Nothing, [])
    validRides' ->
      let best = earliestStart currentCoord time validRides'
      in  (Just best, delete best validRides')

coordDiff :: (Int, Int) -> (Int, Int) -> Int
coordDiff (a, b) (x, y) = abs (x - a) + abs (y - b)

canCompleteRide :: (Int, Int) -> Int -> Ride -> Bool
canCompleteRide currentCoord time ride@Ride {..} =
  startTime currentCoord time ride
    + coordDiff (startRow, startCol) (endRow, endCol)
    < finish

earliestStart :: (Int, Int) -> Int -> [Ride] -> Ride
earliestStart currentCoord time rides =
  minimumBy (comparing (startTime currentCoord time)) rides

startTime :: (Int, Int) -> Int -> Ride -> Int
startTime currentCoord time Ride{..} =
  max (time + coordDiff currentCoord (startRow, startCol)) start

rideLessThan :: Int -> Ride -> Bool
rideLessThan d Ride{..} = coordDiff (startRow, startCol) (endRow, endCol) < d
