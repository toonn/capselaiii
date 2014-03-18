-- An artificial bee colony algorithm for the maximally diverse grouping problem
-- Francisco J. Rodriguez, M. Lozano, C. Garcia-martinez,
-- Jonathan D. Gonzalez-Barrera
-- January 2013

-- Stolen from Criterion.Measurement
import Data.Time.Clock.POSIX (getPOSIXTime)
getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime
-- No more stolen code from this point on.

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List

data Distances = MkDist [(String, String, Double)]
data Group = MkGroup {ag::Integer, bg::Integer, members::[String]}
    deriving Show
type Groups = [Group]
type Solution = [Group]

main :: IO Solution
main = do
    file <- readFile "/tmp/mdgplib/Geo/Geo_n010_ds_01.txt"
    return $ abc distances elems groups m limit np pls tmax
    where
        (header:dists) = lines file
        (nstring:(mstring:(grouptypestring:grouplimitstrings))) = words header
        m = read mstring :: Integer
        grouplimits = map (\x -> read x :: Integer) grouplimitstrings
        groups = groups_from_limits grouplimits
        distances = map_from_list dists
        limit = np
        np = 20
        pls = 0.5
        tmax = 10
        elems = nub $ map (\x -> head $ words x) dists


groups_from_limits :: [Integer] -> Groups
groups_from_limits [] = []
groups_from_limits (ag:(bg:limits)) =
    (MkGroup ag bg []):(groups_from_limits limits)

map_from_list :: [String] -> Map.Map
map_from_list [] = Map.empty
map_from_list (pair:pairs) =
    Map.insert key value (map_from_list pairs)
    where
        (elem1:(elem2:diststring)) = words pair
        key = Set.fromList [elem1, elem2]
        value = read diststring :: Double

abc :: Distances -> [String] -> Groups -> Integer -> Integer -> Integer 
        -> Double -> Integer -> Solution 
-- Initialization phase
abc distances elems groups m limit np pls tmax =
    best_solution_found (abc_ distances elems groups m limit np pls
                            tstart tmax nrng initial_solutions)
        where
            tstart = getTime
            rng = mkStdGen 123
            (nrng, initial_solutions) = init_solutions n m np pls


abc_ :: Integer m -> Integer limit -> Integer np
        -> Double pls -> Integer tstart -> Integer tmax -> StdGen g
        -> [Solution] -> [Solution]


init_solutions :: Integer -> Integer -> Double -> StdGen
                    -> (StdGen, [Solution])
init_solutions m np pls rng =
    (nrng, sol:sols)
    where
        (trng, sol) = construct_solution m rng
        (nrng, sols) = init_solutions m (np - 1) pls trng


construct_solution :: Map.Map -> [String] -> Groups -> StdGen
                        -> (StdGen, Solution)
construct_solution distances elems groups rng =
    (nrng, ngroups)
    where
        ([], ngroups, nrng) =
            fill distances elems_m groups_m rng_m
        (elems_m, groups_m, rng_m) = choose_m distances elems groups rng

fill :: Map.Map -> [String] -> Groups -> StdGen
            -> ([String], Groups, StdGen)
fill distances [] groups rng = ([], groups, rng)
fill distances elems groups rng = 
    fill distances nelems (nrgroup:restGroups) nrng
    where
        gfilter gs | smaller ag gs /= [] = smaller ag gs
                   | otherwise = smaller bg gs
        smaller boundFun = filter (\g -> length (members g) < boundFun g)
        randomGroup gs = (rg, restgs, trng)
            where
                (rindex, trng) = randomR (0, (length groups) - 1) rng
                rg = gs!!rindex
                restgs = delete rg gs

        (rgroup, restGroups, nrng) = randomGroup $ gfilter groups

        nelem = maximumBy (comparing (\elem -> diversity elem rgroup)) elems
        nelems = delete nelem elems
        nrgroup = ginsert rgroup nelem


choose_m :: Map.Map -> [String] -> Groups -> StdGen
            -> ([String], Groups, StdGen)
choose_m distances elems [] rng = (elems, [], rng)
choose_m distances elems (group:groups) rng =
    (nelems, ngroup:ngroups, nrng)
    where
        (rindex, trng) = randomR (0, (length elems) - 1) rng
        relem = elems!!rindex
        telems = delete relem elems
        ngroup = ginsert group relem
        (nelems, ngroups, nrng) = choose_m distances telems groups trng

ginsert :: Group -> String -> Group
ginsert group elem = group {members = elem:(members group)}

diversity :: String element -> Group g -> Double div
diversity = sum $ map (distance elem) g

distance :: String i -> String j -> Double dist

fitness :: Solution s -> Double z
fitness = sum $ map fitness_group

fitness_group :: Group g -> Double delta
fitness_group [] = 0
fitness_group [elem] = 0
fitness_group (elem:restG) = diversity elem restG + fitness_group restG

local_improvement :: Solution s_in -> Solution s_out

generate_neigbouring :: Solution s_in -> Solution e

binary_tournament :: [Solution] -> Solution j

best_solution_found :: [Solution] -> Solution bestS

