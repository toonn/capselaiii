-- An artificial bee colony algorithm for the maximally diverse grouping problem
-- Francisco J. Rodriguez, M. Lozano, C. Garcia-martinez,
-- Jonathan D. Gonzalez-Barrera
-- January 2013

-- Stolen from Criterion.Measurement
import Data.Time.Clock.POSIX (getPOSIXTime)
getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime
-- No more stolen code from this point on.

data Distances = MkDist [(String, String, Double)]
data Group = MkGroup {ag::Integer, bg::Integer, members::[String]} deriving Show
type Groups = [Group]
type Solution = [Group]

main :: IO ()
main = do
    file = readFile "./mdgplib/Geo/Geo_n010_ds_01.txt"
    (header:dists) = lines file

abc :: Distances -> Groups -> Integer -> Integer -> Integer 
        -> Double -> Integer -> Solution 
-- Initialization phase
abc distances groups m limit np pls tmax =
    best_solution_found (abc_ distances groups m limit np pls tstart tmax nrng
                                initial_solutions)
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


construct_solution :: Integer m -> StdGen rng
                        -> (nrng, Solution s)

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

