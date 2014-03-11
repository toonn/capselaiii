-- An artificial bee colony algorithm for the maximally diverse grouping problem
-- Francisco J. Rodriguez, M. Lozano, C. Garcia-martinez,
-- Jonathan D. Gonzalez-Barrera
-- January 2013

-- Stolen from Criterion.Measurement
import Data.Time.Clock.POSIX (getPOSIXTime)
getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime
-- No more stolen code from this point on.

data Group = (ag, bg, [String])
type Solution = [Group]

abc :: Integer n -> Integer m -> Integer limit -> Integer np
        -> Double pls -> Integer tmax -> Solution bestS
-- Initialization phase
abc n m limit np pls tmax =
    best_solution_found (abc_ n m limit np pls tstart tmax rng
                                initial_solutions)
        where
            tstart = getTime
            rng = mkStdGen 123
            (nrng, initial_solutions) = init_solutions n m np pls


abc_ :: Integer n -> Integer m -> Integer limit -> Integer np
        -> Double pls -> Integer tstart -> Integer tmax -> StdGen g
        -> [Solution] -> [Solution]


init_solutions :: n -> m -> np -> pls -> StdGen rng
                    -> (nrng, [Solution])
init_solutions n m np pls rng =
    (nrng, sol:sols)
    where
        (trng, sol) = construct_solution n m rng
        (nrng, sols) = init_solutions n m (np - 1) pls trng


construct_solution :: Integer n -> Integer m -> StdGen rng
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
