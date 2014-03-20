-- An artificial bee colony algorithm for the maximally diverse grouping problem
-- Francisco J. Rodriguez, M. Lozano, C. Garcia-martinez,
-- Jonathan D. Gonzalez-Barrera
-- January 2013

import System.Random
import Data.Ord
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Stolen from Criterion.Measurement
import Data.Time.Clock.POSIX (getPOSIXTime)
getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime
-- No more stolen code from this point on.

mkset :: Ord a => a -> a -> Set.Set a
mkset x y = Set.fromList [x, y]

type Element = String
type Elements = [Element]
data Group = MkGroup {ag::Integer, bg::Integer, members::Elements}
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
        key = mkset elem1 elem2
        value = read diststring :: Double

abc :: Distances -> Elements -> Groups -> Integer -> Integer -> Integer 
        -> Double -> Integer -> Solution 
-- Initialization phase
abc distances elems groups m limit np pls tmax =
    best_solution_found (abc_ distances elems groups m limit np pls
                            tstart tmax nrng initial_solutions)
        where
            tstart = getTime
            rng = mkStdGen 123
            (nrng, initial_solutions) = init_solutions m np pls


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


construct_solution :: Map.Map -> Elements -> Groups -> StdGen
                        -> (StdGen, Solution)
construct_solution distances elems groups rng =
    (nrng, ngroups)
    where
        ([], ngroups, nrng) =
            fill distances elems_m groups_m rng_m
        (elems_m, groups_m, rng_m) = choose_m distances elems groups rng

fill :: Map.Map -> Elements -> Groups -> StdGen
            -> (Elements, Groups, StdGen)
fill distances [] groups rng = ([], groups, rng)
fill distances elems groups rng = 
    fill distances nelems (nrgroup:restGroups) nrng
    where
        gfilter gs | smaller ag gs /= [] = smaller ag gs
                   | otherwise = smaller bg gs
        smaller boundFun = filter (\g -> length (members g) < boundFun g)

        (rgroup, restGroups, nrng) = randomElem (gfilter groups) rng

        nelem = maximumBy
                    (comparing (\el -> diversity distances el rgroup))
                    elems
        nelems = delete nelem elems
        nrgroup = ginsert rgroup nelem

randomElem :: [a] -> StdGen -> (a, [a], StdGen)
randomElem gs rng = (rg, restgs, nrng)
    where
        (rindex, nrng) = randomR (0, (length gs) - 1) rng
        rg = gs!!rindex
        restgs = delete rg gs


choose_m :: Map.Map -> Elements -> Groups -> StdGen
            -> (Elements, Groups, StdGen)
choose_m distances elems [] rng = (elems, [], rng)
choose_m distances elems (group:groups) rng =
    (nelems, ngroup:ngroups, nrng)
    where
        (rindex, trng) = randomR (0, (length elems) - 1) rng
        relem = elems!!rindex
        telems = delete relem elems
        ngroup = ginsert group relem
        (nelems, ngroups, nrng) = choose_m distances telems groups trng

ginsert :: Group -> Element -> Group
ginsert group el = group {members = el:(members group)}

diversity :: Map.Map -> Element -> Group -> Double
diversity distances el group =
    sum $ map (\e -> lookup (mkset e el) distances) group

fitness :: Map.Map -> Solution -> Double
fitness distances = sum $ map (fitness_group distances)

fitness_group :: Map.Map -> Group -> Double
fitness_group distances [] = 0
fitness_group distances [el] = 0
fitness_group distances (el:restG) =
    diversity distances el restG + fitness_group restG

local_improvement :: Map.Map -> Solution -> Solution 
local_improvement distances s_in =
    li_swap distances s_move s_move (members $ head s_move)
    where
        elems = sort $ Set.toList (unions $ keys distances)
        s_move = li_move distances s_in elems

li_move :: Map.Map -> Solution -> Elements -> Solution
li_move distances s_in [] = s_in
li_move distances s_in (el:elems)
    | first_improv_index == Nothing = li_move distances s_in elems
    | otherwise =
        local_improvement
            distances
            (apply_transform s_in elgroup figroup $ move el elgroup figroup)
    where
        elgroup = filter (elem el $ members) s_in
        fs = map (\g -> (- diversity distances el elgroup)
                            + diversity distances el (snd $ move el elgroup g))
                    s_in
        first_improv_index = findIndex (> 0) fs
        figroup = (s_in!! $ fromJust first_improv_index)

move :: Element -> Group -> Group -> (Group, Group)
move el og ng = (og {members = (delete el og)}, ng {members = el:(members ng)})

li_swap :: Map.Map -> Solution -> Groups -> Elements -> Solution
li_swap distances s_in [g] elems = s_in
li_swap distances s_in (group:groups) [] =
    li_swap distances s_in groups (members $ head groups)
li_swap distances s_in (group:groups) (el:elems)
    | first_improv_group_index == Nothing =
        li_swap distances s_in (group:groups) elems
    | otherwise =
        li_swap distances s_new s_new (members $ head s_new)
    where
        fs = map (\g ->
                map (\el2 -> let (ngroup, ng) = swap el el2 group g
                    in  (- diversity distances el group)
                        - diversity el2 g
                        + diversity el ng
                        + diversity el2 ngroup)
                    (members g))
                groups
        first_improv_group_index = findIndex (isJust $ findIndex (> 0)) fs
        first_improv_group = (fs!! $ fromJust first_improv_group_index)
        first_improv_index =
            findIndex (> 0) first_improv_group
        first_improv_elem = first_improv_group!! $ fromJust first_improv_index
        s_new = apply_transform
                    s_in
                    group
                    first_improv_group
                    (swap el first_improv_elem group first_improv_group)
    

swap :: Element -> Element -> Group -> Group -> (Group, Group)
swap el1 el2 = uncurry (move el2) $ move el1

apply_transform :: Solution -> Group -> Group -> (Group, Group) -> Solution
apply_transform s_in g1 g2 (ng1, ng2) =
    ng1:(ng2:(delete g1 (delete g2 s_in)))

generate_neighbouring :: Map.Map -> Solution -> Integer
                        -> [(Solution -> Double -> Solution)]
                        -> StdGen
                        -> (Solution, StdGen)
generate_neighbouring distances s_in ndp nos rng = (s_out, nrng)
    where
        (rindex, trng) = randomR (0, length nos - 1) rng
        rno = nos!!rindex
        (s_out, nrng) = rno distances s_in ndp trng

no1 :: Map.Map -> Solution -> Integer -> StdGen -> (Solution, StdGen)
no1 distances s_in nd rng = (s_out, nrng)
    where
        (rn, trng) = randomR (1, nd) rng
        (groups, elems, ttrng) = no1_ distances s_in [] rn trng
        ([], s_out, nrng) = fill distances elems groups ttrng

no1_ :: Map.Map -> Groups -> Elements -> Integer -> StdGen
            -> (Groups, Elements, StdGen)
no1_ distances s_in elems 0 rng = (s_in, elems, rng)
no1_ distances s_in elems n rng =
    no1_ distances (nelem:elems) (nrgroup:restGroups) (n-1) nrng
    where
        nemptygroups = filter (\g -> not $ null $ members g) s_in
        (rgroup, restGroups, trng) = randomElem nemptygroups rng
        (nelem, restElems, nrng) = randomElem (members rgroup) trng
        nrgroup = rgroup {members = restElems}
        
no2 :: Map.Map -> Solution -> Integer -> StdGen -> (Solution, StdGen)
no2 distances s_in nd rng = (s_out, nrng)
    where
        (rn, trng) = randomR (1, nd) rng
        (groups, elems) = no2_ distances s_in [] rn
        ([], s_out, nrng) = fill distances elems groups trng

no2_ :: Map.Map -> Groups -> Elements -> Integer -> (Groups, Elements)
no2_ distances groups elems 0 = (groups, elems)
no2_ distances groups elems n = no2_ distances tgroups (nelem:elems) (n-1)
    where
        (tgroups, nelem) = least_diverse distances groups
    

least_diverse :: Map.Map -> Groups -> (Groups, Element)
least_diverse distances groups = (ngroups, nelem)
    where
        nelem = minimumBy
            (comparing
                (\el -> diversity
                        distances
                        el
                        (delete el (head (filter (elem el $ members) groups)))))
            (least_diverse_ distances groups)
        ngroups = map
                    (\g -> if elem nelem g
                            then g {members = delete nelem g}
                            else g)
                    groups

least_diverse_ :: Map.Map -> Groups -> Elements
least_diverse_ distances [] = []
least_diverse_ distances (group:groups) =
        (least_diverse__ distances group):(least_diverse_ distances groups)

least_diverse__ :: Map.Map -> Group -> Element
least_diverse__ distances group =
    minimumBy
        (comparing (\el -> diversity
                            distances
                            el
                            (group {members = (delete el $ members group)})))
        (members group)

no3 :: Map.Map -> Solution -> Integer -> StdGen -> (Solution, StdGen)
no3 distances s_in p rng = no3_ s_in rp trng
    where
        (rp, trng) = randomR (1, p) rng
        
no3_ :: Map.Map -> Groups -> Integer -> StdGen
            -> (Groups, Elements, StdGen)
no3_ distances groups 0 rng = (groups, rng)
no3_ distances groups q rng = no3_ distances ngroups (q-1) nrng
    where
        (rindex1, trng) = randomR (0, length groups - 1) rng
        rgroup1 = groups!!rindex1
        (rindex2, ttrng) = randomR (0, length (delete rgroup1 groups) - 1) trng
        rgroup2 = (delete rgroup1 groups)!!rindex2
        (rindex11, tttrng) = randomR (0, length rgroup1 - 1) ttrng
        relem1 = (members rgroup1)!!rindex11
        (rindex21, nrng) = randomR (0, length rgroup2 - 1) tttrng
        relem2 = (members rgroup2)!!rindex21
        ngroups =
            (rgroup1 {members = relem2:(delete relem1 (members rgroup1))})
            :((rgroup2 {members = relem1:(delete relem2 (members rgroup2))})
                :(delete rgroup1 (delete rgroup2 groups)))


binary_tournament :: distances -> [Solution] -> StdGen -> (Solution, StdGen)
binary_tournament distances sols rng = best_solution_found [rsol1, rsol2]
    where
        (rindex1, trng) = randomR (0, length sols - 1) rng
        rsol1 = sols!!rindex1
        (rindex2, nrng) = randomR (0, length (delete rsol1 sols) - 1) trng
        rsol2 = (delete rsol1 sols)!!rindex2

best_solution_found :: Map.Map -> [Solution] -> Solution
best_solution_found distances = maximumBy (comparing (fitness distances))
