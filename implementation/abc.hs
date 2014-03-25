-- An artificial bee colony algorithm for the maximally diverse grouping problem
-- Francisco J. Rodriguez, M. Lozano, C. Garcia-martinez,
-- Jonathan D. Gonzalez-Barrera
-- January 2013

import System.Random
import System.Environment
import Data.Ord
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.IORef
import System.Timeout
import Debug.Trace (trace)

mkset :: Ord a => a -> a -> Set.Set a
mkset x y = Set.fromList [x, y]

type Distances = Map.Map (Set.Set Element) Double
type Element = String
type Elements = [Element]
data Group = MkGroup {ag::Integer, bg::Integer, members::Elements}
    deriving (Show, Eq)
type Groups = [Group]
type Solution = [Group]

main :: IO ()
main = do
    home <- getHomeDirectory
    geo <- getDirectoryContents $ home ++ "/mdgplib/Geo/"
    let geoN = sort $ filter (isInfixOf "_01.") geo
    map runOnce geoN
    

runOnce :: String -> IO Solution
runOnce filename = do
    file <- readFile filename
    let (header:dists) = lines file
    let (nstring:(mstring:(grouptypestring:grouplimitstrings))) = words header
    let m = read mstring :: Integer
    let grouplimits = map (\x -> read x :: Integer) grouplimitstrings
    let groups = groups_from_limits grouplimits
    let distances = map_from_list dists
    let elems = nub $ concatMap (\x -> take 2 $ words x) dists
    let nos = [no1]
    best_solution <- abc distances elems groups limit np ndp pls nos tmax
    print $ filename ++ ":" ++ (fitness distances $ readIORef best_solution)
    readIORef best_solution
    where
        limit = 2*120
        np = 20
        pls = 0.1
        tmax = 1000000*20
        ndp = 8


groups_from_limits :: [Integer] -> Groups
groups_from_limits [] = []
groups_from_limits (ag:(bg:limits)) =
    (MkGroup ag bg []):(groups_from_limits limits)

map_from_list :: [String] -> Distances
map_from_list [] = Map.empty
map_from_list (pair:pairs) =
    Map.insert key value (map_from_list pairs)
    where
        [elem1, elem2, diststring] = words pair
        key = mkset elem1 elem2
        value = read diststring :: Double

abc :: Distances -> Elements -> Groups -> Integer -> Integer -> Integer -> Double
            -> [(Distances -> Solution -> Integer -> StdGen -> (Solution, StdGen))]
            -> Int -> IO (IORef Solution)
-- Initialization phase
abc distances elems groups limit np ndp pls nos tmax =
    do
        --print "abc"
        best_sol <- newIORef (best_solution_found distances initial_sols)
        timeout tmax (seq initial_sols $ abc_ distances elems groups limit ndp pls nos
                        iterations initial_sols nrng best_sol)
        return best_sol
        where
            rng = mkStdGen 123
            (nrng, initial_sols) =
                init_solutions distances elems groups np pls rng
            iterations = zipWith const [0..] initial_sols


abc_ :: Distances -> Elements -> Groups -> Integer -> Integer -> Double
            -> [(Distances -> Solution -> Integer -> StdGen -> (Solution, StdGen))]
            -> [Integer] -> [Solution] -> StdGen -> IORef Solution
            -> IO ()
abc_ distances elems groups limit ndp pls nos iterations sols rng best_sol =
    do
        --print "abc_"
        --print sols
        --print $ best_solution_found distances sols
        modifyIORef' best_sol ((best_solution_found distances) . (flip (:) nsols))
        --print niterations
        abc_ distances elems groups limit ndp pls nos
            (map (+1) niterations) nsols nrng best_sol
        where
            (nsols, niterations, nrng) = abc__ distances elems groups limit ndp
                                pls nos iterations sols rng

abc__ :: Distances -> Elements -> Groups -> Integer -> Integer -> Double
            -> [(Distances -> Solution -> Integer -> StdGen -> (Solution, StdGen))]
            -> [Integer] -> [Solution] -> StdGen 
            -> ([Solution], [Integer], StdGen)
abc__ distances elems groups limit ndp pls nos iterations sols rng =
    (uncurry scouting) $ (uncurry onlooking) $ (employing sols rng)
    where
        employing = employed distances ndp pls nos
        onlooking = onlooker distances ndp pls nos
        scouting = scout distances elems groups pls limit iterations

employed :: Distances -> Integer -> Double
            -> [(Distances -> Solution -> Integer -> StdGen -> (Solution, StdGen))]
                -> [Solution] -> StdGen 
                -> ([Solution], StdGen)
employed distances ndp pls nos cur_sols rng =
    (map (\sol -> let
                    (neighbour_sol, trng) =
                        generate_neighbouring distances sol ndp nos rng
                    (imp_sol, ttrng) =
                        local_improvement distances pls neighbour_sol trng
                 in
                    if fitness distances imp_sol > fitness distances sol
                    then imp_sol
                    else sol)
        cur_sols,
    refresh rng rn)
    where
        (rn, _) = randomR (1, 1000) rng

onlooker :: Distances -> Integer -> Double
            -> [(Distances -> Solution -> Integer -> StdGen -> (Solution, StdGen))]
                -> [Solution] -> StdGen
                -> ([Solution], StdGen)
onlooker distances ndp pls nos cur_sols rng =
    onlooker_ distances cur_sols ndp pls nos rng (toInteger $ length cur_sols)

onlooker_ :: Distances -> [Solution] -> Integer -> Double
                -> [(Distances -> Solution -> Integer -> StdGen -> (Solution, StdGen))] -> StdGen -> Integer
                -> ([Solution], StdGen)
onlooker_ distances cur_sols ndp pls nos rng 0 = (cur_sols, rng)
onlooker_ distances cur_sols ndp pls nos rng count =
    if fitness distances imp_sol > fitness distances sol
    then onlooker_ distances (imp_sol:(delete sol cur_sols))
                ndp pls nos ttrng (count-1)
    else onlooker_ distances cur_sols ndp pls nos ttrng (count-1)
    where
                    (sol, trng) = binary_tournament distances cur_sols rng
                    (neighbour_sol, ttrng) =
                        generate_neighbouring distances sol ndp nos trng
                    (imp_sol, tttrng) =
                        local_improvement distances pls neighbour_sol ttrng

scout :: Distances -> Elements -> Groups -> Double
            -> Integer -> [Integer] -> [Solution] -> StdGen 
            -> ([Solution], [Integer], StdGen)
scout distances elems groups pls limit iterations cur_sols rng =
    (sols ++ nsols, orig_iters ++ niters, nrng)
    where
        (sols, orig_iters) = unzip $
                filter (\(s, iters) -> iters < limit) (zip cur_sols iterations)
        (nrng, nsols) = init_solutions distances elems groups
                            (toInteger $ length cur_sols - length sols) pls rng
        niters = take (length cur_sols - length sols) [0..]

refresh :: StdGen -> Integer -> StdGen
refresh rng 0 = rng
refresh rng n = refresh nrng (n-1)
    where
        (_, nrng) = random rng :: (Double, StdGen)


init_solutions :: Distances -> Elements -> Groups -> Integer -> Double -> StdGen
                    -> (StdGen, [Solution])
init_solutions distances elems groups 0 _ rng = (rng,[])
init_solutions distances elems groups np pls rng =
    (nrng, map (\s -> fst $ local_improvement distances pls s nrng) (sol:sols))
    where
        (trng, sol) = construct_solution distances elems groups rng
        (nrng, sols) = init_solutions distances elems groups (np - 1) pls trng


construct_solution :: Distances -> Elements -> Groups -> StdGen
                        -> (StdGen, Solution)
construct_solution distances elems groups rng =
    {- trace (show "cs: " ++ show elems ++ "\n" ++ show ngroups)-} (nrng, ngroups)
    where
        ([], ngroups, nrng) = if (1 == length groups_m) then error "consol" else
            fill distances elems_m groups_m rng_m
        (elems_m, groups_m, rng_m) = choose_m distances elems groups rng

-- Wijs elk element in elements toe aan een groep uit groups
fill :: Distances -> Elements -> Groups -> StdGen
            -> (Elements, Groups, StdGen)
--fill _ es gs _ | trace (show es ++ "\n" ++ show gs ++ "\n") False = undefined
fill _ es [g] _ = error $ (show es) ++ (show g)
fill distances [] groups rng = ([], groups, rng)
fill distances elems groups rng = 
    fill distances nelems (nrgroup:restGroups) nrng
    where
        -- Als alle groepen in gs nog niet tot aan ag gevuld zijn vul tot ag
        -- anders vul tot bg
        gfilter gs | (smaller ag) gs /= [] = (smaller ag) gs
                   | otherwise = (smaller bg) gs
        smaller boundFun = filter (\g -> length (members g) < (fromInteger $ boundFun g))
        -- Kies een random element uit de nog niet genoeg gevulde groepen
        (rgroup, restG, nrng) = if not $ null $ gfilter groups then randomElem (gfilter groups) rng else error $ show groups ++ show (gfilter groups)
        restGroups = delete rgroup groups
        -- Kies het element dat het meeste diversiteit toevoegt
        nelem = maximumBy
                    (comparing (\el -> diversity distances el $ members rgroup))
                    elems
        nelems = delete nelem elems
        nrgroup = ginsert rgroup nelem

randomElem :: Eq a => [a] -> StdGen -> (a, [a], StdGen)
randomElem [] _ = error "randomElem"
randomElem [g] rng = (g, [], rng)
randomElem gs rng = (rg, restgs, nrng)
    where
        (rindex, nrng) = randomR (0, (length gs) - 1) rng
        rg = gs!!rindex
        restgs = delete rg gs


choose_m :: Distances -> Elements -> Groups -> StdGen
            -> (Elements, Groups, StdGen)
choose_m distances elems [] rng = (elems, [], rng)
choose_m distances elems (group:groups) rng =
    (nelems, ngroup:ngroups, nrng)
    where
        (relem, telems, trng) = randomElem elems rng
        ngroup = ginsert group relem
        (nelems, ngroups, nrng) = choose_m distances telems groups trng

ginsert :: Group -> Element -> Group
ginsert group el = group {members = el:(members group)}

diversity :: Distances -> Element -> Elements -> Double
diversity distances el group = sum $ map (fromJust . lu) group
    where
        lu e | e == el = Just 0
             | otherwise = Map.lookup (mkset el e) distances

fitness :: Distances -> Solution -> Double
fitness distances = sum . map ((fitness_group distances) . members)

fitness_group :: Distances -> Elements -> Double
fitness_group distances [] = 0
fitness_group distances (el:rest_el) =
    diversity distances el rest_el + fitness_group distances rest_el

local_improvement :: Distances -> Double -> Solution -> StdGen
                        -> (Solution, StdGen)
--local_improvement _ _ s_in _ = error $ show s_in
local_improvement distances pls s_in rng
    | u < pls = (li_swap distances s_move s_move (members $ safehead s_move "locimp"), nrng)
    | otherwise = (s_in, rng)
    where
        (u, trng) = random rng :: (Double, StdGen)
        elems = sort $ Set.toList (Set.unions $ Map.keys distances)
        (s_move, nrng) = li_move distances pls s_in elems trng

safehead :: [a] -> String -> a
safehead [] s = error $ show s
safehead (e:el) _ = e
        

li_move :: Distances -> Double -> Solution -> Elements -> StdGen
            -> (Solution, StdGen)
--li_move distances pls s_in els rng | 
--         trace ("lm: " ++ show els ++ "\n" ++ show s_in ++ "\n") False = undefined
li_move distances pls s_in [] rng = (s_in, rng)
li_move distances pls s_in (el:elems) rng
    | first_improv_index == Nothing = li_move distances pls s_in elems rng
    | otherwise =
        li_move
            distances
            pls
            (apply_transform s_in elgroup figroup $ move el elgroup figroup)
            rng
    where
        -- Groep waar el vandaan komt
        elgroup = safehead (filter (elem el . members) s_in) (show s_in)
        -- Lijst van toegevoegde fitnesses per groep als we el daar in zouden steken 
        fs = map (\g -> - (diversity distances el $ members elgroup)
                            + (diversity distances el $ members (snd $ move el elgroup g)))
                    (filter 
                        (\g -> (toInteger . length . members $ g) < (bg g)) 
                        $ delete elgroup s_in)
        -- Eerste groep waarvoor we meer fitness krijgen door daarnaar te moven
        first_improv_index = if (toInteger . length $ members elgroup) <= (ag elgroup)
                             then Nothing
                             else findIndex (> 0) fs
        figroup = (s_in!!(fromJust first_improv_index))

move :: Element -> Group -> Group -> (Group, Group)
-- move el og ng | 
    -- trace ("move: " ++ show el ++ " " ++ show og ++ " " ++ show ng ++ "\n") False = undefined
move el og ng =
    (og {members = (delete el $ members og)}, ng {members = el:(members ng)})

li_swap :: Distances -> Solution -> Groups -> Elements -> Solution
li_swap distances s_in [g] elems = s_in
li_swap distances s_in (group:groups) [] =
    li_swap distances s_in groups (members $ safehead groups "liswap-[]")
li_swap distances s_in (group:groups) (el:elems)
    | first_improv_group_index == Nothing =
        li_swap distances s_in (group:groups) elems
    | otherwise =
        li_swap distances s_new s_new (members $ safehead s_new "liswap-other")
    where
        fs = map (\g ->
                map (\el2 -> let (ngroup, ng) = swap el el2 group g
                    in  - (diversity distances el $ members group)
                        - (diversity distances el2 $ members g)
                        + (diversity distances el $ members ng)
                        + (diversity distances el2 $ members ngroup))
                    (members g))
                groups
        first_improv_group_index = findIndex (isJust . findIndex (> 0)) fs
        first_improv_group = (groups!!(fromJust first_improv_group_index))
        first_improv_index =
            findIndex (> 0) (fs!!(fromJust first_improv_group_index))
        first_improv_elem =
            (members first_improv_group)!!(fromJust first_improv_index)
        s_new = apply_transform
                    s_in
                    group
                    first_improv_group
                    (swap el first_improv_elem group first_improv_group)
    

swap :: Element -> Element -> Group -> Group -> (Group, Group)
swap el1 el2 g1 g2 = (uncurry $ flip (move el2)) $ move el1 g1 g2

apply_transform :: Solution -> Group -> Group -> (Group, Group) -> Solution
apply_transform s_in g1 g2 (ng1, ng2) =
    ng1:(ng2:(delete g1 (delete g2 s_in)))

generate_neighbouring :: Distances -> Solution -> Integer
                        -> [(Distances -> Solution -> Integer -> StdGen -> (Solution, StdGen))]
                        -> StdGen
                        -> (Solution, StdGen)
generate_neighbouring distances s_in ndp nos rng = (s_out, nrng)
    where
        (rindex, trng) = randomR (0, length nos - 1) rng
        rno = nos!!rindex
        (s_out, nrng) = rno distances s_in ndp trng

no1 :: Distances -> Solution -> Integer -> StdGen -> (Solution, StdGen)
no1 distances s_in nd rng = (s_out, nrng)
    where
        (rn, trng) = randomR (1, nd) rng
        (groups, elems, ttrng) = no1_ distances s_in [] rn trng
        ([], s_out, nrng) = if (1 == length groups) then error "no1" else fill distances elems groups ttrng

no1_ :: Distances -> Groups -> Elements -> Integer -> StdGen
            -> (Groups, Elements, StdGen)
no1_ distances s_in elems 0 rng = (s_in, elems, rng)
no1_ distances s_in elems n rng =
    no1_ distances (nrgroup:restGroups) (nelem:elems) (n-1) nrng
    where
        nemptygroups = filter (\g -> not $ null $ members g) s_in
        (rgroup, restG, trng) = randomElem nemptygroups rng
        restGroups = delete rgroup s_in
        (nelem, restElems, nrng) = randomElem (members rgroup) trng
        nrgroup = rgroup {members = restElems}
        
no2 :: Distances -> Solution -> Integer -> StdGen -> (Solution, StdGen)
no2 distances s_in nd rng = (s_out, nrng)
    where
        (rn, trng) = randomR (1, nd) rng
        (groups, elems) = no2_ distances s_in [] rn
        ([], s_out, nrng) = if (1 == length groups) then error "no2" else fill distances elems groups trng

no2_ :: Distances -> Groups -> Elements -> Integer -> (Groups, Elements)
no2_ distances groups elems 0 = (groups, elems)
no2_ distances groups elems n = no2_ distances tgroups (nelem:elems) (n-1)
    where
        (tgroups, nelem) = least_diverse distances groups
    

least_diverse :: Distances -> Groups -> (Groups, Element)
least_diverse distances groups = (ngroups, nelem)
    where
        nelem = minimumBy
            (comparing
                (\el ->
                    let containing_group =
                            (safehead (filter ((elem el) . members) groups) "leadiv")
                    in
                        diversity
                            distances
                            el
                            (delete el $ members containing_group)))
            (least_diverse_ distances groups)
        ngroups = map
                    (\g -> if elem nelem $ members g
                            then g {members = delete nelem $ members g}
                            else g)
                    groups

least_diverse_ :: Distances -> Groups -> Elements
least_diverse_ distances [] = []
least_diverse_ distances (group:groups) =
        (least_diverse__ distances group):(least_diverse_ distances groups)

least_diverse__ :: Distances -> Group -> Element
least_diverse__ distances group =
    minimumBy
        (comparing (\el -> diversity
                            distances
                            el
                            (delete el $ members group)))
        (members group)

no3 :: Distances -> Solution -> Integer -> StdGen -> (Solution, StdGen)
no3 distances s_in p rng = no3_ distances s_in rp trng
    where
        (rp, trng) = randomR (1, p) rng
        
no3_ :: Distances -> Groups -> Integer -> StdGen
            -> (Solution, StdGen)
no3_ distances groups 0 rng = (groups, rng)
no3_ distances groups q rng = no3_ distances ngroups (q-1) nrng
    where
        (rindex1, trng) = randomR (0, length groups - 1) rng
        rgroup1 = groups!!rindex1
        (rindex2, ttrng) = randomR (0, length (delete rgroup1 groups) - 1) trng
        rgroup2 = (delete rgroup1 groups)!!rindex2
        (rindex11, tttrng) = randomR (0, (length $ members rgroup1) - 1) ttrng
        relem1 = (members rgroup1)!!rindex11
        (rindex21, nrng) = randomR (0, (length $ members rgroup2) - 1) tttrng
        relem2 = (members rgroup2)!!rindex21
        ngroups =
            (rgroup1 {members = relem2:(delete relem1 (members rgroup1))})
            :((rgroup2 {members = relem1:(delete relem2 (members rgroup2))})
                :(delete rgroup1 (delete rgroup2 groups)))


binary_tournament :: Distances -> [Solution] -> StdGen -> (Solution, StdGen)
binary_tournament distances sols rng =
    (best_solution_found distances [rsol1, rsol2], nrng)
    where
        (rindex1, trng) = randomR (0, length sols - 1) rng
        rsol1 = sols!!rindex1
        (rindex2, nrng) = randomR (0, length (delete rsol1 sols) - 1) trng
        rsol2 = (delete rsol1 sols)!!rindex2

best_solution_found :: Distances -> [Solution] -> Solution
--best_solution_found distances = head
best_solution_found distances =
       maximumBy (comparing (fitness distances))
