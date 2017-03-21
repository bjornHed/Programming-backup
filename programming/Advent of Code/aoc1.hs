input :: [String]
input = ["L5", "R1", "L5", "L1", "R5", "R1", "R1", "L4", "L1", "L3", "R2", 
        "R4", "L4", "L1", "L1", "R2", "R4", "R3", "L1", "R4", "L4", "L5", "L4", "R4", 
        "L5", "R1", "R5", "L2", "R1", "R3", "L2", "L4", "L4", "R1", "L192", "R5",
        "R1", "R4", "L5", "L4", "R5", "L1", "L1", "R48", "R5", "R5", "L2", "R4",
        "R4", "R1", "R3", "L1", "L4", "L5", "R1", "L4", "L2", "L5", "R5", "L2", 
        "R74", "R4", "L1", "R188", "R5", "L4", "L2", "R5", "R2", "L4", "R4", 
        "R3", "R3", "R2", "R1", "L3", "L2", "L5", "L5", "L2", "L1", "R1", "R5", 
        "R4", "L3", "R5", "L1", "L3", "R4", "L1", "L3", "L2", "R1", "R3", "R2", "R5",
        "L3", "L1", "L1", "R5", "L4", "L5", "R5", "R2", "L5", "R2", "L1", "L5", "L3", 
        "L5", "L5", "L1", "R1", "L4", "L3", "L1", "R2", "R5", "L1", "L3", "R4", "R5", "L4", "L1",
        "R5", "L1", "R5", "R5", "R5", "R2", "R1", "R2", "L5", "L5", "L5", "R4", 
        "L5", "L4", "L4", "R5", "L2", "R1", "R5", "L1", "L5", "R4", "L3", "R4", 
        "L2", "R3", "R3", "R3", "L2", "L2", "L2", "L1", "L4", "R3", "L4", "L2", "R2", "R5", "L1", "R2"]

type Dir = (Int, Int)

mai :: Int
mai = aoc1 input (0,1) (0,0)

--mai2 :: Int
--mai2 = (abs (fst dist)) + (abs (snd dist)) 
  --  where dist = aoc1p2 input (0,1) (0,0) [(0,0)]
input2 :: [String]
input2 = ["R8","R4","R4","R8"]
  
  
aoc1 :: [String] -> Dir -> Dir -> Int
aoc1 [] dir dist= (abs (fst dist)) + (abs (snd dist)) 
aoc1 ((v:xs):xss) dir (z,y) = 
    case v of
        'R' -> case newdir of
                 (1,0)    -> aoc1 xss newdir ((z+move),y)
                 ((-1),0) -> aoc1 xss newdir ((z-move),y)
                 (0,1)    -> aoc1 xss newdir (z,(y+move))
                 (0,(-1)) -> aoc1 xss newdir (z,(y-move))
        'L' -> case newdir of
                 (1,0)    -> aoc1 xss newdir ((z+move),y)
                 ((-1),0) -> aoc1 xss newdir ((z-move),y)
                 (0,1)    -> aoc1 xss newdir (z,(y+move))
                 (0,(-1)) -> aoc1 xss newdir (z,(y-move))
  where newdir = changeDir dir v
        move = read xs
        
        
aoc1p2 :: [String] -> Dir -> Dir -> [Dir] -> Dir 
aoc1p2 [] _ _ prev = last prev
aoc1p2 ((v:xs):xss) dir (x,y) prev =
    if fst (isPartOf possiblePos prev)
    then snd (isPartOf possiblePos prev)
    else aoc1p2 xss newdir newdist (prev ++ possiblePos)
  where newdir = changeDir dir v
        move = read xs
        newdist = ((x+(move*(fst newdir))),(y+(move*(snd newdir))))
        possiblePos = [((x+(n*(fst newdir))),(y+(n*(snd newdir)))) | n <- [1..move] ]

isPartOf :: [Dir] -> [Dir] -> (Bool, Dir)
isPartOf [] prev = (False,(0,0))
isPartOf (x:xs) prev = 
    if x `elem` prev
    then (True, x)
    else isPartOf xs prev
       
changeDir :: Dir -> Char -> Dir
changeDir dir c = 
    case c of
        'R' -> case dir of
                 (1,0)    -> (0,(-1))
                 ((-1),0) -> (0,1)
                 (0,1)    -> (1,0)
                 (0,(-1)) -> ((-1),0)
        'L' -> case dir of
                 (1,0)    -> (0,1)
                 ((-1),0) -> (0,(-1))
                 (0,1)    -> ((-1),0)
                 (0,(-1)) -> (1,0)
                 
                 
                 