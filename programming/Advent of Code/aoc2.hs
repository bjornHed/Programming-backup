input :: [String]
input = ["DULUDRDDDRLUDURUUULRRRURDRDULRUDDUDRULUDDUDRLDULRRLRDRUDUUULUUDLRURDUDDDDRDLLLLULRDLDRDLRLULRUURDDUULUDLRURRDDRDDRDDLDRDLLUURDRUULRRURURRDLRLLLUDULULULULUDRLLRUDUURLDRLRLRDRRDRLLLDURRDULDURDDRLURRDURLRRRLDLLLDRUUURLRDLDLLLLRDURRLDLULRLDDLDLURLRRDDRUDDUULRURRUDLRDLDUURDDDDRLRURUDULUDLRRLLLLLRDRURLLDLDULUUDLUDDDRLLDRRUDLLURRUUDDRRLLRRLDDDURLDRDRLURRRRDRRRDDUDULULDURRUUURRRDULUUUDDRULDRLLRDLDURLURRLLRUUUULRDURLLDDRLLDLRLRULUUDRURUDLLURUDDRDURLRDRRRDURLDDRDRLRLLURULUUULUDDDULDLRDDDRDLLRRLDRDULLUUUDLDDLDDDLLLLLLLDUDURURDURDRUURRRDDRDUDLULDURDUDURDDDRULDURURURRLURLURLUURLULDLLRUULURDDRLRDDLRDLRRR",
        "LUURLRUDRRUDLLDLUDDURULURLUUDUUDDRLUULRDUDDUULDUUDRURDDRRDRLULLRDRDLRLLUURRUULRLDRULUDLDUUDDDRDDLRDLULDRLDUULDLRDLLLDLDLRDUULUDURRULLRLDUDRLLLULUUUULUUDUUURRRDULLUURUDRRLDURRUULDRDULDUDRDUUULUUDDRLUDRLDLDRUUURDLDUDRUDUURLLRRLRLLRRLDULDDULUDUUURULDDUDUDRURRDLULRUDDURDLDLLRRRLDRLULLLRUULDUDLUUDURRLLLRLUDURRDDLDRDDDLURDLDRRUDUDLUDULULRUUUDLUURLLRLDDLURULDURDLRRDDDDURLDDLLDDULLLRLDLDULDUUDDRLDUURDDLDLUUDULRRLRLUURURUURLRLURUURLDRUURLLRDDUUUDULUDDDRDRLDRDRRLRLDULLRRUDLURULULRDRURURLULDUDLRURLRDDRULDDLRD",
        "LUDRULUULRRDDDDRRDUURUDDRLDDLDRDURRURULRDLDLDUUDRRDUUDUDLLLRRLDUDDRLDDLRRLRDRLUDLULUDDUUDULDUUULUDLDDURLDURUDLDRUUDRLRRLDLDDULDUUDDLDDLLURDRLRUURDDRUDDUDLDRRLRUDRUULRRRLRULULURDLRRURDRLRULDDDRDUULLURUUUURUDDLRRRRRDURLULDLUULUDRRUDUDRRDDRURDURLRLUDDLDLRRULUDLDDRLDDLDDDLLLLRDLLUULDDLULDLDRDDUDLURUDLDLDDRRUUDDDLRLLLDRRDDDUURDUDURUURRDRLLDUDLDUULLDLDLLUULLRRULDLDRURLDULDRUURDURRURDLRDLLLDRRUDRUUDRURLUDDRURLDURRDLUUDLUUDULLLDDDDRRDLLLDLURULDDRDLUUURRDRRUUDDUL",
        "DUUULDUDDDURLLULDDLLUDURLLLURULULURUURDRURLRULLLLDRDDULRRDRRLLLRDDDUULLRRURRULLDDURRRLRDDLULDULLDUDLURRDLDDLURDLRLLDRURLLRLLRRRDRRRURURUUDDLLDDLDDDLRLURUUUULRDLUDDDURLLDDRLDRRLLUDUUULRLLDRRRLRUUDLDUULRLUDRULLLLDUDLLUUDDRUURLURUDRDDDLRURUDRLULLULUUDLDURDULRRDRLDURUULRDRRRDRDRRLRLRDDUULLRDLDURDDDULURRLULDDURDURDDUDURDLLUUULUDULRDDLDRDRUDLLUURDLRDURURULURULLDRLLRRULDLULULDLULRURLRRLUDLLLRLUDLURLULDULDRLLLDLDDDDRDRLRRLRDULUUDULDDLDURDLLLDDDDLLUURRDURLDLUDDLULRUUUDDRRLDLLLRDLLDRRRDDLULLURDDRRRRLDLRLLLRL",
        "LULLRRDURRLDUUDRRURLURURRRLRDRUULUULURLLURRDRULRDURDDDDUULLLLDUULDLULURDRLDLULULDRLLDLLRLRULURUDRUUDULRULLLUDRULUDRLLUDLDRRDRUUURURLRDURDRLRDDDURLURRDLRUUUDUURULULDLUULRDLRRRDRDRLLLDLRRDRLLDDULDRUDRRLULLRDLDUDDULRDDLULRURULRLLLULDLLLLRDLDRURUDUURURLDRLUULLDUDULUDDDULUDLRUDDUDLULLUULUUURULURRULRDDURDDLURLRRDRDLDULRLRDRRRULRDDDRLLDDDDRRRRDRDLULUURDURULDLRDULDUDLDURUDLUDLUDDDUDURDURDDURLLRUDUURRRUDRRRRULLLLDDDLUULLUULRRRULDLURDLULRULDRLR"]
        
input2 :: [String]
input2 = ["ULL",
         "RRDDD",
         "LURDL",
         "UUUUD"]        
        
        
{-getKey :: [String] -> String -> Int -> Int
getKey [] key pos = read key
getKey (x:xs) key pos = getKey xs (key ++ inp) (read inp)
    where inp = (getInput x pos)

getInput :: String -> Int -> String
getInput [] currentKey = show currentKey 
getInput (x:xs) currentKey = 
    case x of
        'D' -> if currentKey `elem` [7, 8, 9]
               then getInput xs currentKey
               else getInput xs (currentKey + 3)
        'U' -> if currentKey `elem` [1, 2, 3]
               then getInput xs currentKey
               else getInput xs (currentKey - 3)
        'L' -> if currentKey `elem` [1, 4, 7]
               then getInput xs currentKey
               else getInput xs (currentKey - 1)
        'R' -> if currentKey `elem` [3, 6, 9]
               then getInput xs currentKey
               else getInput xs (currentKey + 1)-}
               
mai :: String
mai = getKey input2 "" '5'

---------------------------------------------------------------------------------

getKey :: [String] -> String -> Char -> String
getKey [] key _ = key
getKey (x:xs) key currentKey = getKey xs (key ++ inp) (head inp)
    where inp = getInput x currentKey

getInput :: String -> Char -> String
getInput [] currentKey = [currentKey]
getInput (x:xs) c = 
    case x of 
        'D' -> if c `elem` ['5', 'A', 'D', 'C', '9']
               then getInput xs c
               else case c of
                        '6' -> getInput xs 'A'
                        '7' -> getInput xs 'B'
                        '8' -> getInput xs 'C'
                        'B' -> getInput xs 'D'
                        '1' -> getInput xs '3'
                        '2' -> getInput xs '6'
                        '3' -> getInput xs '7'
                        '4' -> getInput xs '8'
        'U' -> if c `elem` ['5', '1', '2', '4', '9']
               then getInput xs c
               else case c of
                        '6' -> getInput xs '2'
                        '7' -> getInput xs '3'
                        '8' -> getInput xs '4'
                        '3' -> getInput xs '1'
                        'A' -> getInput xs '6'
                        'B' -> getInput xs '7'
                        'C' -> getInput xs '8'
                        'D' -> getInput xs 'B'
        'L' -> if c `elem` ['1', '2', '5', 'A', 'D']
               then getInput xs c
               else case c of
                        '6' -> getInput xs '5'
                        '7' -> getInput xs '6'
                        '8' -> getInput xs '7'
                        '9' -> getInput xs '8'
                        '3' -> getInput xs '2'
                        '4' -> getInput xs '3'
                        'B' -> getInput xs 'A'
                        'C' -> getInput xs 'B'
        'R' -> if c `elem` ['4', '1', 'D', 'C', '9']
               then getInput xs c
               else case c of
                        '2' -> getInput xs '3'
                        '3' -> getInput xs '4'
                        '5' -> getInput xs '6'
                        '6' -> getInput xs '7'
                        '7' -> getInput xs '8'
                        '8' -> getInput xs '9'
                        'A' -> getInput xs 'B'
                        'B' -> getInput xs 'C'