module Enigma where
    {-Written by Joshua O'Sullivan (180170098) for COM2108 Assignment 2.
    -}
    import AssignmentHelp
    import Data.List

    type Rotor = Cipher 
    type Reflector = [(Char, Char)]
    type Offsets = (Int, Int, Int)
    type Steckerboard = [(Char,Char)] 

    data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets 
     | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Steckerboard 
    
    {-inverseOf returns the inverse of the cipher (so, if A mapped to B
        in a cipher, its inverse would map B to A.)
    -}
    inverseOf :: Cipher -> Cipher
    inverseOf cipher = sortOn (\letter -> cipher !! alphaPos letter) cipher

    {-getReflectorLookup turns a Reflector into a lookup table (Cipher) for 
        efficiency's sake.
     -}
    type ReflectorLookup = Cipher
    getReflectorLookup :: Reflector -> ReflectorLookup
    getReflectorLookup r = map (\(_,to) -> to) 
                               (sortOn (\(from,_) -> from) 
                                       (r ++ map (\(from,to) -> (to,from)) r)
                                ) 

    {-getSteckerboardLookup works similarly to getReflectorLookup although it
        has to account for characters that may not be present.
        
        This could probably be more efficient, but it's only called once for 
        each enigmaEncode and enigmaEncodeMessage, so it wasn't a priority.
    -}
    type SteckerboardLookup = Cipher
    getSteckerboardLookup :: Steckerboard -> SteckerboardLookup
    getSteckerboardLookup st = 
        map (\(_,to)->to) 
            (sortOn (\(from,_)->from) 
                    (let pairs = nub (st ++ map (\(fr,to)->(to,fr)) st)
                         charsSupplied = map (\(from,_)->from) pairs
                     in pairs ++ [(char,char) | char <- alphabet,
                                                not (char `elem` charsSupplied)]
                    )
            )

    {-componentPass encodes a character with a given lookup (Cipher) and offset.
        Defining the alphabet as its own constant and defining a special case
        for offset=0 saves a bit of memory.
    -}
    alphabet = ['A'..'Z']
    componentPass :: (Cipher, Int) -> Char -> Char
    componentPass (component, 0) character = component !! (alphaPos character)
    componentPass (component, offset) character = 
        alphabet   !! ((alphaPos (
         component !! ((alphaPos character + offset) `mod` 26)
                                         ) - offset) `mod` 26)

    {-getNthOffsetFrom gets the nth offset from a given set of rotor positions,
        (Offsets) assuming the rotors' notches are all at position 25.
    -}
    getNthOffsetFrom :: Offsets -> Int -> Offsets
    getNthOffsetFrom (a,b,c) n = (((n+c+b*26+a*26*26) `div` (26*26)) `mod` 26,
                                  ((n+c+b*26) `div` 26) `mod` 26,
                                   (n+c) `mod` 26)

    {-enigmaEncode encodes a single character with a given an Enigma.
        The use of id as the first value for foldr does eat a tiny bit of
        memory, but I decided it's a sacrifice worth the additional readability
        of keeping all the components in an array together.
        --------------------------------------------------
        lr = left rotor, mr = middle rotor, rr = right rotor, rf = reflector
        os = offsets, lo = left offset, mo = middle offset, ro = right offset.
    -}
    enigmaEncode :: Char -> Enigma -> Char 
    enigmaEncode char (SimpleEnigma lr mr rr rf os) = 
        let (lo,mo,ro) = getNthOffsetFrom os 1
        in foldr (.) id ((map componentPass [(inverseOf rr,ro),
                                             (inverseOf mr,mo),
                                             (inverseOf lr,lo),
                                             (getReflectorLookup rf,0),
                                             (lr,lo),
                                             (mr,mo),
                                             (rr,ro)]) ++ [id]) char

    {-enigmaEncode is essentially the same for a SteckeredEnigma, except the
        steckerboard has to be added at either end of the components array.
        --------------------------------------------------
        st = steckerboard.
    -}
    enigmaEncode char (SteckeredEnigma lr mr rr rf os st) = 
        let (lo,mo,ro) = getNthOffsetFrom os 1
        in foldr (.) id ((map componentPass [(getSteckerboardLookup st,0),
                                             (inverseOf rr,ro),
                                             (inverseOf mr,mo),
                                             (inverseOf lr,lo),
                                             (getReflectorLookup rf,0),
                                             (lr,lo),
                                             (mr,mo),
                                             (rr,ro),
                                             (getSteckerboardLookup st,0)]) 
                                             ++ [id]) char

    {-enigmaEncodeMessage takes a message and encodes it with the given Enigma.
        rflu, lrr, mrr, rrr, and getEnigmaState are defined in a let in order to
        reduce memory usage.
        --------------------------------------------------
        lr = left rotor, mr = middle rotor, rr = right rotor, rf = reflector,
        os = offsets, rflu = reflector lookup, lrr = left rotor reversed,
        mrr = middle rotor reversed, rrr = right rotor reversed.
    -}
    enigmaEncodeMessage :: String -> Enigma -> String 
    enigmaEncodeMessage msg (SimpleEnigma lr mr rr rf os) =
        let rflu = getReflectorLookup rf
            lrr = inverseOf lr
            mrr = inverseOf mr
            rrr = inverseOf rr
            getEnigmaState = (\(lo,mo,ro) -> foldr (.) id (
                map componentPass [(rrr,ro),(mrr,mo),(lrr,lo),
                                   (rflu,0),
                                   (lr,lo),(mr,mo),(rr,ro)])
                                   )
        in map (\i -> getEnigmaState (getNthOffsetFrom os (i+1)) (msg !! i))
               [0..(length msg - 1)]

    {-enigmaEncodeMessage essentially the same with a SteckeredEnigma, except
        a lookup for the steckerboard has to be generated (stlu) and added at
        either end of the components array.
        --------------------------------------------------
        st = steckerboard, stlu = steckerboard lookup,
        stlur = steckerboard lookup reversed.
    -}
    enigmaEncodeMessage msg (SteckeredEnigma lr mr rr rf os st) = 
        let stlu = getSteckerboardLookup st
            rflu = getReflectorLookup rf
            rrr = inverseOf rr
            mrr = inverseOf mr
            lrr = inverseOf lr
            getEnigmaState = (\(lo,mo,ro) -> foldr (.) id (
                map componentPass [(stlu,0),
                                   (rrr,ro),(mrr,mo),(lrr,lo),
                                   (rflu,0),
                                   (lr,lo),(mr,mo),(rr,ro),
                                   (stlu,0)]
                                   ))
        in map (\i -> getEnigmaState (getNthOffsetFrom os (i+1)) (msg !! i))
               [0..(length msg - 1)]

    {-Checking that I can still decode a message enciphered by a steckered
        Enigma with an identical steckered Enigma as per part pt.8 of the
        assignment.
    -}

    {-testDoubleEncode tests an enigma by checking it is symmetric by doubly
        encoding a plaintext and checking it comes out as the same plaintext.
    -}
    testutil_doubleEncodeTest :: String -> Enigma -> Bool
    testutil_doubleEncodeTest plaintext enigma = plaintext == 
        (enigmaEncodeMessage (enigmaEncodeMessage plaintext enigma) enigma)

    testdata_plaintext = 
        "THEXQUICKXBROWNXFOXXJUMPSXOVERXTHEXLAZYXDOG"

    testdata_steckeredEnigma = 
        SteckeredEnigma rotor5 rotor2 rotor3 reflectorB 
        (0,0,0) [('A','B'),('Q','R'),('Z','G'),('I','O')]

    test_steckeredEnigmaDoubleEncode = 
        testutil_doubleEncodeTest testdata_plaintext testdata_steckeredEnigma

    ------------------------------------------------------------longestMenu

    {-Cribs are treated as graphs where index N represents a directed edge from
        the character in the plaintext at index N to the character in the
        ciphertext at index N.
    -}
    type Crib = (String,String) 
    type Menu = [Int]

    {-removeEdge takes a crib and an index, and removes the edge from the crib
        represented by the characters in the plain and cipher texts at that
        index by effectively replacing it with a loop around a node for the
        arbitrary character '-'.
    -}
    removeEdge :: Crib -> Int -> Crib
    removeEdge (from,to) i =  ((take i from) ++ "-" ++ (drop (i+1) from),
                              (take i to) ++ "-" ++ (drop (i+1) to))

    {-longestMenus returns the longest non-repeating path(s) in the graph (where
        each index n is an edge from the plaintext character at n to the 
        ciphertext character at n) from the root letter supplied, which may be a
        wildcard (any letter).
    -}
    longestMenus :: Crib -> Char -> [Menu]
    longestMenus (from,to) root 
        | root `elem` from || root == '*' = 
          let subms = concat [map ([i]++) 
                             (longestMenus (removeEdge (from,to) i) (to !! i))
                             | i <- if root == '*' then [0..length from - 1]
                                                   else root `elemIndices` from]
              longestSubm = maximum (map length subms)
          in filter (\s -> length s == longestSubm) subms
        | otherwise = [[]]

    {-longestMenu gets a longestMenu from a given crib.
        I can think of no more efficient way to find the longestMenu without
        exhaustively searching the entire graph and just picking one of the
        longest ones.
    -}
    longestMenu :: Crib -> Menu 
    longestMenu crib = (longestMenus crib '*') !! 0