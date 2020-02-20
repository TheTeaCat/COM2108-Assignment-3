module Main where 
    {-Written by Joshua O'Sullivan (180170098) for COM2108 Assignment 3.
    -}
    import AssignmentHelp
    import Data.List
    import Data.Maybe
    import Enigma
    import Debug.Trace
    import Control.Parallel.Strategies

    type SteckerPair = (Char,Char)
    type Rotors = (Rotor,Rotor,Rotor)

    {-steckerAdd takes a SteckerPair and adds it to a Steckerboard if it is 
        compatible with that Steckerboard, otherwise if the SteckerPair already 
        exists in the Steckerboard it just returns the Steckerboard, otherwise it 
        returns Nothing.
        -}
    steckerAdd :: SteckerPair -> Steckerboard -> Maybe Steckerboard
    steckerAdd (r, c) steckerboard
        | (r,c) `elem` steckerboard = Just steckerboard
        | (c,r) `elem` steckerboard = Just steckerboard
        | all (\(a,b) -> not (a `elem` [r,c] || b `elem` [r,c])) steckerboard =
            Just (steckerboard ++ [(r,c)])
        | otherwise = Nothing

    {-stecker takes a Char and a Steckerboard and runs that Char through the
        Steckerboard.
        -}
    stecker :: Char -> Steckerboard -> Maybe Char
    stecker char (st:steckerboard) | char == fst st = Just (snd st)
                                | char == snd st = Just (fst st)
                                | steckerboard == [] = Nothing
                                | otherwise = stecker char steckerboard

    {-followMenu recursively explores a Menu to create a Steckerboard until 
        either the Menu is empty, in which case it returns the Steckerboard it 
        has constructed; or a contradiction is found, in which case it returns
        Nothing.
        -}
    followMenu :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe Steckerboard
    followMenu (plain,cipher) menu steckerboard offsets
        | menu == [] = Just steckerboard
        | otherwise = 
            let i = head menu
                p = plain !! i
                q = AssignmentHelp.fromMaybe (stecker p steckerboard)
                r = enigmaEncode q (SimpleEnigma bombe_lrotor 
                                                 bombe_mrotor 
                                                 bombe_rrotor
                                                 bombe_reflector
                                                 (getNthOffsetFrom offsets i))
                c = cipher !! i
                result = steckerAdd (r,c) steckerboard
            in if isNothing result
                then Nothing
                else followMenu (plain,cipher) (tail menu) 
                                (AssignmentHelp.fromMaybe result) offsets

    {-findStecker recurses through all the 26 possible initial steckers from the
        provided start character of the menu.
        -}
    findStecker :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe Steckerboard
    findStecker crib menu [(x,y)] offsets = 
        let result = followMenu crib menu [(x,y)] offsets
        in if isNothing result
            then if y == 'Z'
                then Nothing
                else let stecker = [(x,alphabet !! ((alphaPos y)+1))] --
                        in findStecker crib menu stecker offsets
            else result

    {-breakEAP iterates through all the possible offsets the enigma could have, 
        but tries a batch of offsets in parallel.

        In order for this to actually give any performance improvement, you need
        to compile with the flag -threaded, then run with the flags +RTS -Nx, 
        where x is the number of threads you'd like to allow the program to use.
        Like so:
                >ghc Extension.hs -threaded
                >Extension +NTS -N4
    -}
    batch_size = 26*26 --batch_size needs to be a factor of 26^3.
    breakEAP :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe (Offsets, Steckerboard)
    breakEAP crib menu steckerboard offsets = 
        let result = filter (\(o,s) -> not (isNothing s))
                    ([let o = (getNthOffsetFrom offsets i)
                      in (o, findStecker crib menu steckerboard o) 
                      | i <- [0..batch_size]] 
                     `using` parList rdeepseq)
        in if result /= []
           then let (offs, stecker) = result !! 0
                in Just (offs, AssignmentHelp.fromMaybe stecker)
           else let nextOffset = traceShowId 
                                 (getNthOffsetFrom offsets batch_size)
                in if nextOffset == (0,0,0)
                   then Nothing
                   else breakEAP crib menu steckerboard nextOffset

    {-breakEnigma takes a crib, then finds its longest menus and calls breakEA 
        for each of those menus to get a steckerboard and some offsets. 
        
        Given that steckerboard and those offsets, it attempts to decode the 
        full ciphertext in the crib (this brings an advantage to the (String, 
        String) definition of a crib as opposed to [(Char,Char)] as the full 
        ciphertext can be used with a shorter plaintext without issue.).

        It finally returns a list of these menus in tuples with the offsets and
        steckerboard it found, which may be nothing; and a String which, 
        assuming the steckerboard and offsets aren't Nothing, will be the full
        ciphertext decoded using the offsets and steckerboard that breakEA 
        found. 
    -}
    breakEnigma :: Crib -> [(Menu, Maybe (Offsets, Steckerboard), String)]
    breakEnigma (plain,cipher) =
        [let result = breakEAP (plain,cipher) 
                               (traceShowId menu) 
                               [(plain !! (head menu), 'A')] 
                               (0,0,0)
         in traceShowId (menu, result, 
                         if isNothing result then ""
                         else let (off, steck) = AssignmentHelp.fromMaybe result 
                              in enigmaEncodeMessage cipher 
                                 (SteckeredEnigma bombe_lrotor 
                                                  bombe_mrotor 
                                                  bombe_rrotor 
                                                  bombe_reflector
                                                  off 
                                                  steck))
        | menu <- longestMenus (plain,cipher) '*']

    bombe_lrotor = rotor1
    bombe_mrotor = rotor2
    bombe_rrotor = rotor3
    bombe_reflector = reflectorB

    main = putStrLn ("Solutions found: " ++ (show 
                        (breakEnigma ("COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOP",
                                      "YZCSDCVUFVJAAEMVILWRVSQZFCBPJFVYHUUPHLAPJMTMFNLURRADJFCBRBXBCUSSXVYWAPQIRCUVVNODKELDMNNQHYFEFOZPBUIPWKPXIYPKQHMVOAVXFVDCKMZOULMTQNUFBVHFUSXYCYPWFKBYW"))))
