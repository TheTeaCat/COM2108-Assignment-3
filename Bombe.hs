module Bombe where 
    {-Written by Joshua O'Sullivan (180170098) for COM2108 Assignment 3.
    -}
    import AssignmentHelp
    import Data.List
    import Data.Maybe
    import Enigma

    import Debug.Trace
    import Control.Parallel.Strategies

    type SteckerPair = (Char,Char)

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
                r = enigmaEncode q (SimpleEnigma rotor1 rotor2 rotor3
                                                    reflectorB 
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

    {-breakEA recurses through all the possible offsets, calling findStecker for
        each, until it finds a solution, or it has explored all the offsets.
        -}
    breakEA :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe (Offsets, Steckerboard)
    breakEA crib menu steckerboard offsets = 
        let result = findStecker crib menu steckerboard offsets
        in if isNothing result
            then let nextOffset = getNthOffsetFrom offsets 1
                in if nextOffset == (0,0,0)
                    then Nothing
                    else breakEA crib menu steckerboard nextOffset
            else Just (offsets, AssignmentHelp.fromMaybe result)        

    {-breakEnigma takes a crib, constructs a Menu from it, then calls breakEA 
        -}
    breakEnigma :: Crib -> Maybe (Offsets, Steckerboard)
    breakEnigma (plain,cipher) =
        let menu = longestMenu (plain,cipher)
        in breakEA (plain,cipher) menu [(plain !! (head menu), 'A')] (0,0,0)
