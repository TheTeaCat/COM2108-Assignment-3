# COM2108-Assignment-3

This was an assignment to implement the Bombe in Haskell, given some constraints. Assignment 2 (Enigma.hs) is also included in this repository as it is a dependency for assignment 3.

Part of this assignment was an extension for which there were some suggestions, but we were free to go beyond. Extension.hs is an extended version of Bombe.hs, a description of the changes can be found in report.md, but in summary it is parallelised. 



## Setup

Enigma.hs and Bombe.hs should run happily in GHCi.

Extension.hs has a dependency and needs to be compiled to take advantage of the parallelisation.



- Install dependencies:

  ```
  cabal update
  cabal install parallel
  ```

- Compile:

  ```
  ghc Extension.hs -threaded
  ```

- Run:

  ```
  ./Extension.hs +RTS -N{x}
  ```

  Replace `{x}` with the number of threads you wish to utilise.



## Extension.hs Default Setup

Extension.hs is set up by default to decode the following message:

```haskell
YZCSDCVUFVJAAEMVILWRVSQZFCBPJFVYHUUPHLAPJMTMFNLURRADJFCBRBXBCUSSXVYWAPQIRCUVVNODKELDMNNQHYFEFOZPBUIPWKPXIYPKQHMVOAVXFVDCKMZOULMTQNUFBVHFUSXYCYPWFKBYW
```

Assuming: 

- The crib is `COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOP`
- The left rotor is `EKMFLGDQVZNTOWYHXUSPAIBRCJ`
- The middle rotor is `AJDKSIRUXBLHWTMCQGZNPYFVOE`
- The right rotor is `BDFHJLCPRTXVZNYEIWGAKMUSQO`
- The reflector is paired as follows: `[('A','Y'),('B','R'),('C','U'),('D','H'),('E','Q'),('F','S'),('G','L'),('I','P'),('J','X'),('K','N'),('M','O'),('T','Z'),('V','W')]`

You can change all of this in the definition of `main`.



It should decode to:

```haskell
COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOPENIGMAMACHINESAREINTERESTINGBUTTHOSECODEBREAKERSWHOCRACKEDTHECODEMUSTHAVEBEENGENIUSESTODOSOWITHOUTCOMPUTERSSTOP
```

With offsets `(4,3,7)` of the left, middle and right rotors respectively, and a steckerboard equivalent to:

```haskell
[('T','T'),('K','C'),('N','E'),('O','M'),('V','V'),('P','P'),('S','S'),('J','U'),('X','F'),('G','R'),('L','D'),('B','Z')]
```