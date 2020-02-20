# COM2108 Assignment 3 Report

Bombe.hs is my solution for parts 1 and 2. Extension.hs is a copy of Bombe.hs, but with modifications made for the extension (part 3) of the assignment.



## Initial changes

I modified breakEnigma such that it attempts to use all of the menus that are as long as the longest menu as per part **3.C**.

I also adapted my solution so that I could more easily alter the rotors and the reflector that are used.

This can be done by altering the definitions of:
 - bombe_lrotor
 - bombe_mrotor
 - bombe_rrotor
 - bombe_reflector

at the bottom of **Extension.hs**.

I intended to implement part **3.D** but it didn't make sense to do this unless I could get Haskell to perform well enough to exhaustively search for a solution through all offsets with a single set of rotors first. This lead me onto looking into parallelism in Haskell...



## Parallelism

I wanted to attempt to make better use of my hardware by implementing some multithreading as my laptop isn't particularly new, so isn't great in terms of single threaded performance, but does have the capacity to handle several more threads of processing in parallel.

This also seemed a sensible change to make as it's a practice they also made use of with the Bombe - it didn't just try one combination of rotors at any one time; it had several combinations being run in parallel together.

I was also curious as to how easy it is to do this as functional programming, at least in Haskell, has appeared to me to neatly lend itself to parallelism.

There already exists a module, Control.Parallel.Strategies, that had everything I needed to implement this nicely. You might need to install it.

``cabal update``

then,

``cabal install parallel``

I initially wanted to write all of the possible offsets as a list comprehension in breakEA and then have every element evaluated in parallel, then have all of the threads terminate once one of them discovers and element that's not Nothing, but this didn't seem to be possible with Haskell as it stands.

Instead I settled for expressing a "batch" of possible offsets as a list comprehension at a time, and evaluating that in parallel. Then, once it's all evaluated I filtered down to those that are not `Nothing`, and if the list isn't empty, I pick the first one. Otherwise I recurse onto the next batch.

To benefit from these changes' performance improvements, you need to compile Extension.hs with the flag `-threaded`, then run it with the flags `+RTS -Nx`, where `x` is the number of threads you'd like to utilise.

It's provided set up with a main function that should break the following example provided in the FAQ:

```haskell
("COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOP",
"YZCSDCVUFVJAAEMVILWRVSQZFCBPJFVYHUUPHLAPJMTMFNLURRADJFCBRBXBCUSSXVYWAPQIRCUVVNODKELDMNNQHYFEFOZPBUIPWKPXIYPKQHMVOAVXFVDCKMZOULMTQNUFBVHFUSXYCYPWFKBYW")
```

As it can run for a long time, I'm using traceShowId in some places to display the progress of the machine when:

 - It starts looking for a solution for a menu (as it tries all menus as long as the longest menu); Here it simply shows the menu being tested.

 - It recurses onto the next batch of offsets; here it displays the offset it has progressed to, non inclusive (e.g. *(4,0,0)* would mean it's tried all the offsets up to, but not including, *(4,0,0)*).

 - breakEA finally returns a set of offsets and a steckerboard for a menu; at which point it displays that menu, the offset, the steckerboard, and the full ciphertext that was provided decoded with the steckerboard and offsets found.

 It should provide the following output:

 ```haskell
[5,13,14,2,9,27,3,34,24,19,7,4,23]
(1,0,0)
(2,0,0)
(3,0,0)
(4,0,0)
([5,13,14,2,9,27,3,34,24,19,7,4,23],Just ((4,3,7),[('T','T'),('K','C'),('N','E'),('O','M'),('V','V'),('P','P'),('S','S'),('J','U'),('X','F'),('G','R'),('L','D'),('B','Z')]),"COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOPENIGMAMACHINESAREINTERESTINGBUTTHOSECODEBREAKERSWHOCRACKEDTHECODEMUSTHAVEBEENGENIUSESTODOSOWITHOUTCOMPUTERSSTOP")
[14,2,13,21,15,27,3,34,24,19,7,4,23]
(1,0,0)
(2,0,0)
(3,0,0)
(4,0,0)
([14,2,13,21,15,27,3,34,24,19,7,4,23],Just ((4,3,7),[('E','N'),('O','M'),('K','C'),('S','S'),('V','V'),('P','P'),('J','U'),('X','F'),('G','R'),('L','D'),('B','Z')]),"COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOPENIGMAMACHINESAREINTERESTINGBUTTHOSECODEBREAKERSWHOCRACKEDTHECODEMUSTHAVEBEENGENIUSESTODOSOWITHOUTCOMPUTERSSTOP")
[14,2,13,21,30,27,3,34,24,19,7,4,23]
(1,0,0)
(2,0,0)
(3,0,0)
(4,0,0)
([14,2,13,21,30,27,3,34,24,19,7,4,23],Just ((4,3,7),[('E','N'),('O','M'),('K','C'),('S','S'),('V','V'),('P','P'),('J','U'),('X','F'),('G','R'),('L','D'),('B','Z')]),"COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOPENIGMAMACHINESAREINTERESTINGBUTTHOSECODEBREAKERSWHOCRACKEDTHECODEMUSTHAVEBEENGENIUSESTODOSOWITHOUTCOMPUTERSSTOP")
[25,13,14,2,9,27,3,34,24,19,7,4,23]
(1,0,0)
(2,0,0)
(3,0,0)
(4,0,0)
([25,13,14,2,9,27,3,34,24,19,7,4,23],Just ((4,3,7),[('N','E'),('K','C'),('O','M'),('V','V'),('P','P'),('S','S'),('J','U'),('X','F'),('G','R'),('L','D'),('B','Z')]),"COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOPENIGMAMACHINESAREINTERESTINGBUTTHOSECODEBREAKERSWHOCRACKEDTHECODEMUSTHAVEBEENGENIUSESTODOSOWITHOUTCOMPUTERSSTOP")
Solutions found: [([5,13,14,2,9,27,3,34,24,19,7,4,23],Just ((4,3,7),[('T','T'),('K','C'),('N','E'),('O','M'),('V','V'),('P','P'),('S','S'),('J','U'),('X','F'),('G','R'),('L','D'),('B','Z')]),"COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOPENIGMAMACHINESAREINTERESTINGBUTTHOSECODEBREAKERSWHOCRACKEDTHECODEMUSTHAVEBEENGENIUSESTODOSOWITHOUTCOMPUTERSSTOP"),([14,2,13,21,15,27,3,34,24,19,7,4,23],Just ((4,3,7),[('E','N'),('O','M'),('K','C'),('S','S'),('V','V'),('P','P'),('J','U'),('X','F'),('G','R'),('L','D'),('B','Z')]),"COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOPENIGMAMACHINESAREINTERESTINGBUTTHOSECODEBREAKERSWHOCRACKEDTHECODEMUSTHAVEBEENGENIUSESTODOSOWITHOUTCOMPUTERSSTOP"),([14,2,13,21,30,27,3,34,24,19,7,4,23],Just ((4,3,7),[('E','N'),('O','M'),('K','C'),('S','S'),('V','V'),('P','P'),('J','U'),('X','F'),('G','R'),('L','D'),('B','Z')]),"COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOPENIGMAMACHINESAREINTERESTINGBUTTHOSECODEBREAKERSWHOCRACKEDTHECODEMUSTHAVEBEENGENIUSESTODOSOWITHOUTCOMPUTERSSTOP"),([25,13,14,2,9,27,3,34,24,19,7,4,23],Just ((4,3,7),[('N','E'),('K','C'),('O','M'),('V','V'),('P','P'),('S','S'),('J','U'),('X','F'),('G','R'),('L','D'),('B','Z')]),"COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOPENIGMAMACHINESAREINTERESTINGBUTTHOSECODEBREAKERSWHOCRACKEDTHECODEMUSTHAVEBEENGENIUSESTODOSOWITHOUTCOMPUTERSSTOP")]
 ```