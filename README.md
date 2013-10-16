Distributed-Haskell
===================

A library for distributed computing using haskell.


## Get started:

#### Requirements
To make things easy, you should have: cabal-install >= 1.8

You need: ghc (The Glorious Glasgow Haskell Compilation System) or some other haskell compiler.

You need: Modules!

     base 4.6.* network 2.4.* ConfigFile 1.1.* MissingH 1.2.* containers 0.5.* 
     old-time 1.1.* random 1.0.* timers 0.2.* suspend 0.1.*
 
#### Test It Out

Build the sample with: 
  
    ghc --make main.hs

Edit your config: `DHaskell.conf` (Instructions Inside!)

Run across multiple machines and watch the membership lists!

## Actually Use the Thing!

Build the docs with: 

    cabal haddock

Take a look at `main.hs` to see how it uses the Gossip Protocol (That's all thats in the lib right now anyway)

With docs in hand, maybe help build the library, or I guess do something else with it.
