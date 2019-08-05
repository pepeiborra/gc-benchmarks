# Shorter GC pauses for free

GHC >=8.10 is getting a new incremental garbage collector with a mark&sweep strategy for the older generation collections, instead of the standard copy strategy. Incrementality comes from performing the sweep phase concurrently with the mutator (i.e. the program), after a blocking, hopefully short marking phase. Ben Gamari gave a [talk][1] about it at MuniHac last year, please check it for all the details. Now that the collector is publicly available in the GHC repository, in this post we benchmark it to find out how much shorter the GC pauses are, and what the impact is in performance. The results are quite encouraging and present an alternative to the [solution][2] using compact regions. All the experiments are reproducible via a [Nix expression][nix] that will build the GHC branch, run the benchmarks and extract the graphs.

## Benchmark methodology
To build GHC, I checked out the branch `wip/gc/ghc-8.8-rebase` and rebased it again on top of 8.8, including submodules, then just `./boot && ./configure && make -j4`. I didn't bother installing it, using the `inplace/bin/ghc-state2` binary directly. 

To test the new incremental garbage collector I turned to the well known [Pusher][3] problem: can the generation 1 GC pauses be short for a program that keeps a large amount of long-lived state in the heap ? The answer currently is no, but there are workarounds like [compact regions][4] that effectively move the data out of the garbage collected heap. These workarounds, however, require modifying or rewriting the program, and usually involve a sacrifice in performance. The new incremental collector should be able to reduce the Gen 1 pauses without any code changes, and I wanted to see how much shorter the pauses are and how much performance is lost.

The code for the Pusher example is very short and included below for convenience: it uses a `Data.Map.Strict` to store up to `_N` messages in 2 million iterations. By varying `_N` we can control the size of the Haskell heap and relate it to the length of the Gen 1 pauses:
```
module Main (main) where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map

data Msg = Msg !Int !ByteString.ByteString

type Chan = Map.Map Int ByteString.ByteString

_N :: Int
_N = 500.000

message :: Int -> Msg
message n = Msg n (ByteString.replicate 1024 (fromIntegral n))

pushMsg :: Chan -> Msg -> IO Chan
pushMsg chan (Msg msgId msgContent) =
  Exception.evaluate $
    let
      inserted = Map.insert msgId msgContent chan
    in
      if _N_ < Map.size inserted
      then Map.deleteMin inserted
      else inserted

main :: IO ()
main = Monad.foldM_ pushMsg Map.empty (map message [1..2000000])
```

To measure the max gen 1 pause length, I rely on the output of `+RTS -s`. This is what it looks like for the new incremental GC:
```
                                         Tot time (elapsed)  Avg pause  Max pause
      Gen  0     59746 colls,     0 par    4.244s   4.283s     0.0001s    0.0002s
(1a)  Gen  1        25 colls,     0 par    0.730s   0.731s     0.0292s    0.0228s
(1b)  Gen  1        25 syncs,                       0.003s     0.0001s    0.0003s
(1c)  Gen  1      concurrent,              5.062s  10.174s     0.4070s    1.1760s

```

`Gen 1` now gets two extra lines showing (1b) the time spent in sync pauses (after sweeping) and (1c) the time spent in concurrent sweeping. Note that (1c) is *not* a pause regardless of what the header says. Finally, my understanding is that (1a) shows the time spent marking, which is indeed a blocking pause.

## Benchmarking pauses

The graph below shows the max length of the Gen 1 pauses per dataset size, both with the standard (red) and incremental (dotted) GC for various sizes of N.

![][pauses]

For the incremental collector we are looking only at the marking pause. The standard GC pause lengths are linear with N as expected, and the incremental GC pauses are sub linear with N. In the worst case, the incremental GC pauses are up to six times shorter than the copying GC pauses. However, at 100ms, is this short enough? I asked Ben Gamari and he said:

>The Pusher benchmark allocates and retains lots of large objects (namely each message carries a large ByteString).
> However, the cost of the preparatory GC is linear in the number of large
> objects (the assumption being these are relatively rare in most
> programs) as we must clear the mark bit of each. Consequently, the
> preparatory collection pause scales linearly with the size of the test's queue. 
> In my experience it is rather unusual for programs to carry around millions of large and pinned bytearrays.

## Benchmarking performance

As far as I can see, the incremental collector does not have any impact on the run time. If you think this is too good to be true, that makes two of us. I repeated the benchmarks several times, and run times were consistently similar for both collectors. It seems that the mark&sweep collector is able to perform the sweeping phase in parallel using a second core of the CPU, thereby negating the costs and any disadvantages due to lower cache locality. The graph below shows the runtimes for each collector per dataset size:

![][runtimes]

## Conclusion

The incremental garbage collector offers shorter pauses than the copying collector without the need to change any code, and little to no performance costs assuming an extra core available. Compact regions afford more control to decide when and for how long to pause, and even to perform the compaction concurrently with the main program, therefore achieving pauses as short as desired with the same performance characteristics. But this is at the cost of significant complexity, whereas the incremental collector can be turned on with a simple flag.

The incremental garbage collector will hopefully make GHC a better fit for many applications that require shorter GC pauses, such as games, event sourcing engines, and high-frequency trading systems. It is currently [under review][5] for merging to GHC HEAD.

Finally, an obligatory disclaimer. The work carried out by Well-Typed has been sponsored by my employer, Standard Chartered. All the views expressed in this blog post are my own and not that of my employer. 

[1]: https://www.youtube.com/watch?v=7_ig6r2C-d4
[2]: https://www.reddit.com/r/haskell/comments/81r6z0/trying_out_ghc_compact_regions_for_improved/
[3]: https://stackoverflow.com/questions/36772017/reducing-garbage-collection-pause-time-in-a-haskell-program
[4]: http://hackage.haskell.org/package/compact-0.1.0.1
[5]: https://gitlab.haskell.org/ghc/ghc/merge_requests/972
[pauses]: pauses.svg
[runtimes]: runtimes.svg
[nix]: http://pepeiborra.github.com/gc-benchmarks/default.nix
