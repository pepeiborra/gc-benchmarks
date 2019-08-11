# Shorter GC pauses coming to GHC

GHC >=8.10 is getting a new incremental garbage collector with a mark&sweep strategy for the older generation collections, as an alternative to the standard copy collector. Incrementality comes from performing the sweep phase concurrently with the mutator (i.e. the program), after a blocking, hopefully short marking phase. Ben Gamari gave a [talk][1] about it at MuniHac last year, please check it for all the details. Now that the collector is publicly available in the GHC repository, we can benchmark it to find out how much shorter the GC pauses are, and what the impact is in performance. The results are quite encouraging, and the new collector might be ready for mainstream use soon. 

All experiments are reproducible via a [Nix expression][nix] and [Shake][shake] script that will build the GHC branch, run the benchmarks and render the graphs.

## Benchmark methodology
To build GHC, you can check out the branch `wip/gc/ghc-8.8-rebase`, rebase it again on top of the 8.8 branch (including submodules), and then `./boot && ./configure && make -j4`. This is roughly what the Nix script does, using a pregenerated source snapshot to simplify things.

To test the new incremental garbage collector I use the well known [Pusher][3] problem: can the generation 1 pauses be short for a program that keeps a large amount of long-lived state in the heap ? The answer with the current garbage collector is no, but there are workarounds like [compact regions][2] that effectively move the data out of the garbage collected heap. These workarounds, however, require modifying or rewriting the program, and usually involve a sacrifice in performance. The new incremental collector should be able to reduce the Gen 1 pauses without any code changes, and I couldn't wait to see how much shorter the pauses are and how much performance is lost.

The code for the Pusher example is very short and included below for convenience: it uses a `Data.Map.Strict` to store up to `_N` bytestring messages in 2 million iterations. By varying `_N` we can control the size of the Haskell heap and relate it to the length of the Gen 1 pauses:
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
      if _N < Map.size inserted
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

## Results
### Pauses

The graph below shows the max length of the Gen 1 pauses per dataset size, both with the standard (red) and incremental (dotted) GC for various sizes of N.

![][pauses]

For the incremental collector the graph is showing the length of the marking pause (1a). In the copy collector case, the pause lengths are linear with _N as expected. For the incremental collector pauses are shorter, but still linear. On average, the incremental GC pauses are between five and six times shorter than the copying GC pauses. However, at 100ms, is this short enough? I asked Ben Gamari and he said:

>The Pusher benchmark allocates and retains lots of large objects (namely each message carries a large ByteString).
> However, the cost of the preparatory GC is linear in the number of large
> objects (the assumption being these are relatively rare in most
> programs) as we must clear the mark bit of each. Consequently, the
> preparatory collection pause scales linearly with the size of the test's queue. 
> In my experience it is rather unusual for programs to carry around millions of large and pinned bytearrays.

So the new collector is managing a 5-fold improvement on a worst case scenario. That's not bad but let's check the non worst case situation too; replacing the bytestrings with doubles yields the following graph:

![][pauses.double]

The pauses with the new collector are very short and clearly less-than-linear in the size of the surviving set, as promised.
Very good!

### Runtimes

In my benchmarks the incremental collector does not have any impact on the run time. If you think this is too good to be true, that makes two of us. The mark&sweep collector is able to perform the sweeping phase in parallel using a second core of the CPU, so this performance relies on having an extra CPU core available and will probably not hold if that's not the case.

The graph below shows the runtimes for each collector per dataset size:

![][runtimes]

If we replace the bytestrings with doubles, the outcome is pretty much the same:

![][runtimes.double]


### Memory

What effect does the new collector have on the total memory footprint of our process? To measure this, I again rely on the output of `+RTS -s`, concretely on the "maximum residency" line. The results are quite interesting. Encouragingly, the new collector uses less memory than the copy collector for the bytestrings example. 

![][maxResidency]

However, this behaviour is specific to bytestrings. If we look at the version using doubles, the new collector requires quite a bit more memory than the copy collector. 

![][maxResidency.double]

It's a bit surprising that the behaviour is so different between the two examples. More worryingly though, the memory usage seems to be linear with the number of iterations in our main loop. This is shown by the chart below illustrating the maximum residency per dataset size under the incremental collector for two different number of iterations. The two lines should be identical, as it is the case for the standard copy collector, but the graph shows the two lines differ, with the ExtraIterations version using more memory. What's going on ?

![][maxResidencyPerIterations]

Drilling into how the heap evolves over time for a concrete example, using the Live bytes column of the `+RTS -S ~RTS` output, we can see that the incremental collector is "lagging behind" the main program, allowing the heap to grow very large before completing the garbage collection.

![][liveBytesComparisonDouble]

For some reason this behaviour doesn't appear in the Bytestring example, and the incremental GC is able to collect the heap multiple times concurrently with the main program:

![][liveBytesComparisonBS]

## Conclusion

The incremental garbage collector offers shorter pauses than the copying collector without the need to change any code, and little to no performance costs assuming an extra core available. Compact regions afford more control to decide when and for how long to pause, and even to perform the compaction concurrently with the main program, therefore achieving pauses as short as desired with the same performance characteristics. But this is at the cost of significant complexity, whereas the incremental collector can be turned on with a simple flag. This holds the potential to make GHC a better fit for many applications that require shorter GC pauses, such as games, event sourcing engines, and high-frequency trading systems. But the collector is still [under review][5] for merging to GHC HEAD, and there are some issues with memory usage.

Finally, two obligatory disclaimers. First, these benchmarks are only as accurate as the output of `+RTS -S -RTS`; it's entirely possible that it does not give a full picture in the case of the new collector, as it is still in development. Secondly, I must mention that the work carried out by Well-Typed has been sponsored by my employer, Standard Chartered, but all the views expressed in this blog post are my own and not that of my employer. 

[1]: https://www.youtube.com/watch?v=7_ig6r2C-d4
[2]: https://www.reddit.com/r/haskell/comments/81r6z0/trying_out_ghc_compact_regions_for_improved/
[3]: https://stackoverflow.com/questions/36772017/reducing-garbage-collection-pause-time-in-a-haskell-program
[4]: http://hackage.haskell.org/package/compact-0.1.0.1
[5]: https://gitlab.haskell.org/ghc/ghc/merge_requests/972
[pauses]: Pauses.PusherBS.Normal.svg
[pauses.double]: Pauses.PusherDouble.Normal.svg
[runtimes]: Runtimes.PusherBS.Normal.svg
[runtimes.double]: Runtimes.PusherDouble.Normal.svg
[maxResidency]: MaxResidency.PusherBS.Normal.svg
[maxResidency.double]: MaxResidency.PusherDouble.Normal.svg
[maxResidencyPerIterations]: MaxResidency.PusherDouble.Incremental.svg
[liveBytesComparisonDouble]: Live.1600.ExtraIterations.PusherDouble.svg
[liveBytesComparisonBS]: Live.1600.ExtraIterations.PusherBS.svg
[nix]: https://github.com/pepeiborra/gc-benchmarks/blob/master/default.nix
[shake]: https://github.com/pepeiborra/gc-benchmarks/blob/master/Shake.hs
[nofib]: https://gitlab.haskell.org/ghc/ghc/wikis/building/running-no-fib
