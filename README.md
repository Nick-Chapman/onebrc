# onebrc

My attempts at the Billion Row Challenge. In Haskell, C and Ocaml.

### The original challenge repo
[The One Billion Row Challenge](https://github.com/gunnarmorling/1brc)

Start with one million... Now ten million...

Using prefix letters `[kmthb]` for: thousand, million, ten-million, hundred-million, billion.


### Timings (on my machine). Best of 3.

Comparing my runtimes for the published runtimes of the Java solutions, I estimate the slowdown factor for my machine w.r.t the competition machine to be:  x15 -- x20.

#### 10 million
- _Java baseline_ 3.6s
- _Java #1 (thomaswue)_ (no native) 0.5s
- _Java #2 (artsiomkorzum)_ (no native) 0.6s
- _Java #4 (serkan-ozal)_ 0.8
- _My latest Haskell_ 22s
- _My latest C_ 1.8s
- _My BEST Ocaml_ 2.3s
- _My latest Ocaml_ 2.5s

#### 100 million
- _Java baseline_ 32s
- _Java #1 (thomaswue)_ (no native) 1.6s
- _Java #2 (artsiomkorzum)_ (no native) 1.5s
- _Java #4 (serkan-ozal)_ 1.6s
- _My latest C_ 18s
- _My BEST Ocaml_ 23s
- _My latest Ocaml_ 25s

#### 1 billion
- _Java #1 (thomaswue)_ (no native) 29s
- _Java #2 (artsiomkorzum)_ (no native) 28s
- _Java #4 (serkan-ozal)_ 27s
- _My latest C_ 194s (3m 14s)


#### Build/run/time my C solutions
```
(cd c; make main.exe)
time c/main.exe data/t.txt > /dev/null
time c/main.exe data/h.txt > /dev/null
```

#### Build/run/time my Haskell solutions
```
stack build
rm res/m ; make m
rm res/t ; make t
```

#### Data and expected results
```
$ l data/[kmthb].txt
-rw-rw-r-- 1 nic nic 13795376673 Mar  5 13:40 data/b.txt
-rw-rw-r-- 1 nic nic  1379529866 Mar  4 15:18 data/h.txt
-rw-rw-r-- 1 nic nic       13856 Mar  4 11:30 data/k.txt
-rw-rw-r-- 1 nic nic    13788056 Mar  4 11:31 data/m.txt
-rw-rw-r-- 1 nic nic   137955363 Mar  4 11:31 data/t.txt

$ l data/[kmthb].res
-rw-rw-r-- 1 nic nic 10664 Mar  4 15:20 data/h.res
-rw-rw-r-- 1 nic nic  9060 Mar  4 11:31 data/k.res
-rw-rw-r-- 1 nic nic 10558 Mar  4 11:32 data/m.res
-rw-rw-r-- 1 nic nic 10651 Mar  4 11:32 data/t.res
```
