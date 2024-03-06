
k: res/k
	git diff --no-index --word-diff=color ../expect/k.res res/k

m: res/m
	git diff --no-index --word-diff=color ../expect/m.res res/m

t: res/t
	git diff --no-index --word-diff=color ../expect/t.res res/t

exe = .stack-work/dist/x86_64-linux/ghc-9.2.7/build/main.exe/main.exe

res/k: $(exe)
	bash -c 'time $(exe) k > $@'

res/m: $(exe)
	bash -c 'time $(exe) m > $@'

res/t: $(exe)
	bash -c 'time $(exe) t > $@'

$(exe): src/*.hs
	stack build; touch $(exe)
