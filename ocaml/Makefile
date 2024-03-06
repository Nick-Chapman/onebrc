
k: res/k
	git diff --no-index --word-diff=color ../expect/k.res res/k

m: res/m
	git diff --no-index --word-diff=color ../expect/m.res res/m

t: res/t
	git diff --no-index --word-diff=color ../expect/t.res res/t

exe = _build/default/bin/main.exe

res/k: $(exe)
	bash -c 'time $(exe) k > $@'

res/m: $(exe)
	bash -c 'time $(exe) m > $@'

res/t: $(exe)
	bash -c 'time $(exe) t > $@'
