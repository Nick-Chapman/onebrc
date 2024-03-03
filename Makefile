
m: res/million
	git diff --word-diff=color res/million

k: res/thousand
	git diff --word-diff=color res/thousand

exe = .stack-work/dist/x86_64-linux/ghc-9.2.7/build/main.exe/main.exe

res/million: $(exe) data/million.txt Makefile
	bash -c 'time $(exe) million > $@'

res/thousand: $(exe) data/thousand.txt Makefile
	bash -c 'time $(exe) thousand > $@'

$(exe): src/*.hs
	stack build; touch $(exe)
