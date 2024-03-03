
top: res/million
	git diff --word-diff=color res/million

exe = .stack-work/dist/x86_64-linux/ghc-9.2.7/build/main.exe/main.exe

res/million: $(exe) data/million.txt Makefile
	bash -c 'time $(exe) million > $@'

$(exe): src/*.hs
	stack build; touch $(exe)
