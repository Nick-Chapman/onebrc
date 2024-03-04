
k: res/k
	git diff --no-index --word-diff=color ../expect/k.res res/k

m: res/m
	git diff --no-index --word-diff=color ../expect/m.res res/m

t: res/t
	git diff --no-index --word-diff=color ../expect/t.res res/t

res/k: main.exe
	bash -c 'time ./main.exe ../data/k.txt > $@'

res/m: main.exe
	bash -c 'time ./main.exe ../data/m.txt > $@'

res/t: main.exe
	bash -c 'time ./main.exe ../data/t.txt > $@'

main.exe: main.o Makefile
	gcc main.o -o main.exe

main.o: main.c Makefile
	gcc -O1 -Wall -Werror -c main.c -o main.o
