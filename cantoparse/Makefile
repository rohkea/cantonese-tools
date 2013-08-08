CC=gcc

parse: parse.o wordlist.o
	gcc -o parse parse.o wordlist.o

parse.o: parse.c common.h wordlist.h
	${CC} -c -o parse.o parse.c

wordlist.o: common.h wordlist.c wordlist.h
	${CC} -c -o wordlist.o wordlist.c

clean:
	rm -f parse *.o

.PHONY: parse
