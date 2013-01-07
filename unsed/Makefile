all: readlines-main

readlines-main: readlines-main.o readlines.o
		gcc -o readlines-main readlines-main.o readlines.o

readlines-main.o: readlines-main.c readlines.h	
		gcc -c -o readlines-main.o readlines-main.c

readlines.o: readlines.c readlines.h
		gcc -c -o readlines.o readlines.c