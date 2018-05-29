HEADERS = program.h headers.h
NAME = pilisp

default: program

program.o: program.c $(HEADERS)
    gcc -c program.c -o program.o

program: program.o
    gcc program.o -o program

clean:
    -rm -f program.o
    -rm -f program