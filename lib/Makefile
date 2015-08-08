all: libfastconvert.so

fastconvert.o: fastconvert.c fastconvert.h
	clang -c -fpic -std=c99 fastconvert.c -o fastconvert.o

libfastconvert.so: fastconvert.o
	clang -shared fastconvert.o -o libfastconvert.o

clean:
	rm -rf fastconvert.o libfastconvert.so
