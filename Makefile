# TODO: add build for libyaml

helper.o : helper.h helper.c
	$(CC) `idris --include` -c helper.c -o helper.o

.PHONY: clean
clean :
	-rm helper.o
