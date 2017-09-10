# TODO: add build for libyaml
# TODO: is that correct that I need to put the h/o/so files in the root dir
# of a project so that Idris can copy them while building? Seems like it's not possible
# to have different path in `objs` clause...

yaml.h :
	-cp vendor/libyaml/include/yaml.h .

libyaml.so :
	-cd vendor/libyaml && ./bootstrap && ./configure && make
	-cp vendor/libyaml/src/.libs/libyaml.so .

helper.o : helper.h helper.c libyaml.so
	$(CC) `idris --include` -c helper.c -o helper.o

.PHONY: clean
clean :
	-rm helper.o
