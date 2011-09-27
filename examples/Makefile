ERL=erl
LIB_DIR=../..

all:
	ERL_LIBS=$(LIB_DIR) $(ERL) -make

clean:
	rm -f *.beam

test:
	ERL_LIBS=$(LIB_DIR) $(ERL) -s ssl start -s simplebot test
