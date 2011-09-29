ERL=erl
LIB_DIR=../..

all:
	ERL_LIBS=$(LIB_DIR) $(ERL) -make

clean:
	rm -f *.beam
	rm -f erl_crash.dump

test:
	ERL_LIBS=$(LIB_DIR) $(ERL) -s ssl start -s simplebot test
