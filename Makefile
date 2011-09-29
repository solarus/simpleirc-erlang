REBAR=./rebar
APPFILE=simpleirc.app

all: ebin/$(APPFILE)
	$(REBAR) compile

clean:
	$(REBAR) clean
	rm -f ebin/erl_crash.dump
	cd examples; \
	make clean

test:
	cd examples; \
	make; \
	make test

# (cd ebin && erl -s application load simpleirc -s ssl start -s simplebot test)
