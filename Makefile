ERL=erl
APPFILE=simpleirc.app

all: ebin/$(APPFILE)
	$(ERL) -make 

clean:
	rm -f ebin/*.beam

test:
	cd examples; \
	make; \
	make test

# (cd ebin && erl -s application load simpleirc -s ssl start -s simplebot test)
