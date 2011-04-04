ERL=erl
APPFILE=simpleirc.app

all: ebin/$(APPFILE)
	$(ERL) -make 

clean:
	rm -f ebin/*.beam
