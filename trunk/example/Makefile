
all: example.bin

example.bin : example.fs
	gforth $< -e 'interpreter cdata flash dump-section: $@ host CR BYE'

run:
	@echo "*** Abort emulation by executing 'killall softgun' ***"
	softgun -c softgun.cfg -l 0 example.bin
clean:
	rm -f example.bin

