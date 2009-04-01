all: compile

compile:
	@erl -make

clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump

run: compile
#	erl -sname console -pa ebin -s main run
	bin/run localhost 17676
