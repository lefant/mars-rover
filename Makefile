all: compile

compile:
	@erl -make

clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump

run:
	erl -sname console -pa ebin
