all: compile ct

compile:
	./rebar compile

ct:
	./rebar ct
