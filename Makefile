all: compile ct eunit

compile:
	rebar3 compile

ct:
	rebar3 ct

eunit:
	rebar3 eunit
