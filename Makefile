all:
	rebar compile

doc:
	rebar doc

check: all
	rebar -v eunit
	dialyzer --src src
