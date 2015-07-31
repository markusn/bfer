.PHONY: compile get-deps eunit doc all dialyzer typer clean distclean
DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt

DIALYZER_OPTS := \
	-Wunmatched_returns \
	-Werror_handling \
	-Wrace_conditions \
	-Wunderspecs \
	-Wunknown

all: get-deps compile doc check

get-deps:
	rebar get-deps

compile:
	rebar skip_deps=true compile

doc:
	rebar doc

$(DEPSOLVER_PLT):
	dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
		--apps erts kernel stdlib -r deps

dialyzer: $(DEPSOLVER_PLT)
	dialyzer $(DIALYZER_OPTS) --plt $(DEPSOLVER_PLT) --src src

typer: $(DEPSOLVER_PLT)
	typer --plt $(DEPSOLVER_PLT) -r ./src

clean:
	rebar clean

eunit:
	rebar skip_deps=true -v eunit

check: eunit dialyzer
