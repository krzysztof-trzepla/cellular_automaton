REBAR := $(shell pwd)/rebar
.PHONY: deps

all: deps compile

deps:
	@${REBAR} get-deps

compile:
	@${REBAR} compile

clean:
	@${REBAR} clean

distclean: clean
	@${REBAR} delete-deps
	@rm -rf rel/cellular_automaton

generate:
	@cd rel && ${REBAR} generate

##
## Release
##

rel: deps compile generate

##
## Dialyzer targets local
##

PLT ?= .dialyzer.plt

# Builds dialyzer's Persistent Lookup Table file.
.PHONY: plt
plt:
	@dialyzer --check_plt --plt ${PLT}; \
	if [ $$? != 0 ]; then \
		dialyzer --build_plt --output_plt ${PLT} --apps kernel stdlib sasl erts \
			eunit compiler; \
	fi; exit 0

# Dialyzes the project.
dialyzer: plt
	@dialyzer ./ebin --plt ${PLT} -Werror_handling -Wrace_conditions --fullpath

##
## Testing
##

eunit:
	@${REBAR} eunit skip_deps=true
