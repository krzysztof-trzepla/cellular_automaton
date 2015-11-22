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

dev:
	@for i in 1 2 3; do \
		echo "Creating rel/cellular_automaton_dev$$i"; \
		cp -R rel/cellular_automaton rel/cellular_automaton_dev$$i; \
		sed -i -e s/"'worker@127.0.0.1'"/"'worker1@127.0.0.1','worker2@127.0.0.1',\
		'worker3@127.0.0.1'"/g rel/cellular_automaton_dev$$i/releases/1/sys.config; \
		sed -i -e s/worker@127.0.0.1/worker$$i@127.0.0.1/g \
		rel/cellular_automaton_dev$$i/releases/1/vm.args; \
	done

devclean:
	@for i in 1 2 3; do \
		echo "Removing rel/cellular_automaton_dev$$i"; \
		rm -rf rel/cellular_automaton_dev$$i; \
	done

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
