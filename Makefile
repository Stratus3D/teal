PROJECT = teal

.PHONY: console

CT_SUITES = teal teal_lists teal_modules teal_processes teal_behaviours \
			teal_types teal_numbers

console: all
	erl -pa ebin/

include erlang.mk
