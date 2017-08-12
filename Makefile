PROJECT = teal

.PHONY: console

BUILD_DEPS = hexer_mk

dep_hexer_mk = git https://github.com/inaka/hexer.mk.git 1.1.0

DEP_PLUGINS = hexer_mk

EDOC_OPTS = {overview, "edoc/overview.edoc"}

include erlang.mk

console: all
	erl -pa ebin/
