

SHELL_DEPS = kjell
# SHELL_ERL = $(DEPS_DIR)/kjell/bin/kjell
SHELL_OPTS = -sname epmd_triq -sync non_descendants ignore

BUILD_DEPS = wrangler
DEPS = triq sync
PROJECT = epmd_triq
include erlang.mk
