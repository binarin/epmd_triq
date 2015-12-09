C_SRC_TYPE = executable
C_SRC_OUTPUT = $(CURDIR)/priv/pseudo_node

DEPS = triq sync
PROJECT = epmd_triq
include erlang.mk
