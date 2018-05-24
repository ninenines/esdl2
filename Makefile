# See LICENSE for licensing information.

PROJECT = esdl2
PROJECT_DESCRIPTION = SDL2 Erlang NIF.
PROJECT_VERSION = 0.1.0

# Dependencies.

BUILD_DEPS = nif_helpers
dep_nif_helpers = git https://github.com/ninenines/nif_helpers master
DEP_PLUGINS = nif_helpers

TEST_DEPS = $(if $(CI_ERLANG_MK),ci.erlang.mk)

SDL2_CFLAGS = $(shell sdl2-config --cflags)
# SDL 2.0.3 has this option enabled that causes problems with NIF functions.
SDL2_LIBS_FILTER_OUT = -Wl,--no-undefined
SDL2_LIBS = $(filter-out $(SDL2_LIBS_FILTER_OUT),$(shell sdl2-config --static-libs))

# CI configuration.

dep_ci.erlang.mk = git https://github.com/ninenines/ci.erlang.mk master
DEP_EARLY_PLUGINS = ci.erlang.mk

AUTO_CI_OTP ?= OTP-19+
# AUTO_CI_HIPE ?= OTP-LATEST
# AUTO_CI_ERLLVM ?= OTP-LATEST
AUTO_CI_WINDOWS ?= OTP-19+

# Standard targets.

include erlang.mk

# SDL2 flags.

CFLAGS += $(SDL2_CFLAGS)
# @todo -undefined dynamic_lookup on OSX?
LDLIBS += $(SDL2_LIBS) -lSDL2_image -lSDL2_ttf

# Clean the environment before each CI builds.

ci-setup:: distclean-c_src-env

# Additional checks.

check:: cppcheck scan-build

cppcheck:
	$(gen_verbose) cppcheck -f -q --error-exitcode=2 \
		--enable=warning,style --inconclusive --std=posix \
		$(firstword $(SDL2_CFLAGS)) -U_System -USDL_CreateThread c_src/

scan-build:
	$(verbose) $(MAKE) clean
	$(gen_verbose) scan-build $(MAKE)

hello_sdl:: all
	erlc -o examples/hello_sdl examples/hello_sdl/*.erl
	cd examples/hello_sdl && ./start.sh

bullet_engine:: all
	erlc -o examples/bullet_engine examples/bullet_engine/*.erl
	cd examples/bullet_engine && ./start.sh
