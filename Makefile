REBAR ?= rebar

IS_REBAR3:=$(shell expr `$(REBAR) --version | awk -F '[ .]' '/rebar / {print $$2}'` '>=' 3)

all: src

src:
	$(REBAR) compile

clean:
	$(REBAR) clean

ifeq "$(IS_REBAR3)" "1"
test:
	$(REBAR) eunit -v
else
test: all
	$(REBAR) -v skip_deps=true eunit
endif

ifeq "$(IS_REBAR3)" "1"
dialyzer:
	$(REBAR) dialyzer
else
dialyzer/erlang.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/erlang.plt \
	-o dialyzer/erlang.log --apps kernel stdlib erts inets crypto compiler edoc tools syntax_tools xmerl; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/p1_utils.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/p1_utils.plt \
	-o dialyzer/p1_utils.log ebin; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

erlang_plt: dialyzer/erlang.plt
	@dialyzer --plt dialyzer/erlang.plt --check_plt -o dialyzer/erlang.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

p1_utils_plt: dialyzer/p1_utils.plt
	@dialyzer --plt dialyzer/p1_utils.plt --check_plt -o dialyzer/p1_utils.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer: erlang_plt p1_utils_plt
	@dialyzer --plts dialyzer/*.plt --no_check_plt \
	--get_warnings -o dialyzer/error.log ebin; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi
endif

.PHONY: clean src test dialyzer erlang_plt p1_utils_plt
