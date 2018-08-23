BUILD		:=	build
SOURCES		:=	src

MAIN		:=	$(shell basename $(CURDIR))

ERLFILES	:=	$(foreach dir,$(SOURCES),$(notdir $(wildcard $(dir)/*.erl)))
TARGETS		:=	$(ERLFILES:%.erl=%.beam)

ERL			:= erl
ERLC		:= erlc

IN	:= gfx/rgb-stripes-transparent.gif

.PHONY: $(BUILD) run clean

$(BUILD) :
	@[ -d $@ ] || mkdir -p $@
	@make $(TARGETS)

run:
	# Once the module has been compiled, run the module's entry function, then
	# run `init:stop` to end the process.
	#
	# Notice the `-noshell` to prevent a sub-shell from starting.
	$(ERL) -pa $(BUILD) -noshell -run $(MAIN) go "$(IN)" -run init stop

clean:
	@echo clean ...
	@rm -fr $(BUILD)

%.beam : $(SOURCES)/%.erl
	$(ERLC) -W -bbeam -o$(BUILD) $<
