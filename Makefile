BUILD   := build
SOURCES := src

MAIN :=	giferly

ERLFILES := $(foreach dir,$(SOURCES),$(notdir $(wildcard $(dir)/*.erl)))
TARGETS  := $(ERLFILES:%.erl=%.beam)

ERL  := erl
ERLC := erlc

IN := gfx/rgb-stripes-transparent.gif

.PHONY: all run clean

all: $(TARGETS)

run:
	# Once the module has been compiled, run the module's entry function, then
	# run `init:stop` to end the process.
	#
	# Notice the `-noshell` to prevent a sub-shell from starting.
	$(ERL) -pa $(BUILD) -noshell -run $(MAIN) go "$(IN)" -run init stop

$(BUILD):
	@echo creating build directory...
	@[ -d $@ ] || mkdir -p $@

%.beam: $(SOURCES)/%.erl $(BUILD)
	$(ERLC) -W -bbeam -o$(BUILD) $<

clean:
	@echo cleaning...
	@rm -fr $(BUILD)
