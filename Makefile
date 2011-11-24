BUILD		:=	build
SOURCES		:=	src

MAIN		:=	$(shell basename $(CURDIR))

ERLFILES	:=	$(foreach dir,$(SOURCES),$(notdir $(wildcard $(dir)/*.erl)))
TARGETS		:=	$(ERLFILES:%.erl=%.beam)

ERL			:= erl
ERLC		:= erlc
ESDLDIR		:= /usr/lib/esdl
ERLINC		:= $(ESDLDIR)/include

ERL_COMPILE_FLAGS	:= -pa $(ESDLDIR)/ebin

.PHONY: $(BUILD) run clean

$(BUILD) :
	@[ -d $@ ] || mkdir -p $@
	@make $(TARGETS)

run:
	$(ERL) -pa $(BUILD) -pa $(ESDLDIR)/ebin $(MAIN) -run $(MAIN) go

clean:
	@echo clean ...
	@rm -fr $(BUILD)

%.beam : $(SOURCES)/%.erl
	$(ERLC) -W -I$(ERLINC) -bbeam \
		$(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(BUILD) $<
