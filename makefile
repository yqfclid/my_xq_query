
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

all:
	@$(REBAR) get-deps compile
clean_compile:
	@$(REBAR) clean compile
only_compile:
	@$(REBAR) compile
clean:
	@$(REBAR) clean
remove:
	@$(REBAR) delete-deps 
get:
	@$(REBAR) get-deps 
publish:
	@$(REBAR) generate 
