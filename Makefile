# Compiler config
ocamlc ?= ocamlopt
flags ?= -g -w -8

ifeq ($(ocamlc), ocamlopt)
	obj = cmx
	lib = cmxa
else
	obj = cmo
	lib = cma
endif

# Libraries needed
libraries = str

# Source dependencies
lang_deps = utils
unparse_deps = lang
simplify_deps = lang utils
parse_deps = lang utils bark
demo_deps = simplify parse unparse
tests_deps = parse unparse

# Main modules
programs = demo tests

# Build rules
.PHONY: all clean
all: $(programs)
clean:
	rm -rf *.cmi *.cmo *.cmx *.o $(programs)

$(programs):
	$(ocamlc) $(flags) $(libraries:=.$(lib)) $^ -o $@ 

%.cmi: %.mli
	$(ocamlc) -opaque $(flags) -c $<

%.cmi %.$(obj): %.ml
	$(ocamlc) $(flags) -c $<

# Dependency helper functions
resolve = $($(1)_deps:=.$(2))
transitive = $(foreach dep,$($(1)_deps),$(call transitive,$(dep),$(2))) $(1).$(2)

# Register dependencies as declared
$(foreach i,$(wildcard *.mli),$(eval $(i:.mli=.$(obj)): $(i:.mli=.cmi)))
$(foreach src,$(wildcard *.ml),$(eval $(src:.ml=.cmi) $(src:.ml=.$(obj)) &: $(call resolve,$(src:.ml=),cmi)))
$(foreach prog,$(programs),$(eval $(prog): $(call transitive,$(prog),$(obj))))