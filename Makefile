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
enumerate_deps = utils lang
simplify_deps = utils lang
parse_deps = utils lang bark
unparse_deps = lang
demo_deps = lang simplify enumerate parse unparse
examples_deps = utils lang simplify parse unparse
parse_tests_deps = utils parse unparse

# Main modules
programs = demo examples parse_tests
outputs = demo.txt examples.md

# Build rules
.PHONY: all clean
all: $(programs) $(outputs)
clean:
	rm -rf *.cmi *.cmo *.cmx *.o $(programs)

$(outputs):
	./$< <<< "" > $@

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
$(foreach out,$(outputs),$(eval $(out): $(basename $(out))))
