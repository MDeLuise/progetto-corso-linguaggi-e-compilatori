COMMON := AbsGrammar.hs ErrM.hs LexGrammar.hs ParGrammar.hs SkelGrammar.hs Type.hs
PRINT := $(COMMON) PrintGrammar.hs Printer.hs
TAC := $(COMMON) TypeChecking.hs TACGen.hs
MINE := $(TAC) Mine.hs
TESTS := $(TAC) Test.hs
MAIN_SOURCES := Mine.hs Test.hs Printer.hs

HS_SOURCES := $(TAC) $(MAIN_SOURCES) PrintGrammar.hs

OBJECTS := $(patsubst %.hs, %.o, $(HS_SOURCES))
INTERFACES := $(patsubst %.hs, %.hi, $(HS_SOURCES))

EXECS := $(patsubst %.hs, %, $(MAIN_SOURCES))

TEST := 0

.PHONY: clean demo do_tests testout

Mine: $(MINE)
	ghc Mine.hs

demo: Mine Printer
	@echo "DEMO:"
	@echo "input from file testcases/demo:"
	@cat testcases/demo
	@echo "TAC:"
	@./Mine testcases/demo

do_tests: Test
	./Test

Test: $(TESTS)
	ghc Test.hs

Printer: $(PRINT)
	ghc Printer.hs

ParGrammar.hs: ParGrammar.y
	happy -gca ParGrammar.y

LexGrammar.hs: LexGrammar.x
	alex -g LexGrammar.x

testout: Mine
	@./Mine -s testcases/$(TEST) | tail -n +2 | tr '\n' '|' | sed -e 's/|/\\n/g' -e 's/\t/\\t/g' -e 's/"/\\"/g' | head -c -2

clean:
	rm -f $(OBJECTS) $(INTERFACES) $(EXECS) ParGrammar.hs LexGrammar.hs
