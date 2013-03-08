.PHONY: all test

TESTS = $(wildcard tests/*.c)

PROGS = $(patsubst %.c,%,$(TESTS))

#PASSED = $(grep -c ./tests/results.txt)
#TESTS = $(wc -l ./tests/results.txt)

all: test

test: $(PROGS)
	@./tests/test-results.sh

%: %.c
	@./tests/test.sh $<
