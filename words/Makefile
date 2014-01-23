.DEFAULT_GOAL: build

SRC_FILES = examples.m util.m unittests.m words.m grid.m
BINARIES = words unittests

build: $(BINARIES)

words: $(SRC_FILES)
	mmc --make words

unittests: $(SRC_FILES)
	mmc --make unittests

test: unittests
	./unittests

clean:
	rm -f *.err *.mh
	rm -rf Mercury
	rm -f $(BINARIES)
