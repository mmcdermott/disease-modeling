# C++ compiler to use
UNAME_S := $(shell uname -s)
agentTarget := agentModelStrictHill
ifeq ($(UNAME_S),Linux)
    CXX=g++
    # flags passed to compiler
    CXXFLAGS=-g -pedantic -std=c++11 -Wall
    OPEN := evince
endif
ifeq ($(UNAME_S),Darwin)
    CXX=clang++
    # Flags passed to compiler
    CXXFLAGS=-g -pedantic -stdlib=libc++ -Wall
    OPEN := open
endif

# programs being compiled
TARGETS = $(agentTarget)

# various targets and how to compile them
all: $(TARGETS)

agentModelStrictHill: turtleStrictHill.o agentbasedStrictHill.o
	$(CXX) $(CXXFLAGS) -o $@ $^

turtleStrictHill: turtleStrictHill.o
	$(CXX) $(CXXFLAGS) -o $@ $^

plots: $(agentTarget)
	time ./$(agentTarget) 1 1 && R < plotData.R --save --args ${DELTA_T} && ${OPEN} Rplots.pdf

just_plots: 
	R < plotData.R --save --args ${DELTA_T} && ${OPEN} Rplots.pdf

test_run: clean clean_plots plots

experiment_setup: clean clean_plots agentModelStrictHill

clean_plots:
	$(RM) Rplots*.* && $(RM) incData.csv

clean:
	$(RM) $(TARGETS) *.o a.out *\~ modelData.csv
