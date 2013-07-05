# C++ compiler to use
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    CXX=g++
    # flags passed to compiler
    CXXFLAGS=-g -pedantic -std=c++11 -Wall
endif
ifeq ($(UNAME_S),Darwin)
    CXX=clang++
    # Flags passed to compiler
    CXXFLAGS=-g -pedantic -stdlib=libc++ -Wall
endif

# programs being compiled
TARGETS=agentModel

# various targets and how to compile them
all: $(TARGETS)

agentModel: turtle.o agentbased.o
	$(CXX) $(CXXFLAGS) -o $@ $^

clean:
	$(RM) $(TARGETS) *.o a.out *\~
