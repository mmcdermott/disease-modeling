# C++ compiler to use
CXX=g++

# Flags passed to compiler
CXXFLAGS=-g -pedantic -std=c++11 -Wall
#CXXFLAGS=-std=c++11

# programs being compiled
TARGETS=agentModel

# various targets and how to compile them
all: $(TARGETS)

agentModel: turtle.o agentbased.o
	$(CXX) $(CXXFLAGS) -o $@ $^

clean:
	$(RM) $(TARGETS) *.o a.out *\~
