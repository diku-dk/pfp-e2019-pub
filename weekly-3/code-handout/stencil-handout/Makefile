CXX        = g++

SRC1 = blur-fusion.cpp
SRC2 = it1d-stencil.cpp
EXECUTABLE1  = blur-fusion
EXECUTABLE2  = it1d-stencil

default: compile

.cu.o: $(SOURCES_CPP) $(HELPERS)
	$(CXX) -c $@ $<


compile: $(EXECUTABLE)
$(EXECUTABLE1): $(SRC1)
	$(CXX) -o $(EXECUTABLE1) -fopenmp -O3 -std=c++11 $(SRC1)
#	$(CXX) -o $(EXECUTABLE) -fopenmp -O3 -std=c++11 -mavx -ftree-vectorizer-verbose=1 $(SOURCES_CPP)

$(EXECUTABLE2): $(SRC2)
	$(CXX) -o $(EXECUTABLE2) -fopenmp -O3 -std=c++11 $(SRC2)

run-blur: $(EXECUTABLE1)
	./$(EXECUTABLE1)

run-it1d: $(EXECUTABLE2)
	./$(EXECUTABLE2)

clean:
	rm -f $(EXECUTABLE)
