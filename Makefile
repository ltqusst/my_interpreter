CPP=g++
CFLAGS=-I. -g -O0
DEPS = 

%.o: %.cpp $(DEPS)
	$(CPP) $(CFLAGS) -c -o $@ $<

test0: test0.o
	$(CPP) -o test0 test0.o

clean:
	rm *.o
