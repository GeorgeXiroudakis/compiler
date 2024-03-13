CC = gcc
PP = g++


all = alpha_parser

alpha_parser: src/alpha_parser.o src/alpha_lexical_analyzer.o
	$(PP) $^ -o $@

src/alpha_lexical_analyzer.o: src/alpha_lexical_analyzer.c alpha_parser.h
	$(CC) -c $^ -o $@


src/alpha_lexical_analyzer.c: src/alpha_lexical_analyzer.l
	flex --outfile=$@ $<

alpha_parser.o: src/alpha_parser.cpp
	$(PP) -c $^ -o $@


src/alpha_parser.cpp: src/alpha_parser.y
	bison --yacc --defines --output=$@ $<


clean:
	rm -f alpha_parser src/alpha_lexical_analyzer.c src/alpha_parser.cpp src/alpha_parser.h src/*.o
