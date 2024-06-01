CC = gcc
PP = g++


all = alpha_parser

alpha_parser: src/alpha_parser.o src/alpha_lexical_analyzer.o src/symtable.o
	$(CC) -g $^ -o $@ 

src/symtable.o: src/lib/symtable.c src/lib/definitions.h
	$(CC) -g -c $< -o $@ 

src/alpha_lexical_analyzer.o: src/alpha_lexical_analyzer.c src/alpha_parser.h src/lib/symtable.h
	$(CC) -g -c $< -o $@


src/alpha_lexical_analyzer.c: src/alpha_lexical_analyzer.l
	flex --outfile=$@ $<

src/alpha_parser.o: src/alpha_parser.c
	$(CC) -g -c $^ -o $@


src/alpha_parser.c: src/alpha_parser.y src/lib/symtable.h
	bison -v --yacc --defines --output=$@ $<


clean:
	rm -f alpha_parser src/*.o src/*.c src/*.cpp src/*.hpp src/*.h src/*.output quads.txt instructions.txt
