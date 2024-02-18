CC = gcc


all = alpha_lexical_analyzer

alpha_lexical_analyzer: src/alpha_lexical_analyzer.c
	$(CC) src/alpha_lexical_analyzer.c -o alpha_lexical_analyzer

alpha_lexical_analyzer.c: src/alpha_lexical_analyzer.l
	flex --outfile=alpha_lexical_analyzer.c src/alpha_lexical_analyzer.l

clean:
	@ -rm -f alpha_lexical_analyzer src/alpha_lexical_analyzer.c alpha_lexical_analyzer.h
