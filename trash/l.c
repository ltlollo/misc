#include <err.h>
#include <memory.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdint.h>
#include <stdio.h>
#include <assert.h>

typedef int TokId;

struct Type {
	int size;
	TokId name;
	short kind;
	TokId aggr[];
};


struct Stable_Poly_Alloc {
	char *data;
	TokId size;
	TokId alloc;
};

struct Stable_Poly_Alloc types = { 0 };
struct Stable_Poly_Alloc names = { 0 };

TokId
add_poly(struct Stable_Poly_Alloc *mm, void *ele, int size) {
	TokId res = mm->size;
	size_t alloc = mm->alloc;

	if (mm->size + size > mm->alloc) {
		alloc = alloc * 2 + size;
		mm->data = (char *)realloc(mm->data, alloc);
		if (mm->data == 0) {
			err(1, "alloc");
		} else {
			mm->alloc = alloc;
		}
	}
	memcpy(mm->data + mm->size, ele, size);
	mm->size += size;
	return res;
}

#define ADD_TYPE(alloc, ele) add_poly(alloc, &ele, sizeof (ele))

struct Tok {
	enum Kind {
		VAR, GRAM, NUM, STR, COMM
	} kind;
	union {
		TokId id;
	};
};


enum ParseErr {
	PARSE_OK,
	PARSE_ERR,
	PARSE_INCOMPLETE,
};

struct Stable_Poly_Alloc tokstream = { 0 };

#define MONO_BEG(poly, type) (type *)(poly)->data;
#define MONO_END(poly, type) (type *)((poly)->data + (poly)->size);

enum GRAM {
	GRAM_DIV,
	GRAM_MUL,
	GRAM_COMMA,
	GRAM_PLUS,
	GRAM_MINUS,
	GRAM_LESS,
	GRAM_MORE,
	GRAM_EQ,
	GRAM_COLON,
	GRAM_LPAREN,
	GRAM_RPAREN,
	GRAM_LBRACK,
	GRAM_RBRACK,
	GRAM_LBRACE,
	GRAM_RBRACE,
	GRAM_TYPE,
	GRAM_NL,
	GRAM_TO,
	GRAM_FOR,
	GRAM_WHILE,
	GRAM_IF,
};

int
num(char a) {
	return a >= '0' && a <= '9';
}

int
alpha_(char a) {
	return
		(a >= 'a' && a <= 'z') ||
		(a >= 'A' && a <= 'A') || 
		(a == '_');
}

TokId
alloc_add_str(char *str, size_t size, struct Stable_Poly_Alloc *names) {
	TokId curr_id = add_poly(names, str, size + 1);
	names->data[curr_id + size] = 0;
	return curr_id;
}

static const char *gram_repr[] = {
	[GRAM_DIV]		=	" / ",
	[GRAM_MUL]		=	" * ",
	[GRAM_TYPE]		=	" # ",
	[GRAM_TO]		=	" -> ",
	[GRAM_PLUS]		=	" + ",
	[GRAM_MINUS]	=	" - ",
	[GRAM_LESS]		=	" < ",
	[GRAM_MORE]		=	" > ",
	[GRAM_EQ]		=	" = ",
	[GRAM_COLON]	=	" : ",
	[GRAM_COMMA]	=	",",
	[GRAM_LPAREN]	=	"(",
	[GRAM_RPAREN]	=	")",
	[GRAM_LBRACK]	=	"[",
	[GRAM_RBRACK]	=	"]",
	[GRAM_LBRACE]	=	"{",
	[GRAM_RBRACE]	=	"}",
	[GRAM_NL]		=	"\n",

	[GRAM_FOR]		= "for",
	[GRAM_WHILE]	= "while",
	[GRAM_IF]		= "if",
};

#define MATCH(g, b, e) (strncmp(b, gram_repr[g], e - b) == 0)
#define RET_MATCH(g, b, e) do { if (MATCH(g, b, e)) return g } while (0)

int
check_keywrd(char *beg, char *end) {
	RET_MATCH(GRAM_FOR, beg, end);
	RET_MATCH(GRAM_WHILE, beg, end);
	RET_MATCH(GRAM_IF, beg, end);
	return -1;
}

struct Tok *
parse_string(char *beg, char *end, struct Stable_Poly_Alloc *names,
			 struct Stable_Poly_Alloc *tokstream) {
	TokId curr_id;
	struct Tok curr_tok;
	char *emit;
	int size;

	size_t last = tokstream->size;
	size_t names_snap = names->size;
	int keyw;

	while (beg != end) {
		if (alpha_(*beg)) {
			emit = beg;
			beg++;
			while (alpha_(*beg) || num(*beg)) {
				beg++;
			}
			keyw = check_keywrd(beg, end);
			if (keyw != -1) {
				curr_tok = { Tok::GRAM, keyw };
				ADD_TYPE(tokstream, curr_tok);
				continue;
			}
			curr_id = alloc_add_str(emit, beg - emit, names);
			curr_tok.kind = Tok::VAR;
			curr_tok.id = curr_id;
			ADD_TYPE(tokstream, curr_tok);
		} else if (*beg == ' ' || *beg == '\t') {
			beg++;
		} else if (*beg == '/') {
			if (beg[1] == '/') {
				emit = beg;
				while (*beg != 0) {
					beg++;
				}
				curr_id = alloc_add_str(emit, beg - emit, names);
				curr_tok.kind = Tok::COMM;
				curr_tok.id = curr_id;
				ADD_TYPE(tokstream, curr_tok);
			} else {
				curr_tok = { Tok::GRAM, GRAM_DIV };
				ADD_TYPE(tokstream, curr_tok);
				beg++;
			}
		} else {
			switch (*beg) {
			case ',': curr_tok = { Tok::GRAM, GRAM_COMMA }; break;
			case '-':
					  if (beg[1] == '>') {
						  beg++;
						  curr_tok = { Tok::GRAM, GRAM_TO }; break;
					  }
					  curr_tok = { Tok::GRAM, GRAM_MINUS }; break;
			case ':': curr_tok = { Tok::GRAM, GRAM_COLON }; break;
			case '#': curr_tok = { Tok::GRAM, GRAM_TYPE }; break;
			case '+': curr_tok = { Tok::GRAM, GRAM_PLUS }; break;
			case '*': curr_tok = { Tok::GRAM, GRAM_MUL }; break;
			case '(': curr_tok = { Tok::GRAM, GRAM_LPAREN }; break;
			case ')': curr_tok = { Tok::GRAM, GRAM_RPAREN }; break;
			case '[': curr_tok = { Tok::GRAM, GRAM_LBRACK }; break;
			case ']': curr_tok = { Tok::GRAM, GRAM_RBRACK }; break;
			case '{': curr_tok = { Tok::GRAM, GRAM_LBRACE }; break;
			case '}': curr_tok = { Tok::GRAM, GRAM_RBRACE }; break;
			case '\n': curr_tok = { Tok::GRAM, GRAM_NL }; break;
			default:
					   printf("ERROR: malformed expression\n"
							  "%c is not a valid token\n", *beg);
					   names->size = names_snap;
					   return (struct Tok *)(tokstream->data + last);
			}
			ADD_TYPE(tokstream, curr_tok);
			beg++;
		}
	}
	return (struct Tok *)(tokstream->data + last);
}

void
print_pretty(struct Tok *beg, struct Tok *end,
			 struct Stable_Poly_Alloc *names) {

	int nest = 0;
	enum { PREN, BRAK, BRAC } last;
	while (beg != end) {
		switch(beg->kind) {
		case Tok::VAR:
			printf("%s", names->data + beg->id);
		case Tok::GRAM:

		}
	}
}

void
print_toks(struct Tok *beg, struct Tok *end, struct Stable_Poly_Alloc *names) {
	while (beg != end) {
		switch (beg->kind) {
		case Tok::VAR:
			printf("VAR: %s\n", names->data + beg->id);
			break;
		default:
			printf("UNIMPL TOK %d\n", beg->kind);
		}
		beg++;
	}
}

void
phase_1(struct Tok *beg, struct Tok *end) {
	Tok *next = beg + 1;

	while (beg != end) {
		

		next++;
		beg++;
	}
}

int
main() {
	char *line = NULL;	
	size_t alloc = 0;
	ssize_t size = 0;

	do {
		size = getline(&line, &alloc, stdin);
		if (size == -1) {
			err(1, "getline");
		} else if (size == 1 || size == 0) {
			continue;
		} else if (size == 2) {
			if (*line != '\\') {
				fprintf(stderr, "ERROR: unexpected symbol %s", line);
				continue;
			}
			size = getdelim(&line, &alloc, '\\', stdin);
			if (size == -1) {
				err(1, "getdelim");
			}
			assert(line[size - 1] == '\\');
			line[size - 1] = 0;
			size -= 1;
			if (line[size - 1] == '\n') {
				line[size - 1] = 0;
				size -= 1;
			}
		} else {
			assert(line[size - 1] == '\n');
			line[size - 1] = 0;
			size -= 1;
		}
		struct Tok *curr = parse_string(line, line + size, &names, &tokstream); 
		struct Tok *end = MONO_END(&tokstream, struct Tok);

		print_toks(curr, end, &names);
	} while (1);
}
