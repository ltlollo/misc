#include <stdio.h>

enum Input {
	None,
	E,
	N,
	EN,
	P,
	EP,
	NP,
	NEP
};

struct State {
	unsigned char delay;	/*ms*/
	unsigned long out_b;	/*PortB output*/
	unsigned long out_f;	/*PortF output*/
	unsigned long next[8];	/*Next state*/
};

#define E(c) (c << 3)
#define N(c) (c << 0)
#define G	 (0x1 << 0)
#define Y 	 (0x1 << 1)
#define R 	 (0x1 << 2)
#define PR	 (0x1 << 1)
#define PG 	 (0x1 << 3)
#define P_ 	 (0x0 << 0)

#define EN(a, b) (a|b)
#define SAME(SYM)		\
		[None]	= SYM,	\
		[E]		= SYM,	\
	 	[N]		= SYM,	\
	 	[EN]	= SYM,	\
	 	[P]		= SYM,	\
	 	[EP]	= SYM,	\
	 	[NP]	= SYM,	\
	 	[NEP]	= SYM 

enum StateN {
	GE,
	GN,
	GP,
	E2N,
	N2E,
	E2P,
	N2P,
	P2N0,
	P2N1,
	P2N2,
	P2N3,
	P2E0,
	P2E1,
	P2E2,
	P2E3,
};

const struct State fsm[] = {
	[GE] = {50, EN(G, R), PR, {
		[None]	= GE,
		[E]		= GE,
	 	[N]		= E2N,
	 	[EN]	= E2N,
	 	[P]		= E2P,
	 	[EP]	= E2P,
	 	[NP]	= E2N,
	 	[NEP]	= E2N
	}},
	[GN] = {50, EN(R, G), PR, {
		[None]	= GN,
		[E]		= N2E,
	 	[N]		= GN,
	 	[EN]	= N2E,
	 	[P]		= N2P,
	 	[EP]	= N2P,
	 	[NP]	= N2P,
	 	[NEP]	= N2P
	}},
	[GP] = {50, EN(R, R), PG, {
		[None]	= GP,
		[E]		= P2E0,
	 	[N]		= P2N0,
	 	[EN]	= P2E0,
	 	[P]		= GP,
	 	[EP]	= P2E0,
	 	[NP]	= P2N0,
	 	[NEP]	= P2E0
	}},
	[E2N] = {50, EN(Y, R), PR, { SAME(GN) }},
	[N2E] = {50, EN(R, Y), PR, { SAME(GE) }},
	[E2P] = {50, EN(Y, R), PR, { SAME(GP) }},
	[N2P] = {50, EN(Y, R), PR, { SAME(GP) }},
	[P2N0] = {50, EN(R, R), PR, { SAME(P2N1) }},
	[P2N1] = {50, EN(R, R), P_, { SAME(P2N2) }},
	[P2N2] = {50, EN(R, R), PR, { SAME(P2N3) }},
	[P2N3] = {50, EN(R, R), P_, { SAME(GN)   }},
	[P2E0] = {50, EN(R, R), PR, { SAME(P2E1) }},
	[P2E1] = {50, EN(R, R), P_, { SAME(P2E2) }},
	[P2E2] = {50, EN(R, R), PR, { SAME(P2E3) }},
	[P2E3] = {50, EN(R, R), P_, { SAME(GE)   }},

};

int square(int a) {
	int i = 2;

	int pow(int a) {
		int j;
		for (j = 0; j < i; j++) {
			a *= a;
		}
		return a;
	}
	return pow(a);
}

int main(void) {
	printf("%d\n", square(2));
	return 0;
}
	/*
		[None]	= ,
		[E]		= ,
	 	[N]		= ,
	 	[EN]	= ,
	 	[P]		= ,
	 	[EP]	= ,
	 	[NP]	= ,
	 	[NEP]	= 
	*/

