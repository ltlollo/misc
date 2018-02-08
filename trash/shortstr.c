#include "fstr.c"

int
main() {
	struct FStr f = {
		.sstr = "world",
	};
	struct FStr s = {
		.sstr = "1234567890"
				"1234567890"
				"1234567890"
				"1",
	};
	struct FStr ss = {
		.sstr = "1234567890"
				"1234567890"
				"1234567890"
				"1",
	};
	assert(str_equal(&f, &f) == 1);
	assert(str_equal(&f, &s) == 0);

	str_push(&s, '2');
	assert(str_equal(&s, &ss) == 0);
	str_push(&ss, '2');

	assert(str_equal(&f, &s) == 0);
	assert(str_equal(&s, &s) == 1);
	assert(str_equal(&s, &ss) == 1);


	str_pop(&s);

	return 0;
}
