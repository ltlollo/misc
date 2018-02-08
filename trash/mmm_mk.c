#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

struct Dim {
	int vert;
	int horiz;
}

struct Matrix {
	float *data;
	int vert;
	int horiz;
};

struct Slice {
	struct Matrix *m;
	int horiz_off;
	int vert_off;
	int vert;
	int horiz;
};



int matrix_alloc(struct Matrix *m, int v, int h) {
	float *data = malloc(v * h * sizeof(*data));

	if (data == NULL) {
		return -1;
	}
	m->data = data;
	m->vert = v;
	m->horiz = h;
	return 0;
}


enum DIREC {
	DIREC_VERT,
	DIREC_HORIZ,
};


int slice_empty(struct Slice *s) {
	return s->horiz == 0 || s->vert == 0;
}

int slice_advance(struct Slice *s, enum DIREC direc) {
	struct Matrix *m = s->m;

	if (slice_empty(s)) {
		return 0;
	}
	if (direc == DIREC_VERT) {
		assert(s->vert_off + s->vert <= m->vert);

		s->vert_off += s->vert;

		if (s->vert_off + s->vert > m->vert) {
			s->vert = m->vert - s->vert_off;
		}
	} else {
		assert(s->horiz_off + s->horiz <= m->horiz);

		s->horiz_off += s->horiz;

		if (s->horiz_off + s->horiz > m->horiz) {
			s->horiz = m->horiz - s->horiz_off;
		}
	}
	return 1;
}

void matrix_partition(struct Matrix *m, size_t bytes, enum DIREC direc,
					  struct Slice *s) {
	s->m = m;
	s->vert_off = 0;
	s->horiz_off = 0;
	s->horiz = m->horiz;
	s->vert = m->vert;

	if (direc == DIREC_HORIZ) {
		s->horiz = bytes / (sizeof(*m->data) * s->vert);
		if (s->horiz == 0) {
			s->horiz = 1;
		}
	} else {
		s->vert = bytes / (sizeof(*m->data) * s->horiz);
		if (s->vert == 0) {
			s->vert = 1;
		}
	}
}

void slice_partition(struct Slice *src, size_t bytes, enum DIREC direc,
					  struct Slice *s) {
	struct Matrix *m = src->m;

	s->m = m;
	s->vert_off = src->vert_off;
	s->horiz_off = src->horiz_off;
	s->horiz = src->horiz;
	s->vert = src->vert;
	if (direc == DIREC_HORIZ) {
		s->horiz = bytes / (sizeof(*m->data) * s->vert);
		if (s->horiz == 0) {
			s->horiz = 1;
		}
	} else {
		s->vert = bytes / (sizeof(*m->data) * s->horiz);
		if (s->vert == 0) {
			s->vert = 1;
		}
	}
}

void slice_print(struct Slice *s) {
	struct Matrix *m = s->m;

	for (int i = 0; i < s->vert; i++) {
		for (int j = 0; j < s->horiz; j++) {
			printf("%f, ",
				   s->m->data[(s->horiz_off + j) * m->vert + s->vert_off + i]);
		}
		printf("\n");
	}
	printf("\n");
}

int main() {

	struct Matrix m;
	struct Slice sl;
	struct Slice *s = &sl;

	int horiz = 5;
	int vert = 5;

	assert(matrix_alloc(&m, vert, horiz) != -1);

	for (int i = 0; i < vert * horiz; i++) {
		m.data[i] = i;
	}


	for (matrix_partition(&m, 48, DIREC_VERT, s);
		 !slice_empty(s);
		 slice_advance(s, DIREC_VERT)) {
		struct Slice ss, *ssp = &ss;
		

		slice_print(s);
		for(slice_partition(s, 16, DIREC_VERT, ssp);
			!slice_empty(ssp);
			slice_advance(ssp, DIREC_VERT)) {
			slice_print(ssp);
		}
	}

	return 0;
}
