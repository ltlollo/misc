#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <immintrin.h>

#define ENABLE_PREFETCH
#define VSIZE 8
#define SAFE_PAD 0
#define PREFETCH_DIST 2
#define ALIGN_SIZE 32

#define ALIGN __attribute__((aligned(ALIGN_SIZE)))
#define ALIGN_DOWN(n, p) ((uintptr_t)(n) & ~(p - 1))
#define ALIGN_UP(n, p)   (((uintptr_t)(n) + (p - 1)) & ~(p - 1)) 
#define MIN(a, b) (a < b ? a : b)
#define MAX(a, b) (b > a ? b : a)

#ifdef NDEBUG
#define debug_assert(expr) (void)(expr)
#else
#define debug_assert(expr) assert(expr)
#endif

#define brk_assert(expr) do { if ((expr) == 0) { return -1; } } while (0)

#ifdef ENABLE_PREFETCH
#define PREFETCH(p, m, l)	_mm_prefetch(p + m->size.vert * PREFETCH_DIST, l)
#define PREFETCH2(p, m)		PREFETCH(p, m, _MM_HINT_T2)
#define PREFETCH1(p, m)		PREFETCH(p, m, _MM_HINT_T1)
#define PREFETCH0(p, m)		PREFETCH(p, m, _MM_HINT_T0)
#else
#define PREFETCH(p, m, l)
#define PREFETCH2(p, m)
#define PREFETCH1(p, m)
#define PREFETCH0(p, m)
#endif

typedef size_t _dim_t_;
typedef float _num_t_;
typedef int _err_t_;

enum DIREC {
	DIREC_VERT = 0,
	DIREC_HORZ = 1
};

struct Dim {
	_dim_t_ vert;
	_dim_t_ horz;
};

struct Matrix {
	struct Dim size;
	_num_t_ *ALIGN data;
};

struct Slice {
	struct Dim off;
	struct Dim size;
};

struct HwConf {
	_dim_t_ l2size;
	_dim_t_ l1size;
};

struct Matrix *matrix_calloc(_dim_t_, _dim_t_);
struct Matrix *matrix_calloc_dim(struct Dim);
struct Slice slice_all_size(struct Dim, struct Matrix *);
struct Dim calc_gemm_dim(struct Dim, struct Dim);

_err_t_ matrix_pack(struct Matrix *, struct Slice *, struct Matrix *,
					struct Slice *);
_err_t_ slice_empty(struct Slice *);
_err_t_ slice_advance(struct Slice *, enum DIREC , struct Slice *);
_num_t_ *slice_at(struct Slice *, _dim_t_, _dim_t_, struct Matrix *);
_dim_t_ slice_partition_quota(struct Slice *, _dim_t_, enum DIREC,
							  struct Slice *);
_dim_t_ slice_partition_dim(struct Slice *, _dim_t_, enum DIREC,
							struct Slice *);
void slice_print(struct Slice *slice, struct Matrix *mat);

_err_t_ matrix_pack_checked(struct Matrix *, struct Slice *, struct Matrix *,
							struct Slice *);
_err_t_ gemm_checked(struct Matrix *, struct Matrix *, struct Matrix *,
					 struct Slice *, struct Slice *, struct Slice *,
					 struct HwConf *);
_err_t_ gern_checked(struct Matrix *, struct Matrix *, struct Matrix *,
					 struct Slice *, struct Slice *, struct Slice *,
			 		 struct HwConf *);
_err_t_ gemm_2nd_checked(struct Matrix *, struct Matrix *, struct Matrix *,
						 struct Slice *, struct Slice *, struct Slice *);
_err_t_ gemm_1st_checked(struct Matrix *, struct Matrix *, struct Matrix *,
						 struct Slice *, struct Slice *, struct Slice *);
_err_t_ gern_mkernel_checked(struct Matrix *, struct Matrix *, struct Matrix *,
							 struct Slice *, struct Slice *, struct Slice *);

void matrix_pack_unchecked(struct Matrix *, struct Slice *, struct Matrix *,
						   struct Slice *);
void gemm_unchecked(struct Matrix *, struct Matrix *, struct Matrix *,
					struct Slice *, struct Slice *, struct Slice *,
					struct HwConf *);
void gern_unchecked(struct Matrix *, struct Matrix *, struct Matrix *,
					struct Slice *, struct Slice *, struct Slice *,
			 		struct HwConf *);
void gemm_2nd_unchecked(struct Matrix *, struct Matrix *, struct Matrix *,
						struct Slice *, struct Slice *, struct Slice *);
void gemm_1st_unchecked(struct Matrix *, struct Matrix *, struct Matrix *,
						struct Slice *, struct Slice *, struct Slice *);
void gern_mkernel_unchecked(struct Matrix *, struct Matrix *, struct Matrix *,
							struct Slice *, struct Slice *, struct Slice *);

struct Matrix *
matrix_calloc(_dim_t_ vert, _dim_t_ horz) {
	struct Matrix *res;
	size_t prelude = sizeof (struct Matrix) + ALIGN_SIZE;

	vert = ALIGN_UP(vert, VSIZE);
	horz = ALIGN_UP(horz, VSIZE) + SAFE_PAD * VSIZE;

	if (vert > (SIZE_MAX - prelude) / horz / sizeof (_num_t_)) {
		return NULL;
	}
	res = calloc(vert * horz * sizeof (_num_t_) + prelude, 1);
	
	if (res == NULL) {
		return res;
	}
	res->size.vert = vert;
	res->size.horz = horz;
	res->data = (_num_t_ *)ALIGN_UP((char *)res + sizeof (struct Matrix),
									ALIGN_SIZE);

	return res;
}

struct Matrix *
matrix_calloc_dim(struct Dim dim) {
	return matrix_calloc(dim.vert, dim.horz);
}

struct Slice
slice_all_size(struct Dim dim, struct Matrix *mat) {
	struct Slice res;

	debug_assert(dim.vert <= mat->size.vert && dim.horz <= mat->size.horz);

	res.size.vert = dim.vert;
	res.size.horz = dim.horz;
	res.off.vert = 0;
	res.off.horz = 0;

	return res;
}

_err_t_
slice_empty(struct Slice *slice) {
	return slice->size.horz == 0 || slice->size.vert == 0;
}

_err_t_
slice_advance(struct Slice *slice, enum DIREC direc, struct Slice *paren) {
	if (slice_empty(slice)) {
		return 0;
	}
	if (direc == DIREC_VERT) {
		debug_assert(slice->off.vert + slice->size.vert <=
					 paren->size.vert + paren->off.vert);

		slice->off.vert += slice->size.vert;

		if (slice->off.vert + slice->size.vert >
			paren->size.vert + paren->off.vert) {
			slice->size.vert = paren->size.vert + paren->off.vert -
				slice->off.vert;
		}
	} else {
		debug_assert(slice->off.horz + slice->size.horz <=
					 paren->size.horz + paren->off.horz);

		slice->off.horz += slice->size.horz;

		if (slice->off.horz + slice->size.horz >
			paren->size.horz + paren->off.horz) {
			slice->size.horz = paren->size.horz + paren->off.horz -
				slice->off.horz;
		}
	}
	return 1;
}

_num_t_ *
slice_at(struct Slice *slice, _dim_t_ vert, _dim_t_ horz, struct Matrix *mat) {
	_num_t_ *f = mat->data + (slice->off.horz + horz) * mat->size.vert +
		slice->off.vert + vert;

	debug_assert(f >= mat->data &&
				 f < mat->data + mat->size.vert * mat->size.horz);

	return f;
}

_dim_t_
slice_partition_quota(struct Slice *paren, _dim_t_ nmemb, enum DIREC direc,
				struct Slice *subs) {
	_dim_t_ quota = nmemb / (direc == DIREC_HORZ ?
							paren->size.vert : paren->size.horz);

	quota = ALIGN_UP(quota, VSIZE);

	return slice_partition_dim(paren, quota, direc, subs);
}

_dim_t_
slice_partition_dim(struct Slice *paren, _dim_t_ nmemb, enum DIREC direc,
				struct Slice *subs) {
	debug_assert(nmemb <=
				 (direc == DIREC_HORZ ? paren->size.horz : paren->size.vert));

	subs->off.vert = paren->off.vert;
	subs->off.horz = paren->off.horz;
	subs->size.horz = paren->size.horz;
	subs->size.vert = paren->size.vert;

	if (direc == DIREC_HORZ) {
		subs->size.horz = nmemb;
		if (subs->size.horz == 0) {
			subs->size.horz = 1;
		}
	} else {
		subs->size.vert = nmemb;
		if (subs->size.vert == 0) {
			subs->size.vert = 1;
		}
	}
	debug_assert(subs->size.horz <= paren->size.horz);
	debug_assert(subs->size.vert <= paren->size.vert);

	return direc == DIREC_HORZ ? subs->size.horz : subs->size.vert;
}

_err_t_
matrix_pack_checked(struct Matrix *src, struct Slice *all_src,
					struct Matrix *dst, struct Slice *all_dst) {

	if (dst->size.vert != all_src->size.vert ||
		dst->size.horz != all_src->size.horz) {
		return -1;
	}
	matrix_pack_unchecked(src, all_src, dst, all_dst);

	return 0;
}

void
matrix_pack_unchecked(struct Matrix *src, struct Slice *all_src,
					  struct Matrix *dst,
					  struct Slice *all_dst) {
	_dim_t_ i;
	_dim_t_ j;

	debug_assert(dst->size.vert == all_src->size.vert &&
				 dst->size.horz == all_src->size.horz);

	all_dst->size = dst->size;
	all_dst->off.vert = 0;
	all_dst->off.horz = 0;

	for (j = 0; j < all_src->size.horz; j++) {
		for (i = 0; i < all_src->size.vert; i++) {
			*slice_at(all_dst, i, j, dst) = *slice_at(all_src, i, j, src);
		}
	}
}

struct Slice
slice_intersect(struct Slice fs, struct Slice ss) {
	struct Slice res;

	_dim_t_ a = MAX(fs.off.vert, ss.off.vert);
	_dim_t_ c = MAX(fs.off.horz, ss.off.horz);
	_dim_t_ b = MIN(fs.off.vert + fs.size.vert, ss.off.vert + ss.size.vert);
	_dim_t_ d = MIN(fs.off.horz + fs.size.horz, ss.off.horz + ss.size.horz);
	
	res.off.vert = a;
	res.off.horz = c;
	res.size.vert = b > a ? b - a : 0;
	res.size.horz = d > c ? d - c : 0;

	return res;
}

void
slice_print(struct Slice *slice, struct Matrix *mat) {
	_dim_t_ i;
	_dim_t_ j;

	if (slice_empty(slice) != 0) {
		return;
	}
	for (i = 0; i < slice->size.vert; i++) {
		for (j = 0; j < slice->size.horz; j++) {
			printf("%.2f ", *slice_at(slice, i, j, mat));
		}
		printf("\n");
	}
	printf("\n");
}

_err_t_
gemm_checked(struct Matrix *c, struct Matrix *a, struct Matrix *b,
			 struct Slice *all_c, struct Slice *all_a, struct Slice *all_b,
			 struct HwConf *conf) {
	if (all_c->size.vert != all_a->size.vert ||
		all_c->size.horz != all_b->size.horz ||
		all_a->size.horz != all_b->size.vert) {
		return -1;
	}
	gemm_unchecked(c, a, b, all_c, all_a, all_b, conf);

	return 0;
}

void
gemm_unchecked(struct Matrix *c, struct Matrix *a, struct Matrix *b,
			   struct Slice *all_c, struct Slice *all_a, struct Slice *all_b,
			   struct HwConf *conf) {
	struct Slice slices[2];
	struct Slice *horz_a = &slices[0];
	struct Slice *vert_b = &slices[1];

	_dim_t_ quota = conf->l2size / sizeof(_num_t_);
	_dim_t_ dimh = slice_partition_quota(all_a, quota, DIREC_HORZ, horz_a);
	_dim_t_ dimv = slice_partition_dim(all_b, dimh, DIREC_VERT, vert_b);

	debug_assert(all_a->size.horz == all_b->size.vert &&
				 all_c->size.vert == all_a->size.vert &&
				 all_c->size.horz == all_b->size.horz);

	debug_assert(dimh == dimv);

	for (;
		 slice_empty(horz_a) == 0;
		 slice_advance(horz_a, DIREC_HORZ, all_a),
		 slice_advance(vert_b, DIREC_VERT, all_b)) {
		debug_assert(slice_empty(vert_b) == 0);
		gern_unchecked(c, a, b, all_c, horz_a, vert_b, conf);
	}
}

_err_t_
gern_checked(struct Matrix *c, struct Matrix *a, struct Matrix *b,
			 struct Slice *all_c, struct Slice *all_a, struct Slice *all_b,
			 struct HwConf *conf) {
	if (all_a->size.horz != all_b->size.vert ||
		all_c->size.vert != all_a->size.vert ||
		all_c->size.horz != all_b->size.horz) {
		return -1;
	}
	gern_unchecked(c, a, b, all_c, all_a, all_b, conf);

	return 0;
}

void
gern_unchecked(struct Matrix *c, struct Matrix *a, struct Matrix *b,
	  		   struct Slice *all_c, struct Slice *all_a, struct Slice *all_b,
			   struct HwConf *conf) {
	struct Slice slices[2];
	struct Slice *vert_a = &slices[0];
	struct Slice *vert_c = &slices[1];

	_dim_t_ quota = conf->l1size / sizeof(_num_t_);
	_dim_t_ dimv = slice_partition_quota(all_a, quota, DIREC_VERT, vert_a);
	_dim_t_ dimv1 = slice_partition_dim(all_c, dimv, DIREC_VERT, vert_c);

	debug_assert(all_a->size.horz == all_b->size.vert &&
				 all_c->size.vert == all_a->size.vert &&
				 all_c->size.horz == all_b->size.horz);

	debug_assert(dimv == dimv1);

	for (;
		 slice_empty(vert_a) == 0;
		 slice_advance(vert_a, DIREC_VERT, all_a),
		 slice_advance(vert_c, DIREC_VERT, all_c)) {
		debug_assert(slice_empty(vert_c) == 0);
		gemm_2nd_unchecked(c, a, b, vert_c, vert_a, all_b);
	}
}

_err_t_
gemm_2nd_checked(struct Matrix *c, struct Matrix *a, struct Matrix *b,
				 struct Slice *all_c, struct Slice *all_a,
				 struct Slice *all_b) {
	if (all_c->size.vert != all_a->size.vert ||
		all_c->size.horz != all_b->size.horz ||
		all_a->size.horz != all_b->size.vert) {
		return -1;
	}
	gemm_2nd_unchecked(c, a, b, all_c, all_a, all_b);

	return 0;
}

void
gemm_2nd_unchecked(struct Matrix *c, struct Matrix *a, struct Matrix *b,
				   struct Slice *all_c, struct Slice *all_a,
				   struct Slice *all_b) {
	struct Slice slices[2];
	struct Slice *horz_c = &slices[0];
	struct Slice *horz_b = &slices[1];

	_dim_t_ dimh  = slice_partition_dim(all_c, VSIZE, DIREC_HORZ, horz_c);
	_dim_t_ dimh1 = slice_partition_dim(all_b, VSIZE, DIREC_HORZ, horz_b);

	debug_assert(all_a->size.horz == all_b->size.vert &&
				 all_c->size.vert == all_a->size.vert &&
				 all_c->size.horz == all_b->size.horz);

	debug_assert(dimh == dimh1 && dimh1 == VSIZE);

	for (;
		 slice_empty(horz_b) == 0;
		 slice_advance(horz_b, DIREC_HORZ, all_b),
		 slice_advance(horz_c, DIREC_HORZ, all_c)) {
		debug_assert(slice_empty(horz_c) == 0);
		gemm_1st_unchecked(c, a, b, horz_c, all_a, horz_b);
	}
}

_err_t_
gemm_1st_checked(struct Matrix *c, struct Matrix *a, struct Matrix *b,
				 struct Slice *all_c, struct Slice *all_a,
				 struct Slice *all_b) {
	if (all_c->size.vert != all_a->size.vert ||
		all_c->size.horz != all_b->size.horz ||
		all_a->size.horz != all_b->size.vert) {
		return -1;
	}
	gemm_1st_unchecked(c, a, b, all_c, all_a, all_b);

	return 0;
}

void
gemm_1st_unchecked(struct Matrix *c, struct Matrix *a, struct Matrix *b,
				   struct Slice *all_c, struct Slice *all_a,
				   struct Slice *all_b) {
	struct Slice slices[2];
	struct Slice *vert_c = &slices[0];
	struct Slice *vert_a = &slices[1];


	_dim_t_ dimv  = slice_partition_dim(all_c, VSIZE, DIREC_VERT, vert_c);
	_dim_t_ dimv1 = slice_partition_dim(all_b, VSIZE, DIREC_VERT, vert_a);

	debug_assert(all_a->size.horz == all_b->size.vert &&
				 all_c->size.vert == all_a->size.vert &&
				 all_c->size.horz == all_b->size.horz);

	debug_assert(dimv == dimv1 && dimv1 == VSIZE);

	for (;
		 slice_empty(vert_a) == 0;
		 slice_advance(vert_a, DIREC_VERT, all_a),
		 slice_advance(vert_c, DIREC_VERT, all_c)) {
		debug_assert(slice_empty(vert_c) == 0);
		gern_mkernel_unchecked(c, a, b, vert_c, vert_a, all_b);
	}
}

_err_t_
gern_mkernel_checked(struct Matrix *c, struct Matrix *a, struct Matrix *b,
					 struct Slice *all_c, struct Slice *all_a,
					 struct Slice *all_b) {
	if (all_c->size.horz != all_c->size.vert ||
		all_c->size.vert != all_a->size.vert ||
		all_a->size.vert != all_b->size.horz ||
		all_b->size.horz != VSIZE) {
		return -1;
	}
	gern_mkernel_checked(c, a, b, all_c, all_a, all_b);
	return 0;
}

void
gern_mkernel_unchecked(struct Matrix *c, struct Matrix *a, struct Matrix *b,
					   struct Slice *all_c, struct Slice *all_a,
					   struct Slice *all_b) {
	_dim_t_ dimh = all_a->size.horz;
	__m256 regs_c[VSIZE];
	__m256 reg_a;
	__m256 reg_b;
	float *src_c;
	float *src_a;
	float *src_b;
	_dim_t_ i;
	_dim_t_ j;

	debug_assert(all_c->size.horz == all_c->size.vert &&
				 all_c->size.vert == all_a->size.vert &&
				 all_a->size.vert == all_b->size.horz &&
				 all_b->size.horz == VSIZE);


	for (i = 0; i < VSIZE; i++) {
		src_c = slice_at(all_c, 0, i, c);
		PREFETCH2(src_c, c);
		regs_c[i] = _mm256_load_ps(src_c);
	}

	for (j = 0; j < dimh; j++) {
		src_a = slice_at(all_a, 0, j, a);
		PREFETCH2(src_a, a);
		reg_a = _mm256_load_ps(src_a);
		for (i = 0; i < VSIZE; i++) {
			src_b = slice_at(all_b, 0, i, b);
			PREFETCH0(src_b, b);
			reg_b = _mm256_set1_ps(*src_b);
			regs_c[i] = _mm256_fmadd_ps(reg_a, reg_b, regs_c[i]);
		}
	}

	for (i = 0; i < VSIZE; i++) {
		src_c = slice_at(all_c, 0, i, c);
		_mm256_stream_ps(src_c, regs_c[i]);
	}
}

struct Dim
calc_gemm_dim(struct Dim a, struct Dim b) {
	struct Dim ret;

	debug_assert(a.horz == b.vert);

	ret.vert = a.vert;
	ret.horz = b.horz;

	return ret;
}

void
slice_fill_iota(struct Slice *slice, struct Matrix *mat,
				_num_t_ s, _num_t_ inc) {
	_dim_t_ i;
	_dim_t_ j;

	for (j = 0; j < slice->size.horz; j++) {
		for (i = 0; i < slice->size.vert; i++) {
			*slice_at(slice, i, j, mat) = s;
			s += inc;
		}
	}
}

struct Slice
slice_align_up(struct Slice slice) {
	slice.size.vert = ALIGN_UP(slice.size.vert, VSIZE);
	slice.size.horz = ALIGN_UP(slice.size.horz, VSIZE);
	
	return slice;
}

_err_t_
test_gemm(void) {
	_err_t_ err;

	struct HwConf cfg = { 1000, 100 };

	struct Matrix *mat_c;
	struct Matrix *mat_a;
	struct Matrix *mat_b;

	struct Dim dim_a = { 5, 5 };
	struct Dim dim_b = { 5, 5 };
	struct Dim dim_c = calc_gemm_dim(dim_a, dim_b);

	struct Slice all_a;
	struct Slice all_b;
	struct Slice all_c;

	struct Slice algn_a;
	struct Slice algn_b;
	struct Slice algn_c;

	mat_a = matrix_calloc_dim(dim_a);
	mat_b = matrix_calloc_dim(dim_b);
	mat_c = matrix_calloc_dim(dim_c);

	brk_assert(mat_a && mat_b && mat_c);

	all_a = slice_all_size(dim_a, mat_a);
	all_b = slice_all_size(dim_b, mat_b);
	all_c = slice_all_size(dim_c, mat_c);

	slice_fill_iota(&all_a, mat_a, 0.0, 1.0);
	slice_fill_iota(&all_b, mat_b, 0.0, 1.0);
	slice_fill_iota(&all_c, mat_c, 0.0, 0.0);

	algn_a = slice_align_up(all_a);
	algn_b = slice_align_up(all_b);
	algn_c = slice_align_up(all_c);

	err = gemm_checked(mat_c, mat_a, mat_b, &algn_c, &algn_a, &algn_b, &cfg);

	brk_assert(err == 0);

	return 0;
}

_err_t_
test_print(void) {
	struct Slice slices[4];
	struct Slice *slice = &slices[0];
	struct Slice *all	= &slices[1];
	struct Slice *algn	= &slices[2];
	struct Slice *subs	= &slices[3];
	struct Slice isc;
	struct Dim dim = { 5, 5 };
	struct Matrix *mat = matrix_calloc_dim(dim);

	brk_assert(mat);

	*all = slice_all_size(dim, mat);

	slice_fill_iota(all, mat, 0.0, 1.0);
	slice_print(all, mat);

	*algn = slice_align_up(*all);

	for (slice_partition_quota(algn, 12, DIREC_VERT, slice);
		 slice_empty(slice) == 0;
		 slice_advance(slice, DIREC_VERT, algn)) {
		for(slice_partition_quota(slice, 4, DIREC_HORZ, subs);
			slice_empty(subs) == 0;
			slice_advance(subs, DIREC_HORZ, slice)) {
			isc = slice_intersect(*subs, *all);
			slice_print(&isc, mat);
		}
	}
	return 0;
}

int
main(int argc, char *argv[]) {
	_err_t_ err;

	(void)argc;
	(void)argv;

	err = test_print();

	brk_assert(err == 0);

	return 0;
}
