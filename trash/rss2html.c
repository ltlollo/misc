#include <strings.h>
#include <memory.h>
#include <stdio.h>
#include <assert.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#define STRLEN(s) (sizeof(s) - 1) 

struct Range {
	char *data;
	size_t size;
};

struct Content {
	struct Range title;
	struct Range link;
	struct Range descr;
	struct Range pubDate;
	struct Range lastBuildDate;
};

enum OPTION {
	OPTION_NONE,
	OPTION_USE_FANME,
};

struct Range
mapfile(char *fn);
struct Range
enclosed(struct Range super, char *beg, char *end);
int
pivot_ge_range(char *p, struct Range r);
int
range_is_empty(struct Range r);
struct Range
pivot_split_rhs(char *p, struct Range r);
char *
find(char *restrict str, char *restrict strstr);
void
range_write(struct Range r, FILE *f);
void
range_writenl(struct Range r, FILE *f);

void
html_start(char *fname, struct Range title, enum OPTION opt) {
	printf("<html lang=\"en-US\"><head><title>");
	if (range_is_empty(title) || (opt & OPTION_USE_FANME)) {
		printf("%s", fname);
	} else {
		range_write(title, stdout);
	}
	printf("</title><meta charset=\"utf-8\"></head><body>\n");
}

void
html_finish() {
	printf("\n</body></html>\n");
}

int
main() {
	char *fname = "in.xml";

	struct Range file = mapfile(fname);
	struct Range it = file;
	struct Content curr_content;
	struct Range item;
	struct Range title;

	assert(!range_is_empty(file));
	title = enclosed(file, "<title>", "</title>");
	html_start(fname, title, OPTION_NONE);

	do {
		item = enclosed(it, "<item>", "</item>");
		if (range_is_empty(item)) {
			break;
		}
		curr_content.title = enclosed(item, "<title>", "</title>");
		curr_content.link = enclosed(item, "<link>", "</link>");
		curr_content.descr = enclosed(item, "<description>", "</description>");
		curr_content.pubDate = enclosed(item, "<pubDate>", "</pubDate>");
		curr_content.lastBuildDate = enclosed(item, "<lastBuildDate>", "</lastBuildDate>");

		//if (!range_is_empty(curr_content.title)) {
		//	range_writenl(curr_content.title, stdout);
		//}
		if (!range_is_empty(curr_content.descr)) {
			range_writenl(curr_content.descr, stdout);
		}
		it = pivot_split_rhs(item.data + item.size, it);
	} while (1);

	html_finish();
	return 0;
}

struct Range
mapfile(char *fn) {
	int fd = open(fn, O_RDONLY);
	struct stat sb;
	struct Range f = { NULL, 0 };

	if (fd == -1) {
		return f;
	}
	if (fstat(fd, &sb) == -1) {
		goto FAIL;
	}
	f.size = sb.st_size + 1;
	f.data = mmap(NULL, f.size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
	f.data[f.size - 1] = 0;
	if (f.data == MAP_FAILED) {
		goto FAIL;
	}
	close(fd);
	return f;
FAIL:
	f.data = NULL;
	f.size = 0;
	close(fd);
	return f;
}

int
pivot_ge_range(char *p, struct Range r) {
	return p >= r.data + r.size;
}

int
range_is_empty(struct Range r) {
	return r.data == NULL || r.size == 0;
}

struct Range
pivot_split_rhs(char *p, struct Range r) {
	struct Range res = { r.data + r.size, 0 };

	assert(p >= r.data);
	if (pivot_ge_range(p, r)) {
		return res;
	}
	res.data = p;
	res.size = r.size - (p - r.data);

	return res;
}

struct Range
enclosed(struct Range super, char *beg, char *end) {
	size_t beg_size = strlen(beg);
	size_t end_size = strlen(end);
	char *enc_beg = find(super.data, beg);
	struct Range r = { NULL, 0 };
	char *enc_nx;
	char *enc_end;

	if (super.data == NULL || super.size < beg_size + end_size) {
		return r;
	}
	if (enc_beg == NULL || pivot_ge_range(enc_beg, super)) {
		return r;
	}
	enc_nx = enc_beg;
	do {
		enc_end = find(enc_nx + beg_size, end);
		if (enc_end == NULL || pivot_ge_range(enc_end, super)) {
			return r;
		}
		enc_nx = find(enc_nx + beg_size, beg);
		if (enc_end < enc_nx || enc_nx == NULL) {
			r.data = enc_beg + beg_size;
			r.size = enc_end - (enc_beg + beg_size);
			return r;
		}
		enc_nx = enc_end + end_size;
	} while (1);
}

char *
find(char *restrict str, char *restrict sub) {
	char *strcurr;

	while (*str) {
		if (strncmp(str, "<![CDATA[", STRLEN("<![CDATA[")) == 0) {
			strcurr = strstr(str, "]]>");
			if (strcurr == NULL) {
				return NULL;
			}
			memset(str, ' ', STRLEN("<![CDATA["));
			memset(strcurr, ' ', STRLEN("]]>"));
			str = strcurr;
		}
		strcurr = sub;
		while (*str == *strcurr && *str && *strcurr) {
			str++;
			strcurr++;
		}
		if (*strcurr == 0) {
			return str - (strcurr - sub);
		}
		str++;
	}
	return NULL;
}

void
range_write(struct Range r, FILE *f) {
	fwrite(r.data, 1, r.size, f);
}

void
range_writenl(struct Range r, FILE *f) {
	range_write(r, f);
	putc('\n', f);
}
