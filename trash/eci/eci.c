#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>

#include <err.h>

#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>

#include "elf.h"
#include "strelf.h"

extern const char *__progname;

#define xensurem(cond, ...)\
	do {\
		if ((cond) == 0) {\
			errx(1, __VA_ARGS__);\
		}\
	} while (0)

#define xensure(cond)\
		xensurem(cond, "malformed ELF, FILE : \"%s\", LINE : \"%d\""\
			, __FILE__\
			, __LINE__\
		)

#define xinboundpm(ptr, end, ...) xensurem((void *)ptr < end, __VA_ARGS__)
#define xinboundp(ptr, end) xensure((void *)ptr < end)
#define xinboundm(ptr, ...) xinboundpm(ptr, program_end, __VA_ARGS__)
#define xinbound(ptr) xinboundp(ptr, program_end)

static void parse_dynamic_section(struct elf64_dyn *, struct elf64_dyn *, void *, void *);
static void parse_section_header(struct elf64_ehdr *, void *, void *);
static void parse_program_header(struct elf64_ehdr *, void *, void *);
static void parse_elf_header(struct elf64_ehdr *, void *, void *);

static const char *usage = "USAGE: %s in\nPARAMS:\n\tin: The input elf file\n";

static void
parse_section_header(struct elf64_ehdr *eh
	, void *program_beg
	, void *program_end
	) {
	void *sheader_beg = program_beg + eh->e_shoff;
	void *sheader_end = sheader_beg + eh->e_shentsize;
	void *dyn_beg = NULL;
	void *dyn_end = NULL;
	struct elf64_shdr *es = sheader_beg;
	struct elf64_shdr *shstr;
	void *sheader;
	char *sh_names_beg;
	char *sh_names_end;
	char *sh_name;

	int i;

	xinboundm(sheader_beg, "section header too long");
	xinboundm(sheader_end, "entry section not present");

	if (eh->e_shnum == SHN_UNDEF) {
		sheader_end = sheader_beg + es->sh_size * eh->e_shentsize;
	} else {
		sheader_end = sheader_beg + eh->e_shnum * eh->e_shentsize;
	}
	xensurem(sheader_end <= program_end, "section header offset out of range");

	shstr = sheader_beg + eh->e_shstrndx * eh->e_shentsize;
	xinbound(shstr);
	sh_names_beg = program_beg + shstr->sh_offset;
	xinbound(sh_names_beg);
	sheader = sheader_beg + eh->e_shentsize;

	while (sheader != sheader_end) {
		es = sheader;
		sh_name = sh_names_beg + es->sh_name;
		xinboundp(sh_name, sheader_end);

		printf("\n\tshname : %s", sh_names_beg + es->sh_name);
		printf("\n\tshtype : ");

		if (strcmp(sh_name, ".dynamic") == 0) {
			dyn_beg = program_beg + es->sh_offset;
			dyn_end = dyn_beg + es->sh_size;
		}
		if (es->sh_type < SHT_NUM) {
			printf("%s", shtype[es->sh_type]);
		} else {
			switch(es->sh_type) {
				case SHT_LOOS:
					printf("Start OS-specific.");
					break;
				case SHT_GNU_ATTRIBUTES:
					printf("Object attributes.");
					break;
				case SHT_GNU_HASH:
					printf("GNU-style hash table.");
					break;
				case SHT_GNU_LIBLIST:
					printf("Prelink library list");
					break;
				case SHT_CHECKSUM:
					printf("DSO content.");
					break;
				case SHT_LOSUNW:
					printf("Sun-specific low bound.");
					break;
				case SHT_GNU_verdef:
					printf("Version definition section.");
					break;
				case SHT_GNU_verneed:
					printf("Version needs section.");
					break;
				case SHT_GNU_versym:
					printf("Version symbol table.");
					break;
				case SHT_LOPROC:
					printf("Start of processor-specific");
					break;
				case SHT_HIPROC:
					printf("End of processor-specific");
					break;
				case SHT_LOUSER:
					printf("Start of application-specific");
					break;
				case SHT_HIUSER:
					printf("End of application-specific");
					break;
				default:
					errx(1, "unkonwn section type : 0x%x", es->sh_type);
					break;
			}
		}
		if (es->sh_flags == 0) {
			printf("\n\tshattr : none\n");
		} else {
			printf("\n\tshattr :\n");
			for (i = 0; i < 11; i++) {
				if (es->sh_flags & (1<<i)) {
					printf("\t\t%s\n", shattr[i]);
				}
			}
		}

#ifdef DEBUG
	printf("\n\tshaddr : 0x%lx\n\tshoffset : 0x%lx\n\tshsize : 0x%lx\n\t"
		"shlink : 0x%x\n\tshinfo : 0x%x\n\tshentsize : 0x%lx\n\t"
		"shaddralign : 0x%lx\n"
		, es->sh_addr
		, es->sh_offset
		, es->sh_size
		, es->sh_link
		, es->sh_info
		, es->sh_addralign
		, es->sh_entsize
	);
#endif
		sheader += eh->e_shentsize;
	}
	if (dyn_beg != NULL) {
		parse_dynamic_section(dyn_beg, dyn_end, program_beg, program_end);
	}
}

static void
parse_dynamic_section(struct elf64_dyn *dyn_beg
	, struct elf64_dyn *dyn_end
	, void *program_beg
	, void *program_end
	) {
	struct elf64_dyn *dyn_curr;
	char *dyn_info;
	void *dst_beg = NULL;
	void *dst_end = NULL;
	uintptr_t section_bytes = (uintptr_t)dyn_end - (uintptr_t)dyn_beg;
	struct elf64_dyn *strtab = NULL;
	struct elf64_dyn *strsz = NULL;

	if (section_bytes % sizeof(struct elf64_dyn) != 0) {
		errx(1, "malformed dynamic section");
	}
	xensure(dst_end <= program_end);
	for (dyn_curr = dyn_beg; dyn_curr != dyn_end; dyn_curr++) {
		if (dyn_curr->d_tag == DT_STRTAB) {
			strtab = dyn_curr;
		} else if (dyn_curr->d_tag == DT_STRSZ) {
			strsz = dyn_curr;
		} else if (dyn_curr == DT_NULL) {
			break;
		}
	}
	xensure(strtab != NULL && strsz != NULL);

	dst_beg = program_beg + strtab->d_un.d_ptr;
	dst_end = program_beg + strtab->d_un.d_ptr;

	printf("\n\t.dynamic section info :\n");

	for (dyn_curr = dyn_beg; dyn_curr != dyn_end; dyn_curr++) {
		dyn_info = dst_beg + dyn_curr->d_un.d_ptr;
		switch(dyn_curr->d_tag) {
			case DT_SONAME:
				xinbound(dyn_info);
				printf("\t\tSONAME : %s\n", dyn_info);
				break;
			case DT_RPATH:
				xinbound(dyn_info);
				printf("\t\tRPATH : %s\n", dyn_info);
				break;
			case DT_RUNPATH:
				xinbound(dyn_info);
				printf("\t\tRUNPATH : %s\n", dyn_info);
				break;
			case DT_NEEDED:
				xinbound(dyn_info);
				printf("\t\tNEEDED : %s\n", dyn_info);
				break;
			case DT_NULL:
				goto DYN_END;
			default:
				break;
		}
	}
DYN_END:
	return;
}



static void
parse_program_header(struct elf64_ehdr *eh
	, void *program_beg
	, void *program_end
	) {
	void *pheader_beg = program_beg + eh->e_phoff;
	void *pheader_end = pheader_beg + eh->e_phnum * eh->e_phentsize;
	void *pheader = pheader_beg;
	struct elf64_phdr *ep;
	void *segment_beg;
	void *segment_end;
	char *str;
	int i;

	xinboundm(pheader_beg, "program header too long");
	xensurem(pheader_end <= program_end, "program header offset out of range");

	while (pheader != pheader_end) {
		ep = pheader;

		printf("\n\tphtype : ");
		if (ep->p_type < PT_NUM) {
			printf("%s", phtype[ep->p_type]);
		} else {
			switch (ep->p_type) {
				case PT_LOOS:
					printf("Start of OS-specific");
					break;
				case PT_GNU_EH_FRAME:
					printf("GCC .eh_frame_hdr segment");
					break;
				case PT_GNU_STACK:
					printf("Indicates stack executability");
					break;
				case PT_GNU_RELRO:
					printf("Read-only after relocation");
					break;
				case PT_LOSUNW:
					printf("Sun Specific segment");
					break;
				case PT_SUNWSTACK:
					printf("Stack segment");
					break;
				case PT_HISUNW:
					printf("End of OS-specific");
					break;
				case PT_LOPROC:
					printf("Start of processor-specific");
					break;
				case PT_HIPROC:
					printf("End of processor-specific");
					break;
				default:
					errx(1, "Unknown section type : 0x%x", ep->p_type);
					break;
			}
		}
		if (ep->p_flags == 0) {
			printf("\n\tphflags : none\n");
		} else {
			printf("\n\tphflags :\n");
			for (i = 0; i < 3; i++) {
				if (ep->p_flags & (1 << i)) {
					printf("\t\t%s\n", pflags[i]);
				}
			}
		}
#ifdef DEBUG
		printf("\tphoffset : 0x%lx\n\tphvaddr : 0x%lx\n\tphpaddr : 0x%lx\n\t"
			"phfilesz : 0x%lx\n\tphmemsz : 0x%lx\n\tphalign : 0x%lx\n"
			, ep->p_offset
			, ep->p_vaddr
			, ep->p_paddr
			, ep->p_filesz
			, ep->p_memsz
			, ep->p_align
		);
#endif
		// memsz > filesz == extra allocated memory when mapped
		// eg fsz = 10, mmsz = 1000 ==  alloc 1000, the load 10 in mem

		segment_beg = program_beg + ep->p_offset;
		segment_end = segment_beg + ep->p_filesz;

		if (segment_beg > program_end) {
			errx(1, "elf file too small");
		}
		if (segment_end > program_end) {
			errx(1, "program segment too big");
		}
		if (__builtin_popcountl(ep->p_align) > 1){
			errx(1, "invalid segment alignment");
		}
		if (ep->p_type == PT_INTERP) {
			str = segment_beg;
			printf("\tinterpreter string : %s\n", str);
		}
		pheader += eh->e_phentsize;
	}
}


static void
parse_elf_header(struct elf64_ehdr *eh
	, void *program_beg
	, void *program_end) {
	__elf64_addr entry;

	if (program_beg + sizeof(struct elf64_ehdr) > program_end) {
		errx(1, "elf header too small");
	}
	if (memcmp(eh->e_ident, EHMAGIC, 3) != 0) {
		errx(1, "not an elf file");
	}
	if (ehclass + eh->e_ident[EHCLASS] >= ehclass + sizeof(ehclass)
		&& ehclass[eh->e_ident[EHCLASS]] != NULL) {
		errx(1, "ehclass out of range");
	}
	if (ehdata + eh->e_ident[EHDATA] >= ehdata + sizeof(ehdata)
		&& ehdata[eh->e_ident[EHDATA]] != NULL) {
		errx(1, "ehdata out of range");
	}
	if (ehvers + eh->e_ident[EHVERS] >= ehvers + sizeof(ehvers)
		&& ehvers[eh->e_ident[EHVERS]] != NULL) {
		errx(1, "ehvers out of range");
	}
	if (ehosabi + eh->e_ident[EHOSABI] >= ehosabi + sizeof(ehosabi)
		&& ehosabi[eh->e_ident[EHOSABI]] != NULL) {
		errx(1, "ehosabi out of range");
	}
	if (ehtype + eh->e_type >= ehtype + sizeof(ehtype)
		&& ehtype[eh->e_type] != NULL) {
		errx(1, "ehtype out of range");
	}
	if (ehmachine + eh->e_machine >= ehmachine + sizeof(ehmachine)
		&& ehmachine[eh->e_machine] != NULL) {
		errx(1, "ehmachine out of range");
	}
	if (eh->e_version != eh->e_ident[EHVERS]) {
		errx(1, "version not matching");
	}
	entry = eh->e_entry;

	printf("\n\tehclass : %s\n\tehdata : %s\n\tehvers : %s\n\tehosabi : %s\n\t"
		"ehabivers : %d\n\tehtype : %s\n\tehmachine : %s\n\tehentry : 0x%lx\n"
		, ehclass[eh->e_ident[4]]
		, ehdata[eh->e_ident[5]]
		, ehvers[eh->e_ident[6]]
		, ehosabi[eh->e_ident[7]]
		, eh->e_ident[8]
		, ehtype[eh->e_type]
		, ehmachine[eh->e_machine]
		, entry
	);
}

int
main(int argc, char *argv[]) {
	char *fn;
	int fd;
	off_t len;
	struct elf64_ehdr *eh;
	void *program_beg;
	void *program_end;

	if (argc - 1 < 1) {
		fn = argv[0];
	} else {
		fn = argv[1];
	}
	fd = open(fn, O_RDONLY);

	if (fd == -1) {
		err(1, "open");
	}
	len = lseek(fd, 0, SEEK_END);

	if (len == -1) {
		err(1, "lseek");
	}
	if (lseek(fd, 0, SEEK_SET) == -1) {
		err(1, "lseek");
	}
	program_beg = mmap(NULL, len, PROT_READ, MAP_PRIVATE, fd, 0);
	program_end = program_beg + len;
	eh = program_beg;

	if (program_beg == MAP_FAILED) {
		err(1, "program_begap");
	}
	printf("INFO : parsing elf header\n");
	parse_elf_header(eh, program_beg, program_end);

	if (eh->e_phoff != 0) {
		printf("\nINFO : parsing program header\n");
		parse_program_header(eh, program_beg, program_end);
	}
	if (eh->e_shoff != 0) {
		printf("\nINFO : parsing section header\n");
		parse_section_header(eh, program_beg, program_end);
	}
	return 0;
}
