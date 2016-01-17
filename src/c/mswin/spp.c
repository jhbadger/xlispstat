#ifdef DOCUMENTATION

spp is a preprocessor for C code that allows you to take
string constants in your source and convert them into
calls that load the string constant from a resource. You provide
the GetStringResource() call, and change your makefile a bit.

spp generates three files:
	filename.sc - Contains the new C code with strings replaced.
	filename.src - Contains the STRINGTABLE of string constants.
	filename.sh - Contains the generated int constants for the strings.

The options are described below. There is one undocumented
feature, the "escape". If you place the comment /*!*/ in front
of any string, spp will leave it alone.

Makefile entries change from a C compiler call to something like:

file.obj: file.c
	spp -b 5000 -a inert -o src\file.c
	$(CC) /Tc inert\file.sc

This invokes the spp preprocessor with the source as input, and
places the results in a subdirectory called inert. This keeps
"generated" files away from your source. The /Tc option in the
C compiler command points C to the generated source file.

Also, you will need to make the .src file depend on the .obj
file generated, and the .res file depend on the .src file.

	file.res: file.rc inert\file.src
	inert\file.src: file.obj
	file.obj: file.c

or you might try:

	file.res: file.rc inert\src
	file.obj: inert\file.src
	inert\file.src: file.c

Then, you also need to make sure you #include "file.src" in
your .rc file to include the strings, and #include "file.sh"
to get the generated string ids.

Example GetStringResource function:

GetStringResource(id)
int		id;
{
static char		gsr_buffer[256];

	if (LoadString(gInstance, id, (LPSTR)gsr_buffer, 255) < 1)
		strcpy(gsr_buffer, "*** LoadString() ERROR");

	return gsr_buffer;
	}


#endif


/*
** This code was written by Tim Endres on August 14, 1991.
**
** This code will read C source and convert all string constants
** into function calls to a routine designed to load the string
** constant from a resource. I wrote this primarily for Windows
** development because I have an application with over 50K bytes
** of string constants, and I could not fit this in the 64K data
** segment.
**
** I have donated this code to USENET, and poor miserable DOS
** and Windows developers everywhere dealing with a horrifying
** CPU model (oh, I *know* it has some advantages, but not for 
** Windows in my opinion).
**
** Please do as you wish with this code, but please be kind
** enough to leave a little credit here for me someplace. :^)
**
** This, and all of my code is dedicated to Sky Marie.
** Daddy misses you Sky Marie!
**
*/


struct {
	char *str;
	} usage_tbl [] = {

"OPTIONS:",

"	-a <directory>",
"		Sets ALL output file directories.",

"	-d <directory>",
"		Sets the header output file directory.",

"	-s <directory>",
"		Sets the source output file directory.",

"	-r <directory>",
"		Sets the resource script output file directory.",

"	-i <rsrc_id_increment>",
"		Sets the resource ID increment.",

"	-b <rsrc_id_base>",
"		Sets the resource ID starting value.",

"	-v",
"		Turns on verbose output to stdout.",

"	-o",
"		Sets the resource script output file directory.",

"	-w",
"		Turn off warning messages (produced on stderr).",

"	-1 <suffix>",
"		Sets the resource output file suffix (default .src).",

"	-2 <suffix>",
"		Sets the source output file suffix (default .sc).",

"	-3 <suffix>",
"		Sets the header output file suffix (default .sh).",

"	--",
"		Sends the source output to stdio.",

	(char *) 0

	};


#include <dos.h>

#include <stdio.h>
#include <string.h>

extern char		*malloc();

struct str_cache {
	struct str_cache *next;
	int	rsrc_id;
	char *str;
	};

struct str_cache *cache_list = NULL;

char	progname[256];

int	verbose = 0,
	optimize = 0,
	warning = 1,
	stringing = 0,
	commenting = 0,
	quoting = 0,
	pass_string = 0,
	rsrc_id = 1000,
	rsrc_id_inc = 1;

long	line_num = 0;

char	def_id_name[64],
	filename[64],
	rcname[64],
	defname[64],
	outname[64],
	src_path[256] = ".",
	res_path[256] = ".",
	def_path[256] = ".",
	src_suffix[8] = ".sc",
	res_suffix[8] = ".src",
	def_suffix[8] = ".sh",
	path[256],
	inbuf[8192],
	strbuf[8192];


FILE	*infile,
	*outfile,
	*rcfile,
	*deffile;

usage()
{
int i;

	printf("USAGE: %s [options] file_spec\n", progname);
	for (i=0; usage_tbl[i].str != NULL; i++)
		printf("%s\n", usage_tbl[i].str);
	}


main(argc, argv)
int	argc;
char	*argv[];
{
int		myerr;
char	*ptr;
struct find_t	myfind;

	ptr = strrchr(argv[0], '\\');
	strcpy(progname, (ptr == NULL) ? argv[0] : ptr + 1);
	ptr = strchr(progname, '.');
	if (ptr != NULL)
		*ptr = '\0';

	argc--; argv++;

	outfile = NULL;

	while (argv[0][0] == '-') {

		switch (argv[0][1]) {
			case 'a':
				strcpy(def_path, argv[1]);
				strcpy(src_path, argv[1]);
				strcpy(res_path, argv[1]);
				argc--; argv++;
				break;
			case 'd':
				strcpy(def_path, argv[1]);
				argc--; argv++;
				break;
			case 's':
				strcpy(src_path, argv[1]);
				argc--; argv++;
				break;
			case 'r':
				strcpy(res_path, argv[1]);
				argc--; argv++;
				break;
			case 'i':
				rsrc_id_inc = atoi(argv[1]);
				argc--; argv++;
				break;
			case 'b':
				rsrc_id = atoi(argv[1]);
				argc--; argv++;
				break;
			case 'v':
				verbose = 1;
				break;
			case 'o':
				optimize = 1;
				break;
			case 'w':
				warning = 0;
				break;
			case '1':
				strcpy(res_suffix, argv[1]);
				argc--; argv++;
				break;
			case '2':
				strcpy(src_suffix, argv[1]);
				argc--; argv++;
				break;
			case '3':
				strcpy(def_suffix, argv[1]);
				argc--; argv++;
				break;
			case '-':
				outfile = stdout;
				break;
			case '?':
				usage();
				exit(1);
				break;
			default:
				fprintf(stderr, "ERROR: invalid option -%c\n", argv[0][1]);
				usage();
				exit(1);
				break;
			}

		argc--; argv++;
		}

	ptr = strrchr(argv[0], '\\');
	if (ptr != NULL) {
		*ptr = '\0';
		strcpy(path, argv[0]);
		*ptr = '\\';
		}
	else
		strcpy(path, ".");

	if (src_path[0] == '\0')
		strcpy(src_path, path);
	if (res_path[0] == '\0')
		strcpy(res_path, path);
	if (def_path[0] == '\0')
		strcpy(def_path, path);

	myerr = _dos_findfirst(argv[0], _A_NORMAL, &myfind);
	if (myerr != 0)
		fprintf(stderr, "File not found - '%s'\n", argv[0]);
	for ( ; myerr == 0 ; ) {
		if (myfind.attrib == _A_SUBDIR) {
			if (verbose)
				printf("Skipping directory %s...\n", myfind.name);
			}
		else {
			if (verbose)
				printf("Converting %s\\%s...\n", path, myfind.name);
			strcpy(filename, myfind.name);
			do_string_to_rsrc();
			}

		myerr = _dos_findnext(&myfind);
		}

	if (myerr != 18 && myerr != -1)  // *** -1 seems OK too ***
		printf("Error #%d getting next file.\n", myerr);
	else
		myerr = 0;

	return myerr;
	}

buildpath(fullname, path, name)
char	*fullname;
char	*path;
char	*name;
{
	sprintf(fullname, "%s\\%s", path, name);
	}

do_string_to_rsrc()
{
char	fullname[256];
char	*ptr;

	strcpy(rcname, filename);
	ptr = strrchr(rcname, '.');
	if (ptr != NULL)
		strcpy(ptr, res_suffix);
	else
		strcat(rcname, res_suffix);

	strcpy(defname, filename);
	ptr = strrchr(defname, '.');
	if (ptr != NULL)
		strcpy(ptr, def_suffix);
	else
		strcat(defname, def_suffix);

	strcpy(outname, filename);
	ptr = strrchr(outname, '.');
	if (ptr != NULL)
		strcpy(ptr, src_suffix);
	else
		strcat(defname, src_suffix);

	buildpath(fullname, path, filename);
	infile = fopen(fullname, "r");

	buildpath(fullname, res_path, rcname);
	rcfile = fopen(fullname, "w");

	buildpath(fullname, def_path, defname);
	deffile = fopen(fullname, "w");

	if (outfile == NULL) {
		buildpath(fullname, src_path, outname);
		outfile = fopen(fullname, "w");
		}

	if (rcfile == NULL || deffile == NULL || infile == NULL || outfile == NULL) {
		if (rcfile != NULL) fclose(rcfile);
		if (deffile != NULL) fclose(deffile);
		if (infile != NULL) fclose(infile);
		if (outfile != NULL) fclose(outfile);
		}
	else {
		process_strings_to_rsrc();

		fclose(rcfile);
		fclose(deffile);
		fclose(infile);
		fclose(outfile);
		}
	}

process_strings_to_rsrc()
{
char	*ptr,
		*strptr;

	strcpy(def_id_name, filename);
	ptr = strchr(def_id_name, '.');
	if (ptr != NULL)
		*ptr = '_';

	fprintf(rcfile, "\n\nSTRINGTABLE\nBEGIN\n\n");

	fprintf(outfile, "\n/* Include %s generated header file...*/\n", progname);
	fprintf(outfile, "#include \"%s\"\n\n", defname);
	fprintf(outfile, "extern char *GetStringResource(int);\n\n");

	fprintf(outfile, "extern char *_gStr_emptyStr;\n");
	fprintf(outfile, "extern char *_gStr_dotStr;\n\n");

	for ( ; ; ) {
		if (fgets(inbuf, 8192, infile) == NULL)
			break;

		line_num++;

		if (inbuf[0] == '#') {
			if (verbose)
				printf("[%ld] Skip compiler directive <%s>\n",
							line_num, inbuf);
			fprintf(outfile, "%s", inbuf);
			continue;
			}

		strptr = strbuf;

		for (ptr = inbuf; *ptr; ptr++) {
			if (commenting) {
				fputc(*ptr, outfile);
				if (*ptr == '*' && *(ptr + 1) == '/') {
					commenting = 0;
					fputc('/', outfile);
					ptr++;
					}
				}

			else if (quoting) {
				fputc(*ptr, outfile);
				if (*ptr == '\\' && *(ptr + 1) == '\'') {
					fputc('\'', outfile);
					ptr++;
					}
				else if (*ptr == '\\' && *(ptr + 1) == '\\') {
					fputc('\\', outfile);
					ptr++;
					}
				else if (*ptr == '\n') {
					if (warning)
						fprintf(stderr, "[%d] WARNING New line in quote constant.\n", line_num);
					quoting = 0;
					}
				else if (*ptr == '\'')
					quoting = 0;
				}

			else if (*ptr == '"') {
				if (pass_string) {
					fputc('"', outfile);
					if (pass_string > 0)
						pass_string = -1;
					else
						pass_string = 0;
					}
				else if (stringing) {
					stringing = 0;
					*strptr = '\0';
					do_string_entry();
					}
				else {
					stringing = 1;
					strptr = strbuf;
					}
				}

			else if (*ptr == '\'' && (! (stringing || pass_string))) {
				fputc(*ptr, outfile);
				quoting = 1;
				}

			else if ( *ptr == '/' && *(ptr + 1) == '*' &&
						(! (stringing || pass_string)) )
				{
				if (*(ptr + 2) == '!' && *(ptr + 3) == '*' && *(ptr + 4) == '/') {
					pass_string = 1;
					fprintf(outfile, "/*!*/");
					ptr += 4;
					}
				else {
					fputc('/', outfile); fputc('*', outfile);
					ptr++;
					commenting = 1;
					}
				}

			else if (*ptr == '\\' && (stringing || pass_string)) {
				if (*(ptr + 1) == '"') {
					ptr++;
					if (pass_string) {
						fputc('\\', outfile);
						fputc('"', outfile);
						}
					else {
						*strptr++ = '\\';
						*strptr++ = '"';
						}
					}
				else if (*(ptr + 1) == '\\') {
					ptr++;
					if (pass_string) {
						fputc('\\', outfile);
						fputc('\\', outfile);
						}
					else {
						*strptr++ = '\\';
						*strptr++ = '\\';
						}
					}
				else {
					if (pass_string)
						fputc(*ptr, outfile);
					else
						*strptr++ = *ptr;
					}
				}

			else if (*ptr == '\n' && (stringing || pass_string)) {
				if (warning)
					fprintf(stderr, "[%d] WARNING New line in string constant.\n", line_num);
				}

			else {
				if (! stringing)
					fputc(*ptr, outfile);
				else
					*strptr++ = *ptr;
				}
			}

		stringing = 0;
		quoting = 0;
		}

	fprintf(rcfile, "\nEND\n");
	}

do_string_entry()
{
int		used_id;

	if (strlen(strbuf) > 254) {
		if (verbose)
			printf("[%ld] SKIP string longer than 254 <%s>\n",
						line_num, strbuf);
		fprintf(outfile, /*!*/"\"%s\"", strbuf);
		}
	else {
		if (optimize && strbuf[0] == '\0') {
			if (verbose)
				printf("[%ld] OPT Empty String\n", line_num);
			fprintf(outfile, "_gStr_emptyStr");
			}
		else if (optimize && strcmp(strbuf, ".") == 0) {
			if (verbose)
				printf("[%ld] OPT Dot String\n", line_num);
			fprintf(outfile, "_gStr_dotStr");
			}
		else {
			if (optimize)
				used_id = check_string(strbuf);
			else
				used_id = -1;

			if (verbose)
				printf("[%ld] RSRC %sID #%d <%s>\n",
						line_num, ((used_id != -1) ? "CACHED " : ""),
						((used_id != -1) ? used_id : rsrc_id), strbuf);

			if (used_id != -1) {
				fprintf(outfile, "GetStringResource(IDS_%s_%d)",
							def_id_name, used_id);
				/* NO rc file entry... Its already there!!! */
				/* DITTO for the define file entry! */
				}
			else {
				fprintf(outfile, "GetStringResource(IDS_%s_%d)",
							def_id_name, rsrc_id);
				fprintf(rcfile, "\tIDS_%s_%d, \"%.254s\"\n",
							def_id_name, rsrc_id, strbuf);
				fprintf(deffile, "#define IDS_%s_%d\t\t%d\n",
							def_id_name, rsrc_id, rsrc_id);
				rsrc_id += rsrc_id_inc;
				}
			}
		}
	}

check_string(string)
char	*string;
{
struct str_cache *cache_ptr;

	for (cache_ptr=cache_list; cache_ptr != NULL; cache_ptr=cache_ptr->next) {
		if (strcmp(cache_ptr->str, string) == 0) {
			if (verbose)
				printf("[%ld] HIT CACHE <%s> %d\n",
						line_num, cache_ptr->str, cache_ptr->rsrc_id);
			return cache_ptr->rsrc_id;
			}
		}

	add_new_string(string, rsrc_id);

	return -1;
	}

add_new_string(string, rsrc_id)
char	*string;
int		rsrc_id;
{
struct str_cache *cache_ptr;

	cache_ptr = (struct str_cache *) malloc(sizeof(struct str_cache));
	if (cache_ptr == NULL)
		return 0;

	cache_ptr->str = malloc(strlen(string) + 1);
	if (cache_ptr->str == NULL) {
		free(cache_ptr);
		return 0;
		}

	strcpy(cache_ptr->str, string);
	cache_ptr->rsrc_id = rsrc_id;

	cache_ptr->next = cache_list;
	cache_list = cache_ptr;
	return 1;
	}

