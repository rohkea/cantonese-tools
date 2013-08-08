#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "wordlist.h"

	/*
	 * Output grammar:
	 * 
	 * Word := FullWord | UnknownWord
	 * FullWord := OriginalForm "::" Description
	 * UnknownWord := OriginalForm
	 * OriginalForm := "<[" Word "]>"
	 * 
	 * Word cannot contain sequences "<[" or "]>" (if the input data
	 * does, it is treated as two UnknownWord’s).
	 * 
	 * The format for Translation is like this (but it is changed by
	 * editing data.h):
	 * Description := DescPart DescPart*
	 * DescPart := "<" DescPart ">"
	 */

/* Get the largest string that corresponds to char.
   Writes the length of selected text (in bytes) to len.
   Returns the index in the written_froms array or -1 (if not found). */
int
find_written_form(const char *text, int maxlen, int *len) {
	int from, to, id, compared, currlen;
	
	from = 0;
	to = written_forms_length -1;
	
	while (to - from > 1) {
		id = from + (to - from) / 2;
		
		compared = strncmp(text, written_forms[id].text, maxlen);
		if (compared == 0) {
			from = to = id;
		} else if (compared < 0) {
			to = id;
		} else {
			from = id;
		}
	}
	
	if (compared == 0) {
		*len = strlen(written_forms[id].text);
		return id;
	} else {
		if (compared < 0)
			id--;
	
		/* Maybe a smaller string would fit? */
		do {
			currlen = strlen(written_forms[id].text);
			compared = strncmp(written_forms[id].text, text, currlen);
			if (compared == 0) {
				*len = currlen;
				return id;
			}
			id--;
		} while (compared < 0 && id >= 0);
	}
	
	return -1;
}

void
append_to_result(const char *text, int text_len, char **result, int *res_allocd_p, int *res_len_p) {
	int len;
	
	if (!text_len)
		text_len = strlen(text);
	
	if (*res_len_p + text_len >= *res_allocd_p) {
		*res_allocd_p *= 2;
		*result = realloc(*result, *res_allocd_p);
		if (!*result) {
			fprintf(stderr, "Not enough memory!\n");
			exit(-1);
		}
	}
	
	strncpy(&(*result)[*res_len_p], text, text_len);
	*res_len_p += text_len;
	(*result)[*res_len_p] = 0;
}

/* Returns malloc()’d string, caller must free() it after using */
char *
parse_sentence(const char *sent, int maxlen) {
	char *result;
	int id, res_len, res_allocd, curr, unknown_start, unknown_len, known_len;
	
	res_allocd = 1024;
	res_len = 0;
	result = malloc(res_allocd);
	curr = 0;
	unknown_len = 0;
	
	/* If no length have been provided, calculate it */
	if (!maxlen)
		maxlen = strlen(sent);
	
	/* If sentence ends in carriage return, strip it */
	if (sent[maxlen -1] == 0x0D)
		maxlen--;
	
	while (curr < maxlen) {
		id = find_written_form(&sent[curr], maxlen, &known_len);		
		if (id == -1) {
			/* Not found, it’s unknown */
			if (unknown_len <= 0) {
				unknown_start = curr;
				unknown_len = 1;
			}
			else {
				if ((sent[unknown_len] == '<' && sent[unknown_len + 1] == '[')
						|| (sent[unknown_len] == ']'
							&& sent[unknown_len + 1] == '>')) {
					/* Don't allow sequence of <[ or ]> to appear in result */
					append_to_result("<[", 1, &result, &res_allocd, &res_len);
					append_to_result(&sent[unknown_start], 1, &result, &res_allocd, &res_len);
					append_to_result("]> ", 3, &result, &res_allocd, &res_len);
					
					unknown_start = curr;
					unknown_len = 1;
				} else {
					++unknown_len;
				}
			}
			++curr;
		} else {
			/* Found something! */
			if (unknown_len > 0) {
				append_to_result("<[", 2, &result, &res_allocd, &res_len);
				append_to_result(&sent[unknown_start], unknown_len, &result, &res_allocd, &res_len);
				append_to_result("]> ", 3, &result, &res_allocd, &res_len);
				
				unknown_len = 0;
			}
			
			append_to_result("<[", 2, &result, &res_allocd, &res_len);
			append_to_result(&sent[curr], known_len, &result, &res_allocd, &res_len);
			append_to_result("]>::", 4, &result, &res_allocd, &res_len);
			append_to_result(definitions[written_forms[id].def_id].text, 0, &result, &res_allocd, &res_len);
			append_to_result(" ", 1, &result, &res_allocd, &res_len);
			curr += known_len;
		}
	}
	
	
	if (unknown_len > 0) {
		append_to_result("<[", 2, &result, &res_allocd, &res_len);
		append_to_result(&sent[unknown_start], unknown_len, &result, &res_allocd, &res_len);
		append_to_result("]> ", 3, &result, &res_allocd, &res_len);
		
		unknown_len = 0;
	}
	return result;
}

int
main() {
	char buffer[2048], *r, *read;
	
	do {
		read = fgets(buffer, LENGTH(buffer), stdin);
		if (read == buffer) {
			r = parse_sentence(buffer, strlen(buffer) -1);
			printf("%s\n", r);
			free(r);
		}
	} while (!feof(stdin));
}

/*
Please comment on my C code.

I’ve written a simple tool that takes Chinese text and outputs it parsed into single words. It may be somewhat useful to Chinese learners like me. Text is read from stdin line by line.

Similar functionality is offered by sinoparserd/AdsoTrans, but I don’t like them much.

I’ve put all the data in the C file itself, since I don't need to change it often. Is this a bad idea? AdsoTrans also has a similar option.

*/
