#define LENGTH(x) (sizeof(x)/sizeof(x[0]))

typedef struct written_form {
	int def_id;
	const char *text;
} WrittenForm;

typedef struct cantonese_definition {
	const char *text;
} CantoneseDefinition;
