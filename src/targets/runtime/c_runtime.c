#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum { TYPE_NULL, TYPE_NUMBER, TYPE_STRING } AbyssType;

typedef struct {
    AbyssType type;
    union {
        double number;
        char* string;
    } data;
} AbyssValue;

AbyssValue create_null() {
    AbyssValue v; v.type = TYPE_NULL; return v;
}

AbyssValue create_number(double n) {
    AbyssValue v; v.type = TYPE_NUMBER; v.data.number = n; return v;
}

AbyssValue create_string(const char* s) {
    AbyssValue v;
    v.type = TYPE_STRING;
    v.data.string = strdup(s);
    if (!v.data.string) {
        fprintf(stderr, "fatal: strdup failed\n");
        exit(1);
    }
    return v;
}

AbyssValue abyss_add(AbyssValue a, AbyssValue b) {
    return create_number(a.data.number + b.data.number);
}

AbyssValue abyss_mul(AbyssValue a, AbyssValue b) {
    return create_number(a.data.number * b.data.number);
}

void print(AbyssValue v) {
    switch (v.type) {
        case TYPE_NUMBER:
            printf("%f", v.data.number);
            break;
        case TYPE_STRING:
            printf("%s", v.data.string);
            break;
        case TYPE_NULL:
            printf("null");
            break;
        default:
            printf("Unknown Type");
            break;
    }
}

void println(AbyssValue v) {
    print(v);
    printf("\n");
}
