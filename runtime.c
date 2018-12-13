#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdarg.h>
#include <stdbool.h>

/*
 *
 * BitTwiddler runtime types and pools
 *
 */
struct __bt_arr {
    size_t n;               // Number of elements in the array
    size_t elsz;            // Size of each element
    bool owns_data;         // Does this array own the data in *data?
    void *data;             // Backing data
    struct __bt_arr *next;  // Next array in the linked list
};


struct __bt_str {
    size_t n;               // Number of characters, excluding NUL
    struct __bt_arr *arr;   // Backing array
    struct __bt_str *next;  // Next string in the linked list
};

static struct __bt_arr *__bt_arr_ll = NULL;
static struct __bt_str *__bt_str_ll = NULL;

/*
 *
 * Debug functions
 *
 */
void __bt_dbg_print_arr(struct __bt_arr *a) {
    fprintf(stderr, "[arr n=%lu elsz=%lu owns=%d data=%p]\n",
            a->n, a->elsz, a->owns_data, a->data);
}

void __bt_dbg_print_str(struct __bt_str *s) {
    fprintf(stderr, "[str len=%lu arr=[%p", s->n, s->arr);

    if (s->arr)
        fprintf(stderr, " n=%lu elsz=%lu] %p '%s']\n", s->arr->n, s->arr->elsz,
                s->arr->data, (char*)s->arr->data);
    else
        fprintf(stderr, "]\n");
}

void __bt_dbg_print_arr_pool(void) {
    fprintf(stderr, "Array Pool {\n");
    struct __bt_arr *a = __bt_arr_ll;
    while (a) {
        __bt_dbg_print_arr(a);
        a = a->next;
    }
    fprintf(stderr, "}\n");
}

void __bt_dbg_print_str_pool(void) {
    fprintf(stderr, "String Pool {\n");
    struct __bt_str *s = __bt_str_ll;

    while (s) {
        __bt_dbg_print_str(s);
        s = s->next;
    }
    fprintf(stderr, "}\n");
}

/*
 *
 * BitTwiddler arrays
 *
 */

void __bt_arr_freeall(void) {
    struct __bt_arr *next, *p = __bt_arr_ll;

    while (p) {
        next = p->next;

        if (p->owns_data)
            free(p->data);

        free(p);
        p = next;
    }
}

// Allocates, zero-initializes and returns a new BitTwiddler array
struct __bt_arr *__bt_arr_new(size_t n, size_t elsz, void *data)
{
    // Free all arrays at program exit (register this function when the first
    // array is created).
    if (!__bt_arr_ll)
        atexit(__bt_arr_freeall);

    // Allocate space for a new array structure
    struct __bt_arr *a = calloc(1, sizeof(*a));

    if (!a) {
        fprintf(stderr, "Runtime Error: %s failed to allocate new array node\n",
                __func__);
        exit(1);
    }

    // Allocate space for data
    a->n = n;
    a->elsz = elsz;
    a->owns_data = data == NULL;
    a->data = a->owns_data ? calloc(n, elsz) : data;

    if (!a->data) {
        fprintf(stderr, "Runtime Error: %s failed to allocate new array\n",
            __func__);
        exit(1);
    }

    // Put it in the pool
    a->next = __bt_arr_ll;
    __bt_arr_ll = a;

    return a;
}

// Resizes an array
void __bt_arr_resize(struct __bt_arr *arr, size_t new_n) {
    char *data = realloc(arr->data, arr->elsz*new_n);

    if (!data) {
        fprintf(stderr, "Runtime Error: %s failed to resize array\n", __func__);
        exit(1);
    }

    arr->data = data;
    arr->n = new_n;
}

// Returns the underlying array data
char *__bt_arr_get(struct __bt_arr *arr) {
    return arr->data;
}

/*
 *
 *  BitTwiddler strings
 *
 */

// Frees all BitTwiddler strings, runs at program exit
// Backing arrays will be freed by __bt_arr_freeall
void __bt_str_freeall(void) {
    struct __bt_str *next, *p = __bt_str_ll;

    while (p) {
        next = p->next;
        free(p);
        p = next;
    }
}

// Allocates, initializes and returns a new BitTwiddler string
struct __bt_str *__bt_str_new(const char *val) {
    // Free all strings at program exit (register this function when the first
    // string is created).
    if (!__bt_str_ll)
        atexit(__bt_str_freeall);

    // Allocate space for a new string structure
    struct __bt_str *s = calloc(1, sizeof(*s));

    if (!s) {
        fprintf(stderr, "Runtime Error: %s failed to allocate new string\n",
                __func__);
        exit(1);
    }

    // Put it in the pool
    s->next = __bt_str_ll;
    __bt_str_ll = s;

    if (val) {
        size_t n = strlen(val);
        s->n = n;
        s->arr = __bt_arr_new(n + 1, 1, (void*) val);
    }

    return s;
}

// Concatenates two BitTwiddler strings into a third, new string
struct __bt_str *__bt_str_concat(struct __bt_str *s1, struct __bt_str *s2) {
    struct __bt_str *s = __bt_str_new(NULL);

    s->n = s1->n + s2->n;
    s->arr = __bt_arr_new(s->n + 1, sizeof(char), NULL);

    strcpy(s->arr->data, s1->arr->data);
    strcat(s->arr->data, s2->arr->data);
    return s;
}

// Reads a BitTwiddler string from the standard input (NUL-terminated)
struct __bt_str *__bt_str_read(void) {

    // Start with a small buffer
    size_t sz = 32;
    char *buf = calloc(sz, sizeof(*buf));
    size_t n = 0;

    if (!buf) {
        fprintf(stderr, "Runtime Error: %s failed to allocate buffer for"
                        " string from stdin\n", __func__);
        exit(1);
    }

    for(;;) {
        int c = fgetc(stdin);

        if (c == EOF) {
            fprintf(stderr, "Runtime Error: %s EOF while reading string\n",
                    __func__);
            exit(1);
        }

        // Found end of string
        if (!c)
            break;

        buf[n++] = (char) c;

        // Buffer size needs to increase
        if (n == sz - 1) {
            sz *= 2;
            buf = realloc(buf, sz);
            if (!buf) {
                fprintf(stderr, "Runtime Error: %s failed to allocate buffer"
                                " for string from stdin\n", __func__);
                exit(1);
            }
        }
    }

    return __bt_str_new(buf);
}

const char *__bt_str_get(struct __bt_str *s) {
    return s->arr->data;
}

/*
 *
 * BitTwiddler reading from stdin
 *
 */
void __bt_read(void *target, size_t n) {
    errno = 0;
    int r = read(STDIN_FILENO, target, n);

    if (r == n)
        return;

    const char *err;
    if (r < 0)
        err = strerror(errno);
    else if (r == 0)
        err = "reached EOF";
    else
        err = "";

    fprintf(stderr, "Failed to read %lu bytes from standard input: %s\n",
            n, err);
    fprintf(stderr, "Aborting...\n");
    exit(1);
}

int8_t  __bt_read_i8(void)  { int8_t v;  __bt_read((void*)&v, 1); return v; }
int16_t __bt_read_i16(void) { int16_t v; __bt_read((void*)&v, 2); return v; }
int32_t __bt_read_i32(void) { int32_t v; __bt_read((void*)&v, 4); return v; }
int64_t __bt_read_i64(void) { int64_t v; __bt_read((void*)&v, 8); return v; }
float   __bt_read_f32(void) { float v;   __bt_read((void*)&v, 4); return v; }
double  __bt_read_f64(void) { double v;  __bt_read((void*)&v, 8); return v; }

/*
 *
 * BitTwiddler printing function
 *
 */

enum __bt_emit_kind {
    __BT_EMIT_EMIT,
    __BT_EMIT_PRINT,
    __BT_EMIT_FATAL
};

void __bt_emit(int kind, const char *fmt, ...) {
    FILE * stream = kind == __BT_EMIT_EMIT ? stdout : stderr;

    va_list args;
    va_start(args, fmt);
    vfprintf(stream, fmt, args);
    va_end(args);

    if (kind == __BT_EMIT_FATAL) {
        exit(1);
    }
}

