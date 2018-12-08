#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

/*
 *
 * BitTwiddler runtime types and pools
 *
 */
struct __bt_arr {
    size_t n;             // Number of elements in the array
    size_t elsz;          // Size of each element
    char *data;           // Backing memory
};

struct __bt_str {
    size_t n;             // Number of characters, excluding the NUL terminator
    struct __bt_arr *arr; // Backing array
};

struct __bt_arr_node {
    struct __bt_arr *arr;
    struct __bt_arr_node *next;
};

struct __bt_str_node {
    struct __bt_str *str;
    struct __bt_str_node *next;
};

static struct __bt_arr_pool {
    size_t n;
    struct __bt_arr_node *root;
} __bt_arr_pool = {
    0,
    NULL
};

static struct __bt_str_pool {
    size_t n;
    struct __bt_str_node *root;
} __bt_str_pool = {
    0,
    NULL
};


/*
 *
 * Debug functions
 *
 */
void __bt_dbg_print_arr(struct __bt_arr *a) {
    fprintf(stderr, "[arr n=%lu elsz=%lu data=%p '%s']\n", a->n, a->elsz,
            a->data, a->data);
}

void __bt_dbg_print_str(struct __bt_str *s) {
    fprintf(stderr, "[str len=%lu arr=[%p", s->n, s->arr);

    if (s->arr)
        fprintf(stderr, " n=%lu elsz=%lu] %p '%s']\n", s->arr->n, s->arr->elsz,
                s->arr->data, s->arr->data);
    else
        fprintf(stderr, "]\n");
}

void __bt_dbg_print_arr_pool(void) {
    fprintf(stderr, "Array Pool (%lu elements) {\n", __bt_arr_pool.n);
    struct __bt_arr_node *a = __bt_arr_pool.root;
    while (a) {
        __bt_dbg_print_arr(a->arr);
        a = a->next;
    }
    fprintf(stderr, "}\n");
}

void __bt_dbg_print_str_pool(void) {
    fprintf(stderr, "String Pool (%lu elements root=%p) {\n",
            __bt_str_pool.n, __bt_str_pool.root);
    struct __bt_str_node *s = __bt_str_pool.root;
    while (s) {
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
    struct __bt_arr_pool *p = &__bt_arr_pool;

    struct __bt_arr_node *node = p->root;
    while (node) {
        struct __bt_arr_node *nxt = node->next;
        free(node->arr);
        free(node);
        node=nxt;
    }
}

// Allocates a new, uninitialized BitTwiddler array
struct __bt_arr *__bt_arr_alloc(void) {
    struct __bt_arr_pool *p = &__bt_arr_pool;

    // Free all arrays at program exit (register this function when the first
    // array is created).
    if (!p->n)
        atexit(__bt_arr_freeall);

    // Allocate space for a new array structure
    struct __bt_arr *a = calloc(1, sizeof(*a));

    if (!a) {
        fprintf(stderr, "Runtime Error: %s failed to allocate new array\n",
                __func__);
        exit(1);
    }

    // Put it in the pool
    struct __bt_arr_node *node = calloc(1, sizeof(*node));

    if (!node) {
        fprintf(stderr, "Runtime Error: %s failed to allocate new array node\n",
                __func__);
        exit(1);
    }

    node->arr = a;
    node->next = p->root;
    p->root = node;
    ++p->n;

    return a;
}

// Allocates, zero-initializes and returns a new BitTwiddler array
struct __bt_arr *__bt_arr_new(size_t n, size_t elsz) {
    struct __bt_arr *arr = __bt_arr_alloc();

    arr->n = n;
    arr->elsz = elsz;
    arr->data = calloc(arr->n, arr->elsz);

    if (!arr->data) {
        fprintf(stderr, "Runtime Error: %s failed to allocate new array\n",
                __func__);
        exit(1);
    }

    return arr;
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
    struct __bt_str_pool *p = &__bt_str_pool;

    struct __bt_str_node *node = p->root;
    while (node) {
        struct __bt_str_node *nxt = node->next;
        free(node->str);
        free(node);
        node=nxt;
    }
}

// Allocates a new, uninitialized BitTwiddler string
struct __bt_str *__bt_str_alloc(void) {
    struct __bt_str_pool *p = &__bt_str_pool;

    // Free all strings at program exit (register this function when the first
    // string is created).
    if (!p->n)
        atexit(__bt_str_freeall);

    // Allocate space for a new string structure
    struct __bt_str *s = calloc(1, sizeof(*s));

    if (!s) {
        fprintf(stderr, "Runtime Error: %s failed to allocate new string\n",
                __func__);
        exit(1);
    }

    // Put it in the pool
    struct __bt_str_node *node = calloc(1, sizeof(*node));

    if (!node) {
        fprintf(stderr, "Runtime Error: %s failed to allocate new array node\n",
                __func__);
        exit(1);
    }

    node->str = s;
    node->next = p->root;
    p->root = node;
    ++p->n;

    return s;
}

// Allocates, initializes and returns a new BitTwiddler string
struct __bt_str *__bt_str_new(const char *s) {
    struct __bt_str *ns = __bt_str_alloc();

    ns->n = strlen(s);
    ns->arr = __bt_arr_new(ns->n + 1, sizeof(char));

    // Fill the new string with the passed-in data
    strcpy(ns->arr->data, s);

    return ns;
}

// Concatenates two BitTwiddler strings into a third, new string
struct __bt_str *__bt_str_concat(struct __bt_str *s1, struct __bt_str *s2) {
    struct __bt_str *s = __bt_str_alloc();

    s->n = s1->n + s2->n;
    s->arr = __bt_arr_new(s->n + 1, sizeof(char));

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

    // Not very efficient (there's a copy involved)
    struct __bt_str *s = __bt_str_new(buf);
    free(buf);
    return s;
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
