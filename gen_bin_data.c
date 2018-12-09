#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

int fd = STDOUT_FILENO;

int main(int argc, char **argv) {
    argc--; argv++;

    while(argc) {
        char *t = *(argv++);
        char *d = *(argv++);
        argc -= 2;

        if (!strcmp(t, "s")) {
            write(fd, (const void*)d, strlen(d) + 1);
        }
#define CASE(N,T,S,F)\
        else if (!strcmp(t,N)){T v = F(d);write(fd, (const void*)&v, S);}
        CASE("i8",  int8_t,   1, atoi)
        CASE("i16", int16_t,  2, atoi)
        CASE("i32", int32_t,  4, atoi)
        CASE("i64", int64_t,  8, atoi)
        CASE("u8",  uint8_t,  1, atoi)
        CASE("u16", uint16_t, 2, atoi)
        CASE("u32", uint32_t, 4, atoi)
        CASE("u64", uint64_t, 8, atoi)
        CASE("f32", float,    4, atof)
        CASE("f64", double,   8, atof)
#undef CASE
        else {
            printf("unmatched\n");
        }
    }
    return 0;
}
