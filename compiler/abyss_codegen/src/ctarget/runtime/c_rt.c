extern int printf(const char *, ...);

void print(unsigned char *fmt, long long val) {
    printf((char *)fmt, val);
}
