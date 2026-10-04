extern int (*first)(void);
extern int (*second)(void);
extern int (*test_entry)(void);

static int replacement(void) {
    return 42;
}

int main(void) {
    if (first() != 1 || second() != 21 || test_entry() != 1) {
        return 1;
    }
    first = second;
    if (test_entry() != 21) {
        return 2;
    }
    first = replacement;
    return test_entry();
}
