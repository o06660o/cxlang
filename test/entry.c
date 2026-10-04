extern int (*test_entry)(void);

int main(void) {
    return test_entry();
}
