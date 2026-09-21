/*
 * Windows launcher for the HOBBS Rust sampler.
 *
 * Cargo builds the sampler as a Rust static library on Windows and Rtools
 * links this tiny launcher against it.  This mirrors the r-rust/hellorust
 * CRAN pattern while preserving HOBBS's standalone sampler process.
 */
extern void hobbs_main(void);

int main(void) {
    hobbs_main();
    return 0;
}
