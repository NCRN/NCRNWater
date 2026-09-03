## Summary
Describe what changed and why.

## Required (every merge)
- [ ] Ran sampled suite locally: `devtools::test()`
- [ ] Ran exhaustive suite locally: `Rscript tests/run_exhaustive.R`
- [ ] Verified quiet logs (no benign warnings)
- [ ] Updated/added test docstrings (boilerplate header)
- [ ] Ensured type-correct empties (numeric(0), character(0), list())
- [ ] Confirmed class invariants hold (test-classes.R)

## Recommended (for non-trivial changes)
- [ ] Ran full CRAN-style check: `devtools::check()`
- [ ] Measured coverage: 
      ```r
      cov <- covr::package_coverage()
      covr::report(cov)
      ```
- [ ] Addressed any new coverage gaps (where reasonable)

## Notes
Anything relevant — logs, screenshots, coverage deltas, generated docs, edge cases, unusual failures.
