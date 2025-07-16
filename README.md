# scholarAI

The use of this package is straightforward. Simply install and run in a local directory to build an A(E)I scholar.

```{r}
devtools::install_github("arthurgailes/scholarAI")

scholarAI::build_ai_scholar("NAME")
```

This will download a scholar's writing from aei.org and build:
1. A duckdb corpus, which can be queried
2. An instruction template for reproducing the author's writing style