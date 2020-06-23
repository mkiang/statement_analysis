## 00_rerun_everything.R ----
## Just re-runs everything for data updates. Convenience script. 

## Reanalyze data ----
source("./code/01_process_raw_data.R")
source("./code/02_keyword_searches.R")

## Redo fig 1 including private version ----
source("./code/03_plot_keywords_by_institution.R")
source("./code/99_plot_fig1_private.R")

## Reknit results and descriptives ----
knitr::knit("./rmds/01_descriptives.Rmd",
            "./rmds/01_descriptives.html")

knitr::knit("./rmds/02_evaluating_statements.Rmd",
            "./rmds/02_evaluating_statements.html")

## Reknit the github README ----
knitr::knit("./README.Rmd",
            "./README.md")

## Save session information ----
sink(here("session_info.txt"))
sessioninfo::session_info()
sink()
