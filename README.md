<!-- badges: start -->
[![R-CMD-check](https://github.com/ggrlab/Rvarious/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ggrlab/Rvarious/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Howto install: 
# 0. Install devtools
install.packages("devtools")
# 1. Build the package
devtools::document()
devtools::build()
# 2. Install dependencies
install.packages("ROCit")
# 3. Install the package from source
all_packaged_versions <- file.info(list.files("..", pattern="Rvarious_.*.tar.gz", full.names=TRUE))
most_recent <- rownames(all_packaged_versions)[which.max(all_packaged_versions[["mtime"]])]
install.packages(most_recent, repos = NULL, type="source")
