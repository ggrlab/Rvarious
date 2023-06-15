options(warn=1)
pacman::p_load(BiocManager, ggcyto)
pacman::p_load(rpart)
devtools::install_github("YaohuiZeng/grpregOverlap")
devtools::install_github("rehbergT/zeroSum/zeroSum")


devtools::document()
devtools::install_local()
devtools::check()
