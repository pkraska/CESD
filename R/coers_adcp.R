x <- "R:/Science/CESD/COERS/FPage/data/ADCP/Calders Head Salmon2019_1 20190521T180054.pd0"
library(oce)
coers_adcp <- function(x) {
  adcp_raw <- read.adp.rdi(x)
}
