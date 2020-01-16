x <-
  "R:/Science/CESD/COERS/FPage/data/ADCP/Calders Head Salmon2019_1 20190521T180054.pd0"
x <-
  "/home/kraskape/Documents/data/Calders Head Salmon2019_1 20190521T180054.pd0"
library(oce)
library(data.table)
library(tidyverse)

raw <- fread("~/Documents/data/Calders Head Salmon2019_1 20190521T180054.averaged.txt", fill = TRUE, header = FALSE)

variables <- unlist(raw[,8][1:9],use.names = FALSE)
col_names <- unlist(raw[1], use.names = FALSE)

data <- slice(raw, 10:n()) %>%
  mutate(variable = rep(variables, length.out = n())) %>%
  select(variable, everything())
%>%
  t()

%>%
  rename(.vars(col_names))

= vars_select(num_range(prefix = "V", range = 1:9)))
,
         col_names[2] = V2,
         col_names[3] = V3,
         col_names[4] = V4,
         col_names[5] = V5,
         col_names[6] = V6,
         col_names[7] = V7
         )

select(ensemble = Num, variable, everything())


