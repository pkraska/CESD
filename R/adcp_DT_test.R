library(DT)
library(data.table)

raw <- fread(x, header = FALSE, fill = TRUE)

DT <- raw[2:.N][, header := ifelse(
  test =
    shift(V2,-5) == "cm" |
    shift(V2,-4) == "cm" |
    shift(V2,-3) == "cm" |
    shift(V2,-2) == "cm" |
    shift(V2,-1) == "cm" |
    V2 == "cm",
  yes = TRUE,
  no = FALSE
)][header == TRUE][, ensemble := rep(c(1:(.N / 6)), each = 6)][, line := rep(1:6, length.out = .N)]

ensemble <- DT[ensemble == 1][, header := NULL][, ensemble := NULL]


test <- DT[, string := toString(c(DT[1], DT[2],DT[3], DT[4], DT[5], DT[6])), by = .(ensemble)][,string]


%>%
  select(-ensemble, -header) %>%
  as.matrix()

dim(ensemble) <- c(1, dim(ensemble)[1] * dim(ensemble)[2])

for (i in 2:max(headerRaw$ensemble)) {
  ens <- headerRaw %>%
    filter(ensemble == i) %>%
    select(-ensemble, -header) %>%
    as.matrix()

  dim(ens) <- c(1, dim(ens)[1] * dim(ens)[2])

  ensemble <- rbind(ensemble, ens)
}
