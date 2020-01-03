#' Converting ADCP ensembles into a header and ensemble data tables
#'
#' @param x text file containing ADCP data from either a Teledyne Riverray units
#'   with a 6 line header and a single extra line at the start of the data.
#'
#' @return A list composed of the raw data, the flattened header information, as
#'   well as the actual ensemble data.
#'
#' @export
#'
#' @examples ADCP_proc(x)
#'
#' @import tidyverse
#' @import data.table

ADCP_proc <- function(x) {
  raw <- fread(x, header = FALSE, fill = TRUE) %>%
    as.tbl() %>%
    mutate(
      header = case_when(
        V2 == "cm" ~ TRUE,
        lead(V2, 5) == "cm" ~ TRUE,
        lead(V2, 4) == "cm" ~ TRUE,
        lead(V2, 3) == "cm" ~ TRUE,
        lead(V2, 2) == "cm" ~ TRUE,
        lead(V2, 1) == "cm" ~ TRUE,
        TRUE ~ FALSE
      )
    )

  # Function to take the 6 rows identified as the header, and widen them/unstack
  # them into a single line.
  ens2String <- function(x) {
    toString(c(x[1,], x[2,], x[3,], x[4,], x[5,], x[6,]))
  }

  # filter the raw data to only the header information and assign an ensemble
  # number to the 6 rows
  headerRaw <- raw %>%
    filter(header == TRUE) %>%
    mutate(ensemble = rep(c(1:(n(

    ) / 6)), each = 6)) %>%
    group_by(ensemble) %>%
    do(header = ens2String(.)) %>%
    as_tibble() %>%
    separate(header, paste0("V", c(1:90)), sep = ",")

  # Remove the columns created by the header and ensemble columns created to
  # keep track of ensembles in headerRaw
  header <- headerRaw %>%
    select(-15,-16,-30,-31,-45,-46,-60,-61,-75,-76,-90,-91)

  # create a tibble of run lengths, filter out header
  rle <- rle(raw$header) %>%
    unclass() %>%
    as_tibble() %>%
    slice(2:n()) %>%
    filter(values == FALSE)

  ensemble <- raw %>%
    slice(2:n()) %>%
    filter(header == FALSE) %>%
    mutate(ensemble = rep(c(1:length(rle$lengths)), times = rle$lengths)) %>%
    select(-header) %>%
    select(ensemble, everything()) %>%
    mutate_at(vars(starts_with("V")), as.numeric)

  list(raw = raw,
       header = header,
       data = ensemble)

}
