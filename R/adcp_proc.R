#' Converting ADCP ensembles into a header and ensemble data tables
#'
#' @param x text file containing ADCP data from either a Teledyne workshorse or
#'   Riverray units
#'
#' @return A comma delimited file with the ensemble metadata and data in a tidy
#'   format.
#' @export
#'
#' @examples ADCP_proc(x)
#'
#' @import tidyverse
#' @import data.table
#' @import dtplyr

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

    data$raw <- raw

    headerRaw <- raw %>%
      filter(header == TRUE) %>%
      mutate(ensemble = rep(c(1:(n() / 6)), each = 6))

    ens2String <- function(x) {
      toString(c(x[1,], x[2,], x[3,], x[4,], x[5,], x[6,]))
    }

    ensemble <- headerRaw %>%
      group_by(ensemble) %>%
      summarise(header = ens2String(.))

    ensemble %>%
      mutate(header = str_split(string = header, pattern = ","))

    dim(ensemble) <- c(1, dim(ensemble)[1] * dim(ensemble)[2])

    for (i in 2:max(headerRaw$ensemble)) {
      ens <- headerRaw %>%
        filter(ensemble == i) %>%
        select(-ensemble, -header) %>%
        as.matrix()

      dim(ens) <- c(1, dim(ens)[1] * dim(ens)[2])

      ensemble <- rbind(ensemble, ens)
    }

    data$header <- ensemble




    cleanHeader <- header %>%
      select(-27, -33:-40, -46:-53, -63:-66, -73:-79)

    # create a tibble of run lengths, filter out header
    rle <- rle(data$header) %>%
      unclass() %>%
      data.frame() %>%
      filter(values == FALSE)

    body <- data %>%
      filter(header == FALSE) %>%
      mutate(ensemble = rep(c(1:length(rle$lengths)), times = rle$lengths)) %>%
      select(-header) %>%
      select(ensemble, everything())

    allData <- left_join(body, cleanHeader, by = "ensemble")

    message(
      paste0(
        "Finished processing ",
        file.list[i],
        ", saving as CSV in working directory. \n \n"
      )
    )

    write_csv(allData, paste(substr(file.list[i], 1, nchar(file.list[i]) - 4), ".csv", sep = ""))

    progFiles$tick()$print
  }
}
