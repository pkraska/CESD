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
    data <- list(raw = raw,
                 header = )

    headerRaw <- raw %>%
      filter(header == TRUE) %>%
      mutate(ensemble = rep(c(1:(n() / 6)), each = 6))

    ensemble <- headerRaw %>%
      filter(ensemble == 1) %>%
      select(-ensemble, -header) %>%
      as.matrix()

    dim(ensemble) <- c(1, dim(test)[1] * dim(test)[2])

    for (i in 2:max(headerRaw$ensemble)) {
      ens <- headerRaw %>%
        filter(ensemble == i) %>%
        select(-ensemble, -header) %>%
        as.matrix()

      dim(ens) <- c(1, dim(test)[1] * dim(test)[2])

      ensemble <- bind_rows(ensemble, ens)
    }


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
