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
  raw <- read_delim(x, col_names = FALSE, trim_ws = TRUE, skip = 2, delim = " ", guess_max = 1000) %>%
    separate(X1)
  
    raw <- fread(x, header = FALSE, fill = TRUE) %>%
      lazy_dt() %>%
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
    
    headerRaw <- raw %>%
      filter(header == TRUE) %>%
      mutate(ensemble = rep(c(1:(n() / 6)), each = 6)) %>%
      as.tbl()
      group_by(ensemble) %>%
      summarise(dat = bind_cols(.[1], .[2], .[3], .[4], .[5], .[6]))

    

    for (j in 1:max(headerRaw$ensemble, na.rm = TRUE)) {
      headerData <- headerRaw %>%
        filter(ensemble == j) %>%
        select(-ensemble, -header) %>%
        mutate(ensemble = j) %>%
        as.data.table()

      header <- data.table(
        ensemble = j,
        data = c( for (x in 1:6){
          headerData[x]
        }
          headerData[1,],
          headerData[2,],
          headerData[3,],
          headerData[4,],
          headerData[5,],
          headerData[6,]
        )
      ) %>%
        rbind(header, .)
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