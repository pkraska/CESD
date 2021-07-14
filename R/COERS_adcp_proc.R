#' Converting ADCP ensembles into a header and ensemble data tables
#'
#' @param x text file containing ADCP data from either a Teledyne Riverray units
#'   with a 6 line header and a single extra line at the start of the data.
#'
#' @return A list composed of the raw data, the flattened header information, as
#'   well as the actual ensemble data.
#'
#' @export
#' @return list of the raw, header, and body data.
#'
#' @examples ADCP_proc(x)
#'
#' @import readr
#' @import tidyr
#' @import tibble
#' @import dplyr
#'

ADCP_proc <- function(x) {
  raw <- read_lines(x) %>%
    tibble::as_tibble() %>%
    tidyr::separate(value, sep =  "\\s+", into = paste0("V", seq(1, 14, by = 1)))  %>%
    dplyr::mutate(
      header = dplyr::case_when(
        V2 == "cm" ~ TRUE,
        lead(V2, 5) == "cm" ~ TRUE,
        lead(V2, 4) == "cm" ~ TRUE,
        lead(V2, 3) == "cm" ~ TRUE,
        lead(V2, 2) == "cm" ~ TRUE,
        lead(V2, 1) == "cm" ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    dplyr::filter(!is.na(V2))
  
  
  # Function to take the 6 rows identified as the header, and widen them/unstack
  # them into a single line.
  ens2String <- function(x) {
    toString(c(x[1, ], x[2, ], x[3, ], x[4, ], x[5, ], x[6, ]))
  }
  
  # filter the raw data to only the header information and assign an ensemble
  # number to the 6 rows
  headerRaw <- raw %>%
    dplyr::filter(header == TRUE) %>%
    dplyr::mutate(ensemble = rep(c(1:(n(
      
    ) / 6)), each = 6)) %>%
    dplyr::group_by(ensemble) %>%
    dplyr::do(header = ens2String(.)) %>%
    tibble::as_tibble() %>%
    tidyr::separate(header, paste0("V", c(1:96)), sep = ",")
  
  # Remove the columns created by the header and ensemble columns created to
  # keep track of ensembles in headerRaw
  header <- headerRaw %>%
    dplyr::select(-15, -16, -31, -32, -47, -48, -63, -64, -79, -80, -95, -96)
  
  # create a tibble of run lengths, filter out header
  rle <- rle(raw$header) %>%
    unclass() %>%
    tibble::as_tibble() %>%
    dplyr::slice(2:n()) %>%
    dplyr::filter(values == FALSE)
  
  ensemble <- raw %>%
    dplyr::slice(2:n()) %>%
    dplyr::filter(header == FALSE) %>%
    dplyr::mutate(ensemble = rep(c(1:length(rle$lengths)), times = rle$lengths)) %>%
    dplyr::select(-header) %>%
    dplyr::select(ensemble, everything()) %>%
    dplyr::mutate_at(vars(starts_with("V")), as.numeric)
  
  list(raw = raw,
       header = header,
       data = ensemble)
  
}
