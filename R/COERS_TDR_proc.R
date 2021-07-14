#' @title SABS wharf long term temperature logging data transformation function
#'
#' @author Peter Kraska, \email{peter.kraska@dfo-mpo.gc.ca}
#'
#' @description function to transform Hobo temperature logger data that has been
#'   converted from proprietary binary format to a comma separated text file
#'   into a format that can be ingested in to the COERS.TDRLOG Oracle database
#'   tables on the DFO production database (PTRAN).
#'
#' @param x a comma delimited text file created from the Hobo temperature logger
#'   manufacturer software with the following columns `#`, `Date Time,
#'   GMT+00:00`, `Abs Pres, psi (LGR S/N: xxxx, SEN S/N: xxxx)`, `Temp, °C (LGR
#'   S/N: xxxx, SEN S/N: xxxx)`, `Water Level, meters (LGR S/N: xxxx)`
#'
#' @return returns a comma delimited text file in the format
#'   `[YEAR]-[MONTH]-[DAY]_SABS_warf_water_temperature_data.csv` for importing
#'   into COERS.TDRLOG
#'
#' @import readr
#' @import lubridate
#' @import dplyr
#'
#' @return
#' @export
#'
#' @examples tdr_proc('hobo_data_file.csv')
#' @importFrom rlang .data
#'

tdr_proc <- function(x) {
  raw <-
    readr::read_csv(x, skip = 1)
  
  raw %>%
    dplyr::rename_all( ~ c("id", "datet", "press", "temp", "depth")) %>%
    dplyr::mutate(
      datet = lubridate::mdy_hm(datet),
      SDATE = paste0(
        lubridate::day(datet),
        "-",
        lubridate::month(datet, label = TRUE, abbr = TRUE),
        "-",
        lubridate::year(datet),
        " ",
        stringr::str_pad(
          lubridate::hour(datet),
          width = 2,
          side = 'left',
          0
        ),
        ":",
        stringr::str_pad(lubridate::minute(datet), 2, 'left', pad = 0),
        ":",
        stringr::str_pad(lubridate::second(datet), 2, 'left', 0)
      ),
      YEAR = lubridate::year(datet),
      MONTH = lubridate::month(datet),
      DAY = lubridate::day(datet),
      HOUR = lubridate::hour(datet),
      MINUTE = lubridate::minute(datet),
      STATION = 1,
      TEMP = format(round(temp, 2), nsmall = 2),
      DEPTH = format(round(press, 4), nsmall = 4)
    ) %>%
    dplyr::select(STATION,
                  SDATE,
                  YEAR,
                  MONTH,
                  DAY,
                  HOUR,
                  MINUTE,
                  TEMP,
                  DEPTH) %>%
    readr::write_csv(
      paste0(
        "R:/Science/CESD/COERS/FPage/data/TDR/for_oracle_import/",
        last(.$YEAR),
        "-",
        stringr::str_pad(last(.$MONTH), 2, "left", "0"),
        "-",
        stringr::str_pad(last(.$DAY), 2, "left", "0"),
        "_SABS_warf_water_temperature_data.csv"
      )
    )
}
