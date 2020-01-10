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
#' @import tidyverse
#' @import lubridate
#'
#' @export
#'
#' @examples tdr_proc('hobo_data_file.csv')
#' @importFrom rlang .data

tdr_proc <- function(x) {
  raw <-
    read_csv(x, skip = 1)

  raw %>%
    rename_all(~ c("id", "datet", "press", "temp", "depth")) %>%
    mutate(
      datet = mdy_hms(.data$datet),
      SDATE = paste0(
        day(.data$datet),
        "-",
        month(.data$datet, label = TRUE, abbr = TRUE),
        "-",
        format(.data$datet, "%y")
      ),
      YEAR = year(.data$datet),
      MONTH = month(.data$datet),
      DAY = day(.data$datet),
      HOUR = hour(.data$datet),
      MINUTE = minute(.data$datet),
      STATION = 1,
      TEMP = .data$temp,
      DEPTH = .data$press
    ) %>%
    select(
      .data$STATION,
      .data$SDATE,
      .data$YEAR,
      .data$MONTH,
      .data$DAY,
      .data$HOUR,
      .data$MINUTE,
      .data$TEMP,
      .data$DEPTH
    ) %>%
    write_csv(
      paste0(
        "R:/Science/CESD/COERS/FPage/data/TDR/for_oracle_import/",
        last(.data$YEAR),
        "-",
        str_pad(last(.data$MONTH), 2, "left", "0"),
        "-",
        str_pad(last(.data$DAY), 2, "left", "0"),
        "_SABS_warf_water_temperature_data.csv"
      )
    )
}
