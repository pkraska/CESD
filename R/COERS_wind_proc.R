#' SABS wharf long term wind speed and direction logging data transformation
#' function
#'
#' @param x comma delimited wind speed and direction file from the SABS wharf
#'   wind speed and direction logger
#' @param station `STATION` to be used for identification in the
#'   `COERS.WINDDATA` table on `PTRAN`. Default value is `1` (only value in the
#'   table).
#' @param gaugenum `GAUGENUM` probably wind speed and direction logger
#'   identification number, but only `999` present in the the `COERS.WINDDATA`
#'   table
#' @param latitude Latitude of wind speed and direction logger unit measurements
#'   in decimal degrees WGS84
#' @param longitude Longitude of wind speed and direction logger unit
#'   measurements in decimal degrees WGS84
#' @param area Character string for location of wind speed and direction logger
#'
#' @return Returns a comma delimited text file in
#'   `R:/Science/CESD/COERS/FPage/data/Wind logger/FOR ORACLE IMPORT/` for
#'   upload to PTRAN by COERS staff.
#'
#' @import readr
#' @import lubridate
#' @import dplyr
#' 
#' @return
#' @export
#'
#' @examples wind_proc(x)
#'

wind_proc <-
  function(x,
           station = 1,
           gaugenum = 999,
           latitude = 45.08275,
           longitude = 67.08508,
           area = "SABS Wharf") {
    readr::read_csv(
      x,
      skip = 2,
      col_names = c("date", "time", "key", "value", "unit", "x"),
      col_types = list(
        date = col_date(format = "%m/%d/%Y"),
        time = col_time(),
        key = col_factor(),
        value = col_double(),
        unit = col_factor(),
        x = col_factor()
      ),
      trim_ws = TRUE,
      skip_empty_rows = TRUE
    ) %>%
      tidyr::pivot_wider(names_from = c(key, unit), values_from = value) %>%
      dplyr::mutate(
        STATION = station,
        GAUGENUM = gaugenum,
        SDATE = paste0(
          lubridate::day(.$date),
          "-",
          lubridate::month(.$date, label = TRUE, abbr = TRUE),
          "-",
          format(.$date, "%y"), " ", lubridate::hour(.$time), ":", lubridate::minute(.$time), ":", lubridate::second(.$time)
        ),
        YEAR = lubridate::year(date),
        MONTH = lubridate::month(date),
        DAY = lubridate::day(date),
        HOUR = lubridate::hour(time),
        MINUTES = lubridate::minute(time),
        SPEED = trimws(format(round(`WndSpd_m/s`, 2), nsmall = 2)),
        DIRECTION = trimws(format(round(WndDir_Degrees, 2), nsmall = 2)),
        LATITUDE = format(round(latitude, 5), nsmall = 5),
        LONGITUDE = format(round(longitude, 5), nsmall = 5),
        AREA = area
      ) %>%
      select(-1:-5) %>%
      readr::write_csv(
        paste0(
          "R:/Science/CESD/COERS/FPage/data/wind/for_oracle_import/",
          dplyr::last(.$YEAR),
          "-",
          stringr::str_pad(dplyr::last(.$MONTH), 2, "left", "0"),
          "-",
          stringr::str_pad(dplyr::last(.$DAY), 2, "left", "0"),
          "_SABS_wharf_wind_data.csv"
        ),
        row.names = FALSE
      )
  }
