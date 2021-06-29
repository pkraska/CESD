#' Aquatic Invasive Species (AIS) annual biofouling abundance data amalgamation
#'
#' @param input character string to a directory holding only the final yearly
#'   summary data for the presence/absence data in CSV format
#' @param output character string indicating the folder to save the output CSV
#'   to. CSV file is called \code{AIS_biofouling_allyears_presence_absence.csv}
#'   and function will create a folder from \code{output}.
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import readr
#' @import tidyr
#' @import stringr
#'

ais_all_abundance <- function(input, output) {
  file_list <-
    list.files(path = input,
               pattern = "*.csv",
               full.names = TRUE)

  if (!dir.exists(output)) {
    dir.create(output)
  }


  all_data <- read_csv(file_list[1], col_types = cols()) %>%
    select(
      year = Year_Observed ,
      stn_num = StnNum,
      stn_location = StnLocation,
      province = Prov,
      latitude = Latitude,
      longitude = Longitude,
      everything()
    ) %>%
    pivot_longer(!1:6, names_to = "species_name", values_to = "index") %>%
    mutate(species_name = str_replace(species_name, pattern = "_", replacement = " "))

  message(paste0(all_data$year[1], " complete."))

  for (file in file_list[-1]) {
    yearly_data <- read_csv(file, col_types = cols()) %>%
      select(
        year = Year_Observed ,
        stn_num = StnNum,
        stn_location = StnLocation,
        province = Prov,
        latitude = Latitude,
        longitude = Longitude,
        everything()
      ) %>%
      pivot_longer(!1:6, names_to = "species_name", values_to = "index") %>%
      mutate(species_name = str_replace(species_name, pattern = "_", replacement = " "))

    message(paste0(yearly_data$year[1], " complete."))

    all_data <- all_data %>%
      bind_rows(yearly_data)
  }

  arrange(all_data, year, stn_num)

  write_csv(all_data,
            paste0(output, "AIS_biofouling_allyears_abundance.csv"), na = "")

  message(
    paste0(
      "Complete AIS biofouling data file to be sent to ODIS Data Shop for publication can be found here: ",
      output,
      "AIS_biofouling_allyears_presence_absence.csv"
    )
  )
}
