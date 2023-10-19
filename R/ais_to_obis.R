#' AIS Biofouling to OBIS Occurence Darwin Core (DwC)
#'
#' @param file CSV formatted file created using \code{ais_all_presence}
#'
#' @return
#' @export
#'
#' @examples aisIndex_to_OBIS_occurenceCore("file.csv")
#'
#' @import readr
#' @import worrms
#' @import dplyr
#'

aisIndex_to_OBIS_occurenceCore <- function(file) {
  unique_spp <- readr::read_csv(file) %>%
    dplyr::select(species_name) %>%
    dplyr::distinct()
  
  worms <-
    worrms::wm_records_taxamatch(name = unique_spp$species_name) %>%
    dplyr::select(scientificname, lsid)
  
  readr::read_csv(file) %>%
    dplyr::left_join(worms, by = c("species_name" = "scientificname")) %>%
    dplyr::mutate(
      occurrenceID = paste0(.$year, "-", .$stn_num, "-", .$species_name),
      eventDate = paste0(year, "-01-01")  ,
      occurrenceStatus = dplyr::case_when(
        cover_index > 0 ~ "present",
        cover_index == 0 ~ "absent",
        TRUE ~ NA_character_
      ),
      basisOfRecord = "HumanObservation"
    ) %>%
    dplyr::select(
      occurrenceID,
      eventDate,
      decimalLongitude = longitude,
      decimalLatitude = latitude,
      scientificName = species_name,
      scientificNameID = lsid,
      occurrenceStatus,
      locationID = stn_num,
      locationRemarks = stn_location,
      basisOfRecord
    )
}
