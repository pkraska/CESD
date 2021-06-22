# This is a script to convert the AIS index data to DwC compliant format for
# uploading to OBIS. This script is reliant on first running
# 01-AIS_annual_cover_index.R and creating a CSV file to be given as a path to
# the following function
# 
# require(readr)
# require(dplyr)
# require(worms)


aisIndex_to_OBIS_occurenceCore <- function(file) {
  unique_spp <- read_csv(file) %>%
    select(species_name) %>%
    distinct()

  worms <-
    wormsbymatchnames(taxon_names = unique_spp$species_name) %>%
    select(scientificname, lsid)
  
  read_csv(file) %>%
    left_join(worms, by = c("species_name" = "scientificname")) %>%
    mutate(
      occurrenceID = paste0(.$year, "-", .$stn_num, "-", .$species_name),
      eventDate = paste0(year, "-01-01")  ,
      occurrenceStatus = case_when(
        cover_index > 0 ~ "present",
        cover_index == 0 ~ "absent",
        TRUE ~ NA_character_
      ),
      basisOfRecord = "HumanObservation"
    ) %>%
    select(
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

aisIndex_to_OBIS_occurence_eMoF <- function(file) {
  
}
