#' SPOT Drifter tracking script using the SPOT API
#'
#' @param interval The interval to poll the SPOT website, in seconds. Default is
#'   120 seconds as that should not be flagged by the service provider.
#'
#' @param  password The SPOT portal login password
#'
#' @return returns a comma delimited text file named drifter_coords.csv to the
#'   "C:/DFO-MPO/" folder on DFO computers.
#' @export
#'
#' @examples drifter_tracking(interval = 120, password = 'something')
#' @import jsonlite
#' @import tidyverse
#' @import lubridate
#' @import later



drifter_tracking <- function(interval = 120, password) {
  files <- c("C:/DFO-MPO/test_coords.csv")
  for (i in 1:length(files)) {
    if (file.exists(files))
      file.remove(files)
  }

  spot_json <-
    fromJSON(
      paste0(
        "https://api.findmespot.com/spot-main-web/consumer/rest-api/2.0/public/feed/1zCGHaheJeh4l3Gqor3GOl4Yd2pxzpgTh/message.json?feedPassword=",
        password
      )
    )

  all_json <-
    tibble(
      messengerName = NA,
      messageType = NA,
      latitude = NA,
      longitude = NA,
      datetime = NA
    )

  tracking_json <-
    as_tibble(spot_json[[1]]$`feedMessageResponse`$messages$`message`) %>%
    select(messengerName, messageType, latitude, longitude, dateTime) %>%
    bind_rows(all_json, .) %>%
    distinct() %>%
    mutate(dateTime = ymd_hms(dateTime)) %>%
    group_by(messengerName) %>%
    filter(dateTime == max(dateTime)) %>%
    filter(messageType != "POWER-OFF")

  write_csv(tracking_json, "C:/DFO-MPO/drifter_coords.csv", col_names = TRUE)

  if (dim(tracking_json)[1] == 0) {
    print("Nothing to see here, move along...")
    stop()
  } else {
    writeLines(paste0(
      active_units$dateTime,
      " - Tracking units #",
      paste0(
        for (i in 1:dim(tracking_json[2])) {
          tracking_json$messengerName[i]
        }

        ,
        "\ntype `stop()` in the terminal to stop this function from running."
      )
    ))
  }

  # beep("ping")
  tracking_json$messengerName
  # active_units <- tracking_json %>%
  #   mutate(dateTime = ymd_hms(dateTime)) %>%
  #   group_by(messengerName) %>%
  #   filter(dateTime == max(dateTime)) %>%
  #   filter(messageType != "POWER-OFF") %>%
  #   write_csv("C:/DFO-MPO/drifter_coords.csv", col_names = TRUE)
  # print(Sys.time())
  # beep("ping")

  later(drifter_tracking, interval)
}
