#' Title
#'
#' @param file
#' @param cf_title
#' @param cf_institute
#' @param cf_source
#' @param cf_history
#' @param cf_references
#' @param cf_comment
#' @param cf_author
#' @param cf_conventions
#' @param cf_calendar
#' @param creation_date
#'
#' @return
#' @export
#'
#' @examples
odf2nc_convert <-
  function(file,
           cf_title = "Maritime Ecosystem and Ocean Science Hobo temperature logger data",
           cf_institute = c("BIO", "SABS"),
           cf_source = "Hobo temperature logger data collected as part of Fisheries and Oceans Canada (DFO) science activities in the Maritime region of Canada",
           cf_history = "",
           cf_references = "",
           cf_comment = "",
           cf_author = Sys.getenv("USERNAME"),
           cf_conventions = "CF-1.8",
           cf_calendar = "standard",
           creation_date =  format(x = Sys.time(), tz = "GMT", format = "%FT%H:%M%SZ")) {
    odf_file <- readLines(file)
    odf_header_end <- grep(odf_file, pattern = "-- DATA --")
    
    odf_leading_spaces <- stringr::str_count(odf_file, pattern = "\\G\\s")
    
    odf_headers <- stringr::str_remove_all(string = odf_file[odf_leading_spaces == 1], pattern = "\\s+|,")
    
    #### CREATE LIST TO READ ODF DATA FOR NETCDF TO ####
    odf <- list()
    
    #### RAW DATA ####
    odf$raw <- list(raw_data = odf_file)
    
    ind_start <- numeric()
    ind_param <- character()
    
    for (i in unique(odf_headers)) {
      vars_index <- grep(odf_file, pattern = i)
      ind_start <- c(ind_start, vars_index + 1)
      ind_stop <- dplyr::lead(ind_start) - 2
      ind_param <- c(ind_param, rep(x = i, times = length(vars_index)))
    }
    
    odf_tbl <- tibble(ind_start, ind_stop, ind_param) %>%
      group_by(ind_param) %>%
      mutate(
        seq = seq_along(along.with = ind_param),
        n = n(),
        param_name = case_when(n > 1 ~ paste0(ind_param, "_", seq), .default = ind_param)
      )
    
    odf_tbl$ind_stop[nrow(odf_tbl)] <- odf_header_end - 1
    
    for (i in 1:nrow(odf_tbl)) {
      if (odf_tbl$n[i] == 1) {
        odf$metadata[[odf_tbl$param_name[i]]] <- odf_file[odf_tbl$ind_start[i]:odf_tbl$ind_stop[i]]
      } else {
        odf$metadata[[odf_tbl$ind_param[i]]][[odf_tbl$param_name[i]]] <- odf_file[odf_tbl$ind_start[i]:odf_tbl$ind_stop[i]]
      }
    }
    
    param_name <- c()
    param_unit <- c()
    
    for (i in 1:length(odf$metadata[["PARAMETER_HEADER"]])) {
      param_metadata <- odf$metadata[["PARAMETER_HEADER"]][[i]]
      
      param_values <- list()
      for (j in 1:length(param_metadata)) {
        key <- unlist(regmatches(
          x = param_metadata[j],
          m = gregexpr(
            pattern = "\\w{1,}(?==)",
            text = param_metadata[j],
            perl = TRUE
          )
        ))
        
        value <- regmatches(
          x = param_metadata[j],
          m = gregexpr(
            pattern = "(?<==).*$",
            text = param_metadata[j],
            perl = TRUE
          )
        ) %>%
          unlist() %>%
          stringr::str_remove(pattern = "'") %>%
          stringr::str_remove(pattern = "',") %>%
          stringr::str_remove(pattern = ",") %>%
          stringr::str_trim(side = 'both')
        
        if (suppressWarnings(!is.na(as.numeric(value)))) {
          value <- as.numeric(value)
        }
        
        param_values[key] <- value
        odf$metadata[["PARAMETER_HEADER"]][[i]] <- param_values
        
      }
      names(odf$metadata$PARAMETER_HEADER)[i] <- param_values[['NAME']]
    }
    
    #### DATA ####
    odf$data <- as_tibble(odf_file[odf_header_end + 1:length(odf_file)]) %>%
      separate(
        col = value,
        sep = "(\\s{2,})",
        into = c("to_delete", names(odf$metadata[["PARAMETER_HEADER"]])),
        convert = TRUE,
        remove = TRUE
      ) %>%
      select(-to_delete) %>%
      mutate(SYTM_01 = dmy_hms(SYTM_01, tz = "UTC"))
    
    odf
  }
