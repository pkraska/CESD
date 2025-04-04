#' Hobo Temperature Logger to netCDF conversion
#'
#' @param file A character string path to a Hobo temperatuer and pressure logger
#'   csv output file
#'
#' @param destination an existing folder to save netCDF files to
#' @param project_lead  an optional field to add the project lead's name to the
#'   file global attributes
#' @param cf_title  an optional field to add to the generated NetCDF file's
#'   global attributes (required for CF compliant NetCDF file creation). A
#'   succinct description of what is in the dataset.
#' @param cf_institute  an optional field to add to the generated NetCDF file's
#'   global attributes (required for CF compliant NetCDF file creation).
#'   Specifies where the original data was produced.
#' @param cf_source  an optional field to add to the generated NetCDF file's
#'   global attributes (required for CF compliant NetCDF file creation). The
#'   method of production of the original data. If it was model-generated,
#'   source should name the model and its version, as specifically as could be
#'   useful. If it is observational, source should characterize it (e.g.,
#'   "surface observation" or "radiosonde").
#' @param cf_history  an optional field to add to the generated NetCDF file's
#'   global attributes (required for CF compliant NetCDF file creation).
#'   Provides an audit trail for modifications to the original data.
#'   Well-behaved generic netCDF filters will automatically append their name
#'   and the parameters with which they were invoked to the global history
#'   attribute of an input netCDF file. We recommend that each line begin with a
#'   timestamp indicating the date and time of day that the program was
#'   executed.
#' @param cf_references  an optional field to add to the generated NetCDF file's
#'   global attributes (required for CF compliant NetCDF file creation).
#'   Published or web-based references that describe the data or methods used to
#'   produce it.
#' @param cf_comment  an optional field to add to the generated NetCDF file's
#'   global attributes (required for CF compliant NetCDF file creation).
#'   Miscellaneous information about the data or methods used to produce it.
#' @param cf_author  an optional field to add to the generated NetCDF file's
#'   global attributes (required for CF compliant NetCDF file creation)
#' @param creation_date creation date to append to the global attributes of the
#'   NetCDF file
#' @param project Character string indicating the funding source and or
#'   overarching project this data was collected as part of
#' @param cf_conventions an optional field to add to the generated NetCDF file's
#'   global attributes (required for CF compliant NetCDF file creation)
#' @param cf_calendar an optional field to add to the generated NetCDF file's
#'   global attributes (required for CF compliant NetCDF file creation).
#'   Specifies the calendar used for time dimensions.
#'   (http://cfconventions.org/Data/cf-conventions/cf-conventions-1.10/cf-conventions.html#calendar)
#' @param atm_corr_file (optional) if an atmospheric pressure correction Hobo
#'   temperature and pressure logger was deployed, list the character string
#'   path to the file here
#' @param latitude (required to compute depth using the TEOS-10 algorithm,
#'   https://www.teos-10.org/pubs/gsw/html/gsw_z_from_p.html) Latitude of the
#'   Hobo Temperature and Presseure Logger
#' @param longitude longitude in WGS84
#' @param deployment Character string, how the temperature was deployed (e.g. "affixed to floating dock, 1m below water level.")
#'
#' @return \code{hobo2nc_convert()} creates netCDF files from castaway CTD CSV
#'   files
#' @export
#'
#' @examples hobo2nc_convert("data/")
#'
#' @import dplyr
#' @import gsw
#' @import tidyr
#' @import stringr
#' @import readr
#' @import ncdf4
#' @import lubridate
#'

hobo2nc_convert <-
  function(file,
           atm_corr_file = NA,
           latitude,
           longitude,
           deployment,
           destination = "",
           project_lead = "",
           project = "",
           cf_title = "Maritime Ecosystem and Ocean Science Hobo temperature logger data",
           cf_institute = c("BIO", "SABS"),
           cf_source = "Hobo temperature logger data collected as part of Fisheries and Oceans Canada (DFO) science activities in the Maritime region of Canada",
           cf_history = "",
           cf_references = "",
           cf_comment = "",
           cf_author = "Peter Kraska, CESD Divisional data Manager <Peter.Kraska@DFO-MPO.gc.ca>",
           cf_conventions = "CF-1.8",
           cf_calendar = "standard",
           creation_date =  format(x = Sys.time(), tz = "GMT", format = "%FT%H:%M%SZ")) {
    hobo_data_water <- readr::read_csv(file, skip = 1, show_col_types = FALSE)
    
    hobo_colnames_water <- colnames(hobo_data_water)
    
    cleaned_colnames <- c("datetime", "abs_pressure_kpa", "temperature_c")
    
    hobo_header_gmt_offset_water <- substr(
      x = hobo_colnames_water[2],
      start = regexpr(pattern = "GMT", hobo_colnames_water[2]) + 3,
      stop = nchar(hobo_colnames_water[2])
    )
    
    hobo_header_sn_water <- substr(
      x = hobo_colnames_water[3],
      start = regexpr(pattern = "S/N:", hobo_colnames_water[3]) + 5,
      stop = regexpr(pattern = "S/N:", hobo_colnames_water[3]) + 12
    )
    
    good_hobo_data_water <- hobo_data_water %>%
      select(2:4)
    
    colnames(good_hobo_data_water) <- cleaned_colnames
    
    if (is.na(atm_corr_file)) {
      hobo_data <- good_hobo_data_water %>%
        mutate(
          datetime = paste0(datetime, " ", hobo_header_gmt_offset_water),
          datetime = lubridate::mdy_hms(x = datetime),
          gsw_z = -gsw::gsw_z_from_p((abs_pressure_kpa - 101.325) / 100, latitude = latitude)
        )
    } else {
      hobo_data_air <- readr::read_csv(atm_corr_file,
                                       skip = 1,
                                       show_col_types = FALSE)
      
      hobo_colnames_air <- colnames(hobo_data_air)
      
      hobo_header_gmt_offset_air <- substr(
        x = hobo_colnames_air[2],
        start = regexpr(pattern = "GMT", hobo_colnames_air[2]) + 3,
        stop = nchar(hobo_colnames_air[2])
      )
      
      hobo_header_sn_air <- substr(
        x = hobo_colnames_air[3],
        start = regexpr(pattern = "S/N:", hobo_colnames_air[3]) + 5,
        stop = regexpr(pattern = "S/N:", hobo_colnames_air[3]) + 12
      )
      
      good_hobo_data_air <- hobo_data_air %>%
        select(2:4)
      
      colnames(good_hobo_data_air) <- cleaned_colnames
      
      hobo_data <- good_hobo_data_water %>%
        left_join(good_hobo_data_air,
                  by = "datetime",
                  suffix = c("", "_air")) %>%
        mutate(
          datetime = paste0(datetime, " ", hobo_header_gmt_offset_water),
          datetime = lubridate::mdy_hms(x = datetime),
          gsw_z = -gsw::gsw_z_from_p((
            abs_pressure_kpa - abs_pressure_kpa_air
          ) / 100, latitude = latitude)
        )
    }
    
    
    
    # Dimensions
    ## Hobo data will be treated as a single dimension profile datatype
    
    dimLon <-
      ncdf4::ncdim_def(
        name = 'lon',
        units = 'degrees_east',
        longname = 'Longitude',
        vals = longitude
      )
    
    dimLat <-
      ncdf4::ncdim_def(
        name = 'lat',
        units = 'degrees_north',
        longname = 'Latitude',
        vals = latitude
      )
    
    dimTime <-
      ncdf4::ncdim_def(
        calendar = "standard",
        name = 'time',
        units = 'seconds since 1970-01-01',
        longname = 'Time',
        vals = as.numeric(hobo_data$datetime)
      )
    
    
    # Variables
    varPressure_uncorr <- ncdf4::ncvar_def(
      name = 'PRSTPR01',
      units = 'kPa',
      dim = list(dimLon, dimLat, dimTime),
      missval = NA,
      longname = "Pressure (measured variable) exerted by the water body plus atmosphere by semi-fixed in-situ pressure sensor",
      prec = 'double'
    )
    
    if (!is.na(atm_corr_file)) {
      varPressure_air <- ncdf4::ncvar_def(
        name = 'CAPBZZ01',
        units = 'kPa',
        dim = list(dimLon, dimLat, dimTime),
        missval = NA,
        longname = "Depth below surface of the water body by semi-fixed in-situ pressure sensor and correction to zero at sea level and conversion to depth using unspecified algorithm",
        prec = 'double'
      )
    }
    
    varDepth <- ncdf4::ncvar_def(
      name = 'PPSBPR01',
      units = 'm',
      dim = list(dimLon, dimLat, dimTime),
      missval = NA,
      longname = "Depth below surface of the water body by semi-fixed in-situ pressure sensor and correction to zero at sea level and conversion to depth using unspecified algorithm",
      prec = 'double'
    )
    
    varTemperature <- ncdf4::ncvar_def(
      name = 'TEMPPR01',
      units = 'degrees C',
      dim = list(dimLon, dimLat, dimTime),
      missval = NA,
      longname = 'The degree of hotness of the water column expressed against a standard scale. Includes both IPTS-68 and ITS-90 scales.',
      prec = 'double'
    )
    
    if (!is.na(atm_corr_file)) {
      varTemperature_air <- ncdf4::ncvar_def(
        name = 'air_temperature',
        units = 'degrees C',
        dim = list(dimLon, dimLat, dimTime),
        missval = NA,
        longname = 'Temperature of the air measured by the accompanying Hobo temperature pressure logger',
        prec = 'double'
      )
    }
    
    if (file.exists(paste0(
      destination,
      "/",
      tools::file_path_sans_ext(basename(file)),
      ".nc"
    ))) {
      nc <- ncdf4::nc_open(paste0(
        destination,
        "/",
        tools::file_path_sans_ext(basename(file)),
        ".nc"
      ))
      
      ncdf4::nc_close(nc)
      
      file.remove(paste0(
        destination,
        "/",
        tools::file_path_sans_ext(basename(file)),
        ".nc"
      ))
    }
    
    
    if (!is.na(atm_corr_file)) {
      castaway_nc <-
        ncdf4::nc_create(
          force_v4 = TRUE,
          paste0(
            destination,
            "/",
            cf_institute,
            "_",
            cf_author,
            "_",
            cf_title,
            "_",
            tools::file_path_sans_ext(basename(file)),
            ".nc"
          ),
          vars = list(
            varPressure_uncorr,
            varPressure_air,
            varDepth,
            varTemperature,
            varTemperature_air
          )
        )
    } else {
      castaway_nc <-
        ncdf4::nc_create(
          force_v4 = TRUE,
          paste0(
            destination,
            "/",
            cf_institute,
            "_",
            cf_author,
            "_",
            cf_title,
            "_",
            tools::file_path_sans_ext(basename(file)),
            ".nc"
          ),
          vars = list(varPressure_uncorr, varDepth, varTemperature)
        )
    }
    
    ncdf4::ncatt_put(
      nc = castaway_nc,
      varid = 0,
      attname = 'project_lead',
      attval = project_lead
    )
    
    ncdf4::ncatt_put(
      nc = castaway_nc,
      varid = 0,
      attname = 'cf_title',
      attval = cf_title
    )
    
    ncdf4::ncatt_put(
      nc = castaway_nc,
      varid = 0,
      attname = 'cf_institute',
      attval = cf_institute
    )
    
    ncdf4::ncatt_put(
      nc = castaway_nc,
      varid = 0,
      attname = 'cf_source',
      attval = cf_source
    )
    
    ncdf4::ncatt_put(
      nc = castaway_nc,
      varid = 0,
      attname = 'cf_history',
      attval = cf_history
    )
    
    ncdf4::ncatt_put(
      nc = castaway_nc,
      varid = 0,
      attname = 'cf_references',
      attval = cf_references
    )
    
    ncdf4::ncatt_put(
      nc = castaway_nc,
      varid = 0,
      attname = 'cf_comment',
      attval = cf_comment
    )
    
    ncdf4::ncatt_put(
      nc = castaway_nc,
      varid = 0,
      attname = 'cf_author',
      attval = cf_author
    )
    
    ncdf4::ncatt_put(
      nc = castaway_nc,
      varid = 0,
      attname = 'creation_date',
      attval = creation_date
    )
    
    ncdf4::ncatt_put(
      nc = castaway_nc,
      varid = 0,
      attname = 'Conventions',
      attval = cf_conventions
    )
    
    ncdf4::ncatt_put(
      nc = castaway_nc,
      varid = 0,
      attname = 'license',
      attval = "Open Government License - Canada (https://open.canada.ca/en/open-government-licence-canada)"
    )
    
    
    ncdf4::ncvar_put(
      castaway_nc,
      varid = varPressure_uncorr,
      vals = hobo_data$abs_pressure_kpa,
      count = c(1, 1, -1)
    )
    
    if (!is.na(atm_corr_file)) {
      ncdf4::ncvar_put(
        castaway_nc,
        varid = varPressure_air,
        vals = hobo_data$abs_pressure_kpa_air,
        count = c(1, 1, -1)
      )
    }
    
    ncdf4::ncvar_put(
      castaway_nc,
      varid = varTemperature,
      vals = hobo_data$temperature_c,
      count = c(1, 1, -1)
    )
    
    if (!is.na(atm_corr_file)) {
      ncdf4::ncvar_put(
        castaway_nc,
        varid = varTemperature_air,
        vals = hobo_data$temperature_c_air,
        count = c(1, 1, -1)
      )
    }
    
    ncdf4::ncvar_put(
      castaway_nc,
      varid = varDepth,
      vals = hobo_data$gsw_z,
      count = c(1, 1, -1)
    )
    
    
    # close the nc file to ensure no data is lost
    ncdf4::nc_close(castaway_nc)
    
    message(paste0(
      "netCDF file ",
      paste0(
        destination,
        "/",
        cf_institute,
        "_",
        cf_author,
        "_",
        cf_title,
        "_",
        tools::file_path_sans_ext(basename(file)),
        ".nc"
      ),
      " created in folder ",
      destination,
      "."
    ))
  }