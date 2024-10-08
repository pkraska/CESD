#' Castaway CTD to netCDF conversion
#'
#' @param files A character vector of castaway CTD files in CSV format to
#'   process
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
#'
#' @return \code{castaway_convert()} creates netCDF files from castaway CTD CSV
#'   files
#' @export
#'
#' @examples castaway_convert("data/")
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import readr
#' @import ncdf4
#' @import lubridate
#'

castaway_convert <-
  function(files,
           destination = "R:/Science/CESD/CESD_DataManagement/data_out/castawayCTD",
           project_lead = "",
           project = "",
           cf_title = c(
             "Maritime Ecosystem and Ocean Science Castaway CTD data archive – Raw / Archives des données CTD des écosystèmes maritimes et des sciences océaniques – Brutes",
             "Maritime Ecosystem and Ocean Science Castaway CTD data archive – Processed / Archives des données CTD des écosystèmes maritimes et des sciences océaniques – Traitées"
           ),
           cf_institute = c("BIO", "SABS"),
           cf_source = "Castaway CTD data collected as part of Fisheries and Oceans Canada (DFO) science activities in the Maritime region of Canada",
           cf_history = "",
           cf_references = "https://www.xylem.com/en-ca/products--services/analytical-instruments-and-equipment/data-collection-mapping-profiling-survey-systems/ctds/castaway-ctd/ https://www.ysi.com/File%20Library/Documents/White%20Papers/castaway-ctd-principles-of-operation.pdf",
           cf_comment = "",
           cf_author = "Peter Kraska, CESD Divisional data Manager <Peter.Kraska@DFO-MPO.gc.ca>",
           cf_conventions = "CF-1.8",
           cf_calendar = "standard",
           creation_date =  format(x = Sys.time(), tz = "GMT", format = "%FT%H:%M%SZ")) {
    for (i in files) {
      # read data file
      data <- readLines(i) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(header = grepl("% ", value))
      
      
      cast_header <- data %>%
        dplyr::filter(header == TRUE) %>%
        dplyr::select(value) %>%
        tidyr::separate(
          value,
          into = c("key", "value"),
          sep = ",",
          fill = 'right'
        ) %>%
        dplyr::mutate(
          key = tolower(key),
          key = stringr::str_replace(
            string = key,
            pattern = "% ",
            replacement = ""
          ),
          key = stringr::str_trim(string = key, side = "both"),
          key = stringr::str_replace_all(
            string = key,
            pattern = " ",
            replacement = "_"
          ),
          key = stringr::str_remove_all(string = key,
                                        pattern = "\\("),
          key = stringr::str_replace_all(
            string = key,
            pattern = "\\)",
            replacement = ""
          ),
          value = stringr::str_trim(string = value, side = "both")
        ) %>%
        dplyr::filter(key != "")
      
      castaway_data_cols <- data %>%
        dplyr::slice(sum(data$header) + 1) %>%
        dplyr::select(value) %>%
        stringr::str_replace_all(pattern = " ", replacement = "_") %>%
        stringr::str_replace_all(pattern = "\\(", replacement = "") %>%
        stringr::str_replace_all(pattern = "\\)", replacement = "") %>%
        stringr::str_to_lower() %>%
        stringr::str_split(pattern = ",") %>%
        unlist()
      
      cast_data <- data %>%
        dplyr::filter(header == FALSE) %>%
        dplyr::select(value) %>%
        dplyr::slice(-1) %>%
        tidyr::separate(
          col = value,
          into = castaway_data_cols,
          sep = ",",
          convert = TRUE
        )
      if (cast_header$value[cast_header$key == 'cast_data'] == "Raw") {
        # Dimensions
        # Castaway data will be treated as a single dimension profile datatype
        
        dimLon <-
          ncdf4::ncdim_def(
            name = 'lon',
            units = 'degrees_east',
            longname = 'Longitude',
            vals = as.numeric(cast_header$value[cast_header$key == 'start_longitude'])
          )
        
        dimLat <-
          ncdf4::ncdim_def(
            name = 'lat',
            units = 'degrees_north',
            longname = 'Latitude',
            vals = as.numeric(cast_header$value[cast_header$key == 'start_latitude'])
          )
        
        dimTime <-
          ncdf4::ncdim_def(
            calendar = "standard",
            name = 'time',
            units = 'seconds since 1970-01-01',
            longname = 'Time',
            vals = as.numeric(
              lubridate::ymd_hms(cast_header$value[cast_header$key == 'cast_time_utc']) + cast_data$time_seconds
            )
          )
        
        # Variables
        varLongitude <- ncdf4::ncvar_def(
          name = 'lon',
          units = 'degrees_east',
          dim = list(dimLon, dimLat, dimTime),
          missval = NA,
          longname = "Longitude",
          prec = 'double'
        )
        
        varLatitude <- ncdf4::ncvar_def(
          name = 'lat',
          units = 'degrees_north',
          dim = list(dimLon, dimLat, dimTime),
          missval = NA,
          longname = "Latitude",
          prec = 'double'
        )
        
        varPressure <- ncdf4::ncvar_def(
          name = 'Pres_Z',
          units = 'dbar',
          dim = list(dimLon, dimLat, dimTime),
          missval = NA,
          longname = "Pressure (spatial coordinate) exerted by the water body by profiling pressure sensor and correction to read zero at sea level",
          prec = 'double'
        )
        
        varTemperature <- ncdf4::ncvar_def(
          name = 'WC_temp_CTD',
          units = 'degrees C',
          dim = list(dimLon, dimLat, dimTime),
          missval = NA,
          longname = 'Temperature of the water body by CTD or STD',
          prec = 'double'
        )
        
        varCNDCST01 <- ncdf4::ncvar_def(
          name = 'CNDCST01',
          units = 'uS/cm',
          dim = list(dimLon, dimLat, dimTime),
          missval = NA,
          longname = 'Conductivity (\u00B5S/cm)',
          prec = 'double'
        )
        
        castaway_nc <-
          ncdf4::nc_create(
            force_v4 = TRUE,
            paste0(destination, "/", cast_header$value[cast_header$key == 'file_name'], ".nc"),
            vars = list(varPressure,
                        varTemperature,
                        varCNDCST01)
          )
        
        # Include all header information in the netCDF file as attribute data
        for (i in cast_header$key) {
          ncdf4::ncatt_put(
            nc = castaway_nc,
            varid = 0,
            attname = i,
            attval = cast_header$value[cast_header$key == i]
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
          varid = varPressure,
          vals = cast_data$pressure_decibar,
          count = c(1, 1, -1)
        )
        
        ncdf4::ncvar_put(
          castaway_nc,
          varid = varTemperature,
          vals = cast_data$temperature_celsius,
          count = c(1, 1, -1)
        )
        
        ncdf4::ncvar_put(
          castaway_nc,
          varid = varCNDCST01,
          vals = cast_data$conductivity_microsiemens_per_centimeter,
          count = c(1, 1, -1)
        )
        
        # close the nc file to ensure no data is lost
        ncdf4::nc_close(castaway_nc)
        
        message(
          paste0(
            "netCDF file ",
            cast_header$value[cast_header$key == "file_name"],
            ".nc created in folder ",
            destination,
            "."
          )
        )
      }
      else {
        if (cast_header$value[cast_header$key == 'cast_data'] == "Processed") {
          # Dimensions
          # Castaway data will be treated as a single dimension profile datatype
          
          dimLon <-
            ncdf4::ncdim_def(
              name = 'lon',
              units = 'degrees_east',
              longname = 'Longitude',
              vals = as.numeric(cast_header$value[cast_header$key == 'start_longitude'])
            )
          
          dimLat <-
            ncdf4::ncdim_def(
              name = 'lat',
              units = 'degrees_north',
              longname = 'Latitude',
              vals = as.numeric(cast_header$value[cast_header$key == 'start_latitude'])
            )
          
          dimDepth <-
            ncdf4::ncdim_def(
              name = 'depth',
              units = 'm',
              longname = 'Depth in meters',
              vals = cast_data$depth_meter
            )
          
          dimTime <-
            ncdf4::ncdim_def(
              calendar = "standard",
              name = 'time',
              units = 'seconds since 1970-01-01',
              longname = 'Time',
              vals = as.numeric(lubridate::ymd_hms(cast_header$value[cast_header$key == 'cast_time_utc']))
            )
          
          # Variables
          varLongitude <- ncdf4::ncvar_def(
            name = 'lon',
            units = 'degrees_east',
            dim = list(dimLon, dimLat, dimDepth, dimTime),
            missval = NA,
            longname = "Longitude",
            prec = 'double'
          )
          
          varLatitude <- ncdf4::ncvar_def(
            name = 'lat',
            units = 'degrees_north',
            dim = list(dimLon, dimLat, dimDepth, dimTime),
            missval = NA,
            longname = "Latitude",
            prec = 'double'
          )
          
          varPressure <- ncdf4::ncvar_def(
            name = 'Pres_Z',
            units = 'dbar',
            dim = list(dimLon, dimLat, dimDepth, dimTime),
            missval = NA,
            longname = "Pressure (spatial coordinate) exerted by the water body by profiling pressure sensor and correction to read zero at sea level",
            prec = 'double'
          )
          
          
          varTemperature <- ncdf4::ncvar_def(
            name = 'WC_temp_CTD',
            units = 'degrees C',
            dim = list(dimLon, dimLat, dimDepth, dimTime),
            missval = NA,
            longname = 'Temperature of the water body by CTD or STD',
            prec = 'double'
          )
          
          varCNDCST01 <- ncdf4::ncvar_def(
            name = 'CNDCST01',
            units = 'uS/cm',
            dim = list(dimLon, dimLat, dimDepth, dimTime),
            missval = NA,
            longname = 'Conductivity (\u00B5S/cm)',
            prec = 'double'
          )
          
          varConductance <- ncdf4::ncvar_def(
            name = 'specific_conductance',
            units = 'uS/cm',
            dim = list(dimLon, dimLat, dimDepth, dimTime),
            missval = NA,
            longname = 'Specific Conductance (\u00B5S/cm)',
            prec = 'double'
          )
          
          varSalinity <- ncdf4::ncvar_def(
            name = 'PSALST01',
            units = '1',
            dim = list(dimLon, dimLat, dimDepth, dimTime),
            missval = NA,
            longname = 'Salinity (PSS-78)',
            prec = 'double'
          )
          
          varSound <- ncdf4::ncvar_def(
            name = 'SVELCA01',
            units = 'm/s',
            dim = list(dimLon, dimLat, dimDepth, dimTime),
            missval = NA,
            longname = 'Calculated sound velocity in seawater (m/s)',
            prec = 'double'
          )
          
          varDensity <- ncdf4::ncvar_def(
            name = 'density',
            units = 'kg/m^3',
            dim = list(dimLon, dimLat, dimDepth, dimTime),
            missval = NA,
            longname = 'Calculated Seawater Density (kg/m^3)',
            prec = 'double'
          )
          
          castaway_nc <-
            ncdf4::nc_create(
              force_v4 = TRUE,
              paste0(destination, "/", cast_header$value[cast_header$key == 'file_name'], ".nc"),
              vars = list(
                varPressure,
                # varDepth,
                varTemperature,
                varCNDCST01,
                varConductance,
                varSalinity,
                varSound,
                varDensity
              )
            )
          
          # Include all header information in the netCDF file as attribute data
          for (i in cast_header$key) {
            ncdf4::ncatt_put(
              nc = castaway_nc,
              varid = 0,
              attname = i,
              attval = cast_header$value[cast_header$key == i]
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
            varid = varPressure,
            vals = cast_data$pressure_decibar,
            count = c(1, 1, -1, 1)
          )
          
          ncdf4::ncvar_put(
            castaway_nc,
            varid = varTemperature,
            vals = cast_data$temperature_celsius,
            count = c(1, 1, -1, 1)
          )
          
          ncdf4::ncvar_put(
            castaway_nc,
            varid = varCNDCST01,
            vals = cast_data$conductivity_microsiemens_per_centimeter,
            count = c(1, 1, -1, 1)
          )
          
          ncdf4::ncvar_put(
            castaway_nc,
            varid = varConductance,
            vals = cast_data$specific_conductance_microsiemens_per_centimeter,
            count = c(1, 1, -1, 1)
          )
          
          ncdf4::ncvar_put(
            castaway_nc,
            varid = varSalinity,
            vals = cast_data$salinity_practical_salinity_scale,
            count = c(1, 1, -1, 1)
          )
          
          ncdf4::ncvar_put(
            castaway_nc,
            varid = varSound,
            vals = cast_data$sound_velocity_meters_per_second,
            count = c(1, 1, -1, 1)
          )
          
          ncdf4::ncvar_put(
            castaway_nc,
            varid = varDensity,
            vals = cast_data$density_kilograms_per_cubic_meter,
            count = c(1, 1, -1, 1)
          )
          # close the nc file to ensure no data is lost
          ncdf4::nc_close(castaway_nc)
          message(
            paste0(
              "netCDF file ",
              cast_header$value[cast_header$key == "file_name"],
              ".nc created in folder ",
              destination,
              "."
            )
          )
        }
      }
    }
  }
