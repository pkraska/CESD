#' Convert Long Term Temperature Monitoring (LTTM) Ocean Data Format (ODF) to
#' NetCDF (.nc) formatted files
#'
#' @param input character vector path to ODF formatted Long Term Temperature
#'   Monitoring (LTTM) file
#' @param output character vector path to directory to save NetCDF file in
#'
#' @return
#' @export
#'
#' @import oce
#' @import ncdf4
#'
#' @examples lttm_convert_odf_to_nc("data/odf/file.odf")
#'

lttm_convert_odf_to_nc <- function(input, output) {
  # Create OCE object from ODF file
  message(paste0("processing file ", input))
  odf <- oce::read.odf(input,debug = 1)

  # create netCDF dimensions and variables
  dimLon <-
    ncdf4::ncdim_def(
      name = 'lon',
      units = 'degrees_east',
      longname = 'Longitude',
      vals = as.numeric(odf[['initialLongitude']])
    )

  dimLat <-
     ncdf4::ncdim_def(
      name = 'lat',
      units = 'degrees_north',
      longname = 'Latitude',
      vals = as.numeric(odf[['initialLatitude']])
    )

  dimDepth <-
     ncdf4::ncdim_def(
      name = 'depth',
      units = 'm',
      longname = 'meters below sealevel',
      vals = as.numeric(odf[['depthMax']])
    )

  dimTime <-
     ncdf4::ncdim_def(
      name = 'time',
      units = 'seconds since 1970-01-01',
      longname = 'Time',
      vals = as.numeric(odf[['time']])
    )

  varTemperature <- ncdf4::ncvar_def(
    name = 'WC_temp_CTD',
    units = 'degrees C',
    dim = list(dimLon, dimLat, dimDepth, dimTime),
    missval = NA,
    longname = 'Temperature of the water body by CTD or STD',
    prec = 'double'
  )


  odf_nc <-
    ncdf4::nc_create(
      paste0(output,
        odf[['cruiseNumber']],
        "_",
        format(odf[['startTime']], format = "%Y_%m_%d"),
        "_",
        odf[['model']],
        "_",
        odf[['serialNumber']] ,
        ".nc"
      ),
      vars = varTemperature
    )

  ncdf4::ncvar_put(
    odf_nc,
    varid = varTemperature,
    vals = odf[['temperature']],
    count = c(1, 1, 1,-1)
  )


  ncdf4::nc_close(odf_nc)
}
