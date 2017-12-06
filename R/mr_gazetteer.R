
mr_gazetteer_feature_get <- function(mrid) {
  wmsinfo <- httr::content(httr::GET(paste0("http://www.marineregions.org/rest/getGazetteerWMSes.json/", mrid, "/")))
  if(length(wmsinfo) == 0) {
    print(paste(mrid, "No wms found", wmsinfo))
    return(list())
  }
  features <- list()
  for(wms in wmsinfo) {
    if(!grepl("wms[?]$", wms$url) && !grepl("gis[.]ngdc[.]noaa[.]gov", wms$url)) {
      print(paste(mrid, "Url is no WMS", wms$url))
    } else {
      if(grepl("gis[.]ngdc[.]noaa[.]gov", wms$url)) {
        wfs_url <- sub("/arcgis/services/", "/arcgis/rest/services/web_mercator/", wms$url)
        wfs_url <- sub("/MapServer/WmsServer[?]$", "/MapServer/3/query?f=geojson&where=", wfs_url)
        wfs_url <- paste0(wfs_url, wms$featureName,'%3D', wms$value)
      } else {
        wfs_url <- paste0(sub("wms[?]$", "wfs?", wms$url), "request=getfeature&version=1.1.0&service=wfs",
                          "&typename=", wms$namespace, ':', wms$featureType,
                          '&CQL_FILTER=', tolower(wms$featureName), "='", wms$value, "'",
                          "&outputFormat=application/json")
      }
      wfs_url <- URLencode(wfs_url)
      tryCatch({
        ft <- sf::read_sf(wfs_url)
        if(nrow(ft) == 0) {
          print(paste(mrid, "No feature found", wfs_url))
        } else if(is.na(sf::st_crs(ft)$epsg)) {
          print(paste(mrid, "CRS is NA", wfs_url))
          # sf::st_crs(ft) <- 4326
        } else if(sf::st_crs(ft)$epsg != 4326) {
          print(paste(mrid, "CRS is not 4326", wfs_url))
          ft <- sf::st_transform(ft, 4326)
          features[[length(features)+1]] <- ft
        } else if(st_bbox(ft)[1] < -180 || st_bbox(ft)[2] < -90 ||
                  st_bbox(ft)[3] > 180 || st_bbox(ft)[4] > 90) {
          print(paste(mrid, "Geom BBOX out of bounds", wfs_url))
        } else {
          features[[length(features)+1]] <- ft
        }
      },
      error = function(e) print(paste(mrid, "WFS call failed", wfs_url, e))
      )
    }
  }
  features <- do.call(rbind, features)
  features
}
