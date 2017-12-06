fetch_feature <- function(url) {
  ft <- sf::read_sf(url)
  if(nrow(ft) == 0) {
    print(paste(mrid, "No feature found", wfs_url))
    return(NULL)
  } else if(is.na(sf::st_crs(ft)$epsg)) {
    print(paste(mrid, "CRS is NA", wfs_url))
    # sf::st_crs(ft) <- 4326
    return(NULL)
  } else if(sf::st_crs(ft)$epsg != 4326) {
    print(paste(mrid, "CRS is not 4326", wfs_url))
    ft <- sf::st_transform(ft, 4326)
    return(ft)
  } else if(st_bbox(ft)[1] < -180 || st_bbox(ft)[2] < -90 ||
            st_bbox(ft)[3] > 180 || st_bbox(ft)[4] > 90) {
    print(paste(mrid, "Geom BBOX out of bounds", wfs_url))
    return(NULL)
  }  else {
    return(ft)
  }
}

mr_gazetteer_feature_get <- function(mrid) {
  wmsinfo <- httr::content(httr::GET(paste0("http://www.marineregions.org/rest/getGazetteerWMSes.json/", mrid, "/")))
  if(length(wmsinfo) == 0) {
    print(paste(mrid, "No wms found", wmsinfo))
    return(list())
  }
  features <- list()
  for(wms in wmsinfo) {
    if(grepl("wms[?]$", wms$url) || grepl("gis[.]ngdc[.]noaa[.]gov", wms$url)) {
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
      ft <- fetch_feature(wfs_url)
      if(!is.null(ft)) {
        features[[length(features)+1]] <- ft
      }
    } else if (grepl("geogratis[.]gc[.]ca/services/geoname/en/geonames", wms$url)) {
      kml_url <- paste0(wms$url, wms$value, '.kml')
      ft <- fetch_feature(kml_url)
      if(!is.null(ft)) {
        features[[length(features)+1]] <- ft
      }
    } else {
      print(paste(mrid, "Url is no WMS", wms$url))
    }
  }
  features <- do.call(rbind, features)
  features
}
