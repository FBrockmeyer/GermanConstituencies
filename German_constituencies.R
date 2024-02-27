# download shape files
if(exists("hasRun") == FALSE) {
  temp = tempfile()
  temp2 = tempfile()
  download.file("https://www.bundeswahlleiterin.de/dam/jcr/8794eadd-fc0c-4889-a233-2ccae3b4cf5e/btw21_geometrie_wahlkreise_geo_shp.zip", temp) # WGS84
  unzip(zipfile = temp, exdir = temp2)
  wkshp = sf::read_sf(temp2) 
  unlink(c(temp, temp2))
  # sf::write_sf(wkshp, "/Users/USER/Documents/GitHub/GermanConstituencies/WKwgs84.shp") # adapt path 
  hasRun = TRUE 
}
# wkshp = sf::read_sf("/Users/USER/Documents/GitHub/GermanConstituencies/WKwgs84.shp")

# literal GER locations to lon-lat (longitude and latitude coordinates)
to_lonlat = \(address) {
  # second attempt from scratch. under development
  stopifnot(is.character(address), length(address) == 1L)
  address = gsub(pattern = " ",
                 replacement = "+",
                 x = gsub(pattern = "\\.|\\,|\\:|\\;|\\(|\\)|\\+|\\=",
                          replacement = "",
                          x = address))
  # API changed on August 03, 2023, see https://github.com/osm-search/Nominatim/issues/3134
  apienc = utils::URLencode(paste("https://nominatim.openstreetmap.org/search?q=", 
                                  address, "&format=json&limit=1"))
  y = rjson::fromJSON(file = apienc)
  if(length(y)) x = as.numeric(unlist(y[[1L]][c("lon", "lat")])) |> 
    setNames(c("longitude", "latitude")) else x = NA 
  return(x)
}

# ~ Stadtteil, Stadt, Land (optional); Separatoren (wie Kommata) sind ebenso optional
home_towns = c("Borken, Deutschland", "Ganderkesee, Deutschland", "Mariendorf, Berlin, Deutschland")
X = as.data.frame(t(sapply(home_towns, to_lonlat)))
X$geometry = sf::st_as_sf(X, coords = c("longitude", "latitude"), crs = sf::st_crs(wkshp)) 
sf::sf_use_s2(use_s2 = FALSE) 
res = X$geometry |> sf::st_join(wkshp) # resolve/check warning: 
## although coordinates are longitude/latitude, st_intersects assumes that they are planar
# writexl::write_xlsx(x = cbind("Eingabe" = home_towns, res), 
                    # path = "/Users/USER/Documents/GitHub/GermanConstituencies/Ausgabe.xlsx") # adapt 


# vis 
ggplot2::ggplot() +
  ggplot2::geom_sf(data = wkshp) +
  ggplot2::geom_sf(data = wkshp[wkshp$WKR_NR %in% res$WKR_NR, ], fill = "red") +
  ggplot2::geom_sf(data = res, colour = "white", shape = 4L) +
  ggplot2::geom_sf_text(data = wkshp[wkshp$WKR_NR %in% res$WKR_NR, ], ggplot2::aes(label = WKR_NR), size = 3L)


