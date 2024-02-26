# download shape files
temp = tempfile()
temp2 = tempfile()
download.file("https://www.bundeswahlleiterin.de/dam/jcr/8794eadd-fc0c-4889-a233-2ccae3b4cf5e/btw21_geometrie_wahlkreise_geo_shp.zip", temp) # WGS84
unzip(zipfile = temp, exdir = temp2)
wkshp = sf::read_sf(temp2) 
unlink(c("temp", "temp2"))
sf::sf_use_s2(use_s2 = FALSE) 
# sf::write_sf(wkshp, "yourpath/wahlkreise.shp")

# literal GER locations to lon-lat (longitude and latitude coordinates)
to_lonlat = \(address) {
  # first attempt from scratch. under development
  stopifnot(is.character(address), length(address) == 1L)
  address = gsub(pattern = " ",
                 replacement = "+",
                 x = gsub(pattern = "\\.|\\,|\\:|\\;|\\(|\\)|\\+|\\=",
                          replacement = "",
                          x = address))
  # see https://github.com/osm-search/Nominatim/issues/3134
  # API changed on August 03, 2023
  url = "https://nominatim.openstreetmap.org/search?q="
  urlenc = utils::URLencode(paste(url, address, "&format=json&limit=1"))
  y = rjson::fromJSON(file = urlenc)
  # TODO: reshape 
  if(length(y) == 0L) x = NA else {
    x = as.numeric(unlist(y[[1L]][c("lon", "lat")]))
    names(x) = c("longitude", "latitude") }
  x
}

# ~ Stadtteil, Stadt, Land (optional); Separatoren (wie Kommata) sind ebenso optional
home_towns = c("Borken, Deutschland", "Ganderkesee, Deutschland", "Mariendorf, Berlin, Deutschland")
X = as.data.frame(t(sapply(home_towns, to_lonlat)))
X$geometry = sf::st_as_sf(X, coords = c("longitude", "latitude"), crs = sf::st_crs(wkshp)) 
res = X$geometry |> sf::st_join(wkshp) # resolve/check warning: 
# although coordinates are longitude/latitude, st_intersects assumes that they are planar
ggplot2::ggplot() +
  ggplot2::geom_sf(data = wkshp) +
  ggplot2::geom_sf(data = wkshp[wkshp$WKR_NR %in% res$WKR_NR, ], fill = "red") +
  ggplot2::geom_sf(data = res, colour = "white") +
  ggplot2::geom_sf_text(data = wkshp[wkshp$WKR_NR %in% res$WKR_NR, ], ggplot2::aes(label = WKR_NR), size = 3)



