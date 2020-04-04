# Calculate planetary orbits around Sun
# John Godlee (johngodlee@gmail.com)
# 2020-03-31

# Preamble ----

# Remove old crap
rm(list=ls())
dev.off()

# Set working directory to the location of the source file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
library(ggplot2)
library(rgdal)

# Data from NASA Factsheet ----
##' https://nssdc.gsfc.nasa.gov/planetary/factsheet/

planet_name <- c("mercury", "venus", "earth", "mars", "jupiter", "saturn", "uranus", "neptune", "pluto")
perihelion <- c(46.0, 107.5, 147.1, 206.6, 740.5, 1352.6, 2741.3, 4444.5, 4436.8)*10^6
aphelion <- c(69.8, 108.9, 152.1, 249.2, 816.6, 1514.5, 3003.6, 4545.7, 7375.9)*10^6
eccentricity <- c(0.205, 0.007, 0.017, 0.094, 0.049, 0.057, 0.046, 0.011, 0.244)
semi_major_axis <- c(57.91, 108.21, 149.60, 227.92, 778.57, 1433.53, 2872.46, 4495.06, 5906.38)*10^6

planet_df <- data.frame(planet_name, perihelion, aphelion, eccentricity, semi_major_axis)

# Calculate other measurements ----

# Semi-minor axis
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

planet_df$semi_minor_axis <- apply(planet_df[,c("perihelion", "aphelion")], 1, gm_mean)

# Distance from ellipse centre to focus (Sun)
planet_df$centre_focus <- planet_df$eccentricity * planet_df$semi_major_axis

# Generate ellipse data ----

# Generate a numerical sequence
u <- seq(-pi,pi, length.out=80)

# Generate x and y values of ellipse
x_list <- list()
y_list <- list()
for(i in 1:nrow(planet_df)){
  x_list[[i]] <- planet_df$semi_major_axis[i] * cos(u) - planet_df$eccentricity[i]
  y_list[[i]] <- planet_df$semi_major_axis[i]  * sqrt(1-planet_df$eccentricity[i]^2) * sin(u)
}

# Add focus to centre offset to each orbit
x_offset_list <- list()
for(i in 1:length(x_list)){
  x_offset_list[[i]] <- x_list[[i]] + planet_df$centre_focus[i]
}

# Create dataframe
coords_df <- data.frame(x = unlist(x_offset_list), y = unlist(y_list),
  planet_name = factor(rep(planet_name, each = 80), levels = planet_df$planet_name), 
  row.names = NULL)

# Plot ellipses as lines ----
ggplot(coords_df, aes(x = x, y = y, colour = planet_name)) + 
  geom_path() + 
  coord_equal()

# Write data ----
write.csv(coords_df, "orbits.csv", row.names = FALSE)

coords_list <- split(coords_df, coords_df$planet_name)
coords_list <- lapply(coords_list, function(x){
  x["planet_name"] <- NULL
  x
  })

ps <- lapply(coords_list, Polygon)

p1 <- lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]), 
  ID = names(coords_list)[i]  ))

my_spatial_polys <- SpatialPolygons(p1) 

coords_spdf <- SpatialPolygonsDataFrame(my_spatial_polys, 
  data.frame(id = unique(coords_df$planet_name), 
    row.names = unique(coords_df$planet_name)))

writeOGR(coords_spdf, dsn = "orbit_app/orbits_shp", layer = "orbits", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# Test for scaling with Sun model ----

# Diameter of Sun (km)
sun_diam <- 1.39*10^6

# Diameter of Sun model (km)
sun_model_diam <- 5 / 10^5

# Scaling ratio
scale_ratio <-  sun_diam / sun_model_diam

# Scale x and y, convert to m
coords_scale_df <- data.frame(
  x = coords_df$x / scale_ratio * 1000, 
  y = coords_df$y / scale_ratio * 1000,
  planet_name = coords_df$planet_name)

# Plot ellipses as lines
ggplot(coords_scale_df, aes(x = x, y = y, colour = planet_name)) + 
  geom_path() + 
  coord_equal()

