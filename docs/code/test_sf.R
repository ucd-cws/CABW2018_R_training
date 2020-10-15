# Please Donate to support this workhop! Cal-SFS relies on donations to fund chapter activities (like this workshop and participating in CABW) 
# TO DONATE: Venmo @CalSFS-John-Olson or contact John Olson if you'd like to send a check (joolson "at" csumb.edu).

# day 2: 42-8 helpers+instructors
# day 1: 50-8

# sf Lesson ---------------------------------------------------------------

# Load Libraries ----------------------------------------------------------

library(tidyverse) # wrangling/plotting tools
library(viridis) # nice color palette
library(sf) # "simple features" spatial package for vector based data
library(mapview) # interactive web mapping
library(tmap) # static/interactive mapping
#library(USAboundaries) # data for USA boundaries


# Load Data ---------------------------------------------------------------

# from previous lesson, we saved in data:
load(file="data/csci_sites_match.rda")

summary(csci_sites_match)

names(csci_sites_match) # what is column 5?

# Make it Spatial with sf -------------------------------------------------

# The Components of a CRS
# The coordinate reference system is made up of several key components:
  
## Coordinate system: The X, Y grid upon which your data is overlayed and how you define where a point is located in space.
## Horizontal and vertical units: The units used to define the grid along the x, y (and z) axis.
## Datum: A modeled version of the shape of the Earth which defines the origin used to place the coordinate system in space. You will learn this further below.
## Projection Information: The mathematical equation used to flatten objects that are on a round surface (e.g. the Earth) so you can view them on a flat surface (e.g. your computer screens or a paper map).


# https://spatialreference.org

df_sf <- st_as_sf(csci_sites_match, coords = c("lon", "lat"), # lon or X first!
                  # could use numbers instead c(10, 9)
                  remove = FALSE, # we want to keep the original lat/lon cols
                  crs = 4326) # we want CRS WGS84
# look up CRS with:https://spatialreference.org
# look up 3310?


# change CRS using st_transform
df_sf_albers <- st_transform(df_sf, crs=3310)


# Get Spatial Data --------------------------------------------------------

states <- st_read("data/states_boundaries.shp", stringsAsFactors = FALSE, as_tibble=TRUE)

# same as this but this is more quiet:
states <- read_sf("data/states_boundaries.shp")

# geojson
counties <- st_read("data/ca_counties_boundaries.geojson",
                    stringsAsFactors = FALSE, 
                    as_tibble = TRUE)

plot(df_sf)

plot(df_sf$geometry, col = "orange")


# this is better
plot(df_sf$geometry, 
     pch=16, 
     # purple dots are CSCI > 0.75, yellow are <0.75
     col=ifelse(df_sf$CSCI>0.75, adjustcolor("purple4", alpha=0.7), "gold"), 
     cex=1.5, 
     xlab = "Longitude", ylab="Latitude")
# add a title
graphics::title("CSCI ( >0.75=purple, <0.75=yellow)")



# Wrangling and Tidying ---------------------------------------------------

eldor_co <- filter(counties, name=="El Dorado")


# we list the thing we want to crop first, then what we crop by second
eldor_pts <- st_intersection(df_sf, eldor_co)


# plot
plot(eldor_co$geometry)
plot(df_sf$geometry, add=T, bg="gray", pch=21) # all the points
plot(eldor_pts$geometry, add=T, bg ="purple", pch=21) # just the points we cropped


# transform the county to same CRS
eldor_co_albers <- st_transform(eldor_co, crs = st_crs(df_sf_albers))

# now buffer by 5 kilometers! (remember, units are in meters)
eldor_co_buff_5km <- st_buffer(eldor_co_albers, dist = 5000)

# plot
plot(eldor_co_buff_5km$geometry, col="skyblue", border="steelblue")
plot(eldor_co_albers$geometry, lty=2, add=T) #original


eldor_pts_5km <- st_intersection(df_sf_albers, eldor_co_buff_5km)




# MOVING TO MAPPING LESSON ------------------------------------------------
load("data/m2_3_out_eldorado_sf.rda")

library(tidyverse) # wrangling/plotting tools
library(sf) # newer "simple features" spatial package
library(mapview) # interactive web mapping
library(tmap) # static mapping

# this is state boundaries
states <- read_sf("data/states_boundaries.shp")

plot(states$geometry) # so need to filter

CA <- filter(states, name=="California")

plot(CA$geometry)


mapview(df_sf, layer="CSCI Sites")

# alternatively!!
library(tmap)
tmap_mode(mode = "view")

tmap::tm_shape(df_sf, name = "CSCI Sites") +
  tm_dots(col="slateblue")

# fancy it up
mapview(df_sf, 
        col.regions="salmon", 
        cex=3, 
        layer.name="CSCI Sites")


## ggplot
library(ggspatial)

nicemap<-
  ggplot() + # set up the framework
  
  # use GEOM_SF
  geom_sf(data = CA, color="gray", lwd=2) + # add the state outline using geom_sf
  
  # use GEOM_POINT: note, we could use geom_sf instead here too!
  geom_point(data=df_sf, aes(x=lon, y=lat), fill="orange", pch=21, alpha=0.7, size=2)+
  
  # scale bar & north arrow
  ggspatial::annotation_north_arrow(location="tr") +
  ggspatial::annotation_scale() +
  # formatting
  labs(x="Longitude (WGS84)", y="Latitude", title="Map of CSCI Sites") + 
  theme_bw() # change this to sans if it doesn't plot

nicemap


# interactive
mapview(eldor_pts, zcol="CSCI", layer="CSCI")

# add another layer by linking with "+"
mapview(eldor_co, layer="El Dorado County") +
  mapview(eldor_pts, zcol="CSCI", layer="CSCI")


## challenge:
ggplot() +
  geom_sf(data=eldor_co, col="gray", alpha=0.4, lwd=2) +
  geom_sf(data=eldor_pts_5km, pch=21, size=1.5, fill="gray70", alpha=0.4) +
  geom_sf(data=eldor_pts, aes(fill=CSCI), pch=21, size=5) +
  scale_fill_viridis_c("CSCI") +
  # scale bar & north arrow
  ggspatial::annotation_north_arrow(location="tr") +
  ggspatial::annotation_scale() +
  theme_bw()
