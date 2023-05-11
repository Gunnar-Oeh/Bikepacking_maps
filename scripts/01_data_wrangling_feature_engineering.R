###########################################################
###########################################################
### Data Wrangling and Feature Engineering
### Create vector-Linestrings from .gpx tracks,
### get elevation for the points on the route
### Query Location Names from first and last vertexes of
### the Linestring
###########################################################
###########################################################

### Set Working Directory 
setwd ()

### load Packages
load("packages.RData")
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

### GPX data
### GPX Tracks were recorded during the trip daily and saved in the format %M_&D.gpx

### 1. List GPX files in data subdirectory
gpx <- list.files (path = "./data/gpx", pattern = ".gpx")
### Extract the dates from filenames
dates <- as.Date (substr (str_remove(gpx, pattern = "\\.gpx"), 1,5), format = "%m_%d")
### Identity dates on which multiple tracks were recorded, thus having a longer filename (%M_%D_xx.gpx)
double_date <- nchar (str_remove(gpx, pattern = "\\.gpx")) > 5

### Read in gpx.tracks into a list with read_GPX function
gpx_sf <- lapply (gpx, function (x) {
  
  file_gpx <- paste ("./data/gpx/", x, sep = "")
  
  return (read_GPX (file = file_gpx, layers = "tracks")) 
})

### Cast to sf dataframe
gpx_sf <- do.call ("rbind.data.frame", gpx_sf)

### Add date-feature
gpx_sf$date <- dates
gpx_sf$double_date <- double_date
rm (dates, double_date)

### Remeove unnessacary columns
IND <- which (!(names (gpx_sf) %in% c ("geometry", "date", "double_date")))
gpx_sf <- gpx_sf[, - IND]
rm (IND)

### Add features to be filled
gpx_sf$start <- NA # start
gpx_sf$ziel <- NA # stop
gpx_sf$art_unterkunft <- NA # type of accomodation
gpx_sf$name_gastgeberIn <- NA # name of hosts
gpx_sf$distanz <- NA # travelled distance per day

### Type of accomidation
### 1 = camping, 2 = wildcamping, 3 = WS (Warmshowers), 4 = CS (Couchsurfing), 5 = Bezahltes Bett (hostel)

start_ziel <- c ("start", "ziel")

### Use revgeo to query names of start and stop locations
for (i in 1:nrow (gpx_sf)) {
  
  coords <- st_coordinates(gpx_sf[i,]) # get coordinates
  
  nc <- nrow (coords) 
  poss <- c (1,nc)
  
  for (j in c(1:2)) { # 1:2 - start and stop
  
    Sys.sleep(1.1)
    
    pos <- poss[j]
    
    xy <- coords[pos,1:2] # coordinates in right format for rev_geocode
    
    location <- unlist (rev_geocode_OSM(x = xy[1], y = xy[2])) # query location
    
    IND <- which (names (location) %in% c ("city", "village", "town"))
    
    city <- location[IND] # Use city as location - name
    
    IND <- which (names (gpx_sf) == start_ziel[j])
    
    gpx_sf[i,IND] <- city 
    
    rm (pos, xy, location, IND, city, j)}
  
  rm (nc, poss, coords, i)
    
}
rm (start_ziel)

#################################################
### Calculate gained elevation during the day
#################################################

### Raster Tiles 90*90m from cgiarcsi were downloaded manually in EPSG:4326

### ID column in gpx_sf for unequivocal identication of daily locations and tips
gpx_sf$ID <- 1:nrow(gpx_sf)

rst <- list.files (path = "./data/dem", pattern = "tif", full.names = TRUE)
dems <- lapply (rst, raster)

### 1. Within which extent
dems_bbox <- lapply (dems, st_bbox)

### Boolean statements necessary to test, wether the bbox of a route is smaller than the bbox of the DEM-Tile
### Uses the x and y coordinates of the 4 points of a bbox
### > 1, > 2, < 3, < 4

st_bbox_in_bbox <- function (bb_sf, bb_r) {
  
  test <-c (bb_sf[1] > bb_r[1],
            bb_sf[2] > bb_r[2],
            bb_sf[3] < bb_r[3],
            bb_sf[4] < bb_r[4])
  
  return (sum (test) == 4)} 

st_point_in_bbox <- function (point, bb_r) {
  
  test <-c (point[1] > bb_r[1],
            point[2] > bb_r[2],
            point[1] < bb_r[3],
            point[2] < bb_r[4])
  
  return (sum (test) == 4)}

### Output: matrix: Rows - Routes, Columns - DEM-Tiles
gpx_in_dem <- matrix (ncol = length(dems_bbox), nrow = nrow (gpx_sf))

### Test wether the route is within one bounding-box of a DEM-Tile
for (i in 1:nrow(gpx_sf)) {
  
  bb_sf <- st_bbox(gpx_sf[i,])
  
  gpx_in_dem[i,] <- laply (dems_bbox, function (bb_r) {
    
    st_bbox_in_bbox(bb_sf, bb_r)})
rm (bb_sf, i)} 

save.image(file = "./data/script_1.RData")
load ("./data/script_1.RData")
### Output: Long DF with one row per Point within a route (in list)
out <- list ()

### For each route extract Elevation data
for (i in 1:nrow (gpx_sf)) {
  
  ### Info: In which DEM-Tile is a Route located?
  test <- gpx_in_dem[i,]
  
  ID <- gpx_sf$ID[i] ### ID of Route from gpx_sf$ID
  
  ### Cast Route (Linestring) in Points = 1 Point per row
  points_sf <- st_cast (gpx_sf[i,c(1,9)], to = "POINT")
  np <- nrow (points_sf)
  
  df <- data.frame (ID = rep (ID, np), elevation = rep (NA, np))
  
  ### If it's only one Tile where the whole route is located
  if (sum (test) == 1) { 
    
    ### Index of DEM-Tile
    IND <- which (test)
    ### extract the elevations for the entire route
    elevation <- extract (dems[[IND]], points_sf)
    df$elevation <- elevation
    
  } else { ### Where a route is not located within a single DEM-Tile
    ### Extract every point individually
    
    ### For every point of a route which is located in multiple DEM-Tiles
    for (j in 1:nrow (points_sf)) {
      
      ### Single Point
      point_sf <- points_sf[j,]
      
      ### numeric coordinates
      xy <- st_coordinates (point_sf)
      
      ### In which dem-tile is the single point located?
      test_single <- laply ( dems_bbox, 
                             function (bb_r) st_point_in_bbox(xy, bb_r) )
      
      IND <- which (test_single)
      
      ### Extract elevation for single points and write into df
      df$elevation[j] <- extract (dems[[IND]], point_sf)
      
      rm (point_sf, xy, test_single, IND, j)
      
    }
    
  }
  ### Write output into the list as a DF per day
  out[[i]] <- df
  rm (points_sf, test, ID, IND, i, df, np, elevation)
}

### To DataFrame
df_elevation <- do.call (rbind.data.frame, out)
rm (out)
save.image ("./data/script_2.RData")

####################################################
### Add distance travelled values to the elevation
####################################################

load ("./data/script_2.RData")

### Distance from gpx_sf -> transform to EPSG:3035
gpx_sf_3035 <- st_transform (gpx_sf, 3035)

### DCast Line to Points -> Calculate distances between points
out <- list()
for (i in 1:nrow (gpx_sf_3035)) {
  
  points_sf <- st_cast (gpx_sf_3035[i,c(1,9)], "POINT")
  
  dist_cycle <- set_units (rep (0, nrow (points_sf)), m)
  
  for (j in 1:nrow(points_sf)) {
    if (j > 1) { 
      
      dist_j <- st_distance(points_sf[j-1,], points_sf[j,])
      dist_cycle[j] <- dist_cycle[j-1] + dist_j
    rm (dist_j)  
    }
    rm (j)}
  out[[i]] <- dist_cycle
  rm (i, points_sf, dist_cycle)
} 

df_elevation$dist_cycle <- unlist (out)
rm (out)

###########################################################
### Höhenmeter berechnen - Mit cycle travel vergleichen
###########################################################

df_up_down <- ddply (df_elevation, "ID", function (x) {
  
  elev <- rollmean(x$elevation, k = 8, fill = "extend") # k is an important hyperparameter. Tuning it to 8 gives similar results to cycle.travel webpage
       
       ### Calculate for up- and downwards
       up <- 0
       down <- 0
       for (i in 2:length(elev)) {
         if (elev[i] > elev[i-1]) {
           up <- up + (elev[i] - elev[i-1])     
         } else { if (elev[i] < elev[i-1]) {
           down <- down + (elev[i-1] - elev[i])  
         }}
       }

       return (data.frame (up, down))}
)
save.image(file = "./data/script_3.RData")

gpx_sf <- merge (gpx_sf, df_up_down)
rm (gpx_sf_3035, df_up_down)

###################################################
### Host and Accomodation data
###################################################
load ("./data/script_3.RData")

### Enter Acommodation and host data via a prompt. Was previously handwritten in a notebook

df_lodging <- ddply (gpx_sf, "ID", lod_hos <- function (x) {
  
  for_lodging <- c (paste ( x$date, ": ", x$start, " - ", x$ziel, sep = ""), 
  
  "Unterkunft (1 = Camping, 2 = Wildc., 3 = WS, 4 = CS, 5 = Hostel etc.):" )
  
  print (for_lodging)
  lodging <- readline(prompt = "Unterkunft: ") # Prompt for accomodation
  
  if ( lodging %in% c (3,4)) { 
    
    for_hosting <- "Name GastgeberIn: " # prompt for host
    host <- as.character (readline(prompt = for_hosting)) } else { host <- NA
    host <- as.character (host)}
  
  return (data.frame (lodging, host))
})

### Clean the formats
str (df_lodging)
df_lodging$lodging <- as.character (df_lodging$lodging)
df_lodging$host <- as.character (df_lodging$host)

IND <- str_detect (df_lodging$lodging, "NA")
df_lodging$lodging[IND] <- NA

IND <- which (str_detect(df_lodging$lodging, pattern = "^\\d", negate = TRUE))
df_lodging$host[IND] <- df_lodging$lodging[IND]
df_lodging$lodging[IND] <- c(3,3,4,3)
rm (IND)

gpx_sf$art_unterkunft <- as.integer (df_lodging$lodging)
gpx_sf$name_gastgeberIn <- df_lodging$host


save.image(file = "./data/script_4.RData")
