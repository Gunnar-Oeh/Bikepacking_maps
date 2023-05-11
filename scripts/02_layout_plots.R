##############################################################
##############################################################
### Draw Plots
##############################################################
##############################################################

### Set Working Directory 
setwd ("~/Dokumente/IT_Projects/Bikepacking_Maps/")

### load Packages
load("packages.RData")
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

### Load Data saved after running 01_data_wrangling_feature_engineering.R
load ("./data/script_4.RData")

### Set ggplot theme
theme_set (theme_bw())

### Set colors
single_blue <- "#0a1451"
single_red <- "#a40000"

### Changes to gpx_sf
gpx_sf$art_unterkunft <- as.factor (gpx_sf$art_unterkunft) ### Acommodation to factor
levels (gpx_sf$art_unterkunft) <- c ("Camping", "Wildcamping", "WarmShowers", "CouchSurfing", "Zimmer") # factorlevels
gpx_sf$distanz <- as.numeric (st_length (gpx_sf)) ### From units to numeric

#########################################################
### Enlarge the Bounding-Box of the Map 
### in ggplot by a relative amount of the original map
#########################################################

### Input: sf dataframe as basis for the bbox and pad as the relative amount the bbox shall enlarged
enl_bb <- function (df_sf, pad, which = c ("ggmap", "sf")) { 
  
  bbx <- st_bbox (df_sf)
  
  ### xmin + 1/8
  x_pad <- (bbx[3] - bbx[1])/pad
  ### ymin + 1/8 of the extent
  y_pad <- (bbx[4] - bbx[2])/pad
  
  if (which == "ggmap"){ 
    ### Create enlarged bbox
    x_lim <- c (bbx[1] - x_pad, bbx[3] + x_pad)
    y_lim <- c (bbx[2] - y_pad, bbx[4] + y_pad)
    
    bbm <- c (x_lim[1], y_lim[1], x_lim[2], y_lim[2])
    names (bbm) <- c ("left", "bottom", "right", "top") 
    return (bbm) } else {
      bbx[1] <- bbx[1] - x_pad
      bbx[2] <- bbx[2] - y_pad
      bbx[3] <- bbx[3] + x_pad
      bbx[4] <- bbx[4] + y_pad
      
      return (bbx)
    } }

### Function to prevent offset on larger scale
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

####################################################
### 1.Inset-Map - Where is the daily route located
####################################################

### Backgroundmap as empty polygon
europe <- ne_countries(continent = "europe", returnclass = "sf")

st_crs(europe) ### In EPSG:4326 like gpx sf
st_crs (gpx_sf)

### Crop Europe on enlarged bbox around route
bbm <- enl_bb(st_bbox(gpx_sf), 12, which = "sf")

background <- st_crop(europe, bbm)

### Background map including the whole route in black, where the route of the day is highlighted
inset_background <- ggplot () +
  geom_sf (data = background, aes (geometry = geometry), fill = "#98b2a1", alpha = 0.33) +
  geom_sf (data = gpx_sf, aes (geometry = geometry), colour = single_blue, size = 1) +
  theme_void()

### Create List with the the inset maps for each day
p_inset <- list ()
for (i in 1:nrow (gpx_sf)) {
  x <- gpx_sf[i, ]
  
  xpol <- st_as_sfc(st_bbox(x))
  
  point_sf <- st_cast (x, "POINT")
  point_sf <- point_sf[1,]
  
  point_df <- data.frame (st_coordinates(point_sf), ID = point_sf$ID)
  rm (point_sf)
  
  p_inset[[i]] <- inset_background + 
    geom_sf (data = xpol, aes (geometry = geometry), colour = single_red, alpha = 0.5) +
    geom_sf (data = x, aes (geometry = geometry), colour = single_red, size = 1.25) +
    geom_label_repel(data = point_df, aes (x = X, y = Y, label = ID))
  
  rm (x, point_df, i, xpol)}

#################################################
### 2. Zoomed in Map of the daily route
#################################################
p_map <-list () ### list of daily maps
for (i in 1:nrow (gpx_sf)) { # for each day
  
  x <- gpx_sf[i,]
  
  bbm <- enl_bb(x, 7, which = "ggmap") ### Apply enlarging of bounding box of daily route
  
  ### Download backgroundmap with ggmaps
  terrain_map <- get_map(location = bbm)
  terrain_map <- ggmap_bbox (terrain_map)
  
  ### Transform geodata to EPSG:3857 
  x <- st_transform(x, 3857)
  ### Cast to points, to display Names of start and stop towns
  stz_sf <- st_cast (x, "POINT")
  ### Number o points = Index of the last point (stop) 
  n_xy <- nrow (stz_sf)
  
  ### Use only geometry and location names
  stz_sf <- stz_sf[c(1,n_xy),c(11,4,5)]
  ### Single column forstart and stop town-names
  stz_sf$start_ziel <- c (stz_sf$start[1], stz_sf$ziel[2])
  ### restrict to coordinates and start_stop column
  stz_sf <- stz_sf[,c(1,4)]
  ### Cast to base non-spatial df to use with gglabel and ggrepel
  stz_sf <- data.frame (st_coordinates(stz_sf), start_ziel = stz_sf$start_ziel)
  
  ### Annotations (date, number of stage): 
  nam <- paste ("Etappe " , x$ID, " - ", format (x$date, "%d.%m."), sep = "")
  ### Subtitles (accomodationm)
  cap <- paste (round (x$distanz/1000, 0), " km - ", x$art_unterkunft, 
                if (x$art_unterkunft %in% c ("WarmShowers", "CouchSurfing")) {
                  paste (" bei ", x$name_gastgeberIn, sep = "")}, sep = "")
  
  ### Actual route of the day
  p_map[[i]] <- ggmap (terrain_map) +
    geom_label_repel (data = stz_sf, aes (x = X, y = Y, label = start_ziel))+
    geom_sf (data = x, aes (geometry = geometry), 
             colour = single_blue, inherit.aes = FALSE, size = 1.25) +
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "bl", which_north = "true") +
    labs (title = nam, caption = cap)
  
  rm (cap, nam, stz_sf, terrain_map, stz_sf, n_xy, x, bbm)
}

####################################
### 3. Display elevation profile
####################################

# Elevation profiles into a list using dlply
p_elev <- dlply(df_elevation, .(ID), function (x) {
  
  ### Set the limits of the elevation plot depending on the min and max of that day
  max_elev <- max (x$elevation, na.rm = TRUE)
  min_elev <- min (x$elevation, na.rm = TRUE)
  
  ### Set the limits of the x axis (travelled distance) in km
  ylims <- c ( if (min_elev >= 0 ) {floor (min_elev/100)*100}
               else {floor (min_elev/10)*10},  ceiling(max_elev/100) * 100)
  xlim <- max (x$dist_cycle)/1000
  
  ID <- unique (x$ID)
  
  ### Extract gained elevation data from gpx_sf
  IND <- which (gpx_sf$ID == ID)
  up <- round (gpx_sf$up[IND], 0)
  down <- round (gpx_sf$down[IND], 0)
  
  ### Summary string
  cap <- paste ("Auf: ", up, "hm; Ab: ", down, "hm", sep = "")
  
  cap_xy <-  c (  xlim - xlim/2 ,
                  ylims[2] - ylims[2]/ 12 )
  
  ### Create plot
  ggplot( data = x, aes (x = dist_cycle/1000)) +
    geom_ribbon(aes (ymin = ylims[1], 
                     ymax = rollmean (elevation, k = 9, fill = "extend")), 
                fill = single_blue, alpha = 0.75) +
    geom_line (aes (y = rollmean (elevation, k = 9, fill = "extend"))) +
    labs ( x = "Strecke [km]", y = "Höhe über NN [m]") +
    ylim(ylims) +
    annotate ("label", x = cap_xy[1], y = cap_xy[2], label = cap, alpha = 0.66)
} ) 

#############################################################
### Add inlet, route and elevation plot into a single layout
#############################################################

### Layout of the Plotgrid
lay <- rbind (c (NA, NA, 2, 2, 2 ), 
              c (NA, 1, 2, 2, 2), 
              c (NA, NA, 3, 3, 3))

lay <- rbind ( c (1, 1,  2, NA),  
               c (1, 1,  3, 3 ))

### Add into a layout together from the three lists
for (i in 1:length (p_map)) {
  
  ### Set background color
  p1 <- p_map[[i]] + theme (plot.background = element_rect(fill="grey90", color = NA))
  p2 <- p_inset[[i]] + theme (plot.background = element_rect(fill="grey90", color = NA))
  p3 <- p_elev[[i]] + theme (plot.background = element_rect(fill="grey90", color = NA))
  
  ### Date as filename
  if (gpx_sf$double_date[i]) { add <- 2} else { add <- 1 }
  
  file <- paste ("./plots/", format (gpx_sf$date[i], "%m_%d"), "_", add, ".png", sep = "")
  
  ### Save as a png in plots
  png (file = file, width = 2300, height = 1292, bg = "grey90",
       pointsize = 6, res = 150)
  grid.arrange (p1, p2, p3, layout_matrix = lay, widths = c (1, 1, 0.6, 0.6))
  dev.off() 
  
  rm (p1, p2, p3, add)}

rm (df_elevation, gpx_in_dem, background, europe, inset_background, i, gpx,
    p_elev, p_inset, p_map)
### Save Workspace
save.image ("./data/script_5.RData")