###########################################################
###########################################################
### Karten der Reise
###########################################################
###########################################################

### Pakete laden
require (ggplot2)
require (ggrepel)
require (plyr)
require (reshape2)
require (sf)
require (rnaturalearth)
require (maptools)
require (tmaptools)
require (ggspatial)
require (stringr)
require (revgeo)
require (rgbif)
require (raster)
require (units)
require (zoo)
require (rosm)
require (ggmap)
require (gridExtra)
require (exifr)

### Working Directory setzen
setwd ("/home/gunnar/Dokumente/Reise/")

### GPX Daten einlesen
### 1. Welche sind vorhanden
gpx <- list.files (path = "./data/gpx", pattern = ".gpx")
### Daraus die Daten extrahieren
dates <- as.Date (substr (str_remove(gpx, pattern = "\\.gpx"), 1,5), format = "%m_%d")
### Double Date?
double_date <- nchar (str_remove(gpx, pattern = "\\.gpx")) > 5

### Nur tracks auslesen -> Enthält nur ein Feature
### In Liste laden
gpx_sf <- lapply (gpx, function (x) {
  
  file_gpx <- paste ("./data/gpx/", x, sep = "")
  
  return (read_GPX (file = file_gpx, layers = "tracks")) 
})

### In df
gpx_sf <- do.call ("rbind.data.frame", gpx_sf)

### Datumsspalte hinzufügen
gpx_sf$date <- dates
gpx_sf$double_date <- double_date
rm (dates, double_date)

### unnötige Spalten entfernen
IND <- which (!(names (gpx_sf) %in% c ("geometry", "date", "double_date")))
gpx_sf <- gpx_sf[, - IND]
rm (IND)

### Spalten Hinzufügen
gpx_sf$start <- NA
gpx_sf$ziel <- NA
gpx_sf$art_unterkunft <- NA
gpx_sf$name_gastgeberIn <- NA
gpx_sf$distanz <- NA

### Art Unterkunft
### 1 = camping, 2 = wildcamping, 3 = WS, 4 = CS, 5 = Bezahltes Bett

start_ziel <- c ("start", "ziel")

### Start und Zielorte mit revgeo abfragen
for (i in 1:nrow (gpx_sf)) {
  
  coords <- st_coordinates(gpx_sf[i,])
  
  nc <- nrow (coords)
  poss <- c (1,nc)
  
  for (j in c(1:2)) {
  
    Sys.sleep(1.1)
    
    pos <- poss[j]
    
    xy <- coords[pos,1:2]
    
    location <- unlist (rev_geocode_OSM(x = xy[1], y = xy[2]))
    
    IND <- which (names (location) %in% c ("city", "village", "town"))
    
    city <- location[IND]
    
    IND <- which (names (gpx_sf) == start_ziel[j])
    
    gpx_sf[i,IND] <- city 
    
    rm (pos, xy, location, IND, city, j)}
  
  rm (nc, poss, coords, i)
    
}
rm (start_ziel)
#################################################
### Höhenmeter ermitteln
#################################################

### Raster Tiles 90*90m von cgiarcsi in 4326 gedownloadet

### ID Spalte in gpx_sf für eindeutige Identifizierung der Routen
gpx_sf$ID <- 1:nrow(gpx_sf)

rst <- list.files (path = "./data/dem", pattern = "tif", full.names = TRUE)
dems <- lapply (rst, raster)

### 1. Within which extent
dems_bbox <- lapply (dems, st_bbox)

### Manuell: Werte der BBOX der Route kleiner/ größer als die der dems
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

### Ausgabe: matrix: Lines - Routen, Spalte - DEM
gpx_in_dem <- matrix (ncol = length(dems_bbox), nrow = nrow (gpx_sf))

### Testen ob die Route in nur einem raster-bbox verläuft
for (i in 1:nrow(gpx_sf)) {
  
  bb_sf <- st_bbox(gpx_sf[i,])
  
  gpx_in_dem[i,] <- laply (dems_bbox, function (bb_r) {
    
    st_bbox_in_bbox(bb_sf, bb_r)})
rm (bb_sf, i)} 

save.image(file = "./data/script_1.RData")
load ("./data/script_1.RData")
### Output: Langer Dataframe eine Reihe pro Punkt einer Route (in Liste)
out <- list ()

### Für jede Route Höhe Extrahieren
for (i in 1:nrow (gpx_sf)) {
  
  ### Angabe: In welchem Tile des DEM Rasters ist die Route verortet
  test <- gpx_in_dem[i,]
  
  ID <- gpx_sf$ID[i] ### ID der Route aus gpx_sf$ID
  
  ### Route (Linestring) in Points umwandeln = 1 Punkt pro Zeile
  points_sf <- st_cast (gpx_sf[i,c(1,9)], to = "POINT")
  np <- nrow (points_sf)
  
  df <- data.frame (ID = rep (ID, np), elevation = rep (NA, np))
  
  ### Wenn es nur ein Tile gibt, in dem die ganze Route liegt:
  if (sum (test) == 1) { 
    
    ### Index des Tiles
    IND <- which (test)
    ### extract die Höhenangaben für die ganze Route
    elevation <- extract (dems[[IND]], points_sf)
    df$elevation <- elevation
    
  } else { ### Dort wo die Route nicht eindeutig innerhalb eines Tiles ist
    ### Jeden Punkt einzeln extrahieren
    
    ### Für jeden Punkt der Route die in mehreren Tiles liegt
    for (j in 1:nrow (points_sf)) {
      
      ### Einzelner Punkt
      point_sf <- points_sf[j,]
      
      ### numerische Koordinaten
      xy <- st_coordinates (point_sf)
      
      ### In welchem Tile liegt der einzelne Punkt
      test_single <- laply ( dems_bbox, 
                             function (bb_r) st_point_in_bbox(xy, bb_r) )
      
      IND <- which (test_single)
      
      ### Höhe für einzelnen Punkt extrahieren
      df$elevation[j] <- extract (dems[[IND]], point_sf)
      
      rm (point_sf, xy, test_single, IND, j)
      
    }
    
  }
  ### BeIDe Varianten geben DF aus, in die Liste mit DF als Objekt pro Route schreiben
  out[[i]] <- df
  rm (points_sf, test, ID, IND, i, df, np, elevation)
}

#out <- lapply(out, function (x) {names (x) <- c ("ID", "elevation")
# return (x) })

lapply (out, head)

df_elevation <- do.call (rbind.data.frame, out)
rm (out)
save.image ("./data/script_2.RData")

####################################################
### Zu den Höhenpunkten Entfernungen hinzufügen
####################################################

load ("./data/script_2.RData")

### Entfernungen aus gpx_sf -> Umwandeln in EPSG:3035
gpx_sf_3035 <- st_transform (gpx_sf, 3035)

### Dazu: Linie in Punkte umwandeln -> Distanz zwischem vorletztem und letztem Punkt berechnen
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
  
  elev <- rollmean(x$elevation, k = 8, fill = "extend")
       
       ### Berechnen für up und down
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
### Bilder des Höhenprofils erstellen
###################################################
load ("./data/script_3.RData")

### 1. Unterkunft und Personen eintragen
### Mit Prompt
df_lodging <- ddply (gpx_sf, "ID", lod_hos <- function (x) {
  
  for_lodging <- c (paste ( x$date, ": ", x$start, " - ", x$ziel, sep = ""), 
  
  "Unterkunft (1 = Camping, 2 = Wildc., 3 = WS, 4 = CS, 5 = Hostel etc.):" )
  
  print (for_lodging)
  lodging <- readline(prompt = "Unterkunft: ")
  
  if ( lodging %in% c (3,4)) { 
    
    for_hosting <- "Name GastgeberIn: "
    host <- as.character (readline(prompt = for_hosting)) } else { host <- NA
    host <- as.character (host)}
  
  return (data.frame (lodging, host))
})

### Daten Überprüfen
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
### Passen

##############################################################
##############################################################
### Plots zeichnen
##############################################################
##############################################################
save.image(file = "./data/script_4.RData")
load ("./data/script_4.RData")

theme_set (theme_bw())

### Farben festlegen
single_blue <- "#0a1451"
single_red <- "#a40000"

### Anpassungen in gpx_sf
gpx_sf$art_unterkunft <- as.factor (gpx_sf$art_unterkunft)
levels (gpx_sf$art_unterkunft) <- c ("Camping", "Wildcamping", "WarmShowers", "CouchSurfing", "Zimmer")
gpx_sf$distanz <- as.numeric (st_length (gpx_sf))

### Funktion für eine um einen Teiler erweiterte bbox für get_map
enl_bb <- function (df_sf, pad, which = c ("ggmap", "sf")) { ### Einen sf dataframe als zu verarbeitendes Objekt und Teiler 
  ### welcher die Erweiterung der Bounding Box festlegt
  
  bbx <- st_bbox (df_sf)
  
  ### xmin + 1/8
  x_pad <- (bbx[3] - bbx[1])/pad
  ### ymin + 1/8 des Extent
  y_pad <- (bbx[4] - bbx[2])/pad
  
  if (which == "ggmap"){ 
    ### Erweiterte Numerische Bounding box erstellen
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

### Funktion um Versatz auf größerer Skalierung zu verhindern
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

#################################################
### 1.Insetkarte - Wo ist der Streckenabschnitt
#################################################

### Hintergrundkarte nur als leeres Polygon
europe <- ne_countries(continent = "europe", returnclass = "sf")

st_crs(europe) ### 4326 wie gpx sf
st_crs (gpx_sf)

### Europa auf vergrößerter bbox um die Strecke beschneiden
bbm <- enl_bb(st_bbox(gpx_sf), 12, which = "sf")

background <- st_crop(europe, bbm)

### Eine Karte der für jede Etappe in der noch die Tagesroute hervorgehoben wird
inset_background <- ggplot () +
  geom_sf (data = background, aes (geometry = geometry), fill = "#98b2a1", alpha = 0.33) +
  geom_sf (data = gpx_sf, aes (geometry = geometry), colour = single_blue, size = 1) +
  theme_void()

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

### Sieht gut aus!

#################################################
### 2. Karte der Strecke
#################################################
p_map <-list ()
for (i in 1:nrow (gpx_sf)) {
  
  x <- gpx_sf[i,]
  
  bbm <- enl_bb(x, 7, which = "ggmap")
  ### backgroundmap with ggmaps herunterladen
  terrain_map <- get_map(location = bbm)
  terrain_map <- ggmap_bbox (terrain_map)
  
  ### Umwandeln in 3857 
  x <- st_transform(x, 3857)
  ### In Punkte umwandeln um die Namen der STädte einblenden zu können
  stz_sf <- st_cast (x, "POINT")
  ### Wie viele Punkte auf der Route = Index des letzten Punktes (Zielort)
  n_xy <- nrow (stz_sf)
  
  ### Nur Geometrie und Ortspalten auswählen
  stz_sf <- stz_sf[c(1,n_xy),c(11,4,5)]
  ### Neue Spalte die Start und Ziel enthält
  stz_sf$start_ziel <- c (stz_sf$start[1], stz_sf$ziel[2])
  ### Beschränken aufKoordinaten und die Namensspalte
  stz_sf <- stz_sf[,c(1,4)]
  ### Als normalen DF für die geom_label_repel Funktion
  stz_sf <- data.frame (st_coordinates(stz_sf), start_ziel = stz_sf$start_ziel)
  
  ### Beschriftungen
  nam <- paste ("Etappe " , x$ID, " - ", format (x$date, "%d.%m."), sep = "")
  ### Untertitel
  cap <- paste (round (x$distanz/1000, 0), " km - ", x$art_unterkunft, 
                if (x$art_unterkunft %in% c ("WarmShowers", "CouchSurfing")) {
                  paste (" bei ", x$name_gastgeberIn, sep = "")}, sep = "")
  
  ### Eigentliche Route
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
### 3. Höhenprofil
####################################

p_elev <- dlply(df_elevation, .(ID), function (x) {
  
  ### Limits: 0, bzw.darunterliegender hunderterwert bis darüber liegender 
  ### Hunderterwert
  max_elev <- max (x$elevation, na.rm = TRUE)
  min_elev <- min (x$elevation, na.rm = TRUE)
  
  ylims <- c ( if (min_elev >= 0 ) {floor (min_elev/100)*100}
               else {floor (min_elev/10)*10},  ceiling(max_elev/100) * 100)
  xlim <- max (x$dist_cycle)/1000
  
  ID <- unique (x$ID)
  
  IND <- which (gpx_sf$ID == ID)
  up <- round (gpx_sf$up[IND], 0)
  down <- round (gpx_sf$down[IND], 0)
  
  cap <- paste ("Auf: ", up, "hm; Ab: ", down, "hm", sep = "")
  
  cap_xy <-  c (  xlim - xlim/2 ,
                ylims[2] - ylims[2]/ 12 )
  
  ggplot( data = x, aes (x = dist_cycle/1000)) +
    geom_ribbon(aes (ymin = ylims[1], 
                     ymax = rollmean (elevation, k = 9, fill = "extend")), 
                fill = single_blue, alpha = 0.75) +
    geom_line (aes (y = rollmean (elevation, k = 9, fill = "extend"))) +
    labs ( x = "Strecke [km]", y = "Höhe über NN [m]") +
    ylim(ylims) +
    annotate ("label", x = cap_xy[1], y = cap_xy[2], label = cap, alpha = 0.66)
} ) 

### läuft

####################################
### Zusammenfügen


### Layout des Plotgitters
lay <- rbind (c (NA, NA, 2, 2, 2 ), 
              c (NA, 1, 2, 2, 2), 
              c (NA, NA, 3, 3, 3))

lay <- rbind ( c (1, 1,  2, NA),  
        c (1, 1,  3, 3 ))

### Alle Verbinden
for (i in 1:length (p_map)) {
  
  ### Hintergrundfarbe
  p1 <- p_map[[i]] + theme (plot.background = element_rect(fill="grey90", color = NA))
  p2 <- p_inset[[i]] + theme (plot.background = element_rect(fill="grey90", color = NA))
  p3 <- p_elev[[i]] + theme (plot.background = element_rect(fill="grey90", color = NA))
  
  ### Datum als filename
  if (gpx_sf$double_date[i]) { add <- 2} else { add <- 1 }
  
  file <- paste ("./plots/", format (gpx_sf$date[i], "%m_%d"), "_", add, ".png", sep = "")
  
  ### Abspeichern
  png (file = file, width = 2300, height = 1292, bg = "grey90",
       pointsize = 6, res = 150)
  grid.arrange (p1, p2, p3, layout_matrix = lay, widths = c (1, 1, 0.6, 0.6))
  dev.off() 
  
  rm (p1, p2, p3, add)}

save.image ("./data/script_5.RData")

#########################################
#########################################
### Fotos und Plots ordnen
#########################################
#########################################

load ("./data/script_5.RData")

rm (df_elevation, gpx_in_dem, background, europe, inset_background, i, gpx,
    p_elev, p_inset, p_map)

### Trailing Zeros
trail_0 <- function (num) {
  num <- as.character (num)
  nn <- nchar (num)
  if (nn == 1) { num <- paste (0, num, sep = "")} else {
    return (num)
  }
}

### Pfad zu den Fotos
path_img <- "../../Bilder/fr_sp_por_2019/"

imgs <- list.files(path_img, 
                   full.names = TRUE)
df_imgs <- read_exif(imgs)
names (df_imgs)

### Hier nur die DateTags
test <- df_imgs[,c(1,3, 6:8, 28:29, 41:43, 74:76)]

### Welche haben kein sinnvoll nutzbares Date?
img_dates <- ! (is.na (test$DateTimeOriginal)) 
img_dates <- test[img_dates, ]

img_dates <- img_dates[order (img_dates$DateTimeOriginal),]
img_dates$date <- as.Date (substr (img_dates$DateTimeOriginal, 1, 10), format = "%Y:%m:%d")

img_dates <- ddply (img_dates, .(date), function (x) {
  nx <- nrow (x)
  x$run <- c(3:(nx+2))
  return (x)
} )

img_dates$run <- sapply (img_dates$run, trail_0)
### Umbenennen mit Datum und rüberkopieren
for (i in 1:nrow (img_dates)) {
  
  x <- img_dates[i,]
  from <- paste (path_img, x$FileName, sep = "")
  nx <- nchar (x$FileName)
  ending <- substr (x$FileName, nx-3, nx) 
  to <- paste (format (x$date, "%m_%d"), "_", x$run, ending, sep = "")
  to <-paste ("./Fotos/", to, sep = "")
  file.copy(from, to)
  
  rm (i, x, from,nx, to, ending)
}

### Solche Ohne Datumseintrag
IND <- is.na (df_imgs$DateTimeOriginal)
img_no_dates <- df_imgs[IND, c(1,3, 6:8, 28:29, 41:43, 74:76)]

img_no_dates$date <- NA

### Datum eintragen
for (i in 1:nrow (img_no_dates)) {
  
  x <- img_no_dates[i,]
  
  file_path <- paste (path_img, x$FileName, sep = "")
  img <- stack (file_path)
  plotRGB(img)
  
  img_no_dates$date[i] <- readline (prompt = "Datum des Bildes %m_%d: ")
  rm (x, img, i)
}

img_no_dates$date <- as.Date (str_remove_all(img_no_dates$date, '\\"'), "%m_%d")
img_no_dates$run <- NA
### Pro Linie - Welche entsprechen in img_dates dem Datum
### Verbleibende runzahlen hinzufügen
img_no_dates <- ddply (img_no_dates, .(date), function (x) {
  dd <- unique (x$date)
  
  if (dd %in% img_dates$date) { 
    
    IND <- img_dates$date == dd
    y <- img_dates[IND,]
  
    max_run <- max (y$run)
  
    x$run <- c((max_run+1) : (max_run + nrow (x))) } else {
      x$run <- c(4 : (3 + nrow (x))) 
    }
  
  
  return (x)

  })
img_no_dates$run <- sapply (img_no_dates$run, trail_0)

### Rüberkopieren
for (i in 1:nrow (img_no_dates)) {
  
  x <- img_no_dates[i,]
  from <- paste (path_img, x$FileName, sep = "")
  nx <- nchar (x$FileName)
  ending <- substr (x$FileName, nx-3, nx) 
  to <- paste (format (x$date, "%m_%d"), "_", x$run, ending, sep = "")
  to <-paste ("./Fotos/", to, sep = "")
  file.copy(from, to)
  
  rm (i, x, from,nx, to, ending)
}
