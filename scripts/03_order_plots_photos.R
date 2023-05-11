#########################################
#########################################
### Fotos und Plots ordnen
#########################################
#########################################

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

load ("./data/script_5.RData")

### Working Directory setzen
setwd ("/home/gunnar/Dokumente/Reise/")

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
