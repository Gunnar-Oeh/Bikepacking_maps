#########################################
#########################################
### Fotos und Plots ordnen
#########################################
#########################################

### Name photos by date and attach a trailing counter in the name: %m_%d_xx.jpg
### Enables them to be displayed in a slideshow together with the plot for the same day

### load Packages
load("packages.RData")
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

### Load data from the run of previous script
load ("./data/script_5.RData")

### Function to add Trailing Zeros to numbers
trail_0 <- function (num) {
  num <- as.character (num)
  nn <- nchar (num)
  if (nn == 1) { num <- paste (0, num, sep = "")} else {
    return (num)
  }
}

#############################################################
### Photo - Management
#############################################################

### As photos were taken during the trip with a digicam, the photos
### are renamed to arrange them in order by date taken with the plots of the corresponding days
path_img <- "../../Bilder/fr_sp_por_2019/" # path to original photo folder

### List files within that directory
imgs <- list.files(path_img, 
                   full.names = TRUE)

### read the exif information of the photos to extract the date
df_imgs <- read_exif(imgs)
names (df_imgs)

### Only the date-tags
test <- df_imgs[,c(1,3, 6:8, 28:29, 41:43, 74:76)]

### Which do not have a meaningful date?
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

### Rename photos by date and copy into project subdirectory ./photos/
for (i in 1:nrow (img_dates)) {
  
  x <- img_dates[i,]
  from <- paste (path_img, x$FileName, sep = "")
  nx <- nchar (x$FileName)
  ending <- substr (x$FileName, nx-3, nx) 
  to <- paste (format (x$date, "%m_%d"), "_", x$run, ending, sep = "")
  to <-paste ("./photos/", to, sep = "")
  file.copy(from, to)
  
  rm (i, x, from,nx, to, ending)
}

### Solche Ohne Datumseintrag
IND <- is.na (df_imgs$DateTimeOriginal)
img_no_dates <- df_imgs[IND, c(1,3, 6:8, 28:29, 41:43, 74:76)]

img_no_dates$date <- NA

### Manually enter date through prompt for those without one
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

### Which dates previously without a date correspond to which alereday addes photos?
### add the trailing number to the new filename

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

### copy into photo folder
for (i in 1:nrow (img_no_dates)) {
  
  x <- img_no_dates[i,]
  from <- paste (path_img, x$FileName, sep = "")
  nx <- nchar (x$FileName)
  ending <- substr (x$FileName, nx-3, nx) 
  to <- paste (format (x$date, "%m_%d"), "_", x$run, ending, sep = "")
  to <-paste ("./photos/", to, sep = "")
  file.copy(from, to)
  
  rm (i, x, from,nx, to, ending)
}

### All photos now follow the same naming convention %m_%d_xx.jpg
### where xx starts at 01, while the plots are always named %m_%d_01.png
### So both can be copied into a single directory for displaying in a slide-show