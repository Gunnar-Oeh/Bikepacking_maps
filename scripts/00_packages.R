#########################################################
#########################################################
### Package Management for the Bikepacking_Maps Project
#########################################################
#########################################################

### Inspired by: https://statsandr.com/blog/an-efficient-way-to-install-and-load-r-packages/

### Set Working Directory 
setwd ("~/Dokumente/IT_Projects/Bikepacking_Maps/")

### Required packages
packages <- c ('ggplot2', 'ggrepel', 'plyr', 'reshape2', 'sf', 'rnaturalearth', 'maptools', 'tmaptools', 'ggspatial', 'stringr', 'revgeo', 
           'rgbif', 'raster', 'units', 'zoo', 'rosm', 'ggmap', 'gridExtra', 'exifr')

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Save packages vector for loading in the next scripts
save(packages, file = "packages.RData")

