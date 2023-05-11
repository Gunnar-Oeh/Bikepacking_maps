load ("./data/script.RData")

x = gpx_sf[12,]

bbx <- st_bbox (x)

pad <- 7
### xmin + 1/8
x_pad <- (bbx[3] - bbx[1])/pad
### ymin + 1/8 des Extent
y_pad <- (bbx[4] - bbx[2])/pad

x_lim <- c (bbx[1] - x_pad, bbx[3] + x_pad)
y_lim <- c (bbx[2] - y_pad, bbx[4] + y_pad)
  
stz_sf <- st_cast (x, "POINT")
n_xy <- nrow (stz_sf)
  
stz_sf <- stz_sf[c(1,n_xy),c(11,4,5)]
stz_sf$start_ziel <- c (stz_sf$start[1], stz_sf$ziel[2])
stz_sf <- stz_sf[,c(1,4)]

bbm <- c (x_lim[1], y_lim[1], x_lim[2], y_lim[2])
bbm2 <-bbm
names (bbm2) <- c ("left", "bottom", "right", "top")

### backgroundmap with ggmaps
test_map <- get_map(location = bbm2)

ggmap(test_map) +
  #annotation_scale(location = "br", width_hint = 0.5) +
  #annotation_north_arrow(location = "bl", which_north = "true", ) +
  #geom_sf (data = x, aes (geometry = geometry), 
           #colour = single_blue, inherit.aes = FALSE) +
  geom_sf_label(data = stz_sf, aes (geometry = geometry, label = start_ziel))

ggmap(test_map) +
  geom_sf_label (data = stz_sf, aes (label = start_ziel), size = 3) +
  geom_sf (data = x, aes (geometry = geometry), 
           colour = single_blue, inherit.aes = FALSE)



### Sieht wesentlich besser aus

ot <- osm.types()


  
  ggplot(data = x) +
  annotation_map_tile(type = "hotstyle", zoom = 11) + 
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", ) +
  geom_sf (aes (geometry = geometry), 
           colour = single_blue) +
  coord_sf (xlim = x_lim, ylim = y_lim) 
  
# 3 > 2 > 8 > 1 > 7 > 6 > 4 > 5
  
###ggmap <-  
  
p_el  