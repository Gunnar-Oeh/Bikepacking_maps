# Bikepacking maps
Creating daily multipanel maps, elevation diagramms and summaries in one plot for a bikepacking trip from .gpx tracks. 
The project uses the R-Ecosystem while relying heavily on the following packages: 
- [sf](https://cran.r-project.org/web/packages/sf/index.html) for handling spatial vector data
- [raster](https://cran.r-project.org/web/packages/raster/index.html) for handling spatial raster data
- [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html) to draw the beautiful multipanel graphics and its extensions for spatial data:
  - [ggspatial](https://cran.r-project.org/web/packages/ggspatial/index.html)
  - [ggmap](https://cran.r-project.org/web/packages/ggmap/) 

## Motivation
After a two months bikepacking trip from southern Germany to the atlantic coast of Portugal, I wanted to prepare a slideshow of photos ordered by date including a daily infographic which should display the following:
- A Map of the route travelled this day
- A small inlet, locating the route in western Europe
- a summary on accomodation and hosts
- The statistics distance travelled and elevation gain
- an elevation profile

An example generated from the scripts looks like this:
![Infographic](./plots/example_04_18_1.png)





