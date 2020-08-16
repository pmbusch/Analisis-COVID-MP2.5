### Analisis-COVID-MP2.5
## Carga Datos Satelitales MP2.5
## Fuente: http://fizz.phys.dal.ca/~atmos/martin/?page_id=140
## PBH Agosto 2020

library(raster)
library(RColorBrewer)

file_url <- "Data/Data_Original/Satelital/%s"

file <- sprintf(file_url,
                "GlobalGWRnoGWRcwUni_PM25_GL_201601_201612-RH35-NoNegs.asc")
file <- sprintf(file_url,
                "GlobalGWRwUni_PM25_GL_201601_201612-RH35-NoNegs.asc")
file <- sprintf(file_url,
                "ACAG_PM25_V4GL03_201801_201812_0p05.nc")


# mp <- read_file(file)
mp <- raster(file)
mp
mp %>% class()
plot(mp)
crs(mp) <- "+init=EPSG:4326" 

# mapview(mp)

# Filter Chile extent
mp_chile <- crop(mp, extent(-76,-66, -54.85, -17.5))

# View on leaflet
mapview(mp_chile, legend=T, col.regions= brewer.pal(9, "YlOrRd"),
        # at = seq(0,50,5)
        maxpixels=3735000)


library(rgdal)

file <- "GlobalGWRwUni_PM25_GL_201601_201612-RH35_Median.kml"

import <- ogrListLayers(file)
attr(import, "driver")
attr(import, "nlayers")

mp <- readOGR(file, "GlobalGWRwUni_PM25_GL_201601_201612-RH35.png")

## EoF