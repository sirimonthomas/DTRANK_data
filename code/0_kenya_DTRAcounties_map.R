library(pacman)
p_load(terra, sf, tidyverse, tidyterra, here, RColorBrewer)

ke.rain <- rast('C:/Users/s1512187/Downloads/ke_totann/ke_totann/ke_totann/w001001.adf')

plot(ke.rain,1, breaks = c(0,250,600,800,1000,1300,1600,1900,2200,2500,2800))

ke.adm <- st_read('C:/Users/s1512187/Downloads/ke_district_boundaries/ke_district_boundaries.shp')

plot(ke.adm, col=rgb(0, 0, 0, 0), add = T)

ke.adm.dtra <- ke.adm %>% filter(DISTNAME == 'SAMBURU'| DISTNAME == 'MARSABIT'|DISTNAME == 'TURKANA')

plot(ke.adm.dtra, col=rgb(0, 0, 0, 0), main = 'Kenya Mean Annual Rainfall (ml)', add = T)

dtra.map <- ggplot() +
  geom_spatraster(data = ke.rain, aes(fill = ke_totann), na.rm=T) +
  scale_fill_gradientn(name = 'Mean Rainfall (ml)', 
                    breaks = c(0,250,600,800,1000,1300,1600,1900,2200,2500,2800),
                    colours = brewer.pal(11,'RdYlGn')) +
  geom_sf(data = ke.adm)

ggsave(here('output','DTRA_counties_map.jpg'), dtra.map)

pal <- brewer.pal(11,'RdYlGn')
