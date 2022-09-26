
library(sf)
library(raster)
# Zona de Interes
Zona = st_read("shp/Manutata.geojson")
Zona_py <- st_transform(Zona ,
                        crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Viver = st_read("shp/Vivero.geojson")
Viver_py <- st_transform(Viver ,
                         crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Area_Cober = st_read("shp/Area_Cober.shp")
Area_Cober_py <- st_transform(Area_Cober ,
                         crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Cobertura_MDD = st_read("shp/Cobertura_MDD.geojson")
Cobertura_MDD_py <- st_transform(Cobertura_MDD,
                         crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Peru          <- getData('GADM', country='Peru', level=0) %>% st_as_sf()
Per           <- getData('GADM', country='Peru', level=3) %>% st_as_sf()
MDD           <- subset(Per, NAME_1  == "Madre de Dios")
Tambopata <- subset(Per, NAME_3 == "Las Piedras")

require(pacman)
pacman::p_load(tidyverse,sf,ggplot2, ggspatial,sp,osmdata,leaflet, ggmap )
available_tags("highway")

mad_map <- get_map(getbb("Madre de Dios"), maptype = "toner-background") # Localizamos madre de Dios
Via_Mal <- st_read ("SHP/Red_vial.geojson") # Caragmos un shp de puerto maldonado
Via_Maldonado  <- st_transform(Via_Mal ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
Rio_Pol <- st_read ("SHP/Rio_Poli.geojson") # Caragmos un shp de puerto maldonado
Rio_Poli  <- st_transform(Rio_Pol ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
Puer_Mal <- st_read ("SHP/Area_puerto.shp") # Caragmos un shp de puerto maldonado
Puer_Mal <- st_transform(Puer_Mal ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion

CP <- st_read ("SHP/CP.geojson") # Caragmos un shp de puerto maldonado
CetroPo  <- st_transform(CP ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
CetroPo_xy <- cbind(CetroPo, st_coordinates(st_centroid(CetroPo$geometry)))
# Extrayendo información de OSM
Puer_Maldonado <- opq(Puer_Mal) %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf() # Informacion de osm del area

Puer_Maldonado_rios <- opq(Puer_Mal)%>% add_osm_feature(key = "waterway", value = "river") %>%osmdata_sf()
Puer_Maldonado_secu <- opq(Puer_Mal)%>% add_osm_feature(key = "highway",
                                                        value = c("residential", "living_street",
                                                                  "unclassified",
                                                                  "service", "footway")) %>% osmdata_sf()
calles <- Puer_Maldonado$osm_lines
calles <- st_intersection(calles, Puer_Mal )
calles <- calles %>%
  mutate(maxspeed = as.numeric(maxspeed),
         lanes = ifelse(is.na(lanes), 1, as.numeric(lanes)))
#  Cargamos las Librerias ----------------------------------------------------------

library(ggnewscale) 
library(ggspatial)


SurA= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Peru , fill="gray", color="black")+
  geom_sf(data = MDD, fill="black", color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 10, hjust = 0, vjust = 1, 
           label = "a) Sur America",size = 3, family="serif", color = 
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1, 
           label = "Pacific ocean",size = 3, family="serif", color = 
             "black",  fontface="italic", angle=90)+
  annotate(geom = "text", x = -55, y = -50, hjust = 0, vjust = 1, 
           label = "Atlantic ocean",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -10, hjust = 0, vjust = 1, 
           label = "Peru",size = 3, family="serif", color = 
             "black",  fontface="italic")
SurA

col=c("#E6A759","#D6842E", "#CD6F31",
      "#DA875B", "#89D79E", "#949956","#74A6A5", "#E0E080", "#689962", 
      "#7EDDD5", "#82E07F","#ACC38B", "#88BF6D", "#759E80", "#A1D9BE", 
      "#6AB9A2", "#B9DB85" ,"#E1E3A1", "#7AB388", "#85E2C1", "#569986",
      "#B8BA71", "#999973", "#C7C595", "#A4DADA", "#92C487", "#8BAD71")

MDD_GG=ggplot()  +
  geom_sf(data=Cobertura_MDD_py, aes(fill = CobVeg2013),  size=0.05)+
  scale_fill_manual(values = col)+
  geom_sf(data = SurAmeric, fill=NA, color="black", size=0.01)+
  geom_sf(data = Peru , fill=NA, color="black", size=0.01)+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_sf(data = MDD, fill=NA, color="black", size=0.2)+
  geom_sf(data = Tambopata, fill=NA, color="black", size=0.5)+
  geom_sf(data = Zona_py, fill="black", color="black")+
  geom_sf(data = Puer_Mal, fill=NA, color="black", size=0.1)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  coord_sf(xlim = c(-72.40404, -68.65311), ylim = c(-13.7 ,-9.879849)) +
  annotate(geom = "text", x = -70.5, y = -10, hjust = 0, vjust = 1, 
           label = "b) Departamento de \nEstudio",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -71, y = -13.4, hjust = 0, vjust = 1, 
           label = "Madre de Dios",size = 4, family="serif", color = 
             "black",  fontface="italic")

MDD_GG

MDD_CLIM= ggplot()+
  geom_sf(data=Cobertura_MDD_py, aes(fill = CobVeg2013), linetype = 0.6)+
  scale_fill_manual(values = col)+
  geom_sf_label(data = MDD, aes(label=MDD$NAME_3), size=1.5)+
  geom_sf(data = SurAmeric, fill=NA, color="black", size=0.01)+
  geom_sf(data = Peru , fill=NA, color="black", size=0.01)+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_sf(data = MDD, fill=NA, color="black", size=0.5)+
  geom_sf(data = Tambopata, fill=NA, color="black", size=0.5)+
  geom_sf(data = Zona_py, fill="black", color="black")+
  geom_sf(data = Viver_py, fill="black", color="black")+
  coord_sf(xlim = c(-69.77802, -68.86679), ylim = c(-13.00461 ,-12.20361)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -70.5, y = -10, hjust = 0, vjust = 1, 
           label = "b) Departamento de \nEstudio",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -69.7, y = -12.9, hjust = 0, vjust = 1, 
           label = "Inambari",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -69.2, y = -12.9, hjust = 0, vjust = 1, 
           label = "Tambopata",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -69.2, y = -12.4, hjust = 0, vjust = 1, 
           label = "Las Piedras",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -69.8, y = -12.6, hjust = 0, vjust = 1, 
           label = "Laberinto",size = 3, family="serif", color = 
             "black",  fontface="italic")
MDD_CLIM


library(elevatr)
elev = get_elev_raster(Puer_Mal, z=12)

plot(elev)
Poligo_alt    <- crop(elev, Puer_Mal)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Puer_Mal)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")


library(ggnewscale) 
library(ggspatial)


DD= ggplot() +
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data=Cobertura_MDD_py, aes(fill = CobVeg2013), size=0.01)+
  scale_fill_manual(values = col, name="Cobertura vegetal")+
  geom_sf(data = calles,inherit.aes = FALSE,aes(color = maxspeed), size = 1, alpha = .4,
          show.legend = F) +
  scale_color_viridis_c() +
  geom_sf(data = filter(calles, str_detect(name, "Avenida")), color = "salmon") +
  geom_sf(data = Puer_Maldonado_secu$osm_lines,inherit.aes = FALSE,
          color = "gray", size = .5,alpha = .6) +
  geom_sf(data = Puer_Maldonado_rios$osm_lines,
          inherit.aes = FALSE, color = "blue",size = .5,alpha = .5) +
  #geom_point(data =Foco , aes(x = longitude, y = latitude),size=1.5, color="red", pch=21, fill="red")+
  geom_sf(data = Rio_Poli , color="blue", size=0.3, fill="#a2d2ff")+
  geom_sf(data =  CetroPo,size=1.5, color="black", pch=21, fill="black")+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  geom_sf_label(data = CetroPo_xy , aes(label = NOMBCP ), 
                family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
                size = 2, face = "bold",color = 'black',
                point.padding = unit(0.9, "lines"))+
  geom_sf(data = Zona_py, fill=NA, color="black")+
  geom_sf(data = Viver_py, fill=NA, color="black")+
  
  coord_sf(xlim = c(-69.20, -69.085), ylim = c(-12.605 ,-12.43)) +
  labs(color = '',  x = 'Longitud', y = 'Latitud')+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_classic()+
  theme(legend.position = c(0.2, 0.3),
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"), #ancho de cuadrados de referencia 
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  annotate(geom = "text", x = -69.21, y = -12.41, hjust = 0, vjust = 1, 
           label = "C) Zona de \nEstudio",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  ggspatial::annotation_north_arrow(
    location = "rl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="black"))+
  guides(fill = guide_legend(nrow = 30, ncol=1))

legend <- get_legend(DD)

D= DD + theme(legend.position = "none")
D

Alt_Vive= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data=Cobertura_MDD_py, aes(fill = CobVeg2013), size=0.01)+
  scale_fill_manual(values = col, name="Cobertura vegetal")+
  geom_sf(data = Viver_py, fill=NA, color="black")+
  
  coord_sf(xlim = c(-69.14034,-69.11236), ylim = c(-12.47958 ,-12.46336)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -69.130, y = -12.478, hjust = 0, vjust = 1, 
           label = "Bosque de terraza alta",size = 3, family="serif", color = 
             "black",  fontface="italic")


Alt_Manutata= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data=Cobertura_MDD_py, aes(fill = CobVeg2013), size=0.01)+
  scale_fill_manual(values = col, name="Cobertura vegetal")+
  geom_sf(data = Zona_py, fill=NA, color="black")+
  geom_sf(data = Rio_Poli , color="blue", size=0.3, fill="#a2d2ff")+
  coord_sf(xlim = c(-69.16802,-69.14937), ylim = c(-12.51762 ,-12.50666)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -69.158, y = -12.512, hjust = 0, vjust = 1, 
           label = "Bosque inundable \nde palmeras",size = 3, family="serif", color = 
             "black",  fontface="italic")



library(cowplot)
W =ggdraw() +
  coord_equal(xlim = c(0, 17), ylim = c(0, 25), expand = FALSE) +
  draw_plot(legend  , width =8,       height = 8, x = 13, y = 3)+
  
  draw_plot(D  , width = 19, height = 19,x = -3.8, y = 0)+
  
  draw_plot(MDD_CLIM, width = 7,       height = 7, x = 9.7, y = 18.7)+
  draw_plot(SurA, width = 6,       height = 6, x = -1.2, y = 19)+
  draw_plot(MDD_GG, width = 6,   height = 6, x = 3.7, y = 19)+
  
  
  #draw_plot(Manuta_Reli_gg , width = 4,       height = 5, x = 11.8, y = 3.4)+
  
  #draw_plot(legend, width = 5,       height = 5, x = 13, y = 7.5)+
  
  draw_plot(Alt_Manutata, width = 5,       height = 5, x = 11.8, y = 9.2)+
  draw_plot(Alt_Vive, width = 5,       height = 5, x = 11.8, y = 13.2)+
  
  
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))+
  annotate(geom = "text", x = 12, y = 18, hjust = 0, vjust = 1,
           label = "Fundo el Bosque",face="bold",
           size = 5,family="serif", color = "black", face = "italic")+
  annotate(geom = "text", x = 12, y = 14.1, hjust = 0, vjust = 1,
           label = "Cachuela margen Izq.",face="bold",
           size = 4,family="serif", color = "black", face = "italic")

ggsave(plot = W ,"Mapas/01_Mapa de Cobertura.png", units = "cm", width = 17,height = 25, dpi = 1200) 

































































































