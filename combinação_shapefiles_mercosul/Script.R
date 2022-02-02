# Carregamento de pacote

pacotes <- c("rgdal","raster","tmap","maptools","tidyverse","broom","knitr",
             "kableExtra","RColorBrewer")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Mercosul membro pleno e membro suspenso

# Combinacao de shapefiles 
# Carregar os shapefiles

shp_argentina <- readOGR(dsn = 'shapefile_mercosul', layer = 'argentina_shapefile')
shp_brasil <- readOGR(dsn = 'shapefile_mercosul', layer = 'brasil_shapefile')
shp_paraguai <- readOGR(dsn = 'shapefile_mercosul', layer = 'paraguai_shapefile')
shp_venezuela <- readOGR(dsn = 'shapefile_mercosul', layer = 'venezuela_shapefile')


# A combinacao pode ser feita com a funcao bind()

shp_mercosul <- bind(shp_argentina,
                     shp_brasil,
                     shp_paraguai,
                     shp_venezuela)

# observando a base de dados shp_mercosul

shp_mercosul@data %>%
  kable()%>%
  kable_styling(bootstrap_options = 'striped',
                full_width = TRUE,
                font_size = 12)

# visualizacao dos shapefiles


tm_shape(shp = shp_mercosul)+
  tm_borders(lwd = 1)+
  tm_fill(col = 'mercosul')+
  tm_layout(legend.width = 0.8)

