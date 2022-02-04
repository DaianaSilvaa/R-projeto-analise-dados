# carregamento de pacotes

pacotes <- c("tidyverse","sf","tmap","rgdal","rgeos","adehabitatHR","knitr",
             "kableExtra")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Criar objeto a partir de uma base de dados

# Simple Feature

# Carregando base de dados

load('shoppings.RData')

# Observando as variaveis a base de dados

shoppings %>%
  kable() %>%
  kable_styling(bootstrap_options = 'striped',
                full_width = TRUE,
                font_size = 12)

# criando objeto do tipo SF a partir d dataframe

sf_shoppings <- st_as_sf(x = shoppings,
                         coords = c('longitude', 'latitude'),
                         crs = 4326)

# Observando a classe do objeto sf_shoppings

class(sf_shoppings)

sf_shoppings$geometry

# Plotando o de forma espacial

tm_shape(shp = sf_shoppings)+
  tm_dots(size = 1)

# Adicionando uma camada de um mapa leafleet

tmap_mode('view')

tm_shape(shp = sf_shoppings)+
  tm_dots(col = 'deepskyblue4',
          border.col = 'black',
          size = 0.2,
          alpha = 0.8)

# Combinar um objeto simple feature com um shapefile

# carregando o shapefile do minicipio de SP


shp_saopaulo <- readOGR('shapefile_municipio', 'municipio_sp')

# visualizacao grafico sp

tm_shape(shp = shp_saopaulo)+
  tm_borders()

# combinando shp_saopaulo com o sf_shoppings

tm_shape(shp = shp_saopaulo)+
  tm_borders(alpha = 0.5)+
  tm_shape(shp = sf_shoppings)+
  tm_dots(col = 'regiao',
          size = 0.2)

# isolar as coordenadas

coordenadas_shoppings <- cbind(shoppings$longitude,
                               shoppings$latitude)
coordenadas_shoppings

# Fumcao spatialpoints para criar objeto sp

sp_shoppings <- SpatialPoints(coords = coordenadas_shoppings,
                              proj4string = CRS("+proj=longlat"))

# Visualizando o resultado:
tmap_mode("plot")

tm_shape(shp = sp_shoppings) + 
  tm_dots(size = 1)

# Nosso atual objeto se orienta de forma geodésica.
shoppings_UTM <- spTransform(x = sp_shoppings,
                             CRSobj = CRS("+init=epsg:22523"))

# Visualizando o resultado:
tm_shape(shp = shoppings_UTM) + 
  tm_dots(size = 1)

# Agora sim, poderemos aplicar a função gBuffer():
buffer_shoppings <- gBuffer(spgeom = shoppings_UTM, 
                            width = 1500, 
                            byid = TRUE)

# Plotagem do objeto buffer_shoppings:
tm_shape(shp = buffer_shoppings) + 
  tm_borders()

tmap_mode("view")

tm_shape(shp = buffer_shoppings) + 
  tm_borders()

# Combinando os objetos shp_saopaulo, sf_shoppings e buffer_shoppings:
tm_shape(shp = shp_saopaulo) + 
  tm_borders(alpha = 0.5) +
  tm_shape(shp = sf_shoppings) + 
  tm_dots(col = "regiao", 
          size = 0.02) +
  tm_shape(buffer_shoppings) + 
  tm_borders(col = "black") 

# A técnica de buffer union combina aqueles outputs da técnica de buffering que,
# por ventura, se encontrem.
buffer_union <- gUnaryUnion(spgeom = buffer_shoppings)


tm_shape(shp = shp_saopaulo) + 
  tm_borders(alpha = 0.5) +
  tm_shape(shp = sf_shoppings) + 
  tm_dots(col = "regiao", 
          size = 0.02) +
  tm_shape(shp = buffer_union) + 
  tm_borders(col = "black") + 
  tm_fill(col = "gray",
          alpha = 0.5) 
