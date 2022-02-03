
# Análise espacila do índice de desenvolvimento de são Paulo

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

# carrega o shapefile

shp_vul <- readOGR(dsn = "shapefiles",  layer = "35_BASE_MUNIC_INDIG_QUIL_2019")

# característica do objeto

summary(shp_vul)

# classe e tipo do objeto

class(shp_vul)
typeof(shp_vul)

# acessar a base de dados
# carregar a base de dados

shp_vul@data

shp_vul@data%>%
  kable()%>%
  kable_styling(bootstrap_options = "striped",
                full_width = TRUE,
                font_size = 12)

# Acesso as variáveis

shp_vul$NM_MUN
shp_vul$CD_MUN
shp_vul$E_POP_2019

# acessar outros componentes usa o operador @

shp_vul@proj4string
shp_vul@bbox

# plotagem básica

plot(shp_vul)

# carregar base de dados a respeito do munícipio de sp
load("dados_sp.RData")

# observações da base de dados carregada
dados_sp%>%
  kable()%>%
  kable_styling(bootstrap_options = "striped",
                full_width = TRUE,
                font_size = 12)

# Combinar as duas base de dados
# Com a função merge
shp_dados_geral <- merge(x = shp_vul,
                         y = dados_sp,
                         by.x = "CD_MUN",
                         by.y = "codigo")
shp_dados_geral@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# salvando o novo shapefiles
writeOGR(obj = shp_dados_geral, 
         layer = "novo_shapefile", 
         driver = "ESRI Shapefile", 
         dsn = "mbadsa")

# Plotagem dos dados
shp_dados_geral@data %>% 
  ggplot() +
  geom_histogram(aes(x = idh),
                 fill = "deepskyblue4",
                 color = "white") +
  labs(x = "IDH",
       y = "Frequência") +
  theme_bw()


# Plotagem espacial do dataset shp_dados_geral
shp_dados_geral_df <- tidy(shp_dados_geral, region = "CD_MUN")%>%
  rename(CD_MUN = id)%>%
  left_join(shp_dados_geral@data,
            by = "CD_MUN")

#plotagem
shp_dados_geral_df %>%
  ggplot()+
  geom_polygon(aes(x = long, y = lat, group = group, fill = idh),
               color = "black")+
  labs(x = " Longitude",
       y = "Latitude",
       fill = "IDH")+
  scale_fill_viridis_c()+
  theme_bw()

# utilizando o tmap

tm_shape(shp = shp_dados_geral) +
  tm_fill(col = "idh", palette = "Blues")

# cor mais amigavel 
tm_shape(shp = shp_dados_geral) + 
  tm_fill(col = "idh", 
          style = "quantile", 
          n = 4, 
          palette = "Spectral")

# Cor mais amigavel 
tm_shape(shp = shp_dados_geral) + 
  tm_fill(col = "idh", 
          style = "quantile", 
          n = 4, 
          palette = "viridis")

# adicionando um histograma

tm_shape(shp = shp_dados_geral)+
  tm_fill(col = 'idh',
          style = 'quantile',
          n = 4,
          palette = 'plasma',
          legend.hist = TRUE)

# Reposicionando o histogrma

tm_shape(shp = shp_dados_geral)+
  tm_fill(col = 'idh',
          style = 'quantile',
          n = 4,
          palette = 'inferno',
          legend.hist = TRUE)+
  tm_layout(legend.outside = TRUE)

# Posicionando manualmente o histograma e adicionando titulo

tm_shape(shp = shp_dados_geral)+
  tm_fill(col = 'idh',
          style = 'quantile',
          n = 4,
          palette = 'magma',
          legend.hist = TRUE)+
  tm_layout(legend.text.size = 0.7,
            legend.title.size = 0.9,
            legend.hist.size = 0.5,
            legend.hist.height = 0.2,
            legend.hist.width = 0.3,
            frame = FALSE,
            main.title = 'A distribuicao do IDH nos munucipios de SP')




