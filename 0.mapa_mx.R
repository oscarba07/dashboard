# Mapa de estados ---------------------------------------------------------
setwd('###')

carpeta_json <- paste0(getwd(),'/Mapa/json')

# Carga de librerias

paquetes <- c('plotly','magrittr','rjson','ggplot2','geojsonio','dplyr')
for (paq in paquetes) {
  if (!require(paq,character.only = T)) {
    install.packages(paq,dependencies = T)
    library(paq, character.only = T)
  }
}

# Leer datos creados
ent_json <- rjson::fromJSON(file=paste0(carpeta_json,"/ent.geojson"))
ent_json_b <- geojson_read(paste0(carpeta_json,"/ent.geojson"),  what = "sp")

df <- data.frame(region=as.character(formatC(1:32,width = 2,flag = "0")),
                 value=rnorm(32,sd=20))

#df_ent <- pea_estado %>% mutate(region=as.factor(as.character(region)))

for (i in 1:32) {
  ent_json$features[[i]]$properties$CVEGEO <- ent_json$features[[i]]$properties$CVEGEO %>% 
    as.numeric() %>% as.character()
}


# Intento de gráfica con plotly (no funcionando) --------------------------
g <- list(
  #scope = 'north america',
  #showland = T,
  #landcolor = toRGB("White"),
  #showocean = T,
  #oceancolor = toRGB("LightBlue"),
  #showlakes = F,
  #lakecolor = toRGB("Blue"),
  #showrivers = F,
  #rivercolor = toRGB("Blue"),
  #resolution = 50,
  visible = F,
  #showcoastlines=T,
  #showcountries=T,
  #projections = 'equirectangular',
  fitbounds = 'locations'
)

fig <- plot_ly(df)
fig <- fig %>% add_trace(
  type="choropleth",
  geojson=ent_json,
  locations=df$region,
  z=df$value,
  #colorscale="Viridis",
  featureidkey="properties.CVE_ENT",
  #zmin=-300,
  #zmax=300,
  marker=list(opacity=0.5),
  hovertext=df$region
  )

fig <- fig %>% layout(
  geo = g,
  title = "2022_1T PEA"
)

fig <- fig %>% colorbar(title = "PEA")

fig

# Gráfica simple de mapa con ggplot --------------------------------------------------
ggplot() +  
  geom_polygon(data=ent_json_b, aes(x=long, y=lat, group=group),
               fill="white", color="black")

# Grafica choropleth ggplot -----------------------------------------------
df$value <- round(df$value,2)
ent_json_b <- fortify(ent_json_b, region = 'CVEGEO') %>% 
  inner_join(.,df,by=c('id'='region'))
p <- ggplot(ent_json_b) +  
  geom_polygon(aes(x=long, y=lat, 
                   group=group,      #El argumento group=group arma grupos tanto para para los polígonos como para `fill=`. 
                   fill=value,
                   text = id)
               ) +
  labs(title = "México: PEA 2021",
       fill = 'PEA 2021'
       ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
ggplotly(p)

