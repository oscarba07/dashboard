### Este código descarga, limpia y analiza los datos de tasa de ocupación.

# Se crea la carpeta principal donde se almacenarán los resultados del código
carpeta_docup  <- paste0(getwd(),'/Desocupacion')
if (!file.exists(carpeta_docup)){
  dir.create(carpeta_docup)
}

# Se crea la carpeta de datos donde se almacenarán los datos
carpeta_datos  <- paste0(carpeta_docup, "/Datos")
if (!file.exists(carpeta_datos)){
  dir.create(carpeta_datos)
}


# Se crea la carpeta de visualización donde se almacenarán los gráficos
carpeta_visual  <- paste0(carpeta_docup, "/Visualizacion")
if (!file.exists(carpeta_visual)){
  dir.create(carpeta_visual)
}

# Se cargan los paquetes necesarios
{
  paquetes <- c("httr", "jsonlite","rjson","magrittr"
                ,"dplyr","tidyr", "plotly", "zoo")
  
  for(paq in paquetes){
    if(!require(paq, character.only = T)) install.packages(paq, dependencies = T)
  }
}

# Se define el token (Seguir indicaciones para obtenerlo aqui: 
# https://www.inegi.org.mx/servicios/api_indicadores.html
token <- '###'

# Se definen los indicadores con base en la construcción de indicadores de INEGI
ind <- seq(447567, 447567+31, 1)


# Se define la url con base en en la construcción de indicadores de INEGI
url_api <- 'https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/num/es/0700/false/BIE/2.0/[Aquí va tu Token]?type=json'

# Definiciones de vectores de apoyo
ef_l <- c('Aguascalientes', 'Baja California', 'Baja California Sur', 'Campeche', 
          'Coahuila', 'Colima', 'Chiapas', 'Chihuahua', 'Ciudad de México', 
          'Durango', 'Guanajuato', 'Guerrero', 'Hidalgo', 'Jalisco', 'México', 
          'Michoacán', 'Morelos', 'Nayarit', 'Nuevo León', 'Oaxaca', 'Puebla', 
          'Querétaro', 'Quintana Roo', 'San Luis Potosí', 'Sinaloa', 'Sonora', 
          'Tabasco',         'Tamaulipas', 'Tlaxcala', 'Veracruz', 'Yucatán', 
          'Zacatecas')

ef <- c('Ags', 'BC', 'BCS', 'Camp','Coah', 'Col', 'Chis', 'Chih', 'Cdmx', 
        'Dgo', 'Gto', 'Gro', 'Hgo', 'Jal', 'EdoMex','Mich', 'Mor', 'Nay', 'NL', 
        'Oax', 'Pue','Qro', 'QRoo', 'SLP', 'Sin', 'Son', 'Tab','Tamps', 'Tlax', 
        'Ver', 'Yuc', 'Zac')
df <- list()

coln <- c('TIME_PERIOD', 'OBS_VALUE', 'ef')


#Proceso de descarga de datos
df_l <- list()
for(j in 1:length(ef)){
  url <- gsub("\\[Aquí va tu Token\\]",token,url_api) %>% gsub('num', ind[j], .)
  respuesta <- GET(url)
  datosGenerales <- content(respuesta,"text")
  flujoDatos <- paste(datosGenerales,collapse = " ")
  flujoDatos <- fromJSON(flujoDatos)
  flujoDatos <- flujoDatos$Series
  flujoDatos <- flujoDatos[[1]]$OBSERVATIONS
  df_l[[j]] <- do.call(rbind, flujoDatos) %>% as.data.frame()
  df_l[[j]]['ef'] <- ef[j]
}
df <- bind_rows(df_l)
df <- df[,coln]
df <- apply(df, 2, FUN = unlist) %>% as.data.frame(col.names = coln)
df['OBS_VALUE'] <- unlist(df['OBS_VALUE']) %>% as.numeric()
df$TIME_PERIOD <- as.yearqtr(df$TIME_PERIOD, format = '%Y/0%q')

#Almacenamiento de datos
write.csv(df, paste0(carpeta_datos,'/docup_ef.csv'), fileEncoding = "UTF-8")

# # Generales para Graficos ------------------------------------------------------
# Colores institucionales
{
  ilob <- 'rgb(30,45,190)'
  ilor <- "rgb(250,60,75)"
  ilodb <- 'rgb(35,0,80)'
  ilot <- 'rgb(5,210,210)'
  iloy <- 'rgb(255,205,45)'
  ilop <- 'rgb(150,10,85)'
  ilogre <- 'rgb(140,225,100)'
  ilogra <- 'rgb(235.245.253)'
}
# Definición para incluir una línea vertical
vline <- function(x = 0, color = ilor) {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash="dot")
  )
}
# Definición para incluir una línea horizontal
hline <- function(y = 0, color = ilor) {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color)
  )
}

# Gráficos
for(ent in ef){
  fig <- plot_ly(data = df[df$ef==ent,],
                 x = ~as.Date(TIME_PERIOD),
                 y = ~round(OBS_VALUE,2),
                 type = 'scatter',
                 mode = 'lines+markers',
                 colors  = ilob,
                 name = paste0(ent,': Tasa de desocupación trimestral (%)'),
                 marker = list(size = 10),
                 hovertemplate="%{y}, %{x}"
  )
  fig <- fig %>% 
    layout(title         = paste0(ent,': Tasa de desocupación trimestral (%)'),
           paper_bgcolor = 'rgb(255,255,255)', 
           plot_bgcolor  = 'rgb(229,229,229)',
           showlegend    = F,
           xaxis         = list(title          = 'Año - T',
                                gridcolor      = 'rgb(255,255,255)',
                                showgrid       = TRUE,
                                showline       = TRUE,
                                showticklabels = TRUE,
                                tickcolor      = 'rgb(127,127,127)',
                                ticks          = 'inside',
                                zeroline       = FALSE,
                                dtick          = 'M12',
                                tickformat="%Y-T0%q"
                               ),
           yaxis         = list(title          = '(%)',
                                gridcolor      = 'rgb(255,255,255)',
                                showgrid       = TRUE,
                                showline       = TRUE,
                                showticklabels = TRUE,
                                tickcolor      = 'rgb(127,127,127)',
                                ticks          = 'inside',
                                zeroline       = FALSE)
    )
  fig
  
  htmlwidgets::saveWidget(fig, 
                          paste0(carpeta_visual,"/Grafica_docup_",ent,".html"), 
                          selfcontained = F, 
                          libdir = "lib")
}

