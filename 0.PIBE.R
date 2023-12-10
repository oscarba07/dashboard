### Este código descarga, limpia y analiza los datos de crecimiento del PIBE.
### Se utilizan precios constantes base 2013

# Se crea la carpeta principal donde se almacenarán los resultados del código
carpeta_pibe  <- paste0(getwd(),'/PIBE')
if (!file.exists(carpeta_pibe)){
  dir.create(carpeta_pibe)
}

#setwd(file.path(getwd(), carpeta_pibe))

# Se crea la carpeta de datos donde se almacenarán los datos
carpeta_datos  <- paste0(carpeta_pibe, "/Datos")
if (!file.exists(carpeta_datos)){
  dir.create(carpeta_datos)
}


# Se crea la carpeta de visualización donde se almacenarán los gráficos
carpeta_visual  <- paste0(carpeta_pibe, "/Visualizacion")
if (!file.exists(carpeta_visual)){
  dir.create(carpeta_visual)
}

# Se cargan los paquetes necesarios
{
paquetes <- c("httr", "jsonlite","rjson","magrittr","dplyr","tidyr", "plotly")

for(paq in paquetes){
  if(!require(paq, character.only = T)) install.packages(paq, dependencies = T)
}
}

# Se define el token (Seguir indicaciones para obtenerlo aqui: 
# https://www.inegi.org.mx/servicios/api_indicadores.html
token <- '###'

# Se definen los indicadores con base en la construcción de indicadores de INEGI
total <- seq(472080, 472080+31, 1)
primario <- seq(472113, 472113+31, 1)
secundario <- seq(472179, 472179+31, 1)
terciario <- seq(472806, 472806+31, 1)

# Se define la url con base en en la construcción de indicadores de INEGI
url_api <- 'https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/sector/es/0700/false/BIE/2.0/[Aquí va tu Token]?type=json'

# Definiciones de vectores de apoyo
{
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
coln <- c('TIME_PERIOD', 'OBS_VALUE', 'ef', 'sector')
sect <- c('total', 'primario', 'secundario', 'terciario')
}
#Proceso de descarga de datos
for(j in 1:length(ef)){
  url <- vector(length = 4)
  sec <- c(total[j], primario[j], secundario[j], terciario[j])
    for (i in 1:length(url)){
    url[i] <- gsub("\\[Aquí va tu Token\\]",token,url_api) %>% gsub("sector",
                                                                    sec[i],.)
  }
  df_l <- list()
  for (k in 1:length(url)){
    respuesta <- GET(url[k])
    datosGenerales <- content(respuesta,"text")
    flujoDatos <- paste(datosGenerales,collapse = " ")
    flujoDatos <- fromJSON(flujoDatos)
    flujoDatos <- flujoDatos$Series
    flujoDatos <- flujoDatos[[1]]$OBSERVATIONS
    df_l[[k]] <- do.call(rbind, flujoDatos) %>% as.data.frame()
    df_l[[k]]['ef'] <- ef[j]
    df_l[[k]]['sector'] <- sect[k]
    closeAllConnections()
  }
  df[[j]] <- bind_rows(df_l)
  df[[j]] <- df[[j]][coln]
  df[[j]] <- apply(df[[j]], 2, FUN = unlist) %>% as.data.frame(col.names = coln)
}

# Filtrado de datos -------------------------------------------------------
df <- bind_rows(df)
df[c('TIME_PERIOD', 'OBS_VALUE')] <- unlist(df[c('TIME_PERIOD', 'OBS_VALUE')]) %>% 
  as.numeric()
write.csv(df, paste0(carpeta_datos,'/pibe.csv'), fileEncoding = "UTF-8")

# Generales para Graficos ------------------------------------------------------
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


# PIBE total mas reciente -------------------------------------------------
# Filtrado de datos
tot = df[(df$TIME_PERIOD==max(df$TIME_PERIOD) & df$sector=='total'),
         c('ef','OBS_VALUE','TIME_PERIOD')]
tot <- tot[order(tot$OBS_VALUE, decreasing=FALSE),]


# Almacenamiento de datos
write.csv(tot, paste0(carpeta_datos,'/pibe_tot','.csv'), fileEncoding = "UTF-8")

# Gráfico
{
fig <- plot_ly(
  y = tot$ef,
  x = tot$OBS_VALUE,
  type = 'bar', 
  orientation = 'h',
  marker = list(color = ilob)
) 
fig <- fig %>%  
  layout(title = paste("PIBE", max(df$TIME_PERIOD), 
                       '(A precios constantes 2013)'),
         shapes=vline(mean(tot$OBS_VALUE)),
         paper_bgcolor = 'rgb(255,255,255)', 
         plot_bgcolor  = 'rgb(229,229,229)',
         xaxis         = list(title          = 'PIBE (precios constantes 2013)',
                              gridcolor      = 'rgb(255,255,255)',
                              showgrid       = TRUE,
                              showline       = TRUE,
                              showticklabels = TRUE,
                              tickcolor      = 'rgb(127,127,127)',
                              ticks          = 'inside',
                              zeroline       = FALSE),
         yaxis         = list(title          = 'Estado',
                              gridcolor      = 'rgb(255,255,255)',
                              showgrid       = TRUE,
                              showline       = TRUE,
                              showticklabels = TRUE,
                              tickcolor      = 'rgb(127,127,127)',
                              ticks          = 'inside',
                              zeroline       = FALSE,
                              categoryorder = "array",
                              categoryarray = tot$ef)
         ) %>% 
  add_annotations(text=paste('Promedio nacional:', 
                             round(mean(tot$OBS_VALUE),2), 'pesos'), 
                  textfont = list(family = "noto sans",
                                  size = 14), 
                  x =  mean(tot$OBS_VALUE)+300000, 
                  y = 0.5, 
                  xref='plot',
                  yref='paper',
                  position = 'right',
                  showarrow = FALSE,
                  font = list(color = ilor,
                              family = 'noto sans')
  )

htmlwidgets::saveWidget(fig, 
           paste0(carpeta_visual,"/Grafica_pibe_tot.html"), selfcontained = F, 
           libdir = "lib")
}

# Tasa de crecimiento del PIBE -------------------------------------------------
# Calculo de tasa de crecimiento
df_c <- df[df$sector=='total',] %>%  group_by(ef) %>% 
  dplyr::arrange(., TIME_PERIOD, .by_group = TRUE) %>% 
  mutate(pct_change = (OBS_VALUE/lag(OBS_VALUE)-1)*100)

# Almacenamiento de datos
write.csv(df_c, paste0(carpeta_datos,'/pibe_tot_pctch.csv'), fileEncoding = "UTF-8")

# Gráficos
for(ent in ef){
  fig <- plot_ly(data = df_c[df_c$ef==ent,],
                 x = ~TIME_PERIOD,
                 y = ~round(pct_change,2),
                 type = 'scatter',
                 mode = 'lines+markers',
                 colors  = ilob,
                 name = paste0(ent,': Tasa de crecimiento real del PIBE (%)'),
                 marker = list(size = 10)
  )
  fig <- fig %>% 
    layout(title         = paste0(ent,': Tasa de crecimiento real anual del PIBE (%)'),
           paper_bgcolor = 'rgb(255,255,255)', 
           plot_bgcolor  = 'rgb(229,229,229)',
           showlegend    = F,
           shapes=hline(0,ilodb),
           xaxis         = list(title          = 'Año',
                                gridcolor      = 'rgb(255,255,255)',
                                showgrid       = TRUE,
                                showline       = TRUE,
                                showticklabels = TRUE,
                                tickcolor      = 'rgb(127,127,127)',
                                ticks          = 'inside',
                                zeroline       = FALSE),
           yaxis         = list(title          = '(%)',
                                gridcolor      = 'rgb(255,255,255)',
                                showgrid       = TRUE,
                                showline       = TRUE,
                                showticklabels = TRUE,
                                tickcolor      = 'rgb(127,127,127)',
                                ticks          = 'inside',
                                zeroline       = FALSE)
    )
  
  htmlwidgets::saveWidget(fig, 
                          paste0(carpeta_visual,"/Grafica_pibe_tc_",ent,".html"), 
                          selfcontained = F, 
                          libdir = "lib")
}


# Composición del PIBE ----------------------------------------------------

# Calculo de porcentajes
df_s <- pivot_wider(df,names_from = 'sector', values_from = 'OBS_VALUE')
df_s <- df_s %>% dplyr::mutate(across(c(total, primario, secundario, terciario), 
                            function(x){(x/total)*100}))
# Almacenamiento de datos
write.csv(df_s, paste0(carpeta_datos,'/pibe_comp.csv'), fileEncoding = "UTF-8")

# Gráficos
for(ent in ef){
  fig <- plot_ly(data = df_s[df_s$ef==ent,],
                 x = ~TIME_PERIOD,
                 y = ~round(primario,2),
                 type = 'bar',
                 marker = list(color=ilob),
                 name = 'Actividades primarias'
  )
  fig <- fig %>% add_trace(y = ~round(secundario,2),
                           marker = list(color=ilor),
                           name = 'Actividades secundarias')
  fig <- fig %>% add_trace(y = ~round(terciario,2),
                           marker = list(color=ilot),
                           name = 'Actividades terciarias')
  fig <- fig %>% 
    layout(title         = paste0(ent,': Composición del PIBE por actividad económica'),
           paper_bgcolor = 'rgb(255,255,255)', 
           plot_bgcolor  = 'rgb(229,229,229)',
           showlegend    = T,
           barmode = 'stack',
           xaxis         = list(title          = 'Año',
                                gridcolor      = 'rgb(255,255,255)',
                                showgrid       = TRUE,
                                showline       = TRUE,
                                showticklabels = TRUE,
                                tickcolor      = 'rgb(127,127,127)',
                                ticks          = 'inside',
                                zeroline       = FALSE),
           yaxis         = list(title          = '(%)',
                                gridcolor      = 'rgb(255,255,255)',
                                showgrid       = TRUE,
                                showline       = TRUE,
                                showticklabels = TRUE,
                                tickcolor      = 'rgb(127,127,127)',
                                ticks          = 'inside',
                                range = list(0, 100),
                                zeroline       = FALSE)
    )
  
  htmlwidgets::saveWidget(fig, 
                          paste0(carpeta_visual,"/Grafica_pibe_comp_",ent,
                                 ".html"), 
                          selfcontained = F, 
                          libdir = "lib")
}
