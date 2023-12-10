### Este código descarga, limpia y analiza los datos de la Población
### Económicamente Activa

# Se crea la carpeta principal donde se almacenarán los resultados 
# del código
carpeta_docup  <- paste0(getwd(),'/PEA')
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

# Se definen los indicadores con base en la construcción de indicadores 
# de INEGI
ind <- c(446564,446565,446566)
ind_desc <- c('PEA_TOTAL','PEA_OCUPADA','PEA_DESOCUP')

# Se define la url con base en en la construcción de indicadores de INEGI
url_api <- 'https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/446564,446565,446566/es/0700/false/BIE/2.0/[Aquí va tu Token]?type=json'

# Definiciones de vectores de apoyo
coln <- c('TIME_PERIOD', 'OBS_VALUE', 'update','ind')

read.inegil <- function(x){
  df_l <- x$OBSERVATIONS %>% do.call(rbind,.) %>% as.data.frame()
  df_l['update'] <- x$LASTUPDATE
  df_l['ind'] <- x$INDICADOR
  return(df_l)
  }

#Proceso de descarga de datos
url <- gsub("\\[Aquí va tu Token\\]",token,url_api)
respuesta <- GET(url)
datosGenerales <- content(respuesta,"text")
flujoDatos <- paste(datosGenerales,collapse = " ")
flujoDatos <- fromJSON(flujoDatos)
flujoDatos <- flujoDatos$Series
df_l <- lapply(flujoDatos, read.inegil)  
df <- bind_rows(df_l)
df <- df[,coln]
df <- apply(df, 2, FUN = unlist) %>% as.data.frame(col.names = coln)
df['OBS_VALUE'] <- unlist(df['OBS_VALUE']) %>% as.numeric()

df$update <- gsub('p. m.', 'pm', df$update)
df$update <- strptime(df$update, format = '%d/%m/%Y %I:%M:%S %p')

df$TIME_PERIOD <- paste0(df$TIME_PERIOD,'/01')
df$TIME_PERIOD <- as.Date(as.yearqtr(df$TIME_PERIOD, format = '%Y/0%q'))

df$ind <- as.factor(df$ind)
levels(df$ind) <- ind_desc
  
#Almacenamiento de datos
write.csv(df, paste0(carpeta_datos,'/pea.csv'), fileEncoding = "UTF-8")

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

# Gráficos
fig <- plot_ly(data = df[df$ind=='PEA_OCUPADA',],
                 x = as.Date(df[df$ind=='PEA_OCUPADA',]$TIME_PERIOD),
                 y = round(df[df$ind=='PEA_OCUPADA',]$OBS_VALUE/1000000,2),
                 type = 'scatter',
                 mode = 'lines+markers',
                 colors  = ilob,
                 name = 'PEA Ocupada',
                 marker = list(size = 5, color = ilob),
                 line = list(color=ilob),
                 xhoverformat = "%Y-T%q",
                 hovertemplate="%{y}, %{x}"
  )
  fig <- fig %>% 
    add_trace(x = as.Date(df[df$ind=='PEA_DESOCUP',]$TIME_PERIOD),
              y = round(df[df$ind=='PEA_DESOCUP',]$OBS_VALUE/1000000,2),
              type = 'scatter',
              mode = 'lines+markers',
              colors  = ilor,
              name = 'PEA Desocupada',
              marker = list(size = 5, color = ilor),
              line = list(color=ilor),
              xhoverformat = "%Y-T%q",
              hovertemplate="%{y}, %{x}")
  fig <- fig %>% 
    layout(title         = 'Población Económicamente Activa',
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
                                tickformat     = "%Y-T%q"
           ),
           yaxis         = list(title          = 'Millones de personas',
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
                          paste0(carpeta_visual,"/Grafica_pea.html"), 
                          selfcontained = F, 
                          libdir = "lib")


# PEA por género ----------------------------------------------------------

  # Se definen los indicadores con base en la construcción de indicadores 
  # de INEGI
  ind <- c(6200093950,6200093956,6200093974,6200093975)
  ind_desc <- c('PEA_OCUPADA_H','PEA_OCUPADA_M','PEA_DESOCUP_H','PEA_DESOCUP_M')
  
  # Se define la url con base en en la construcción de indicadores de INEGI
  url_api <- 'https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/6200093950,6200093956,6200093974,6200093975/es/0700/false/BISE/2.0/[Aquí va tu Token]?type=json'
  
  # Definiciones de vectores de apoyo
  coln <- c('TIME_PERIOD', 'OBS_VALUE', 
            #'update',
            'ind')
  
  read.inegil <- function(x){
    df_l <- x$OBSERVATIONS %>% do.call(rbind,.) %>% as.data.frame()
    df_l['update'] <- x$LASTUPDATE
    df_l['ind'] <- x$INDICADOR
    return(df_l)
  }
  
  #Proceso de descarga de datos
  url <- gsub("\\[Aquí va tu Token\\]",token,url_api)
  respuesta <- GET(url)
  datosGenerales <- content(respuesta,"text")
  flujoDatos <- paste(datosGenerales,collapse = " ")
  flujoDatos <- fromJSON(flujoDatos)
  flujoDatos <- flujoDatos$Series
  df_l <- lapply(flujoDatos, read.inegil)  
  df <- bind_rows(df_l)
  df <- df[,coln]
  df[df == "NULL"] <- NA
  df <- apply(df, 2, FUN = unlist) %>% as.data.frame(col.names = coln)
  df['OBS_VALUE'] <- unlist(df['OBS_VALUE']) %>% as.numeric()
  
  #df$update <- gsub('p. m.', 'pm', df$update)
  #df$update <- strptime(df$update, format = '%d/%m/%Y %I:%M:%S %p')
  
  df$TIME_PERIOD <- paste0(df$TIME_PERIOD,'/01')
  df$TIME_PERIOD <- as.Date(as.yearqtr(df$TIME_PERIOD, format = '%Y/0%q'))
  
  df$ind <- as.factor(df$ind)
  levels(df$ind) <- ind_desc
  
  #Almacenamiento de datos
  write.csv(df, paste0(carpeta_datos,'/pea_genero.csv'), fileEncoding = "UTF-8")
  
  # Gráficos
  ### Personas ocupadas
  fig <- plot_ly(data = df[df$ind=='PEA_OCUPADA_H',],
                 x = as.Date(df[df$ind=='PEA_OCUPADA_H',]$TIME_PERIOD),
                 y = round(df[df$ind=='PEA_OCUPADA_H',]$OBS_VALUE/1000000,2),
                 type = 'bar',
                 #mode = 'lines+markers',
                 colors  = ilob,
                 name = 'Hombres Ocupados',
                 marker = list(color = ilob),
                 #line = list(color=ilob),
                 xhoverformat = "%Y-T%q",
                 hovertemplate="%{y}, %{x}"
  )
  fig <- fig %>% 
    add_trace(x = as.Date(df[df$ind=='PEA_OCUPADA_M',]$TIME_PERIOD),
              y = round(df[df$ind=='PEA_OCUPADA_M',]$OBS_VALUE/1000000,2),
              type = 'bar',
              #mode = 'lines+markers',
              colors  = ilor,
              name = 'Mujeres Ocupadas',
              marker = list(color = ilor),
              #line = list(color=ilor),
              xhoverformat = "%Y-T%q",
              hovertemplate="%{y}, %{x}")
  fig <- fig %>% 
    layout(title         = 'Población Económicamente Activa',
           paper_bgcolor = 'rgb(255,255,255)', 
           plot_bgcolor  = 'rgb(229,229,229)',
           showlegend    = T,
           xaxis         = list(title          = 'Año - T',
                                gridcolor      = 'rgb(255,255,255)',
                                showgrid       = TRUE,
                                showline       = TRUE,
                                showticklabels = TRUE,
                                tickcolor      = 'rgb(127,127,127)',
                                ticks          = 'inside',
                                zeroline       = FALSE,
                                dtick          = 'M12',
                                tickformat     = "%Y-T%q"
           ),
           yaxis         = list(title          = 'Millones de personas',
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
                          paste0(carpeta_visual,"/Grafica_pea_ocup_genero.html"), 
                          selfcontained = F, 
                          libdir = "lib")
  
  
  ### Personas desocupadas
  fig <- plot_ly(data = df[df$ind=='PEA_DESOCUP_H',],
                 x = as.Date(df[df$ind=='PEA_DESOCUP_H',]$TIME_PERIOD),
                 y = round(df[df$ind=='PEA_DESOCUP_H',]$OBS_VALUE/1000000,2),
                 type = 'bar',
                 #mode = 'lines+markers',
                 colors  = ilob,
                 name = 'Hombres Desocupados',
                 marker = list(color = ilob),
                 #line = list(color=ilob),
                 xhoverformat = "%Y-T%q",
                 hovertemplate="%{y}, %{x}"
  )
  fig <- fig %>% 
    add_trace(x = as.Date(df[df$ind=='PEA_DESOCUP_M',]$TIME_PERIOD),
              y = round(df[df$ind=='PEA_DESOCUP_M',]$OBS_VALUE/1000000,2),
              type = 'bar',
              #mode = 'lines+markers',
              colors  = ilor,
              name = 'Mujeres Desocupadas',
              marker = list(color = ilor),
              #line = list(color=ilor),
              xhoverformat = "%Y-T%q",
              hovertemplate="%{y}, %{x}")
  fig <- fig %>% 
    layout(title         = 'Población Económicamente Activa',
           paper_bgcolor = 'rgb(255,255,255)', 
           plot_bgcolor  = 'rgb(229,229,229)',
           showlegend    = T,
           xaxis         = list(title          = 'Año - T',
                                gridcolor      = 'rgb(255,255,255)',
                                showgrid       = TRUE,
                                showline       = TRUE,
                                showticklabels = TRUE,
                                tickcolor      = 'rgb(127,127,127)',
                                ticks          = 'inside',
                                zeroline       = FALSE,
                                dtick          = 'M12',
                                tickformat     = "%Y-T%q"
           ),
           yaxis         = list(title          = 'Millones de personas',
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
                          paste0(carpeta_visual,"/Grafica_pea_desocup_genero.html"), 
                          selfcontained = F, 
                          libdir = "lib")
  
