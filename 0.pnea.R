### Este código descarga, limpia y analiza los datos de la Población
### No Económicamente Activa

# Se crea la carpeta principal donde se almacenarán los resultados 
# del código
carpeta_docup  <- paste0(getwd(),'/PNEA')
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
  
  library(httr)
  library(jsonlite)
  library(rjson)
  library(magrittr)
  library(dplyr)
  library(tidyr)
  library(plotly)
  library(zoo)
}

# Se define el token (Seguir indicaciones para obtenerlo aqui: 
# https://www.inegi.org.mx/servicios/api_indicadores.html
token <- 'de7fa498-b2e8-f747-cf22-4895402afa13'

# Se definen los indicadores con base en la construcción de indicadores 
# de INEGI
ind <- c(289247,289248,289249)
ind_desc <- c('PNEA_TOTAL','PNEA_DISP','PNEA_NODISP')

# Se define la url con base en en la construcción de indicadores de INEGI
url_api <- 'https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/289247,289248,289249/es/0700/false/BIE/2.0/[Aquí va tu Token]?type=json'

# Definiciones de vectores de apoyo
coln <- c('TIME_PERIOD', 'OBS_VALUE', 'update','ind')

read.inegil <- function(x){
  df_l <- x$OBSERVATIONS %>% do.call(rbind,.) %>% as.data.frame()
  df_l['update'] <- x$LASTUPDATE
  df_l['ind'] <- x$INDICADOR
  closeAllConnections()
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
write.csv(df, paste0(carpeta_datos,'/pnea.csv'), fileEncoding = "UTF-8")

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
fig <- plot_ly(data = df[df$ind=='PNEA_DISP',],
               x = as.Date(df[df$ind=='PNEA_DISP',]$TIME_PERIOD),
               y = round(df[df$ind=='PNEA_DISP',]$OBS_VALUE/1000000,2),
               type = 'scatter',
               mode = 'lines+markers',
               colors  = ilob,
               name = 'PNEA Disponible',
               marker = list(size = 5, color = ilob),
               line = list(color=ilob),
               xhoverformat = "%Y-T%q",
               hovertemplate="%{y}, %{x}"
)
fig <- fig %>% 
  add_trace(x = as.Date(df[df$ind=='PNEA_NODISP',]$TIME_PERIOD),
            y = round(df[df$ind=='PNEA_NODISP',]$OBS_VALUE/1000000,2),
            type = 'scatter',
            mode = 'lines+markers',
            colors  = ilor,
            name = 'PNEA No Disponible',
            marker = list(size = 5, color = ilor),
            line = list(color=ilor),
            xhoverformat = "%Y-T%q",
            hovertemplate="%{y}, %{x}")
fig <- fig %>% 
  layout(title         = 'Población No Económicamente Activa',
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
                        paste0(carpeta_visual,"/Grafica_pnea.html"), 
                        selfcontained = F, 
                        libdir = "lib")


