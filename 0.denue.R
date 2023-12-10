### Este código descarga, limpia y analiza los datos del DENUE

# Se crea la carpeta principal donde se almacenarán los resultados del código
carpeta_denue  <- paste0(getwd(),'/DENUE')
if (!file.exists(carpeta_denue)){
  dir.create(carpeta_denue)
}

#setwd(file.path(getwd(), carpeta_pibe))

# Se crea la carpeta de datos donde se almacenarán los datos
carpeta_datos  <- paste0(carpeta_denue, "/Datos")
if (!file.exists(carpeta_datos)){
  dir.create(carpeta_datos)
}


# Se crea la carpeta de visualización donde se almacenarán los gráficos
carpeta_visual  <- paste0(carpeta_denue, "/Visualizacion")
if (!file.exists(carpeta_visual)){
  dir.create(carpeta_visual)
}

# Se cargan los paquetes necesarios
{
  paquetes <- c("httr", "jsonlite","rjson","magrittr","dplyr","tidyr", "plotly",
                "plyr")
  
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
  library(plyr)
}

# Se define el token (Seguir indicaciones para obtenerlo aqui: 
# https://www.inegi.org.mx/servicios/api_indicadores.html
token <- '###'

# Se definen los indicadores con base en la construcción de indicadores de INEGI


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
}

#Proceso de descarga de diccionario de datos AGEM
url_agem <- 'https://gaia.inegi.org.mx/wscatgeo/mgem/ee'

ee <- sprintf("%02d", 1:32)
df_agem <- list()
for(i in ee){
  url_m <- gsub("ee",i,url_agem)
  respuesta <- GET(url_m)
  datosGenerales <- content(respuesta,"text")
  flujoDatos <- paste(datosGenerales,collapse = " ")
  flujoDatos <- fromJSON(flujoDatos)
  flujoDatos <- flujoDatos$datos
  df_agem[[i]] <- lapply(flujoDatos, as.data.frame)
  df_agem[[i]] <- do.call(rbind.fill, df_agem[[i]])
  df_agem[[i]]$ef <- ef[which(i==ee)]
  closeAllConnections()
}
df_agem <- do.call(rbind.fill, df_agem)


#Proceso de descarga de datos principales
# Se define la url con base en en la construcción de indicadores de INEGI
url_api <- 'https://www.inegi.org.mx/app/api/denue/v1/consulta/Cuantificar/0/0/es/[Aquí va tu token]'

df_l <- list()

for(i in 1:7){
  url <- gsub("\\[Aquí va tu token\\]",token,url_api) %>% gsub('es',i,.)
  respuesta <- GET(url)
  datosGenerales <- content(respuesta,"text")
  flujoDatos <- paste(datosGenerales,collapse = " ")
  flujoDatos <- fromJSON(flujoDatos)
  df_l[[i]] <- do.call(rbind, flujoDatos) %>% as.data.frame
  df_l[[i]] <- apply(df_l[[i]], 2, FUN = unlist) %>% as.data.frame()
  df_l[[i]]$estr <- i
  closeAllConnections()
}
df_c <- do.call(rbind, df_l)

#Se filtra para totales por sector económico
df_c <- df_c[nchar(df_c$AE)==2,]

#Se incluyen los nombres de las EF
df_c <- left_join(df_c, unique(df_agem[,c('cve_agee', 'ef')]),
                  by = c('AG' = 'cve_agee')
                  )
#Se definen los estratos por tamaño de unidad económica
df_c$estr <- as.factor(df_c$estr)
levels(df_c$estr) = c('De 0 a 5 personas.',
                      'De 6 a 10 personas.',
                      'De 11 a 30 personas.',
                      'De 31 a 50 personas.',
                      'De 51 a 100 personas.',
                      'De 101 a 250 personas.',
                      'De 251 y más personas.')

#Se incluyen las definiciones de los sectores económicos
scian <- read.csv('sectores_scian.csv', encoding="UTF-8")
names(scian) <- c('cve','def')
scian$cve <- as.character(scian$cve)
df_c <- left_join(df_c, scian,
                  by = c('AE' = 'cve')
)

#Almacenamiento de datos
write.csv(df_c, paste0(carpeta_datos,'/ue_cont.csv'), fileEncoding = "UTF-8")

#Se calculan los totales
df_c$Total <- as.numeric(df_c$Total)
#aggregate(Total~ef, data=df_c, FUN=sum)
#aggregate(Total~def, data=subset(denue, ef=='Ags'), FUN=sum)
#aggregate(Total~estr, data=subset(denue, ef=='Ags'), FUN=sum)

#sum(df_c[df_c$ef=='Ags','Total'])
