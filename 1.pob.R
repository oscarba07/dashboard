### Este código descarga, limpia y analiza los datos de población.

# Previo ------------------------------------------------------------------

# Se crea la carpeta principal donde se almacenarán los resultados del código
carpeta_pob  <- paste0(getwd(),'/Poblacion')
if (!file.exists(carpeta_pob)){
  dir.create(carpeta_pob)
}

# Se crea la carpeta de datos donde se almacenarán los datos
carpeta_datos  <- paste0(carpeta_pob, "/Datos")
if (!file.exists(carpeta_datos)){
  dir.create(carpeta_datos)
}


# Se crea la carpeta de visualización donde se almacenarán los gráficos
carpeta_visual  <- paste0(carpeta_pob, "/Visualizacion")
if (!file.exists(carpeta_visual)){
  dir.create(carpeta_visual)
}

# Se cargan los paquetes necesarios
{
  paq <- c("httr", "jsonlite","rjson","magrittr","dplyr","tidyr", 
                "plotly", "zoo", "httr", "jsonlite", "rjson", 
           "magrittr", "rgdal")
  
  for(paq in paq){
    if(!require(paq, character.only = T)) install.packages(paq, dependencies = T)
    lapply(paq, require, character.only = T)
  }
}
# Se define el token (Seguir indicaciones para obtenerlo aqui: 
# https://www.inegi.org.mx/servicios/api_indicadores.html
token <- '###'

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


# Población total por estado ----------------------------------------------

#Proceso de descarga de datos
# Se definen los indicadores con base en la construcción de indicadores de INEGI
ind <- paste0('0',seq(07000001, 07000001+31, 1))

# Se define la url con base en en la construcción de indicadores de INEGI
url_api <- 'https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/1002000001/es/num/true/BISE/2.0/[Aquí va tu Token]?type=json'

df <- list()
coln <- c('TIME_PERIOD', 'OBS_VALUE', 'ef')
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
  closeAllConnections()
}
df <- bind_rows(df_l)
df <- df[,coln]
df <- apply(df, 2, FUN = unlist) %>% as.data.frame(col.names = coln)
df['OBS_VALUE'] <- unlist(df['OBS_VALUE']) %>% as.numeric()


#Almacenamiento de datos
write.csv(df, paste0(carpeta_datos,'/pob.csv'), fileEncoding = "UTF-8")

#Mapa
carpeta_shp <- paste0(getwd(),'/Mapa/AGE/conjunto_de_datos')


shape_estados <- readOGR(dsn = carpeta_shp, 
                         layer = "00ent",
                         #  encoding = "UTF-8",
                         use_iconv = TRUE)

shape_estados <- fortify(shape_estados,by="CVE_ENT")

pob <- df
pob$X <- row.names(df) %>% as.numeric()
pob$X <- as.character(pob$X-1)
pob$OBS_VALUE <- round(pob$OBS_VALUE/1000000,2)

capas_estados<-inner_join(shape_estados,
                          pob%>%select(X, ef, OBS_VALUE),
                          by=c("id"="X"))


names(capas_estados)[length(names(capas_estados))] <- 'Población'

mp <- capas_estados %>%
  ggplot() +  
  geom_polygon(aes(x=long, 
                   y=lat,
                   Estado=ef,
                   group=group, #El argumento group=group arma grupos tanto para los polígonos. 
                   color="black",
                   fill=Población)) +
  theme_void() + 
  theme(plot.title = element_text(size=22),
        legend.key.size = unit(0.5, "cm"),
        legend.position = c(0.8, 0.7)) + 
  scale_color_manual(values=c("black")) +
  guides(color = FALSE) +
  labs(title = "Población (millones de personas)",
       fill = "Población",
       caption = "Fuente: Censo de población INEGI 2020") 

mp <- ggplotly(mp, tooltip = c('Estado','Población'))

htmlwidgets::saveWidget(mp, 
                        paste0(carpeta_visual,"/pob_ef.html"), 
                        selfcontained = F, 
                        libdir = "lib")

htmlwidgets::saveWidget(mp, paste0(getwd(),'/mapa_pob.html'), 
                        selfcontained = F, 
                        libdir = "lib")

# Piramide poblacional ----------------------------------------------------

#Proceso de descarga de datos
# Se definen los indicadores con base en la construcción de indicadores de INEGI

nef <- paste0('0',seq(07000001, 07000001+31, 1))

# Se define la url con base en en la construcción de indicadores de INEGI
url_api <- 'https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/num/es/ef/true/BISE/2.0/[Aquí va tu Token]?type=json'
url_api <- gsub("\\[Aquí va tu Token\\]",token,url_api) 

ind <- seq(1002000056, 1002000120, 10)

read_inegil <- function(x){
  ii <- x$INDICADOR
  df_l <- x$OBSERVATIONS %>% do.call(rbind,.) %>% as.data.frame()
  df_l['ef'] <- ef[j]
  df_l['ind'] <- ii
  return(df_l)
} #Función para la automatización del proceso

df <- data.frame()
for(j in 1:length(ef)){
  url_ef <- gsub('ef', nef[j], url_api)
  for (i in 1:length(ind)){
    ind_n <- seq(ind[i], ind[i]+9,1) %>% paste0(., collapse=',')
    url_i <- gsub("num",ind_n,url_ef)
    respuesta <- GET(url_i)
    datosGenerales <- content(respuesta,"text")
    flujoDatos <- paste(datosGenerales,collapse = " ")
    flujoDatos <- fromJSON(flujoDatos)
    flujoDatos <- flujoDatos$Series
    df_l <- lapply(flujoDatos, read_inegil) %>% do.call(rbind,.) 
    df <- rbind(df, df_l)
    closeAllConnections()
  }
}
coln <- c('TIME_PERIOD', 'OBS_VALUE', 'ef', 'ind')
df <- df[,coln]

# Descarga de metadatos
# Se define la URL de API metadatos
url_md <- 'https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/CL_INDICATOR/num/es/BISE/2.0/[Aquí va tu Token]?type=json'
url_md <- gsub("\\[Aquí va tu Token\\]",token,url_md) 

read_inegi_md <- function(x){
  md <- x %>% do.call(cbind,.) %>% as.data.frame()
  return(md)
} #Función para la automatización del procseso

md <- data.frame()
for (i in 1:length(ind)){
  ind_n <- seq(ind[i], ind[i]+9,1) %>% paste0(., collapse=',')
  url_mdt <- gsub("num",ind_n,url_md)
  respuesta <- GET(url_mdt)
  datosGenerales <- content(respuesta,"text")
  flujoDatos <- paste(datosGenerales,collapse = " ")
  flujoDatos <- fromJSON(flujoDatos)
  flujoDatos <- flujoDatos$CODE
  md_l <- lapply(flujoDatos, read_inegi_md) %>% do.call(rbind,.) 
  md <- rbind(md, md_l)
  closeAllConnections()
}

md <- md[grep('\\. *', md$Description),]
s <- strsplit(md$Description, '\\. ') %>% do.call(rbind,.)
md$Description <- s[,1]
md$sexo <- s[,2]
coh <- strsplit(md$Description, 'Población de ') %>% do.call(rbind,.) %>% as.matrix()
md$edad <- coh[,2]

df <- left_join(df, md, by=c('ind'='value'))
df <- as.data.frame(df)
df <- apply(df, 2, FUN = unlist) %>% as.data.frame()
df <- df[complete.cases(df),]
df$OBS_VALUE <- as.numeric(df$OBS_VALUE)
names(df)[2] <-'pob' 
  
# Almacenamiento de datos
write.csv(df, file = paste0(carpeta_datos,'/piramide_pob_ef.csv'), fileEncoding = "UTF-8")
#df <- read.csv(file = paste0(carpeta_datos,'/piramide_pob_ef.csv'))
# Gráficas de pirámides poblacionales -------------------------------------
ilob <- 'rgb(30,45,190,maxColorValue = 255)'
ilor <- "rgb(250,60,75,maxColorValue = 255)"
ilodb <- 'rgb(35,0,80,maxColorValue = 255)'
ilot <- 'rgb(5,210,210,maxColorValue = 255)'
iloy <- 'rgb(255,205,45,maxColorValue = 255)'
ilop <- 'rgb(150,10,85,maxColorValue = 255)'
ilogre <- 'rgb(140,225,100,maxColorValue = 255)'
ilogra <- 'rgb(235,245,253,maxColorValue = 255)'

df$edad <- factor(df$edad,
                  levels=c("0 a 4 años","5 a 9 años","10 a 14 años",
                              "15 a 19 años","20 a 24 años","25 a 29 años",
                              "30 a 34 años","35 a 39 años","40 a 44 años",
                              "45 a 49 años","50 a 54 años","55 a 59 años",
                              "60 a 64 años","65 a 69 años","70 a 74 años",
                              "75 a 79 años","80 a 84 años","85 a 89 años",
                              "90 a 94 años","95 a 99 años","100 años y más",
                              "edad no especificada")
                     )
roundup <- function(x){ceiling(x/10^(nchar(x)-1))*10^nchar(x)}
ilob_h <- eval(parse(text=ilob))
ilor_h <- eval(parse(text=ilor))
cols <- c(ilob_h,ilor_h)

for (e in ef) {
  df_e <- df[df$ef==e,]
  df_e[df_e$sexo=='Hombres',]$pob <- -1*df_e[df_e$sexo=='Hombres',]$pob
  ma <- max(df_e$pob) %>% roundup(.)
  g <- ggplot(df_e, aes(x = edad,
                      y = pob,
                      fill = sexo)) +
    geom_col(width = 0.5) +
    scale_fill_manual(values=cols) +
    coord_flip() + 
    labs(y='Población',x='Cohorte',fill='sexo', 
         title = paste0(e,': Población 2020')) +
    scale_y_continuous(
      breaks = c(seq(-ma, -20000,by = 20000), 
                 sort(seq(0,ma,by = 20000),decreasing=F)),
      labels = c(-seq(-ma, -20000,by = 20000), 
                 sort(seq(0,ma,by = 20000),decreasing=F)) 
      ) 
    g <- ggplotly(g, tooltip = c('edad','sexo', 'y'))
  htmlwidgets::saveWidget(g, 
                          paste0(carpeta_visual,"/Ppob_",e,".html"), 
                          selfcontained = F, 
                          libdir = "lib")
}

