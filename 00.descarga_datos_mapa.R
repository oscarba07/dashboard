### Este codigo descarga los archivos necesarios para crear un mapa de los 
### estados y municipios de Mexico

setwd("###")

options(timeout=30000000)
url<-"https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463849568_s.zip" #diciembre 2021
# Crear carpetas ----------------------------------------------------------
{
  carpeta_mapa <- paste0(getwd(),'/Mapa')
  if(!file.exists(carpeta_mapa)){
    dir.create(carpeta_mapa)
  }
  
  carpeta_age <- paste0(carpeta_mapa,'/AGE')
  if(!file.exists(carpeta_age)){
    dir.create(carpeta_age)
  }
  
  
  if(!file.exists(paste0(carpeta_age,"/ageoest.zip"))){
    download.file(url, paste0(carpeta_age,"/ageoest.zip"),mode="wb")
  }
  
  if(!file.exists(paste0(carpeta_age,
                         "/mg2021_integrado.zip"))){
    unzip(paste0(carpeta_age,
                 "/ageoest.zip"),exdir = carpeta_age)
  }
  
  zipped_files <- unzip(paste0(carpeta_age,"/mg2021_integrado.zip"),list=T)
  shp_files <- grep('\\.shp$|\\.dbf$|\\.shx$'
                    , zipped_files$Name,ignore.case=TRUE, value=TRUE)
  
  if(!file.exists(paste0(carpeta_age,"/conjunto_de_datos"))){
    unzip(paste0(carpeta_age,
                 "/mg2021_integrado.zip"), files = shp_files, 
          exdir = carpeta_age)
  }
  
  carpeta_json <- paste0(carpeta_mapa,'/json')
  if(!file.exists(carpeta_json)){
    dir.create(carpeta_json)
  }
}
# Leer paquetes necesarios ------------------------------------------------
{
  paquetes <- c("rgdal", "geojsonio", "rmapshaper", #"spdplyr",
                "jsonlite", "highcharter", "plotly","sf")
  
  for (paq in paquetes) {
    if (!require(paq,character.only = T)) {
      install.packages(paq,dependencies = T)
      library(paq, character.only = T)
    }
  }
}

# Guardar datos en JSON ---------------------------------------------------

area_m <- readOGR(paste0(carpeta_age,"/conjunto_de_datos"),
                  layer="00mun",
                  verbose = FALSE)

area_ent <- readOGR(paste0(carpeta_age,"/conjunto_de_datos"),
                    layer="00ent",
                    verbose = FALSE)

df_ent <- data.frame(area_ent@data)
df_m <- data.frame(area_m@data)

# Si no funciona, usa area_ent@data. Depende de spdplyr

#area_ent <- area_ent %>% mutate(CVEGEO=as.factor(as.numeric(as.character(CVEGEO))),
#                                          CVE_ENT=as.factor(as.numeric(as.character(CVE_ENT))))

#area_m <- area_m %>% mutate(CVEGEO=as.factor(as.numeric(as.character(CVEGEO))),
#                                      CVE_ENT=as.factor(as.numeric(as.character(CVE_ENT))),
#                                      CVE_MUN=as.factor(as.numeric(as.character(CVE_MUN))))

area_m <- rmapshaper::ms_simplify(area_m)
area_ent <- rmapshaper::ms_simplify(area_ent)

area_mjson<-geojson_json(area_m)
area_entjson<-geojson_json(area_ent)

geojson_write(area_mjson,file = paste0(carpeta_json,"/mun.geojson"))
geojson_write(area_entjson,file = paste0(carpeta_json,"/ent.geojson"))

# Crear jsons por estado
for(i in 1:32){
  edo <- area_m[area_m@data$CVE_ENT==formatC(i,digits = 1, format = "d", flag = "0"),]
  #Convertir a JSON
  edo_json<-geojson_json(edo)
  #guardar
  geojson_write(edo_json,file = paste0(carpeta_json,"/estado",i,".geojson"))
}
