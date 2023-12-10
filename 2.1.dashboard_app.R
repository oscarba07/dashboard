paquetes <- c('shiny', 'magrittr', 'plotly', 'bslib', 'htmltools', 'zoo',
              'rgdal', 'dplyr', 'ggplot2', 'shinydashboard')

for (paq in paquetes) {
  if (!require(paq, character.only = T)) {
    install.packages(paq)
    library(paq, character.only = T)
  }
}


#{library(shiny, quietly = T)
#library(magrittr, quietly = T)
#library(plotly, quietly = T)
#library(bslib, quietly = T)
#library(htmltools, quietly = T)
#library(zoo, quietly = T)
#library(rgdal, quietly = T)
#library(dplyr, quietly = T)
#library(ggplot2, quietly = T)
#library(shinydashboard, quietly = T)
#    }

# Definiciones generales
## Carga de datos y vectores de apoyo
{
    tot <- read.csv(paste0(getwd(),'/PIBE/Datos/pibe_tot.csv'),encoding = "UTF-8")
    pibe_tc <- read.csv(paste0(getwd(),'/PIBE/Datos/pibe_tot_pctch.csv'),encoding = "UTF-8")
    pibe_comp <- read.csv(paste0(getwd(),'/PIBE/Datos/pibe_comp.csv'),encoding = "UTF-8")
    docup <- read.csv(paste0(getwd(),'/Desocupacion/Datos/docup_ef.csv'),encoding = "UTF-8")
    denue <- read.csv(paste0(getwd(),'/DENUE/Datos/ue_cont.csv'),encoding = "UTF-8")
    pob <- read.csv(paste0(getwd(),'/Poblacion/Datos/pob.csv'),encoding = "UTF-8")
    pea <- read.csv(paste0(getwd(),'/PEA/Datos/pea.csv'),encoding = "UTF-8")
    pea_genero <- read.csv(paste0(getwd(),'/PEA/Datos/pea_genero.csv'),encoding = "UTF-8")
    pnea <- read.csv(paste0(getwd(),'/PNEA/Datos/pnea.csv'),encoding = "UTF-8")
    ppob <- read.csv(paste0(getwd(),'/Poblacion/Datos/piramide_pob_ef.csv'),encoding = "UTF-8")
    ef_l <- c('Aguascalientes', 'Baja California', 'Baja California Sur', 'Campeche', 
              'Coahuila', 'Colima', 'Chiapas', 'Chihuahua', 'Ciudad de México', 
              'Durango', 'Guanajuato', 'Guerrero', 'Hidalgo', 'Jalisco', 'México', 
              'Michoacán', 'Morelos', 'Nayarit', 'Nuevo León', 'Oaxaca', 'Puebla', 
              'Querétaro', 'Quintana Roo', 'San Luis Potosí', 'Sinaloa', 'Sonora', 
              'Tabasco',         'Tamaulipas', 'Tlaxcala', 'Veracruz', 'Yucatán', 
              'Zacatecas')
    ef_c <- c('Ags', 'BC', 'BCS', 'Camp','Coah', 'Col', 'Chis', 'Chih', 'Cdmx', 
              'Dgo', 'Gto', 'Gro', 'Hgo', 'Jal', 'EdoMex','Mich', 'Mor', 'Nay', 'NL', 
              'Oax', 'Pue','Qro', 'QRoo', 'SLP', 'Sin', 'Son', 'Tab','Tamps', 'Tlax', 
              'Ver', 'Yuc', 'Zac')
    
    ## Definición de colores
    ilob <- 'rgb(30,45,190,maxColorValue = 255)'
    ilor <- "rgb(250,60,75,maxColorValue = 255)"
    ilodb <- 'rgb(35,0,80,maxColorValue = 255)'
    ilot <- 'rgb(5,210,210,maxColorValue = 255)'
    iloy <- 'rgb(255,205,45,maxColorValue = 255)'
    ilop <- 'rgb(150,10,85,maxColorValue = 255)'
    ilogre <- 'rgb(140,225,100,maxColorValue = 255)'
    ilogra <- 'rgb(235,245,253,maxColorValue = 255)'
    ilob_h <- eval(parse(text=ilob))
    ilor_h <- eval(parse(text=ilor))
    cols <- c(ilob_h,ilor_h)
    
    ilo_theme <- {bs_theme(
        version = version_default(),
        bootswatch = NULL,
        bg = NULL,
        fg = NULL,
        primary = NULL,
        secondary = NULL,
        success = NULL,
        info = NULL,
        warning = NULL,
        danger = NULL,
        #base_font = font_google("Noto Sans"),
        code_font = NULL,
        heading_font = NULL
    )}
    
}
## Definición para incluir una línea vertical
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
## Definición para incluir una línea horizontal
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
## Definición de función para redondear (pob)
roundup <- function(x){ceiling(x/10^(nchar(x)-1))*10^nchar(x)}
## Definición de letra
fnt <- list(
    family = "noto sans",
    size = 11,
    color = 'black')

dbheader <- dashboardHeader(title = "Indicadores estratégicos")
dbsidebar <- {dashboardSidebar(
    menuItem('Nacional', tabName = 'nacional',
             icon = icon('home'), 
             startExpanded = TRUE, 
             menuSubItem("Economía", tabName = "nac_economía", 
                      icon = icon("piggy-bank")
             ),
             menuSubItem("Empleo", tabName = "nac_empleo", 
                      icon = icon("briefcase"), 
                      selected = TRUE
                      ),
             menuSubItem("Población", tabName = "nac_población", 
                      icon = icon("user")
                      )
    ),
    menuItem('Estatal', tabName = 'estatal',
             icon = icon('home'),
             startExpanded = TRUE,
             selectizeInput(
                 inputId = "estado", 
                 label = "Seleccciona un estado", 
                 choices = unique(ef_l), 
                 selected = "Aguascalientes",
                 multiple = F),
             menuSubItem("Economía", tabName = "economía", 
                      icon = icon("piggy-bank")
                      #selected=TRUE
                      ),
             menuSubItem("Empleo", tabName = "empleo", 
                      icon = icon("briefcase")
                      #selected=TRUE
                      ),
             menuSubItem("Población", tabName = "población", 
                      icon = icon("user")
                      )
    )
)}
dbbody <- {dashboardBody(
    tabItems(
        # Economia (nacional)
        tabItem(tabName = 'nac_economía',
                h1('En construcción'),
                ),
        # Población (nacional)
        tabItem(tabName = 'nac_población',
                h1('En construcción'),
        ),
        # Empleo (nacional)
        tabItem(tabName = 'nac_empleo',
                #h1(textOutput('ef2')),
                fluidRow(
                    tabBox(title = "PEA",
                        #status = 'primary',
                        #background = "blue", 
                        #solidHeader = TRUE,
                        id = 'pea_tabset',
                        tabPanel('PEA',
                                 plotlyOutput('plot_pea', height = 250), 
                                 width = 06
                                 ),
                        tabPanel('Ocupación',
                                 plotlyOutput('plot_pea_ocup', height = 250), 
                                 width = 06
                                 ),
                        tabPanel('Desocupación',
                                 plotlyOutput('plot_pea_desocup', height = 250), 
                                 width = 06
                        )
                        ),
                    box(title = "PNEA",
                        status = 'primary',
                        #background = "blue", 
                        solidHeader = TRUE,
                        plotlyOutput('plot_pnea', height = 250), width = 06)
                )
        ),
        # Economía (estatal)
        tabItem(tabName = 'economía',
                h1(textOutput('ef1')),
                fluidRow(
                    box(title = "PIBE total",
                        status = 'primary',
                        #background = "blue", 
                        solidHeader = TRUE,
                        plotlyOutput('plot_pibe', height = 250), width = 12),
                ),
                fluidRow(
                    box(title = "Crecimiento del PIBE",
                        status = 'primary',
                        #background = "blue", 
                        solidHeader = TRUE,
                        plotlyOutput('plot_pctch', height = 250), width = 6),
                    box(title = "Composición del PIBE",
                        status = 'primary',
                        #background = "blue", 
                        solidHeader = TRUE,
                        plotlyOutput('plot_comp', height = 250), width = 6)
                )),
        # Empleo (estatal)
        tabItem(tabName = 'empleo',
                h1(textOutput('ef2')),
                fluidRow(
                    box(title = "Desocupación",
                        status = 'primary',
                        #background = "blue", 
                        solidHeader = TRUE,
                        plotlyOutput('plot_docup', height = 250), width = 12),
                )
        ),
        # Población (estatal)
        tabItem(tabName = 'población',
                h1(textOutput('ef3')),
                fluidRow(
                    box(title = "Población",
                        status = 'primary',
                        #background = "blue", 
                        solidHeader = TRUE,
                        plotlyOutput('plot_ppob', height = 350), width = 12),
                )
        )
    )
)}

ui <- {dashboardPage(
    skin = 'blue',
    dbheader,
    dbsidebar,
    dbbody
    )
}

server <- function(input, output) {

# Nacional ----------------------------------------------------------------
    # PEA
    output$plot_pea <- renderPlotly({
        plot_ly(data = pea[pea$ind=='PEA_OCUPADA',],
                x = as.Date(pea[pea$ind=='PEA_OCUPADA',]$TIME_PERIOD),
                y = round(pea[pea$ind=='PEA_OCUPADA',]$OBS_VALUE/1000000,2),
                type = 'scatter',
                mode = 'lines+markers',
                colors  = ilob,
                name = 'PEA Ocupada',
                marker = list(size = 5, color = ilob),
                line = list(color=ilob),
                xhoverformat = "%Y-T%q",
                hovertemplate="%{y}, %{x}"
        ) %>% 
            add_trace(x = as.Date(pea[pea$ind=='PEA_DESOCUP',]$TIME_PERIOD),
                      y = round(pea[pea$ind=='PEA_DESOCUP',]$OBS_VALUE/1000000,2),
                      type = 'scatter',
                      mode = 'lines+markers',
                      colors  = ilor,
                      name = 'PEA Desocupada',
                      marker = list(size = 5, color = ilor),
                      line = list(color=ilor),
                      xhoverformat = "%Y-T%q",
                      hovertemplate="%{y}, %{x}") %>% 
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
                                        range          = list(0,60),
                                        zeroline       = FALSE)
            )
    })
    output$plot_pea_ocup <- renderPlotly({
        plot_ly(data = pea_genero[pea_genero$ind=='PEA_OCUPADA_H',],
                x = as.Date(pea_genero[pea_genero$ind=='PEA_OCUPADA_H',]$TIME_PERIOD),
                y = round(pea_genero[pea_genero$ind=='PEA_OCUPADA_H',]$OBS_VALUE/1000000,2),
                type = 'bar',
                #mode = 'lines+markers',
                colors  = ilob,
                name = 'Hombres Ocupados',
                marker = list(color = ilob),
                #line = list(color=ilob),
                xhoverformat = "%Y-T%q",
                hovertemplate="%{y}, %{x}"
        ) %>% 
            add_trace(x = as.Date(pea_genero[pea_genero$ind=='PEA_OCUPADA_M',]$TIME_PERIOD),
                      y = round(pea_genero[pea_genero$ind=='PEA_OCUPADA_M',]$OBS_VALUE/1000000,2),
                      type = 'bar',
                      #mode = 'lines+markers',
                      colors  = ilor,
                      name = 'Mujeres Ocupadas',
                      marker = list(color = ilor),
                      #line = list(color=ilor),
                      xhoverformat = "%Y-T%q",
                      hovertemplate="%{y}, %{x}") %>% 
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
    })
    output$plot_pea_desocup <- renderPlotly({
        plot_ly(data = pea_genero[pea_genero$ind=='PEA_DESOCUP_H',],
                x = as.Date(pea_genero[pea_genero$ind=='PEA_DESOCUP_H',]$TIME_PERIOD),
                y = round(pea_genero[pea_genero$ind=='PEA_DESOCUP_H',]$OBS_VALUE/1000000,2),
                type = 'bar',
                #mode = 'lines+markers',
                colors  = ilob,
                name = 'Hombres Desocupados',
                marker = list(color = ilob),
                #line = list(color=ilob),
                xhoverformat = "%Y-T%q",
                hovertemplate="%{y}, %{x}"
        ) %>% 
            add_trace(x = as.Date(pea_genero[pea_genero$ind=='PEA_DESOCUP_M',]$TIME_PERIOD),
                      y = round(pea_genero[pea_genero$ind=='PEA_DESOCUP_M',]$OBS_VALUE/1000000,2),
                      type = 'bar',
                      #mode = 'lines+markers',
                      colors  = ilor,
                      name = 'Mujeres Desocupadas',
                      marker = list(color = ilor),
                      #line = list(color=ilor),
                      xhoverformat = "%Y-T%q",
                      hovertemplate="%{y}, %{x}") %>% 
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
    })
    # PNEA
    output$plot_pnea <- renderPlotly({
        plot_ly(data = pnea[pnea$ind=='PNEA_DISP',],
                       x = as.Date(pnea[pnea$ind=='PNEA_DISP',]$TIME_PERIOD),
                       y = round(pnea[pnea$ind=='PNEA_DISP',]$OBS_VALUE/1000000,2),
                       type = 'scatter',
                       mode = 'lines+markers',
                       colors  = ilob,
                       name = 'PNEA Disponible',
                       marker = list(size = 5, color = ilob),
                       line = list(color=ilob),
                       xhoverformat = "%Y-T%q",
                       hovertemplate="%{y}, %{x}"
        ) %>% 
            add_trace(x = as.Date(pnea[pnea$ind=='PNEA_NODISP',]$TIME_PERIOD),
                      y = round(pnea[pnea$ind=='PNEA_NODISP',]$OBS_VALUE/1000000,2),
                      type = 'scatter',
                      mode = 'lines+markers',
                      colors  = ilor,
                      name = 'PNEA No Disponible',
                      marker = list(size = 5, color = ilor),
                      line = list(color=ilor),
                      xhoverformat = "%Y-T%q",
                      hovertemplate="%{y}, %{x}") %>% 
            layout(title         = 'Población No Económicamente Activa',
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
                                        range          = list(0,60),
                                        zeroline       = FALSE)
            )
    })
        
# Estatal -----------------------------------------------------------------
    # Generales
    ent <- reactive({ef_c[grep(paste0('^',input$estado,'$'), ef_l)]})
    # Economía
    output$ef1 <- renderText(input$estado)
    ec_no <- reactive({33-which(tot$ef==ef_c[grep(paste0('^',input$estado,'$'), 
                                                  ef_l)])})
    ## PIBE
    output$pibet <- renderText({paste('De acuerdo con INEGI, el estado de',
                                      input$estado, 'es la econonomía número', 
                                      ec_no(),'de México.')})
    output$plot_pibe <- renderPlotly({
        co <- rep(ilob, 32)
        co[33-ec_no()] <- ilot
        plot_ly(
            y = tot$ef,
            x = tot$OBS_VALUE,
            type = 'bar', 
            orientation = 'h',
            marker = list(color = co)
        ) %>%  
            layout(font=fnt, 
                   title = paste("PIBE",max(tot$TIME_PERIOD), 
                                 '(A precios constantes 2013, mdp)'),
                   shapes=vline(mean(tot$OBS_VALUE)),
                   paper_bgcolor = 'rgb(255,255,255)', 
                   plot_bgcolor  = 'rgb(229,229,229)',
                   xaxis         = list(title          = 'PIBE (precios constantes 2013, mdp)',
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
                                       format(round(mean(tot$OBS_VALUE),2), 
                                              big.mark=','), 'mdp'), 
                            textfont = list(family = "noto sans",
                                            size = 14), 
                            x =  mean(tot$OBS_VALUE)+500000, 
                            y = 0.5, 
                            xref='plot',
                            yref='paper',
                            position = 'right',
                            showarrow = FALSE,
                            font = list(color = ilor,
                                        family = 'noto sans')
            )
    })
    ## Tasa de crecimiento del PIBE
    output$pibe_tct <- renderText({
        ej_f <- max(pibe_tc$TIME_PERIOD)
        tc_p <- mean(pibe_tc$pct_change[pibe_tc$ef==ent()&
                                            (pibe_tc$TIME_PERIOD %in% ej_f:(ej_f-4))]) %>%
            round(.,2)
        cr <- ifelse (tc_p>0,'crecido','disminuido')
        paste0('En los últimos 5 años, el PIBE del estado de ', ent(), ' ha ',
               cr, ' a una tasa real anual promedio de ', tc_p,'%')
    })
    output$plot_pctch <- renderPlotly({
        plot_ly(data = pibe_tc[pibe_tc$ef==ent(),],
                x = ~TIME_PERIOD,
                y = ~round(pct_change,2),
                type = 'scatter',
                mode = 'lines+markers',
                line = list(color=ilob),
                name = paste0(ent(),': Tasa de crecimiento real del PIBE (%)'),
                marker = list(size = 10, color=ilob)
        ) %>% 
            layout(font=fnt,
                   title = paste0(ent(),': Tasa de crecimiento real anual del PIBE (%)'),
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
    })
    ## Composición del PIBE
    output$pibe_compt <- renderText({
        ej_fisc <- max(pibe_comp$TIME_PERIOD)
        v <- pibe_comp[pibe_comp$ef==ent()&pibe_comp$TIME_PERIOD==ej_fisc,
                       c('primario','secundario','terciario')] %>% as.vector() %>% 
            round(.,2)
        ae_pct <-  v %>% max()
        ae_p <- names(v)[v==ae_pct]
        t <- paste0('El estado se dedica principalmente al sector ', ae_p, 
                    ' el cual representó el ', ae_pct, '% del PIB estatal durante
                 el ejercicio fiscal ', ej_fisc,'.')
        t
    })
    output$plot_comp <- renderPlotly({
        plot_ly(data = pibe_comp[pibe_comp$ef==ent(),],
                x = ~TIME_PERIOD,
                y = ~round(primario,2),
                type = 'bar',
                marker = list(color=ilob),
                name = 'Actividades primarias'
        ) %>% add_trace(y = ~round(secundario,2),
                        marker = list(color=ilor),
                        name = 'Actividades secundarias') %>% 
            add_trace(y = ~round(terciario,2),
                      marker = list(color=ilot),
                      name = 'Actividades terciarias') %>% 
            layout(font=fnt,
                   title         = paste0(ent(),': Composición del PIBE por actividad económica'),
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
    })
    ## DENUE
    denue_dt_estr <- reactive({
        denue_dt_estr <- aggregate(Total~estr, data=subset(denue, ef==ent()), 
                                   FUN=sum)
        denue_dt_estr <- denue_dt_estr[order(denue_dt_estr$Total, decreasing=T),]
        denue_dt_estr$pct <- round(denue_dt_estr$Total/sum(denue_dt_estr$Total)*100 ,
                                   digits=2)
        denue_dt_estr$Total <- format(denue_dt_estr$Total, big.mark=',')
        names(denue_dt_estr) <- c('Estrato','Numero de Unidades Económicas','(%)')
        denue_dt_estr
    })
    output$denue_estr_t <- renderText({
        nue <- sum(denue$Total[denue$ef==ent()]) %>% format(., big.mark=',')
        top_estr_pct <- sum(denue_dt_estr()[1:4,'(%)'])
        
        paste0('De acuerdo con el Directorio Estadístico Nacional de Unidades 
             Económicas (DENUE) de INEGI más reciente, en el estado de ',
               ent(),' existen ',nue, ' unidades económicas. De ellas, ',
               top_estr_pct, '% son micro y pequeñas empresas, es decir que 
             cuentan con menos de 50 empleados.')
    })
    output$denue_estr <- renderDataTable({
        denue_dt_estr()
    })
    output$denue_ae <- renderDataTable({
        df <- aggregate(Total~def, data=subset(denue, ef==ent()), FUN=sum)
        df <- df[order(df$Total, decreasing=T),]
        df$pct <- round(df$Total/sum(df$Total)*100 ,digits=2)
        df$Total <- format(df$Total, big.mark=',')
        names(df) <- c('Actividad Económica','Número de Unidades Económicas','(%)')
        df
    })
    # Mercado laboral
    output$ef2 <- renderText(input$estado)
    output$plot_docup <- renderPlotly({
        plot_ly(data = docup[docup$ef==ent(),],
                x = ~as.Date(as.yearqtr(TIME_PERIOD, '%Y Q%q')),
                y = ~round(OBS_VALUE,2),
                type = 'scatter',
                mode = 'lines+markers',
                line = list(color=ilob),
                name = paste0(ent(),': Tasa de desocupación trimestral (%)'),
                marker = list(size = 10, color=ilob),
                hovertemplate="%{y}, %{x}"
        ) %>% 
            layout(font=fnt,
                   title         = paste0(ent(),': Tasa de desocupación trimestral (%)'),
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
    })
    # Población
    output$ef3 <- renderText(input$estado)
    output$plot_pob <- renderPlotly({
        carpeta_shp <- paste0(getwd(),'/Mapa/AGE/conjunto de datos')
        
        
        shape_estados <- readOGR(dsn = carpeta_shp, 
                                 layer = "01_32_ent",
                                 #  encoding = "UTF-8",
                                 use_iconv = TRUE)
        
        shape_estados <- fortify(shape_estados,by="CVE_ENT")
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
                             group=group, 
                             color="black",
                             fill=Población)) +
            theme_void() + 
            theme(plot.title = element_text(size=12),
                  legend.key.size = unit(0.5, "cm"),
                  legend.position = c(0.8, 0.7)) + 
            scale_color_manual(values=c("black")) +
            guides(color = FALSE) +
            labs(title = "Población (millones de personas)",
                 fill = "Población",
                 caption = "Fuente: Censo de población INEGI 2020") 
        
        mp <- ggplotly(mp, tooltip = c('Estado','Población'))
        mp
        
    })
    output$plot_ppob <- renderPlotly({
        ## Definición para redondeo
        roundup <- function(x){ceiling(x/10^(nchar(x)-1))*10^nchar(x)}
        ## Orden de edades
        ppob$edad <- factor(ppob$edad,
                            levels=c("0 a 4 años","5 a 9 años","10 a 14 años",
                                     "15 a 19 años","20 a 24 años","25 a 29 años",
                                     "30 a 34 años","35 a 39 años","40 a 44 años",
                                     "45 a 49 años","50 a 54 años","55 a 59 años",
                                     "60 a 64 años","65 a 69 años","70 a 74 años",
                                     "75 a 79 años","80 a 84 años","85 a 89 años",
                                     "90 a 94 años","95 a 99 años","100 años y más",
                                     "edad no especificada")
        )
        ## Gráfico
        df_e <- ppob[ppob$ef==ent(),]
        df_e[df_e$sexo=='Hombres',]$pob <- -1*df_e[df_e$sexo=='Hombres',]$pob
        ma <- max(df_e$pob) %>% roundup(.)
        g <- ggplot(df_e, aes(x = edad,
                              y = pob,
                              fill = sexo)) +
            geom_col(width = 0.5) +
            scale_fill_manual(values=cols) +
            coord_flip() + 
            theme(axis.text.x = element_text(angle = 90, 
                                             vjust = 0.5, hjust=1)) +
            labs(y='Población',x='Cohorte',fill='sexo', 
                 title = paste0(ent(),': Población 2020')) +
            scale_y_continuous(
                breaks = c(seq(-ma, -20000,by = 20000), 
                           sort(seq(0,ma,by = 20000),decreasing=F)),
                labels = c(-seq(-ma, -20000,by = 20000), 
                           sort(seq(0,ma,by = 20000),decreasing=F)) 
            ) 
        g <- ggplotly(g, tooltip = c('edad','sexo', 'y'))
        g
    })
    # Administración Pública
    output$ef4 <- renderText(input$estado)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
