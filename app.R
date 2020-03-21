#Test1



library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(RCurl)
library(plotly)
library(viridis)
library(tidyverse)
library(lubridate)

variable <-F

URL <- getURL("https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv")
data <- read.csv(text = URL, check.names = F,stringsAsFactors = F)
data<- data%>%filter(granularite == "departement")%>%select(- source_nom,-source_url,-granularite)
data<- unique(data)
data[is.na(data)]<-0
data<- data%>%group_by(date, maille_code)%>%summarise(cas_confirmes = max(cas_confirmes), deces = max(deces))
data$maille_code<-gsub("DEP-","",data$maille_code)
# Ajout des dates manquantes
dates<-unique(as.Date(data$date,format="%Y-%m-%d"))
datemiss<-NULL

# Creer vecteurs de toutes les dates
for(i in 1:(length(dates)-1)){
  jour<-day(dates[i])
  mois<-month(dates[i])
  diff<-as.numeric(difftime(dates[i+1],dates[i],units="days"))
  if(diff>1){
    for(j in 1:(diff-1)){
      if(mois==1&jour==31){mois<-2;jour<-0}
      jour<-jour+1
      jd<-as.character(jour)
      md<-as.character(mois)
      if(nchar(jd)==1){jd<-paste("0",jd,sep="")}
      if(nchar(md)==1){md<-paste("0",md,sep="")}
      dat<-paste("2020-",md,"-",jd,sep="")
      datemiss<-c(datemiss,dat)
    }
  }
}

# Fusion des nouvelles dates
n<-length(datemiss)
comp<-data.frame(datemiss,rep(NA,n),rep(NA,n),rep(NA,n))
names(comp)<-names(data)
data<-merge.data.frame(data,comp,all=TRUE)

# Separe en deux bases
data_cas<-data[,c("date","maille_code","cas_confirmes")]
data_dec<-data[,c("date","maille_code","deces")]

# Mise au bon format
data_cas<-pivot_wider(data_cas, id_cols = "maille_code",names_from = "date",values_from = "cas_confirmes")
data_dec<-pivot_wider(data_dec, id_cols = "maille_code",names_from = "date",values_from = "deces")

# Remplace les NA par donnee prec ou 0
for(i in 1:nrow(data_cas)){
  for(j in 2:ncol(data_cas)){
    if(is.na(data_cas[i,j])){
      data_cas[i,j]<-0
    }
    if(is.na(data_dec[i,j])){
      data_dec[i,j]<-0
    }
    data_cas[i,j]<-max(data_cas[i,2:j],na.rm=TRUE)
    data_dec[i,j]<-max(data_dec[i,2:j],na.rm=TRUE)
  }
}

data<- data%>%group_by(date, maille_code)%>%summarise(cas_confirmes = max(cas_confirmes), deces = max(deces))
data$date<- as.Date(data$date, "%Y-%m-%d")
data<- data %>%group_by(maille_code)%>%
  
  mutate(cumsumdeath = cumsum(deces)) 

# names(data)
url <- "https://twitter.com/intent/tweet?url=https://thibautfabacher.shinyapps.io/covid-france"
# 

# https://www.data.gouv.fr/fr/datasets/contours-des-departements-francais-issus-d-openstreetmap/

#countries <- readOGR(dsn ="departements-20140306-5m-shp",
                     # encoding = "utf-8",use_iconv = T,
                     # verbose = FALSE)


# save(countries, file="shapeFile.RData")
load("shapeFile.RData")

#https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population



population<- read.csv2("pop.csv",stringsAsFactors = F)
population$maille_code<-gsub("DEP-","",population$maille_code)
population$population<-gsub("\\s","",population$population)
population$Pop<- as.numeric(population$population)

population$population<- NULL

datamaille_code<-function(data=data_cas) return(data)
jour<-names(data_cas%>%select(contains( "-")))
jourDate<- as.Date(jour)
names(data_cas)[str_detect(names(data_cas), "-")]<-format.Date(jourDate, "%m/%d/%y")
names(data_dec)[str_detect(names(data_dec), "-")]<-format.Date(jourDate, "%m/%d/%y")


dateMin<- as.Date("2020-02-24")

countries$code_insee
data_cas<-left_join(population,data_cas)

data_dec<-left_join(population,data_dec)




arrondi<- function(x) 10^(ceiling(log10(x)))

data_dec[is.na(data_dec)]<- 0
data_cas[is.na(data_cas)]<- 0
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}",
             HTML(  ".panel-default {background-color: rgb(256, 256, 256,0.5);
               padding : 10px;;}
               .panel-title {background-color: rgb(256, 256, 256,0.8);
               padding : 10px;
               border-style: solid;
               border-color: grey;}
               .panel-credits {background-color: rgb(256, 256, 256,1);
               padding : 15px;
               border-style: solid;
               border-color: black;}
              .panel-mobile {background-color: rgb(256, 256, 256,0);
               padding : 15px;
               box-shadow: unset;}
               ")
             
  ),
  leafletOutput("map", width = "100%", height = "93%"),
  column(6,HTML("<b><a href='https://www.linkedin.com/in/thibaut-fabacher'>Dr.Thibaut FABACHER,</a> Pr. N. MEYER, Pr. E.-A. SAULEAU</b></br>
               <i>Groupe Methode en Recherche Clinique  <a href='http://www.chru-strasbourg.fr/'  target ='_blank'> CHRU STRASBOURG</a></br>Laboratoire de Biostatistique <a href='https://icube.unistra.fr/'  target ='_blank'> ICube</a></i>")), 

  column(2,
         br(), 
         actionButton("twitter_share",
                      label = "Share",
                      icon = icon("twitter"),
                      onclick = sprintf("window.open('%s')",url))),
  column(2,br(),
         checkboxInput("plotEvolT", "Afficher évolution",F)
  ),
  column(2, br(),checkboxInput("credits", "Credits", FALSE)),  
 
  
  
  absolutePanel(id = "input_date_control",class = "panel panel-default",bottom = 60, left = 10, draggable = F,
                selectInput("choices", "Nombre de cas ou de décès ?", choices = c("Cas","Décès"),selected = "Cas"),
                uiOutput("Slider"),
                helpText("Le detail de chaque département peut être obtenu en cliquant dessus"), 
                uiOutput("selection"),
                checkboxInput("legend", "Afficher la légende", TRUE)
                
  ),
  uiOutput("Credits"),
  uiOutput("plotEvol"),
  uiOutput("mobile"),
  absolutePanel(id = "name",class = "panel panel-title",top  = 10, left  = 100, HTML("<h1>Epidémie COVID-19</h1>"),draggable = T))

server <- function(input, output, session) {
  
  
  datamaille_code<- reactive({
    if(!is.null(input$choices)){
      if(input$choices == "Cas"){
        return( data_cas)
        
      }else{
        return(
          data_dec)
      }}
  })
  
  maxTotal<- reactive( max(datamaille_code()%>%select(-Pop)%>%select_if(is.numeric), na.rm = T)
  )
  maxTotalPrevalence<- reactive(max(datamaille_code()%>%select(-Pop)%>%select_if(is.numeric)%>%mutate_all(function(x) x/datamaille_code()$Pop*100000), na.rm = T)
  )
  # 
  # 
  Top5<-reactive( unique(c(datamaille_code()$maille_code[order(datamaille_code()[,dim(datamaille_code())[2]]%>%unlist(),decreasing = T)][1:5]
  ,"France")))
  # 
  
  #
  #
  output$map <- renderLeaflet({
    
    
    
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    
    leaflet(data = countries) %>%
      
      setView(-0.5, 47, zoom = 6)
    
    
  })
  
  
  pal <- reactive(colorNumeric(c("#FFFFFFFF" ,rev(inferno(256))), domain = c(0,log(arrondi(maxTotal())))))
  
  pal2 <- reactive(colorNumeric(c("#FFFFFFFF" ,rev(inferno(256))), domain = c(0,log(arrondi(maxTotalPrevalence())))))
  
  observe({
    casesDeath<- ifelse(input$choices == "Cas","Cas","Décès")
    if (!is.null(input$day1)) {
      indicator<-format.Date(input$day1, "%m/%d/%y")
      
    }else{
      indicator = format.Date(max(jourDate), "%m/%d/%y")
    }
    
    
    if (!is.null(input$day2)) {
      indicator2<-format.Date(input$day2-c(1,0), "%m/%d/%y")
      
    }else{
      indicator2 =format.Date(c(dateMin-1,max(jourDate)), "%m/%d/%y")
    }
    
    if(is.null(input$variable)){
      
    }else{
      variable<- input$variable
      
      if(variable =="Nombre total de cas/population"){
        # nCases
        countries2 <- merge(countries,
                            datamaille_code(),
                            by.x = "code_insee",
                            by.y = "maille_code",
                            sort = FALSE)
        country_popup <- paste0("<strong>Country: </strong>",
                                countries2$nom,
                                "<br><strong>",
                                "Nombre total de cas/population :",
                                
                                
                                " </strong>",
                                round(countries2[[indicator]]/countries2$Pop*100000,2)," /100 000")
        
        
        leafletProxy("map", data = countries2)%>%
          addPolygons(fillColor = pal2()(log((countries2[[indicator]]/countries2$Pop*100000)+1)),
                      layerId = ~code_insee,
                      fillOpacity = 1,
                      color = "#BDBDC3",
                      weight = 1,
                      popup = country_popup)
        
      }else if(variable =="Nombre total de cas"){
        countries2 <- merge(countries,
                            datamaille_code(),
                            by.x = "code_insee",
                            by.y = "maille_code",
                            sort = FALSE)
        country_popup <- paste0("<strong>Country: </strong>",
                                countries2$nom,
                                "<br><strong>",
                                "Total ",casesDeath," :",
                                
                                
                                " </strong>",
                                round(countries2[[indicator]],2))
        
        
        leafletProxy("map", data = countries2)%>%
          addPolygons(fillColor = pal()(log((countries2[[indicator]])+1)),
                      fillOpacity = 1,
                      layerId = ~code_insee,
                      color = "#BDBDC3",
                      weight = 1,
                      popup = country_popup)
        
        
      }else if(variable =="Nouveaux cas par période"){
        
        datamaille_codeSel<-datamaille_code()%>%select(maille_code, Pop)
        if(indicator2[1] == format.Date(dateMin-1, "%m/%d/%y")){
          
          datamaille_codeSel$ncases<-datamaille_code()[,indicator2[2]]
        }else{
          datamaille_codeSel$ncases<-datamaille_code()[,indicator2[2]]-datamaille_code()[,indicator2[1]]
          
        }
        
        # nCases
        countries2 <- merge(countries,
                            datamaille_codeSel,
                            by.x = "code_insee",
                            by.y = "maille_code",
                            sort = FALSE)
        country_popup <- paste0("<strong>Country: </strong>",
                                countries2$nom,
                                "<br><strong>",
                                "New ",casesDeath," over period :",
                                
                                
                                " </strong>",
                                countries2$ncases)
        
        leafletProxy("map", data = countries2)%>%
          addPolygons(fillColor = pal()(log(countries2$ncases+1)),
                      fillOpacity = 1,
                      color = "#BDBDC3",
                      layerId = ~code_insee,
                      weight = 1,
                      popup = country_popup)
      }else{
        
        datamaille_codeSel<-datamaille_code()%>%select(maille_code, Pop)
        if(indicator2[1] == format.Date(dateMin-1, "%m/%d/%y")){
          
          datamaille_codeSel$ncases<-datamaille_code()[,indicator2[2]]
        }else{
          datamaille_codeSel$ncases<-datamaille_code()[,indicator2[2]]-datamaille_code()[,indicator2[1]]
          
        }
        
        # nCases
        countries2 <- merge(countries,
                            datamaille_codeSel,
                            by.x = "code_insee",
                            by.y = "maille_code",
                            sort = FALSE)
        country_popup <- paste0("<strong>Country: </strong>",
                                countries2$nom,
                                "<br><strong>",
                                "New ",casesDeath," over period / population :",
                                
                                
                                " </strong>",
                                round(countries2$ncases/countries2$Pop*100000,2)," /100 000")
        
        leafletProxy("map", data = countries2)%>%
          addPolygons(fillColor = pal2()(log(countries2$ncases/countries2$Pop*100000+1)),
                      fillOpacity = 1,
                      color = "#BDBDC3",
                      layerId = ~code_insee,
                      weight = 1,
                      popup = country_popup)
        
        
        
      }
      
      
      
    }
    
  }
  
  
  )
  
  
  
  
  
  observe({
    
    
    if(is.null(input$variable)){
      
    }else{
      variable<- input$variable
      
      proxy <- leafletProxy("map", data = countries)
      
      
      # Remove any existing legend, and only if the legend is
      # enabled, create a new one.
      proxy %>% clearControls()
      if (input$legend) {
        if(variable %in% c("Nombre total de cas/population","Nouveaux cas par période/population")){
          proxy %>% addLegend(position = "bottomright",
                              pal = pal2(),opacity = 1,
                              bins = log(10^(seq(0,log10(arrondi(maxTotalPrevalence())),0.5))),
                              value = log(1:10^(log10(arrondi(maxTotalPrevalence())))),
                              data =log(1:10^(log10(arrondi(maxTotalPrevalence())))),
                              labFormat = labelFormat(transform = function(x) round(exp(x)) ,suffix = " /100 000")
                              
          )
          
        }else{
          
          
          
          proxy %>% addLegend(position = "bottomright",
                              pal = pal(),opacity = 1,
                              bins = log(10^(0:log10(arrondi(maxTotal())))),
                              value = log(1:10^(log10(arrondi(maxTotal())))),
                              data = log(10^(0:log10(arrondi(maxTotal())))),
                              labFormat = labelFormat(transform =  exp )
                              
          )
        }
      }
    }
    
  })
  
  output$Slider<-renderUI({
    
    if(is.null(input$variable)){
      
    }else{
      if(input$variable %in% c("Nombre total de cas", "Nombre total de cas/population")){
        sliderInput("day1", "Jour", dateMin, max(jourDate),
                    value =  c(max(jourDate)),animate = T, step = 1
                    
                    #dateMin,
        )}else{
          sliderInput("day2", "Jour", dateMin, max(jourDate),
                      value =  c(max(jourDate)-7,max(jourDate)),animate = T, step = 1
                      
                      #dateMin,
          )
          
        }
    }
  })
  
  output$selection <- renderUI({
    if(input$choices =="Cas"){
      radioButtons("variable", choices =  c("Nouveaux cas par période",
                                            "Nouveaux cas par période/population","Nombre total de cas", 'Nombre total de cas/population' ),
                   label = "Indicateur")
    }else{
      radioButtons("variable", choices =  list("Deaths over period"="Nouveaux cas par période",
                                               "Deaths over period/population"="Nouveaux cas par période/population",
                                               "Total deaths"="Nombre total de cas",
                                               'Total deaths/population'='Nombre total de cas/population' ),
                   label = "Indicateur")
      
      
    }
    
  })
  output$plotEvol<-renderUI({
    if (input$plotEvolT) {
      tagList(absolutePanel(
        id = "name",
        class = "panel panel-credits",
        top = 10,width = "700px",
        right  = 10,draggable = F,
        plotlyOutput(outputId = "evol",width = "600px"),
        actionButton("reset", "Reset Graph"),
        actionButton("clear", "Clear all traces")
      ))
    }
  })
  
  output$evol <-renderPlotly({
    
    if(input$variable %in% c("Nombre total de cas/population","Nombre total de cas")){
      df_evo<- datamaille_code()%>%filter(maille_code%in% trace$data)%>%pivot_longer(cols = -c(maille_code,Pop),
                                                                       values_to = "Cas",names_to = "Date")%>%
        mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
      
      if(input$variable=="Nombre total de cas/population"){
        
        plot_ly(data = df_evo,x = ~Date, y = ~Cas/Pop*100000, color = ~maille_code, type = "scatter",mode = "lines")%>%
          layout(yaxis = list( title = paste(input$choices,"/ 100 000")))
        
      }else{
        
        
        
        plot_ly(data = df_evo,x = ~Date, y = ~Cas, color = ~maille_code, type = "scatter",mode = "lines")%>%
          layout(yaxis = list( title = input$choices))
        
      }
    }else{
      df_evo<- datamaille_code()%>%filter(maille_code%in% trace$data)
      
      
      
      for(i in dim( df_evo)[2]:4)  df_evo[i]<- df_evo[i]- df_evo[i-1]
      
      
      df_evo<- df_evo%>%pivot_longer(cols = -c(maille_code,Pop),
                                     values_to = "Cas",names_to = "Date")%>%
        mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
      
      
      
      if( input$variable=="Nouveaux cas par période/population"){
        
        plot_ly(data = df_evo,x = ~Date, y = ~Cas/Pop*100000, color = ~maille_code, type = "scatter",mode = "lines")%>%
          layout(yaxis = list( title = paste(input$choices,"/ 100 000/jour")))
        
      }else{
        
        plot_ly(data = df_evo,x = ~Date, y = ~Cas, color = ~maille_code, type = "scatter",mode = "lines")%>%
          layout(yaxis = list( title = paste(input$choices,"/jour")))
        
      }
      
    }
    
  })
  
  trace<- reactiveValues()
  observe({trace$data<-Top5()
  })
  
  observeEvent(input$reset, {
    
    
    
    
    
    for (i in 1: length(trace$data)){
      plotlyProxy("evol", session) %>%
        plotlyProxyInvoke("deleteTraces",list(0))
      
    }
    
    
    if(input$variable %in% c("Nombre total de cas/population","Nombre total de cas")){
      
      
      
      
      df_evo<- datamaille_code()%>%filter(maille_code%in% Top5())%>%pivot_longer(cols = -c(maille_code,Pop),
                                                                   values_to = "Cas",names_to = "Date")%>%
        mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
      
      
      if(input$variable=="Nombre total de cas/population"){
        
        for (i in Top5()){
          df_evoi<- df_evo%>%filter(maille_code == i)
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_evoi$Date ,
                                   name =i ,
                                   y = df_evoi$Cases/df_evoi$Pop*100000,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }
      }else{
        for (i in Top5()){
          df_evoi<- df_evo%>%filter(maille_code == i)
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_evoi$Date ,
                                   name =i ,
                                   y = df_evoi$Cases,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }
      }
    }else{
      
      
      
      
      df_evo<- datamaille_code()%>%filter(maille_code%in% Top5())

      for(i in  dim(df_evo)[2]:4) df_evo[i]<-df_evo[i]-df_evo[i-1]
      
      
      df_evo<-df_evo%>%pivot_longer(cols = -c(maille_code,Pop),
                                    values_to = "Cas",names_to = "Date")%>%
        mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
      
      if( input$variable=="Nouveaux cas par période/population"){
        
        for (i in Top5()){
          df_evoi<- df_evo%>%filter(maille_code == i)
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_evoi$Date ,
                                   name =i ,
                                   y = df_evoi$Cases/df_evoi$Pop*100000,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }
        
      }else{
        for (i in Top5()){
          df_evoi<- df_evo%>%filter(maille_code == i)
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_evoi$Date ,
                                   name =i ,
                                   y = df_evoi$Cases,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }
        
      }
      
    }
    
    trace$data<-Top5()
  })
  
  
  observeEvent(input$clear, {
    for (i in 1: length(trace$data)){
      plotlyProxy("evol", session) %>%
        plotlyProxyInvoke("deleteTraces",list(0))
    }
    trace$data<- NULL
  })
  observeEvent(input$map_shape_click, {
    
    
    country_Click<- input$map_shape_click$id
    if (!country_Click%in%trace$data & input$plotEvolT){
      
      trace$data<-c(trace$data,country_Click)
      
      if(input$variable %in% c("Nombre total de cas/population","Nombre total de cas")){
        df_click<- datamaille_code()%>%filter(maille_code%in% country_Click)%>%pivot_longer(cols = -c(maille_code,Pop),
                                                                              values_to = "Cas",names_to = "Date")%>%
          mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
        
        if(input$variable=="Nombre total de cas/population"){
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_click$Date ,
                                   name =country_Click ,
                                   y = df_click$Cases/df_click$Pop*100000,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }else{
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_click$Date ,
                                   name =country_Click ,
                                   y = df_click$Cases,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }
      }else{
        
        df_click<- datamaille_code()%>%filter(maille_code%in% country_Click)
        
        
        

        for(i in  dim( df_click)[2]:4)  df_click[i]<- df_click[i]- df_click[i-1]
        
        
        df_click<- df_click%>%pivot_longer(cols = -c(maille_code,Pop),
                                           values_to = "Cas",names_to = "Date")%>%
          mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
        
        
        
        if( input$variable=="Nouveaux cas par période/population"){
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_click$Date ,
                                   name =country_Click ,
                                   y = df_click$Cases/df_click$Pop*100000,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }else{
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_click$Date ,
                                   name =country_Click ,
                                   y = df_click$Cases,
                                   type = 'scatter',
                                   mode = 'lines'))  
        }
        
      }
      
      
      
      }
  })
  
  output$mobile <- renderUI({
    
    if(input$plotEvolT){
      
    }else{
      
      absolutePanel(id = "mobile",class = "panel panel-mobile",top  = 10, right  = 10, HTML("<a href='https://thibautfabacher.shinyapps.io/covid-19-m/'>Mobile Version</a>"))
      
    }
    
  })
  output$Credits <- renderUI({
    if (input$credits) {
      tagList(
        absolutePanel(
          id = "name",
          class = "panel panel-credits",
          top = "45%",
          left  = "45%",
          HTML(
            "<h1> Data Source : </h1>
<p><li>Source des données : <a href='https://github.com/opencovid19-fr/data' target='_blank'>https://github.com/opencovid19-fr/data</a></li>
  <li>Population par département : <a href='https://www.insee.fr/fr/statistiques/1893198' target='_blank'>Insee</a></li>
  <li>Shapefile : <a href='https://www.data.gouv.fr/fr/datasets/contours-des-departements-francais-issus-d-openstreetmap/' target='_blank'>Data.gouv</a></li>
 <li> <a href ='https://github.com/DrFabach/covid-france' target='_blank'>Code on Github </a></li>
 <li> <a href = 'https://www.r-project.org/'  target='_blank'>The R Project for Statistical Computing</a></li>
  <li> <a href = 'https://shiny.rstudio.com/' target='_blank'>Shiny R package</a></li>
   <li> <a href = 'https://leafletjs.com/' target='_blank'>Leaflet </a></li>
                                                                                                                           </p>"
          ),
          draggable = T
        )
      )
      
    }
    
  })
  
}

shinyApp(ui, server)
