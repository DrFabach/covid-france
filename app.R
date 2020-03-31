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

URL <- getURL("https://www.data.gouv.fr/fr/datasets/r/eceb9fb4-3ebc-4da3-828d-f5939712600a")
URL<-URL%>%paste(collapse = "")%>%stringr::str_extract("href=.*?csv")%>%gsub("href=\"","",.)
URL<-getURL(URL)
data1 <- read.csv(text = URL, check.names = F,stringsAsFactors = F)
data1<-data1%>%filter(sursaud_cl_age_corona == 0)%>%select(dep,nbre_hospit_corona, date_de_passage)
data1$date_de_passage<-as.Date(data1$date_de_passage)
data1<-data1%>%group_by(dep)%>% mutate(nbre_hospit_corona= cumsum(nbre_hospit_corona))

URL <- getURL("https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7")
URL<-URL%>%paste(collapse = "")%>%stringr::str_extract("href=.*?csv")%>%gsub("href=\"","",.)
URL<-getURL(URL)

data <- read.csv2(text = URL, check.names = F,stringsAsFactors = F)
data<-data%>%filter(sexe == 0)%>%select(-sexe)


data<- unique(data)
# Ajout des dates manquantes
dates<-unique(as.Date(data$jour,format="%Y-%m-%d"))
data_cas<-data1%>%as.data.frame()%>%select(dep,jour = date_de_passage,hosp=nbre_hospit_corona)%>%pivot_wider(id_cols =  dep, names_from = jour, values_from = hosp)
data_dec<-data%>%select(dep,jour,dc)%>%pivot_wider(id_cols =  dep, names_from = jour, values_from = dc)
data_rea<-data%>%select(dep,jour,rea)%>%pivot_wider(id_cols =  dep, names_from = jour, values_from = rea)





data_cas[,2]<-ifelse(is.na(data_cas[,2]),0,data_cas[,2]%>%unlist)
data_dec[,2]<-ifelse(is.na(data_dec[,2]),0,data_dec[,2]%>%unlist)
data_rea[,2]<-ifelse(is.na(data_rea[,2]),0,data_rea[,2]%>%unlist)

for(i in 3: dim(data_cas)[2]) data_cas[,i]<- ifelse(is.na(data_cas[,i]), data_cas[,i-1]%>%unlist,data_cas[,i]%>%unlist)
for(i in 3: dim(data_dec)[2]) data_dec[,i]<- ifelse(is.na(data_dec[,i]), data_dec[,i-1]%>%unlist,data_dec[,i]%>%unlist)
for(i in 3: dim(data_rea)[2]) data_rea[,i]<- ifelse(is.na(data_rea[,i]), data_rea[,i-1]%>%unlist,data_rea[,i]%>%unlist)


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



#population<- read.csv2("pop.csv",stringsAsFactors = F)
# save(population, file="pop.RData")
load("pop.RData")


datamaille_code<-function(data=data_cas) return(data)
jour<-names(data_cas%>%select(contains( "-")))
jourDate<- as.Date(jour)
jour2<-names(data_dec%>%select(contains( "-")))
jourDate2<- as.Date(jour2)

names(data_cas)[str_detect(names(data_cas), "-")]<-format.Date(jourDate, "%m/%d/%y")

names(data_dec)[str_detect(names(data_dec), "-")]<-format.Date(names(data_dec%>%select(contains( "-")))%>%as.Date(), "%m/%d/%y")
names(data_rea)[str_detect(names(data_rea), "-")]<-format.Date(names(data_rea%>%select(contains( "-")))%>%as.Date(), "%m/%d/%y")


dateMin1<- min(jourDate)
dateMin<-dateMin1
dateMin2<- min(jourDate2)
countries$code_insee
data_cas<-left_join(population,data_cas,
                    by=c( "maille_code"="dep"))

data_dec<-left_join(population,data_dec,
                    
                    by=c( "maille_code"="dep"))

data_rea<-left_join(population,data_rea,
                    
                    by=c( "maille_code"="dep"))


arrondi<- function(x) 10^(ceiling(log10(x)))


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
               .leaflet-container { background: 	#F5F5F5; }
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
                selectInput("choices", "Statistique ?", choices = c("Hospitalisations Cumulées" = "Hospitalisation",
                                                                    "Décès Cumulés"= "Décès", "Cas en réanimation"="Cas en réanimation" 
                                                                    # "Cas Hosbitalisatés"="Cas"
                                                                    ),selected = "Hospitalisation"),
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
  
  output$Slider<-renderUI({
    
    if(is.null(input$variable)){
      
    }else{
      

      
      if(input$variable %in% c("Nombre total de cas", "Nombre total de cas/population")){
        sliderInput("day1", "Jour", dateMin(), max(jourDate),
                    value =  c(max(jourDate)),animate = T, step = 1
                    
                   
        )}else{
          sliderInput("day2", "Jour", dateMin(), max(jourDate),
                      value =  c(max(jourDate)-2,max(jourDate)),animate = T, step = 1
                   
          )
          
        }
    }
  })
  
  dateMin<- reactive({
  if(input$choices=="Hospitalisation"){
    
    dateMin<- dateMin1
  }else{
    dateMin<-  dateMin2 
  }
    dateMin
  })
  datamaille_code<- reactive({
    if(!is.null(input$choices)){
      if(input$choices == "Hospitalisation"){
        return( data_cas)
        
      }else if(input$choices == "Décès" ){
        return(
          data_dec)
      }else{
        
        return(data_rea)
      }
      }
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
  
  
  pal <- reactive(colorNumeric(c("#FFFFFFFF" ,rev(inferno(256))), domain = c(0,log(arrondi(maxTotal()))), 
                               na.color = "#c6ff45"))
  
  pal2 <- reactive(colorNumeric(c("#FFFFFFFF" ,rev(inferno(256))), domain = c(0,log(arrondi(maxTotalPrevalence()))), 
                                na.color = "#c6ff45"))
  
  observe({

    casesDeath<- (input$choices)
    if (!is.null(input$day1)) {
      if(input$day1>=dateMin()){
        indicator<-format.Date(input$day1, "%m/%d/%y")
      }else{
        indicator<-format.Date(dateMin(), "%m/%d/%y")
      }
      
    }else{
      indicator = format.Date(max(jourDate), "%m/%d/%y")
    }
    
    
    if (!is.null(input$day2)) {
      indicator2<-input$day2
      if(indicator2[1]<dateMin()){

        indicator2[1]<-dateMin()+2
      }
      if(indicator2[2]<dateMin()){
        
        indicator2[2]<-dateMin()
      }
      indicator2<-format.Date(indicator2, "%m/%d/%y")
      
    }else{
      
      indicator2 =format.Date(c(dateMin()-1,max(jourDate)), "%m/%d/%y")
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
                                "Nombre total d'hospitalisation/population :",
                                
                                
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
        if(indicator2[1] == format.Date(dateMin()-1, "%m/%d/%y")){
          
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
                                casesDeath," par période:",
                                
                                
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
        if(indicator2[1] == format.Date(dateMin()-1, "%m/%d/%y")){
          
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
                                casesDeath," par période / habitants :",
                                
                                
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
  
  
  output$selection <- renderUI({
    if(input$choices =="Hospitalisation"){
      radioButtons("variable", choices =  c("Nombre Cumulé d'Hospitalisations par période"="Nouveaux cas par période",
                                            "Nombre Cumulé d'Hospitalisations par période /habitants"="Nouveaux cas par période/population",
                                            "Nombre Cumulé d'Hospitalisations"="Nombre total de cas",
                                            "Nombre Cumulé d'Hospitalisations /habitants"='Nombre total de cas/population' ),
                   label = "Indicateur")
    }else if(input$choices =="Décès"){
      radioButtons("variable", choices =   c("Décès par période"="Nouveaux cas par période",
                                             "Décès par période /habitants"="Nouveaux cas par période/population",
                                             "Total Décès"="Nombre total de cas",
                                             'Total Décès /habitants'='Nombre total de cas/population' ),
                   label = "Indicateur")
      
      
    }else{
      radioButtons("variable", choices =   c("Evolution du nombre d'hospitalisations en réa par période"="Nouveaux cas par période",
                                             "Evolution du nombre d'hospitalisations en réa par période /habitants"="Nouveaux cas par période/population",
                                             "Nombre Hospitalisations en réa à un jour donné"="Nombre total de cas",
                                             'Nombre Hospitalisations en réa à un jour donné /habitants'='Nombre total de cas/population' ),
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
        actionButton("reset", "Reset Graphique"),
        actionButton("clear", "Effacer toutes les traces")
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
      
      #absolutePanel(id = "mobile",class = "panel panel-mobile",top  = 10, right  = 10, HTML("<a href='https://thibautfabacher.shinyapps.io/covid-19-m/'>Mobile Version</a>"))
      
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
<p><li>Source des données : <a href='https://www.data.gouv.fr/fr/datasets/donnees-relatives-a-lepidemie-de-covid-19/' target='_blank'>Data.gouv</a></li>
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
