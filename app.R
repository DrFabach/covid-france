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

# Remplace les NA par donnee prec
for(i in 1:nrow(data_cas)){
  for(j in 3:ncol(data_cas)){
    if(is.na(data_cas[i,j])){
    data_cas[i,j]<-data_cas[i,j-1]
    }
    if(is.na(data_dec[i,j])){
    data_dec[i,j]<-data_dec[i,j-1]
    }
  }
}

# Remplace par 0 si pas de prec
data_cas[is.na(data_cas)]<-0
data_dec[is.na(data_dec)]<-0


# names(data)
# url <- "https://twitter.com/intent/tweet?url=https://thibautfabacher.shinyapps.io/covid-19"
# 

# https://www.data.gouv.fr/fr/datasets/contours-des-departements-francais-issus-d-openstreetmap/

countries <- readOGR(dsn ="departements-20140306-5m-shp",
                     encoding = "utf-8",use_iconv = T,
                     verbose = FALSE)

# save(countries, file="shapeFile.RData")
# load("shapeFile.RData")

#https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population



population<- read.csv2("pop.csv",stringsAsFactors = F)

population$pays<-as.character(unique(countries$NAME)[charmatch(population$Country,unique(countries$NAME))])



dataPays<-function(data=data_cas) return(data)
jour<-names(data_cas%>%select(contains( "-")))
jourDate<- as.Date(jour)
names(data_cas)[str_detect(names(data_cas), "-")]<-format.Date(jourDate, "%m/%d/%y")
names(data_dec)[str_detect(names(data_dec), "-")]<-format.Date(jourDate, "%m/%d/%y")


countries$code_insee
data_cas<-left_join(data.frame(Pays = countries$NAME%>%as.character(), Pop =countries$POP_EST%>%as.character()%>%as.numeric()),data_cas)

data_dec<-left_join(data.frame(Pays = countries$NAME%>%as.character(), Pop =countries$POP_EST%>%as.character()%>%as.numeric()),data_dec)

countries2 <- merge(countries,
                    dataPays(),
                    by.x = "NAME",
                    by.y = "Pays",
                    sort = FALSE)

countries2 <- merge(countries,
                   data_cas,
                    by.x = "nom",
                    by.y = "maille_code",
                    sort = FALSE)


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
         checkboxInput("plotEvolT", "Show Evolution",F)
  ),
  column(2, br(),checkboxInput("credits", "Credits", FALSE)),  
 
  
  
  absolutePanel(id = "input_date_control",class = "panel panel-default",bottom = 60, left = 10, draggable = F,
                selectInput("choices", "Cases or Deaths ?", choices = c("Cases","Deaths"),selected = "Cases"),
                uiOutput("Slider"),
                helpText("The detail of each country can be obtained by clicking on it."), 
                uiOutput("selection"),
                checkboxInput("legend", "Show legend", TRUE)
                
  ),
  uiOutput("Credits"),
  uiOutput("plotEvol"),
  absolutePanel(id = "name",class = "panel panel-title",top  = 10, left  = 100, HTML("<h1>COVID-19 outbreak</h1>"),draggable = T),
  absolutePanel(id = "mobile",class = "panel panel-mobile",top  = 10, right  = 10, HTML("<a href='https://thibautfabacher.shinyapps.io/covid-19-m/'>Mobile Version</a>"))
)

server <- function(input, output, session) {
  
  
  dataPays<- reactive({
    if(!is.null(input$choices)){
      if(input$choices == "Cases"){
        return( data_cas)
        
      }else{
        return(
          data_dec)
      }}
  })
  
  maxTotal<- reactive( max(dataPays()%>%select(-Pop)%>%select_if(is.numeric), na.rm = T)
  )
  maxTotalPrevalence<- reactive(max(dataPays()%>%select(-Pop)%>%select_if(is.numeric)%>%mutate_all(function(x) x/dataPays()$Pop*100000), na.rm = T)
  )
  # 
  # 
  Top5<-reactive( unique(c(dataPays()$Pays[order(dataPays()[,dim(dataPays())[2]]%>%unlist(),decreasing = T)][1:5]
  ,"France")))
  # 
  
  #
  #
  output$map <- renderLeaflet({
    
    
    
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    
    leaflet(data = countries) %>%
      
      setView(0, 30, zoom = 3)
    
    
  })
  
  
  pal <- reactive(colorNumeric(c("#FFFFFFFF" ,rev(inferno(256))), domain = c(0,log(arrondi(maxTotal())))))
  
  pal2 <- reactive(colorNumeric(c("#FFFFFFFF" ,rev(inferno(256))), domain = c(0,log(arrondi(maxTotalPrevalence())))))
  
  observe({
    casesDeath<- ifelse(input$choices == "Cases","Cases","Deaths")
    if (!is.null(input$day1)) {
      indicator<-format.Date(input$day1, "%m/%d/%y")
      
    }else{
      indicator = format.Date(max(jourDate), "%m/%d/%y")
    }
    
    
    if (!is.null(input$day2)) {
      indicator2<-format.Date(input$day2-c(1,0), "%m/%d/%y")
      
    }else{
      indicator2 =format.Date(c(min(jourDate)-1,max(jourDate)), "%m/%d/%y")
    }
    
    if(is.null(input$variable)){
      
    }else{
      variable<- input$variable
      
      if(variable =="Total cases/population"){
        # nCases
        countries2 <- merge(countries,
                            dataPays(),
                            by.x = "NAME",
                            by.y = "Pays",
                            sort = FALSE)
        country_popup <- paste0("<strong>Country: </strong>",
                                countries2$NAME,
                                "<br><strong>",
                                "Total cases/population :",
                                
                                
                                " </strong>",
                                round(countries2[[indicator]]/countries2$Pop*100000,2)," /100 000")
        
        
        leafletProxy("map", data = countries2)%>%
          addPolygons(fillColor = pal2()(log((countries2[[indicator]]/countries2$Pop*100000)+1)),
                      layerId = ~NAME,
                      fillOpacity = 1,
                      color = "#BDBDC3",
                      weight = 1,
                      popup = country_popup)
        
      }else if(variable =="Total cases"){
        countries2 <- merge(countries,
                            dataPays(),
                            by.x = "NAME",
                            by.y = "Pays",
                            sort = FALSE)
        country_popup <- paste0("<strong>Country: </strong>",
                                countries2$NAME,
                                "<br><strong>",
                                "Total ",casesDeath," :",
                                
                                
                                " </strong>",
                                round(countries2[[indicator]],2))
        
        
        leafletProxy("map", data = countries2)%>%
          addPolygons(fillColor = pal()(log((countries2[[indicator]])+1)),
                      fillOpacity = 1,
                      layerId = ~NAME,
                      color = "#BDBDC3",
                      weight = 1,
                      popup = country_popup)
        
        
      }else if(variable =="New cases over period"){
        
        dataPaysSel<-dataPays()%>%select(Pays, Pop)
        if(indicator2[1] == format.Date(min(jourDate)-1, "%m/%d/%y")){
          
          dataPaysSel$ncases<-dataPays()[,indicator2[2]]
        }else{
          dataPaysSel$ncases<-dataPays()[,indicator2[2]]-dataPays()[,indicator2[1]]
          
        }
        
        # nCases
        countries2 <- merge(countries,
                            dataPaysSel,
                            by.x = "NAME",
                            by.y = "Pays",
                            sort = FALSE)
        country_popup <- paste0("<strong>Country: </strong>",
                                countries2$NAME,
                                "<br><strong>",
                                "New ",casesDeath," over period :",
                                
                                
                                " </strong>",
                                countries2$ncases)
        
        leafletProxy("map", data = countries2)%>%
          addPolygons(fillColor = pal()(log(countries2$ncases+1)),
                      fillOpacity = 1,
                      color = "#BDBDC3",
                      layerId = ~NAME,
                      weight = 1,
                      popup = country_popup)
      }else{
        
        dataPaysSel<-dataPays()%>%select(Pays, Pop)
        if(indicator2[1] == format.Date(min(jourDate)-1, "%m/%d/%y")){
          
          dataPaysSel$ncases<-dataPays()[,indicator2[2]]
        }else{
          dataPaysSel$ncases<-dataPays()[,indicator2[2]]-dataPays()[,indicator2[1]]
          
        }
        
        # nCases
        countries2 <- merge(countries,
                            dataPaysSel,
                            by.x = "NAME",
                            by.y = "Pays",
                            sort = FALSE)
        country_popup <- paste0("<strong>Country: </strong>",
                                countries2$NAME,
                                "<br><strong>",
                                "New ",casesDeath," over period / population :",
                                
                                
                                " </strong>",
                                round(countries2$ncases/countries2$Pop*100000,2)," /100 000")
        
        leafletProxy("map", data = countries2)%>%
          addPolygons(fillColor = pal2()(log(countries2$ncases/countries2$Pop*100000+1)),
                      fillOpacity = 1,
                      color = "#BDBDC3",
                      layerId = ~NAME,
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
        if(variable %in% c("Total cases/population","New cases over period/population")){
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
      if(input$variable %in% c("Total cases", "Total cases/population")){
        sliderInput("day1", "Day", min(jourDate), max(jourDate),
                    value =  c(max(jourDate)),animate = T, step = 1
                    
                    #min(jourDate),
        )}else{
          sliderInput("day2", "Day", min(jourDate), max(jourDate),
                      value =  c(max(jourDate)-7,max(jourDate)),animate = T, step = 1
                      
                      #min(jourDate),
          )
          
        }
    }
  })
  
  output$selection <- renderUI({
    if(input$choices =="Cases"){
      radioButtons("variable", choices =  c("New cases over period",
                                            "New cases over period/population","Total cases", 'Total cases/population' ),
                   label = "Indicator")
    }else{
      radioButtons("variable", choices =  list("Deaths over period"="New cases over period",
                                               "Deaths over period/population"="New cases over period/population",
                                               "Total deaths"="Total cases",
                                               'Total deaths/population'='Total cases/population' ),
                   label = "Indicator")
      
      
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
    
    if(input$variable %in% c("Total cases/population","Total cases")){
      df_evo<- dataPays()%>%filter(Pays%in% trace$data)%>%pivot_longer(cols = -c(Pays,Pop),
                                                                       values_to = "Cases",names_to = "Date")%>%
        mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
      
      if(input$variable=="Total cases/population"){
        
        plot_ly(data = df_evo,x = ~Date, y = ~Cases/Pop*100000, color = ~Pays, type = "scatter",mode = "lines")%>%
          layout(yaxis = list( title = paste(input$choices,"/ 100 000")))
        
      }else{
        
        
        
        plot_ly(data = df_evo,x = ~Date, y = ~Cases, color = ~Pays, type = "scatter",mode = "lines")%>%
          layout(yaxis = list( title = input$choices))
        
      }
    }else{
      df_evo<- dataPays()%>%filter(Pays%in% trace$data)
      
      
      
      for(i in dim( df_evo)[2]:4)  df_evo[i]<- df_evo[i]- df_evo[i-1]
      
      
      df_evo<- df_evo%>%pivot_longer(cols = -c(Pays,Pop),
                                     values_to = "Cases",names_to = "Date")%>%
        mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
      
      
      
      if( input$variable=="New cases over period/population"){
        
        plot_ly(data = df_evo,x = ~Date, y = ~Cases/Pop*100000, color = ~Pays, type = "scatter",mode = "lines")%>%
          layout(yaxis = list( title = paste(input$choices,"/ 100 000/day")))
        
      }else{
        
        plot_ly(data = df_evo,x = ~Date, y = ~Cases, color = ~Pays, type = "scatter",mode = "lines")%>%
          layout(yaxis = list( title = paste(input$choices,"/day")))
        
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
    
    
    if(input$variable %in% c("Total cases/population","Total cases")){
      
      
      
      
      df_evo<- dataPays()%>%filter(Pays%in% Top5())%>%pivot_longer(cols = -c(Pays,Pop),
                                                                   values_to = "Cases",names_to = "Date")%>%
        mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
      
      
      if(input$variable=="Total cases/population"){
        
        for (i in Top5()){
          df_evoi<- df_evo%>%filter(Pays == i)
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
          df_evoi<- df_evo%>%filter(Pays == i)
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
      
      
      
      
      df_evo<- dataPays()%>%filter(Pays%in% Top5())

      for(i in  dim(df_evo)[2]:4) df_evo[i]<-df_evo[i]-df_evo[i-1]
      
      
      df_evo<-df_evo%>%pivot_longer(cols = -c(Pays,Pop),
                                    values_to = "Cases",names_to = "Date")%>%
        mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
      
      if( input$variable=="New cases over period/population"){
        
        for (i in Top5()){
          df_evoi<- df_evo%>%filter(Pays == i)
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
          df_evoi<- df_evo%>%filter(Pays == i)
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
      
      if(input$variable %in% c("Total cases/population","Total cases")){
        df_click<- dataPays()%>%filter(Pays%in% country_Click)%>%pivot_longer(cols = -c(Pays,Pop),
                                                                              values_to = "Cases",names_to = "Date")%>%
          mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
        
        if(input$variable=="Total cases/population"){
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
        
        df_click<- dataPays()%>%filter(Pays%in% country_Click)
        
        
        

        for(i in  dim( df_click)[2]:4)  df_click[i]<- df_click[i]- df_click[i-1]
        
        
        df_click<- df_click%>%pivot_longer(cols = -c(Pays,Pop),
                                           values_to = "Cases",names_to = "Date")%>%
          mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
        
        
        
        if( input$variable=="New cases over period/population"){
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
<p> <li><a href='https://coronavirus.jhu.edu/map.html'>Coronavirus COVID-19 Global Cases map Johns Hopkins University</a></li>
  <li>COVID-19 Cases : <a href='https://github.com/CSSEGISandData/COVID-19' target='_blank'>Github Johns Hopkins University</a></li>
  <li>World population : <a href='https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population' target='_blank'>Wikipedia</a></li>
  <li>Shapefile : <a href='https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2/' target='_blank'>Natural Earth Data</a></li>
 <li> <a href ='https://github.com/DrFabach/Corona' target='_blank'>Code on Github </a></li>
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