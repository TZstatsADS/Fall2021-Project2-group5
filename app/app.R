setwd(".")
source("global.R")

if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
if(!require(highcharter)) devtools::install_github("jbkunst/highcharter")
if(!require(RSocrata)) devtools::install_github("Chicago/RSocrata")
if(!require(fontawesome)) devtools::install_github("rstudio/fontawesome")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(ggmap)) install.packages("ggmap", repos = "http://cran.us.r-project.org")
if(!require(googleway)) install.packages("googleway", repos = "http://cran.us.r-project.org")
if(!require(viridis)) install.packages("viridis", repos = "http://cran.us.r-project.org")
if(!require(htmltools)) install.packages("htmltools", repos = "http://cran.us.r-project.org")
# Prepare variables
# Drop down lists for inputs
borough_vars <- c("SELECT A BOROUGH" = "",
                  "BRONX" = "Bronx",
                  "BROOKLYN" = "Brooklyn",
                  "MANHATTAN" = "Manhattan",
                  "QUEENS" = "Queens", 
                  "STATEN ISLAND" = "Staten Island")

# background image address
backgroundpic <- "https://images.theconversation.com/files/319995/original/file-20200311-116236-13j08cg.jpg?ixlib=rb-1.1.0&rect=63%2C229%2C5224%2C2608&q=45&auto=format&w=668&h=324&fit=crop" 



#===============================================Shiny UI=========================================================
ui <- fluidPage(
  #tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
  #titlePanel("Home"),
  navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE,
             title= "",
             id="nav",
             windowTitle = "COVID-19 Homeless Manual 4 New Yorkers",
             header = tagList(
               useShinydashboard()
             ),
             # tab 1 (home page)
             tabPanel('Home',icon = icon("home"), 
                      fluidRow(
                        tags$img(src = backgroundpic, class = "background", height="100%", width="100%", style = "opacity: 0.70"),
                        absolutePanel(id = "foreground", class = "foreground-content",
                                      top = "20%", left = "20%", right = "20%", width = "60%", fixed=FALSE,
                                      draggable = FALSE, height = 300,
                                      fluidRow(style = "padding: 7.5%; background-color: white; text-align: center",
                                               tags$h1("Welcome to the COVID-19 Survival Manual 4 New Yorkers app!", style="font-weight:bold"),tags$br(),
                                               tags$p("The Coronavirus (Covid-19) has so far infected over 100 million people and caused over 2 million deaths globally. In the US, the City of New York has been hit hardest, which many people lost their jobs or went bankrupt due to the devastating blow for the economy, and had restricted access to clean foods and medication. In this project, our goal is to provide a survival guide to help and support the suffering New Yorkers. ", style="font-weight:italic"),tags$br(),
                                               tags$h3("Remember, we are in this together!", style="color:#18bc9c; font-weight:bold"),
                                               tags$h4("We would like to be your source for Coronavirus updates, resources, and trends in New York City.", style="color:#18bc9c")
                                      ),
                                      fluidRow(style = "padding: 7.5%; background-color: white; text-align: center",
                                               tags$img(src = "meli.jpg",  width="100%"),
                                               imageOutput('image')
                                      ),
                                      style = "opacity: 0.95")
                      )
             ),
             # tab 2 (covid-19 tracker)
             tabPanel('COVID-19 Tracker', icon = icon("viruses"),
                      div(class='coverDiv',
                          titlePanel("Latest Data on Coronavirus (COVID-19) Cases in New York City"),
                          span(tags$h5("This page provides up-to-date Covid-19 statistics in NYC including case count, death count, probable count and hospitalized count, as well as a time-series trends plot for a synthetic view.")),
                          
                          fluidRow(
                            # Value Boxes for most recent day
                            column(3, align="center",offset = 1,
                                   valueBoxOutput(outputId = "NewCasesBox",width = 12)
                            ),
                            column(3, align="center",offset = 1,
                                   valueBoxOutput(outputId = "NewProbBox",width = 12)
                            ),
                            column(3, align="center",offset = 1,
                                   valueBoxOutput(outputId = "NewDeathsBox",width = 12)
                            )
                          ),
                          fluidRow(
                            # Value Boxes for cumulative result
                            column(3, align="center",offset = 1,
                                   valueBoxOutput(outputId = "TCasesBox",width = 12)
                            ),
                            column(3, align="center",offset = 1,
                                   valueBoxOutput(outputId = "NewHospitalizedBox",width = 12)
                            ),
                            column(3, align="center",offset = 1,
                                   valueBoxOutput(outputId = "TDeathsBox",width = 12)
                            )
                          ),
                          span(tags$i(h5("Source: ", tags$a(href="https://data.cityofnewyork.us/Health/COVID-19-Daily-Counts-of-Cases-Hospitalizations-an/rc75-m7u3", "NYC Open Data. ")," Data are preliminary and subject to change.", style="font-weight:italic"))),
                          span(tags$i(h6(paste0("Last Update on: ", nyc_latest$date_of_interest[1])))),
                          # plot to compare 5 boroughs
                          span(tags$h2("Covid-19 Overall Situation of the 5 Boroughs in NYC")),
                          
                          fluidRow(
                            column(4, align="center",
                                   highchartOutput("tsnewcase",width = "100%",height = "400px")
                            ),
                            column(4, align="center",
                                   highchartOutput("tsnew",width = "100%",height = "400px")
                            ),
                            column(4, align="center",
                                   highchartOutput("tscum",width = "100%",height = "400px")
                            )
                          ),
                          span(tags$h2("Covid-19 Detailed Situation  of the 5 Boroughs in NYC")),
                          
                          sidebarLayout(position = "left",
                                        sidebarPanel(
                                          h3("NYC Neighborhoods", style="color:#068bd9"),
                                          # select from drop-down lists
                                          selectInput("select_borough", 
                                                      label = NULL, 
                                                      choices = borough_vars, 
                                                      selected = borough_vars[1]),
                                          # select from checkbox
                                          h3("Select time series", align = "left", style="color:#068bd9"),
                                          checkboxInput("daily", label = "Daily", value = TRUE),
                                          checkboxInput("weekly", label = "Weekly", value = FALSE),tags$br(),
                                          
                                          h3("Select the topic(s) to see trends over time", align = "left", style="color:#068bd9"),
                                          checkboxInput("casesummary", label = "Cases", value = TRUE),
                                          checkboxInput("deathsummary", label = "Deaths", value = FALSE),
                                          checkboxInput("hospsummary", label = "Hospitalization", value = FALSE),tags$br(),
                                          h4("Instructions for using the plot:", align = "left"),
                                          h5("1. Select a NYC borough from the drop-down list;"),
                                          h5("2. Select a time series;"),
                                          h5("3. Select the topic(s) to plot trends;"),
                                          h5("4. Move the mouse over lines to see specific points;"),
                                          h5("5. Click on the legends to hide or show lines;"),
                                          h5("6. Click on the plot and drag horizontally to select a date range;"),
                                          h5("7. Click on the button in the top-right corner for more exporting options")
                                        ),
                                        mainPanel(
                                          highchartOutput("ts1",width = "100%",height = "560px")
                                        )
                                        
                          )
                      )
             ),
             # tab 3 (free meal interactive map)
             tabPanel("NYC Free Meal", icon = icon("map-marker-alt"),
                      leafletOutput("mymap", width="100%", height="100%"),
                      titlePanel("NYC FreeMeal Point"),
                      mainPanel(leafletOutput("map"))
                      
                          
              
                                        
             ),
                    
             # tab 4 (shelter)  
             tabPanel("Shelter", icon = icon("hotel"),
                     h2("The Shelter Data"),
                     DT::dataTableOutput("vaccine_table"),
                     h2("More information about shelter_offered type"),
                     uiOutput("tab"),
                     uiOutput("vac")
             ),
             # tab 5 (appendix)  
             tabPanel("Appendix", icon = icon("info-circle"),
                      h2("Data Sources:"),
                      tags$b("NYC COVID-19 Open Data: "), tags$a(href="https://data.cityofnewyork.us/Health/COVID-19-Daily-Counts-of-Cases-Hospitalizations-an/rc75-m7u3", "COVID-19 Daily Counts of Cases, Hospitalizations, and Deaths."),tags$br(),
                      tags$b("Google COVID-19 Mobility Data: "), tags$a(href="https://www.google.com/covid19/mobility/", "Google's COVID-19 Community Mobility Reports."),tags$br(),
                      tags$b("Shiny Dashboard: "), tags$a(href="https://github.com/TZstatsADS/Spring2021-Project2-group5", "4New Yorkers Covid Survival Manual's Github repository."),
                      tags$br(),tags$br(),tags$h2("Contacts:"),
                      tags$a(href="mailto:mb4786@columbia.edu", "Mellisa Bischoff"), ", Columbia University",tags$br(),
                      tags$a(href="mailto:xc2578@columbia.edu", "Xueying Chen"),", Columbia University",tags$br(),
                      tags$a(href="mailto:jl5886@columbia.edu", "Jing Lu"),", Columbia University",tags$br(),
                      tags$a(href="mailto:yw3727@columbia.edu", "Yalin Wang"),", Columbia University",tags$br(),
                      tags$a(href="mailto:yw3598@columbia.edu", "Yarong Wang"),", Columbia University",tags$br(),
                      tags$br(),tags$br(),tags$h2("Github Page:"),
                      tags$a(href="https://github.com/TZstatsADS/Fall2021-Project2-group5", "See code in our Github repository"),tags$br(),tags$br())
             )
 )
   




          
server <- function(input, output, session) {
  #home page graph
  output$image <- renderImage({
    list(src = "./www/meli.png")
  }, deleteFile = TRUE)
  #=========================
  #======== tab 1 ==========
  #=========================
  output$NewCasesBox = renderValueBox({
    valueBox(formatC(nyc_latest$case_count[1], big.mark=","),
             subtitle = paste0("New Cases Confirmed on ",nyc_latest$date_of_interest),
             color = "red",
             width = 4)
  })
  output$NewProbBox = renderValueBox({
    valueBox(formatC(nyc_latest$probable_case_count[1], big.mark=","),
             subtitle = paste0("New Probable Cases on ",nyc_latest$date_of_interest),
             color = "yellow",
             width = 4)
  })
  output$NewDeathsBox = renderValueBox({
    valueBox(formatC(nyc_latest$death_count[1], big.mark=","),
             subtitle = paste0("New Deaths on ",nyc_latest$date_of_interest),
             color = "black",
             width = 4)
  })
  output$NewHospitalizedBox = renderValueBox({
    valueBox(formatC(nyc_latest$hospitalized_count[1], big.mark=","),
             subtitle = paste0("New Hospitalized Cases on ",nyc_latest$date_of_interest),
             color = "blue",
             width = 4)
  })
  output$TCasesBox = renderValueBox({
    valueBox(value = formatC(sum(covid$case_count), big.mark=","),
             subtitle = "Cumulative Cases Confirmed in NYC",
             color = "red",
             width = 4)
  })
  output$TDeathsBox = renderValueBox({
    valueBox(value = formatC(sum(covid$death_count), big.mark=","),
             subtitle = "Cumulative Deaths in NYC",
             color = "black",
             width = 4)
  })
  
  # ================================ line plot =========================================================
  covid$date_of_interest <-ymd(covid$date_of_interest)
  
  # select subset by borough
  SubsetBorough <- function(ts_borough){
    if (ts_borough == ""){
      data <- covid[,c(1:8)]
    } else if (ts_borough == "Bronx"){
      data <- covid[,c(1,9:15)]
    } else if (ts_borough == "Brooklyn"){
      data <- covid[,c(1,16:22)]
    } else if (ts_borough == "Manhattan"){
      data <- covid[,c(1,23:29)]
    } else if (ts_borough == "Queens"){
      data <- covid[,c(1,30:36)]
    } else if (ts_borough == "Staten Island"){
      data <- covid[,c(1,37:43)]
    }
    return(data)
  }
  
  # Interactive time series plot with highcharter
  observe({
    # Render highchart outcome for borough selection
    output$ts1 <- renderHighchart({
      # subset data and make it tidy
      covid_sub <- SubsetBorough(input$select_borough) %>%
        tidyr::pivot_longer(
          cols = -date_of_interest, 
          names_to = "line_var", 
          values_to = "value") %>% #names_prefix = "fight_" 
        dplyr::mutate(line_var = as.factor(line_var))
      # custom title of plot
      ts_title <- ifelse(input$select_borough == "", "New York City", input$select_borough)
      
      # ---------------filter variables--------------------------------------------------
      # If no selection, generate a dataframe of zeros to avoid errors
      if (!input$daily&!input$weekly &!input$casesummary & !input$deathsummary & !input$hospsummary){
        covid_filter <- covid_sub
        covid_filter$value <- 0
      } else {
        if (input$daily&input$casesummary) {
          df1 <- covid_sub %>%
            dplyr::filter(stringr::str_detect(line_var,"case_count")&
                            !stringr::str_detect(line_var,"7day"))
        } else {df1 <- data.frame()}
        if (input$daily&input$deathsummary) {
          df2 <- covid_sub %>%
            dplyr::filter(stringr::str_detect(line_var,"death_count")&
                            !stringr::str_detect(line_var,"7day"))
        } else {df2 <- data.frame()}
        if (input$daily&input$hospsummary) {
          df3 <- covid_sub %>%
            dplyr::filter(stringr::str_detect(line_var,"hosp")&
                            !stringr::str_detect(line_var,"7day"))
        } else {df3 <- data.frame()}
        if (input$weekly&input$casesummary) {
          df4 <- covid_sub %>%
            dplyr::filter(stringr::str_detect(line_var,"case_count")&
                            stringr::str_detect(line_var,"7day"))
        } else {df4 <- data.frame()}
        if (input$weekly&input$deathsummary) {
          df5 <- covid_sub %>%
            dplyr::filter(stringr::str_detect(line_var,"death_count")&
                            stringr::str_detect(line_var,"7day"))
        } else {df5 <- data.frame()}
        if (input$weekly&input$hospsummary) {
          df6 <- covid_sub %>%
            dplyr::filter(stringr::str_detect(line_var,"hosp")&
                            stringr::str_detect(line_var,"7day"))
        } else {df6 <- data.frame()}
        # aggregated dataframe for plot
        covid_filter = rbind(df1,df2,df3,df4,df5,df6)
      }
      
      # ------------------------- plot -------------------------------------------------
      hchart(covid_filter, "line",
             hcaes(x = date_of_interest, y = value, group = line_var)) %>%
        hc_chart(zoomType = "x") %>%
        #hc_colors(c("#0015BC", "#FF0000")) %>% need one color for each variable
        hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
        hc_xAxis(title = list(text = "Date"),
                 labels = list(format = '{value:%b %d %y}')) %>%
        hc_yAxis(title = list(text = "Count"),
                 tickInterval = 400,
                 max = max(covid_filter$value)) %>%
        hc_title(text = paste0("<b>Covid-19 Summary for ",ts_title, ", NY by Date</b>")) %>%
        hc_plotOptions(area = list(lineWidth = 0.5)) %>%
        hc_exporting(enabled = TRUE)
    })
    # Render highchart outcome for overview
    output$tscum <- renderHighchart({
      # subset data and make it tidy
      a<-as.data.frame(covid[,c(9:12,16:19,23:26,30:33,37:40)]%>%map(sum))
      b<-a[,grepl("death",colnames(a))]
      c<-data.frame(type=c('death','death','death','death','death'),
                    borough=c("Bronx","Brooklyn","Manhattan","Queens","Staten Island"),
                    value=c(b[,1],b[,2],b[,3],b[,4],b[,5]))              
      d<-a[,grepl("case",colnames(a))&!grepl("prob",colnames(a))]
      e<-data.frame(type=c('case','case','case','case','case'),
                    borough=c("Bronx","Brooklyn","Manhattan","Queens","Staten Island"),
                    value=c(d[,1],d[,2],d[,3],d[,4],d[,5])) 
      cum<-rbind(c,e) 
      # custom title of plot
      tscum_title <- "Cumulative COVID-19 Confirmed Cases and Deaths of the 5 boroughs in New York City"
      
      # ------------------------- plot -------------------------------------------------
      hchart(cum, "column",
             hcaes(x = borough, y = value, group = type)) %>%
        hc_chart(zoomType = "x") %>%
        #hc_colors(c("#0015BC", "#FF0000")) %>% need one color for each variable
        hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
        hc_xAxis(title = list(text = "Borough")) %>%
        hc_yAxis(title = list(text = "Count"),
                 #tickInterval = 400,
                 max = max(cum$value)) %>%
        hc_title(text = paste0(tscum_title)) %>%
        hc_exporting(enabled = TRUE)
    })
    output$tsnew <- renderHighchart({
      # subset data and make it tidy
      cnew<-covid[,c(9:12,16:19,23:26,30:33,37:40)]%>%tail(1)
      cnewb<-cnew[,grepl("death",colnames(cnew))]
      cnewc<-data.frame(type=c('death','death','death','death','death'),
                        borough=c("Bronx","Brooklyn","Manhattan","Queens","Staten Island"),
                        value=c(cnewb[,1],cnewb[,2],cnewb[,3],cnewb[,4],cnewb[,5]))              
      cnewh<-cnew[,grepl("hos",colnames(cnew))]
      cnewi<-data.frame(type=c('hospitalized','hospitalized','hospitalized','hospitalized','hospitalized'),
                        borough=c("Bronx","Brooklyn","Manhattan","Queens","Staten Island"),
                        value=c(cnewh[,1],cnewh[,2],cnewh[,3],cnewh[,4],cnewh[,5])) 
      latestcase<-rbind(cnewc,cnewi)
      # custom title of plot
      tsnewcase_title <- "New COVID-19 Death and Hospitalized Cases of the 5 Boroughs in New York City"
      
      # ------------------------- plot -------------------------------------------------
      hchart(latestcase, "column",
             hcaes(x = borough, y = value, group = type)) %>%
        hc_chart(zoomType = "x") %>%
        hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
        hc_xAxis(title = list(text = "Borough")) %>%
        hc_yAxis(title = list(text = "Count"),
                 max = max(latestcase$value)) %>%
        hc_title(text = paste0(tsnewcase_title)) %>%
        hc_exporting(enabled = TRUE)
    })
    output$tsnewcase <- renderHighchart({
      # subset data and make it tidy
      cnew<-covid[,c(9:12,16:19,23:26,30:33,37:40)]%>%tail(1)
      cnewd<-cnew[,grepl("case",colnames(cnew))&!grepl("prob",colnames(cnew))]
      cnewe<-data.frame(type=c('confirmed_case','confirmed_case','confirmed_case','confirmed_case','confirmed_case'),
                        borough=c("Bronx","Brooklyn","Manhattan","Queens","Staten Island"),
                        value=c(cnewd[,1],cnewd[,2],cnewd[,3],cnewd[,4],cnewd[,5])) 
      cnewf<-cnew[,grepl("prob",colnames(cnew))]
      cnewg<-data.frame(type=c('probable_case','probable_case','probable_case','probable_case','probable_case'),
                        borough=c("Bronx","Brooklyn","Manhattan","Queens","Staten Island"),
                        value=c(cnewf[,1],cnewf[,2],cnewf[,3],cnewf[,4],cnewf[,5])) 
      latest<-rbind(cnewe,cnewg)
      # custom title of plot
      tsnew_title <- "New COVID-19 Confirmed and Probable Cases of the 5 Boroughs in New York City"
      
      # ------------------------- plot -------------------------------------------------
      hchart(latest, "column",
             hcaes(x = borough, y = value, group = type)) %>%
        hc_chart(zoomType = "x") %>%
        hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
        hc_xAxis(title = list(text = "Borough")) %>%
        hc_yAxis(title = list(text = "Count"),
                 max = max(latest$value)) %>%
        hc_title(text = paste0(tsnew_title)) %>%
        hc_exporting(enabled = TRUE)
    })
  })
  color <- colorFactor(c("red","yellow","green","blue","black"),freemeal$City)
  output$map <- renderLeaflet({
    map <- leaflet(freemeal) %>%
      
      setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(
        lng=~Longitude,
        lat=~Latitude,
        color =~color(freemeal$City),
        
      ) %>%
      addLegend(
        "bottomleft", # Legend position
        pal=color, # color palette
        values=~City, # legend values
        opacity = 1,
        title="Borough"
      )
      
    
    
    
    
    
    
  })
  
  output$vaccine_table = DT::renderDataTable({
    shelters[3:6]})
  
  url1 <- a("shelters", href="https://data.cityofnewyork.us/Social-Services/Individual-Census-by-Borough-Community-District-an/veav-vj3r")
  output$tab <- renderUI({
    tagList("URL link :", url1)
    
    
    
  })
}
  
shiny::shinyApp(ui, server)
#deployApp()