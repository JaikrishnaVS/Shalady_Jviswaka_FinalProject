#install.packages("shiny")
#install.packages("tidyverse")
#install.packages("wordcloud")
#install.packages("ggplot2")
#install.packages("shinythemes")
#install.packages("RColorBrewer")
#install.packages('Rcpp')
#install.packages("shinyjs")
#install.packages("gganimate")
#install.packages("packcircles")
#install.packages("patchwork")
#install.packages("plotly")


library(shiny)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(tidytext)
library(Rcpp)
library(shinyjs)
library(dplyr)
library(ggimage)
library(gganimate)
library(packcircles) 
library(patchwork)
library(plotly)

#Reading the source datafile. 
Olympics <- read.csv(file = 'data/olympics.csv')
Region <- read.csv(file='data/noc_regions.csv')  %>% rename(noc = NOC)
TotalData <- inner_join(Olympics,Region,by = 'noc') %>% filter(medal != 'na')

#OlypicsMedal Color
clr <- list(Gold = "#FFD700",Silver = "#C0C0C0",Bronze = "#cd7f32")

seasonList <- unique(Olympics$season)
genderList <- unique(Olympics$sex)
sportsList <- unique(Olympics$sport)
medalList <- list("Gold","Silver","Bronze")
countryList <- unique(Region$region)

#Data Clensing
#Primary dataset for the country data.
ByCountry <- TotalData  %>% 
    count(region,sex,season,year,sport,event,medal)  %>% 
    count(region,sex,season,year,sport,event,medal) %>%
    count(region,sex,season,year,sport,medal)

#Primary dataset for the By Person data
ByPerson <- TotalData  %>% 
    count(name,region,sex,season,year,sport,event,medal) 


#Filter conditions used in the UI input.
sex_filter <- list('M','F')
season_filter <- list('Summer','Winter')
sport_filter <-   list(unique(ByCountry[c("sport")]))
medal_filter <- list('Gold','Silver','Bronze')


# Define UI for application that draws a histogram
ui <- fluidPage(
    #default theme selected below
    theme = shinytheme("cerulean"),
    
    
    
    
    useShinyjs(),
    # Application title
    titlePanel("Olympics - Team Performance Study"),
    # shinythemes::themeSelector(),  #Theme selector option to provide the users with the ability to select the theme.

    sidebarLayout(
        #Side bar panel componants are defined here. 
        sidebarPanel(
            #header given using html definition for the filter criterion
            h4("Filter Criterion"),
            hr(),
            
            #Using slider input to provide the user the ability to select date range.
            sliderInput("yearRangeSelection", "Select Year Range:",
                        min = 1896, max = 2016, value = c(1990, 2016),step = 2,sep =''
            ),
            hr(),
            
            #Input selection for identifying the medals that we want in our dataset.
            checkboxGroupInput(inputId = "medalSelection",
                               label = "Medal Selection:",
                               choices = medal_filter,
                               selected = medal_filter),
            hr(),
            
            #Input selection to filter out the seasons to display the data for. Summer is selected by default. 
            checkboxGroupInput(inputId = "SeasonInput",
                               label = "Season Selection",
                               choices = seasonList,
                               selected = "Summer", inline = TRUE),
           
            
            #selectInput("teamSelectionInput", "Choose a Team:",unique(regionsSourceData$region)),
            
            hr(),
            #providing user with the ability to select between male and female atheletes. By default both are selected. 
            checkboxGroupInput(inputId = "GenderInput",
                               label = "Select Gender:",
                               choices = genderList,
                               selected = genderList, inline = TRUE),
           
            #Action button that executes the data set. This button needs to be clicked every time the user wants to re render the options in the display panels. 
            hr(),
            wellPanel(
            div(style = "text-align: center", actionButton("runButton", "Execute Selection", class = "btn-success"))
            ),
                            
        ),
        
        #Main panel where the story is told.The output panels are defined here. The data is rendered in the sever section. 
        mainPanel(
            #tabsetpanel used for 
            tabsetPanel(
                tabPanel("Top Countries By Medal",  
                         column(7,htmlOutput("top7TitleHtml", style="color:DarkOliveGreen"),
                         plotlyOutput("EventPlot", width = 600, height = 600)),
                         column(5,htmlOutput("topMedalCountryBubble", style="color:DarkOliveGreen"),
                         plotOutput("EventPlot2", width = 500, height = 500))), 
                tabPanel("Top Performers", plotOutput("Atheletes", width = 1000, height = 700)),
                
                tabPanel("Individual Country Analysis", 
                         selectInput("countrySelection", "Choose Country ",countryList,selected="USA" ),
                         plotOutput("CountryPlot", width = 1000, height = 700))
                
            )
        )
    ),
    # Footer comment
    tags$footer("By Sowmya Halady and Jaikrishna Viswakaran Sreelatha", align = "right",color = 'blue', border = 'white')
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    {
        observeEvent(input$runButton, 
        {
            #print(as.numeric(input$yearRangeSelection[2]))
            year_lower <- input$yearRangeSelection[1]
            year_upper <- input$yearRangeSelection[2]
            sex_filter <- input$GenderInput
            season_filter <- input$SeasonInput
            medal_filter <- input$medalSelection
            
            Graph01 <- ByCountry %>%
                filter(sex %in% sex_filter) %>% 
                filter(season %in% season_filter) %>% 
                #filter(sport %in% sport_filter) %>% 
                filter(medal %in% medal_filter) %>% 
                filter(year >= year_lower) %>% 
                filter(year <= year_upper)   %>% 
                group_by(region,year) %>% summarise(medal_total = sum(n))   %>% 
                arrange(year, desc(medal_total)) %>% 
                group_by(year) %>% 
                mutate(rank = seq_along(medal_total))
            
            
            output$top7TitleHtml <- renderText({
                HTML(paste0("<br>","<b>","Top 7 Trending graph","</b>"))
            })
            
            output$EventPlot <- renderPlotly({
                
                Graph01_2 <- Graph01 %>%  filter(rank <= 7)
                
                g1 <- ggplot(Graph01_2, aes(x = year, y = rank, color = region)) +
                    geom_point(aes(group = seq_along(rank), color = region), size = 2) +
                    geom_line(aes(color = region), size = 1) +
                    scale_x_continuous(breaks = seq(year_lower, year_upper, 4))+
                    scale_y_reverse(breaks = seq(1, 10, 1)) +
                    ylab("Rank")+
                    theme_minimal()
                
                g1
                
                
        })
            
            
            output$topMedalCountryBubble <- renderText({
                HTML(paste0("<br>","<br>","<b>","Medal Collection by Country - Bubble Chart","</b>"))
            })
            
            output$EventPlot2 <- renderPlot({
                
                Graph01_3 <- Graph01 %>%  group_by(region) %>%
                    summarise(medals = sum(medal_total)) %>%
                    mutate(rank = dense_rank(desc(medals))) %>% filter(rank <= 15)
                packing <- circleProgressiveLayout(Graph01_3$medals, sizetype='area') 
                data1 <- cbind(Graph01_3, packing)
                data2 <- circleLayoutVertices(packing, npoints=50)
                
                g2 <- ggplot() +   # Make the bubbles
                    geom_polygon(data = data2, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.5) +
                    theme(legend.position="none", panel.background = element_blank(),
                          axis.ticks.x =element_blank(), axis.ticks.y = element_blank(),
                          axis.text.x = element_blank(), axis.text.y = element_blank(),
                          axis.title.x = element_blank(), axis.title.y = element_blank()) +
                    geom_text(data = data1, aes(x, y, size=medals, label = (paste(region,medals)))) +
                    scale_size_continuous(range = c(1,4))
                
                g2
                
            })
            
            output$Atheletes <- renderPlot({
                Graph02 <- ByPerson %>%
                    filter(sex %in% sex_filter) %>% 
                    filter(season %in% season_filter) %>% 
                    #filter(sport %in% sport_filter) %>% 
                    filter(medal %in% medal_filter) %>% 
                    filter(year >= year_lower) %>% 
                    filter(year <= year_upper)   %>% 
                    count(name,region) %>% mutate(rank = dense_rank(desc(n)))  %>%  
                    filter(rank <= 5)
                
                ggplot(Graph02, aes(y = reorder(name,n) , x = n, color = region)) +
                    geom_col(size = 1) +  ylab("Sports Person Name") + xlab("Number of Medals")
                #ggtitle("Participants with the Most Olympic Medals")
            })
        
        })
        
        #Country Selection Reactive
     
        observeEvent(input$countrySelection, 
                     {
        
             year_lower <- input$yearRangeSelection[1]
             year_upper <- input$yearRangeSelection[2]
             sex_filter <- input$GenderInput
             season_filter <- input$SeasonInput
             medal_filter <- input$medalSelection
             countrySelection <- input$countrySelection
             
             country01 <- ByCountry %>%
                 filter(region %in% countrySelection) %>% 
                 filter(sex %in% sex_filter) %>% 
                 filter(season %in% season_filter) %>% 
                 filter(medal %in% medal_filter) %>% 
                 filter(year >= year_lower) %>% 
                 filter(year <= year_upper)   
             
             country02 <- country01 %>% group_by(sex,medal) %>% summarise(medal_total = sum(n)) %>%
                 mutate(gender = ifelse(sex=="M","Male","Female"))
             country02$medal <-factor(country02$medal,levels=c("Bronze", "Silver", "Gold"))
                
             output$CountryPlot <- renderPlot({
            
            a <- ggplot(country02,aes(x= medal_total,y= reorder(gender,medal_total), fill = medal))+
                geom_col() +  scale_fill_manual(values = clr) +
                theme(legend.position="none", panel.background = element_blank(),
                      axis.title.y = element_blank(),axis.title.x = element_blank(),
                      panel.border = element_rect(colour = "gray", fill=NA, size=1)) + ggtitle("Medal Count by Gender") 
            
            
            country03 <- country01 %>% group_by(season,medal) %>% summarise(medal_total = sum(n))
            country03$medal <-factor(country03$medal,levels=c("Bronze", "Silver", "Gold"))
            
            b <- ggplot(country03,aes(x= season ,y= medal_total, fill = medal))+
                geom_col()   + scale_fill_manual(values = clr) +
                theme(legend.position="none", panel.background = element_blank(),
                      axis.title.x = element_blank(),axis.title.y = element_blank(),
                      panel.border = element_rect(colour = "gray", fill=NA, size=1)) + ggtitle("Medal Count by Season")
            
            
            country04 <- country01 %>% group_by(year,medal) %>% summarise(medal_total = sum(n))
            country04$medal <-factor(country04$medal,levels=c("Bronze", "Silver", "Gold"))
            
            c <- ggplot(country04, aes(x = year, y = medal_total, group = medal, fill = medal))  +
                geom_col() + scale_fill_manual(values = clr) +
                theme(legend.position="none", panel.background = element_blank(),
                      axis.title.x = element_blank(),axis.title.y = element_blank(),
                      panel.border = element_rect(colour = "gray", fill=NA, size=1)) + ggtitle("Medal Count by Year")
            
            country05 <- country01 %>% group_by(sport,medal) %>% summarise(medal_total = sum(n))
            country06 <- country05 %>% group_by(sport) %>% 
                summarise(medal_all = sum(medal_total)) %>% 
                mutate(rank = dense_rank(desc(medal_all))) %>% filter(rank <= 10)
            country07 <- inner_join(country05,country06,by = 'sport')
            country07$medal <-factor(country07$medal,levels=c("Bronze", "Silver", "Gold"))
            
            d <- ggplot(country07, aes(x = medal_total, y =  reorder(sport,medal_total), group = medal, fill = medal))  +
                geom_col() + scale_fill_manual(values = clr) +
                theme(legend.position="none", panel.background = element_blank(),
                      axis.title.x = element_blank(),axis.title.y = element_blank(),
                      panel.border = element_rect(colour = "gray", fill=NA, size=1))  + ggtitle("Medal Count by Sport")
            
            
            (d | c ) / (b | a)
        })
        })
        
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
