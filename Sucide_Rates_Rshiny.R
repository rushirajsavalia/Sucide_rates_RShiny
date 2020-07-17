library(shiny)
library(shinydashboard)
library(dplyr) 
library(ggplot2) 
library(countrycode) # continent
library(rworldmap) # quick country-level heat maps
library(gridExtra) 

#read csv file
setwd("C:/Users/rushi/OneDrive/Desktop/Sucide_rates_Rshiny")
suiciderates <- read.csv("sucide_rates.csv")


head(suiciderates)
countries <- suiciderates %>%
  distinct(ï..country)

#new dataset without missing data
#data 
#head(suiciderates)
data <- filter(suiciderates,year != 2016)
names(data)[5]<-"suicide-no"
names(data)[7]<-"suicides100kpop"
names(data)[8]<-"country-year"
names(data)[9]<-"HDI-for-year"
names(data)[10]<-"gdp-for-year"
names(data)[11]<-"gdp-per-capita"
data <- data %>% select(-c('HDI-for-year'))

#Adding another attribute continent
data$continent <- countrycode(sourcevar = data[, "ï..country"],origin = "country.name",destination = "continent")
global_average <- (sum(as.numeric(data$`suicide-no`)) / sum(as.numeric(data$population))) * 100000


globalsuicide <- data %>%
  group_by(ï..country, year)  %>%
  summarize(suicides100kpop  = (sum(`suicide-no`) /  sum(population)) * 100000)

globalsuicidebysex <- data %>%
  group_by(ï..country, year,sex)  %>%
  summarize(suicides100kpop  = (sum(`suicide-no`) /  sum(population)) * 100000)

globalsuicidebyage <- data %>%
  group_by(ï..country, year,age)  %>%
  summarize(suicides100kpop  = (sum(`suicide-no`) /  sum(population)) * 100000)

#anaysis b continents
cont <-group_by(data,continent) %>% 
  summarize(suicides100kpop = (sum(as.numeric(`suicide-no`)) / sum(as.numeric(population))) * 100000) %>%
  arrange(suicides100kpop)

conttrend <-data %>%
  group_by(year, continent) %>%
  summarize(suicides100kpop = (sum(as.numeric(`suicide-no`)) / sum(as.numeric(population))) * 100000)

server <- function(input, output) 
{
  
  output$Country <- renderMenu({
    print(input$tabs)
    if(input$tabs == 'suicideSummary') 
    {sidebarMenu(
      menuItem(
        selectInput("Country",
                    "Select the Country",
                    choices = countries,
                    selected = 'Albania'))
    )}
    else
    {
      sidebarMenu()}
  })
  
  output$plot10 <- renderPlot({
    
    data %>%
      group_by(year) %>%
      summarize(population = sum(population), 
                suicides = sum(`suicide-no`), 
                suicides100kpop = (suicides / population) * 100000) %>%
      ggplot(aes(x = year, y = suicides100kpop)) +
      geom_line(col = "deepskyblue3", size = 1) +
      geom_point(col = "deepskyblue3", size = 2) +
      labs(
        title = "Global Suicides per 100k",
        subtitle = "Trend over time, 1985 - 2015.",
        x = "Year",
        y = "Suicides per 100k") +
      scale_x_continuous(breaks = seq(1985, 2015, 2)) + 
      scale_y_continuous(breaks = seq(10, 20))
    
  })
  
  output$plot11 <- renderPlot({
    
    data %>%
      group_by(sex) %>%
      summarize(suicides100kpop = (sum(as.numeric(`suicide-no`)) / sum(as.numeric(population))) * 100000) %>%
      ggplot(aes(x = sex, y = suicides100kpop, fill = sex)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Global suicides (per 100k), by Sex",
           x = "Sex", 
           y = "Suicides per 100k")
  })  
  
  output$plot12 <- renderPlot({
    
    data %>%
      group_by(year, sex) %>%
      summarize(suicides100kpop = (sum(as.numeric(`suicide-no`)) / sum(as.numeric(population))) * 100000) %>%
      ggplot(aes(x = year, y = suicides100kpop, col = factor(sex))) + 
      facet_grid(sex ~ ., scales = "free_y") + 
      geom_line() + 
      geom_point() + 
      labs(title = "Trends Over Time, by Sex", 
           x = "Year", 
           y = "Suicides per 100k", 
           color = "Sex")
    
  })
  
  output$plot13<-renderPlot({
    
    data %>%
      group_by(age) %>%
      summarize(suicides100kpop = (sum(as.numeric(`suicide-no`)) / sum(as.numeric(population))) * 100000) %>%
      ggplot(aes(x = age, y = suicides100kpop, fill = age)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Global suicides per 100k, by Age",
           x = "Age", 
           y = "Suicides per 100k") 
    
  })
  
  
  output$plot14 <- renderPlot({
    
    data %>%
      group_by(year, age) %>%
      summarize(suicides100kpop = (sum(as.numeric(`suicide-no`)) / sum(as.numeric(population))) * 100000) %>%
      ggplot(aes(x = year, y = suicides100kpop, col = age)) + 
      facet_grid(age ~ ., scales = "free_y") + 
      geom_line() + 
      geom_point() + 
      labs(title = "Trends Over Time, by Age", 
           x = "Year", 
           y = "Suicides per 100k", 
           color = "Age")
  })
  
  output$plot1 <- renderPlot({
    
    globalsuicide %>%
      filter(ï..country == input$Country) %>%
      ggplot(aes(x = year, y = suicides100kpop)) +
      geom_line(col = "deepskyblue3", size = 1) +
      geom_point(col = "deepskyblue3", size = 2) +
      labs(
        title = paste("Global Suicides (per 100k) in ",input$Country,sep=" "),
        subtitle = "Trend over time, 1985 - 2015.",
        x = "Year",
        y = "Suicides per 100k"
      )
  })
  
  output$plot2 <-renderPlot({
    
    globalsuicidebysex %>%
      filter(ï..country == input$Country) %>% 
      ggplot(aes(x = sex, y = suicides100kpop, fill = sex)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Global suicides (per 100k), by Sex",
           x = "Sex", 
           y = "Suicides per 100k") 
  })
  
  
  output$plot3 <-renderPlot({
    
    globalsuicidebysex %>%
      filter(ï..country == input$Country) %>% 
      ggplot(aes(x = year, y = suicides100kpop, col = factor(sex))) + 
      facet_grid(sex ~ ., scales = "free_y") + 
      geom_line() + 
      geom_point() + 
      labs(title = "Trends Over Time, by Sex", 
           x = "Year", 
           y = "Suicides per 100k", 
           color = "Sex")
    
  })
  
  output$plot4 <-renderPlot({
    
    globalsuicidebyage %>%
      filter(ï..country  == input$Country) %>% 
      ggplot(aes(x = age, y = suicides100kpop, fill = age)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Global suicides per 100k, by Age",
           x = "Age", 
           y = "Suicides per 100k") 
    
  })
  
  output$plot5 <-renderPlot({
    
    globalsuicidebyage %>%
      filter(ï..country == input$Country) %>% 
      ggplot(aes(x = year, y = suicides100kpop, col = age)) + 
      facet_grid(age ~ ., scales = "free_y") + 
      geom_line() + 
      geom_point() + 
      labs(title = "Trends Over Time, by Age", 
           x = "Year", 
           y = "Suicides per 100k", 
           color = "Age") + 
      theme(legend.position = "none")
    
  })
  
  output$plot6<-renderPlot({
    ggplot(cont, aes(x = continent, y = suicides100kpop, fill = continent)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Global Suicides (per 100k), by Continent",
           x = "Continent", 
           y = "Suicides per 100k", 
           fill = "Continent") 
    
  })
  
  output$plot7 <-renderPlot({
    
    ggplot(conttrend,aes(x = year, y = suicides100kpop, col = factor(continent))) + 
      facet_grid(continent ~ ., scales = "free_y") + 
      geom_line() + 
      geom_point() + 
      labs(title = "Trends Over Time, by Continent", 
           x = "Year", 
           y = "Suicides per 100k", 
           color = "Continent")
    
  })
  
  output$plot8 <-renderPlot({
    
    data %>%
      group_by(continent, sex) %>%
      summarize(n = n(), 
                suicides = sum(as.numeric(`suicide-no`)), 
                population = sum(as.numeric(population)), 
                suicides100kpop = (suicides / population) * 100000) %>%
      ggplot(aes(x = continent, y = suicides100kpop, fill = sex)) + 
      geom_bar(stat = "identity", position = "dodge") + 
      geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
      labs(title = "Gender Disparity, by Continent",
           x = "Continent", 
           y = "Suicides per 100k", 
           fill = "Sex") +
      coord_flip()
  })
  
  output$plot9 <-renderPlot({
    
    data %>%
      group_by(continent, age) %>%
      summarize(n = n(), 
                suicides = sum(as.numeric(`suicide-no`)), 
                population = sum(as.numeric(population)), 
                suicides100kpop = (suicides / population) * 100000) %>%
      ggplot(aes(x = continent, y = suicides100kpop, fill = age)) + 
      geom_bar(stat = "identity", position = "dodge") + 
      geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
      labs(title = "Age Disparity, by Continent",
           x = "Continent", 
           y = "Suicides per 100k", 
           fill = "Age")
    
  })
  
  
}


# Define UI for application that draws a histogram
library(shinydashboard)
ui <- dashboardPage(dashboardHeader(
  title = "Suicides Rates Insights",
  titleWidth = 350
),
dashboardSidebar(
  menuItemOutput(outputId = "Country")
  #selectInput("Country",
  #            "Select the Country",
  #            choices = countries)
),
dashboardBody(
  tabsetPanel(id = "tabs",
              tabPanel(
                "Global Suicide Rates",
                fluidRow(plotOutput("plot10")),
                br(),
                fluidRow(column(6,plotOutput("plot11")),
                         column(6,plotOutput("plot12"))),
                br(),
                fluidRow(column(6,plotOutput("plot13")),
                         column(6,plotOutput("plot14")))),                                  
              tabPanel(
                "Suicide Per Country",
                value = 'suicideSummary',
                fluidRow(plotOutput("plot1")),
                br(),
                fluidRow(column(6, plotOutput("plot2")),
                         column(6, plotOutput("plot3"))),
                br(),
                fluidRow(column(6, plotOutput("plot4")),
                         column(6, plotOutput("plot5")))
              ),
              tabPanel("Global Suicide Analysis",
                       fluidRow(column(6,plotOutput("plot6")),
                                column(6,plotOutput("plot7"))),
                       br(),
                       fluidRow(column(6,plotOutput("plot8")),
                                column(6,plotOutput("plot9"))))
              
              
  )))

shinyApp(ui,server)


