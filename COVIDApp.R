library(shiny)
library(shinythemes)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)
library(tools)
library(shinyWidgets)
library(dplyr)

url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
covidData <- read.csv(url, header = T)[,c(2,3,4,5,6,8,9)]

covidData[is.na(covidData)] <- 0
covidData$continent <- gsub("^$", "Other", covidData$continent)

ui <- fluidPage(theme = shinytheme("united"),
                
                titlePanel("Worldwide COVID Dashboard"),
                
                sidebarLayout(
                    sidebarPanel(
                        radioButtons("val", strong("COVID Metric"),
                                     c("Cases" = "cases",
                                       "Deaths" = "deaths")),
                        pickerInput(inputId = "con", label = strong("Continent"),
                                    choices = unique(sort(covidData$continent)),
                                    selected = 'North America',
                                    multiple = FALSE),
                        pickerInput(inputId = "loc", label = strong("Country"),
                                    choices = unique(covidData$location),
                                    options = list(`actions-box` = TRUE),
                                    multiple = TRUE),
                        dateRangeInput(inputId = "dte", label = strong("Select Date Range"), start = Sys.Date() - 30,
                                       end = max(covidData$date), min = min(covidData$date), max = max(covidData$date)),
                        actionButton("go", "Apply")
                    ),
                    mainPanel(
                        plotOutput("lineplot"),
                        plotOutput("lineplot2")
                    )
                )
)
server <- function(input, output, session) {
    
    countryList <- reactive({
        covidData %>% 
            filter(continent == input$con) %>%
            pull(location)
    })
    observe({
        updatePickerInput(session, "loc", choices = unique(countryList()))
    })
    
    overallTrends <- eventReactive(input$go, {
        covidData %>%
            mutate(date = as.Date(date)) %>%
            filter(
                date >= as.POSIXct(input$dte[1]) & date <= as.POSIXct(input$dte[2]),
                location %in% input$loc) %>%
            select("continent", "location", "date", matches(input$val)) %>%
            rename_with(~ gsub(paste("_", input$val, sep = ""), "", .x, fixed = TRUE))
    })
    
    output$lineplot <- renderPlot({
        ggplot(data = overallTrends(), aes(x = overallTrends()$date, y = overallTrends()$total, color = overallTrends()$location)) + 
            geom_line(size = 1.2) + 
            xlab("Date") +
            ylab(paste("Total ", toTitleCase(input$val), sep = "")) +
            ggtitle(paste("Running COVID ", toTitleCase(input$val), " Over Time", sep = "")) +
            labs(color='Country') +
            theme(axis.text = element_text(size = 8),
                  axis.title = element_text(size = 12),
                  axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                  axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                  plot.title = element_text(size = 15, face = "bold", hjust = 0.5,
                                            margin = margin(t = 0, r = 0, b = 20, l = 0)))
    })
    
    output$lineplot2 <- renderPlot({
        ggplot(data = overallTrends(), aes(x = overallTrends()$date, y = overallTrends()$new, color = overallTrends()$location)) + 
            geom_line(size = 1.2) + 
            xlab("Date") +
            ylab(paste("New ", toTitleCase(input$val), sep = "")) +
            ggtitle(paste("Daily New COVID ", toTitleCase(input$val), sep = "")) +
            labs(color='Country') +
            theme(axis.text = element_text(size = 8),
                  axis.title = element_text(size = 12),
                  axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                  axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                  plot.title = element_text(size = 15, face = "bold", hjust = 0.5,
                                            margin = margin(t = 0, r = 0, b = 20, l = 0)))
    })
    
}
shinyApp(ui = ui, server = server)

