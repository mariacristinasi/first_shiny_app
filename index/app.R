library(shiny)
library(tidyverse)
library(shinyjs)
library(ggplot2)
library(ggthemes)
library(Stat2Data)

data(CO2Hawaii)
data=CO2Hawaii
CO2_notrend=diff(data$CO2)
data_notrend=data.frame(t=seq(1,length(CO2_notrend)), CO2=CO2_notrend)
CO2_notrend_noseasonal=diff(CO2_notrend, lag=12)
data_notrend_noseasonal=data.frame(t=seq(1,length(CO2_notrend_noseasonal)), CO2=CO2_notrend_noseasonal)
list_choices = c("Original series", "Series without trend", "Series without trend and stationarity")

aver_year=rep(0,(data$Year[nrow(data)]-data$Year[1]))
a=0
for (i in 1:(data$Year[nrow(data)]-data$Year[1])){
    for (j in 1:nrow(data)){
        if (data$Year[j] == i+data$Year[1]-1){
            aver_year[i]=aver_year[i]+data$CO2[j]
}}}

# Ui
ui <- navbarPage("CO2Hawaii dataset",
                 tabPanel("General information",
                          includeMarkdown("info.md")),
                 tabPanel("Time serie visualization",
                          fluidPage(
                              sidebarLayout(sidebarPanel(
                                  selectInput("select", label = h3("Select the time series to display:"), 
                                              choices = list_choices,
                                              selected = 1)
                              ), mainPanel(
                                  plotOutput(outputId = "plots")
                              )
                              ))),
                 tabPanel("Anual average concentration",
                          sidebarLayout(position = "right",
                                        sidebarPanel(
                                            fluidRow(
                                                h3(style = "margin-left: 20px; margin-bottom: 0px;", "First limit"),
                                                column(2,
                                                       div(style = "margin-top: 37px", checkboxInput("auto_low", label = "auto", value = TRUE))
                                                ),
                                                column(10,
                                                       sliderInput("n_low", label="", min = 1988, max = 2017, value = 1988)
                                                )
                                            ),
                                            fluidRow(
                                                h3(style = "margin-left: 20px; margin-bottom: 0px;", "Second limit"),
                                                column(2,
                                                       div(style = "margin-top: 37px", checkboxInput("auto_large", label = "auto", value = TRUE))
                                                ),
                                                column(10,
                                                       sliderInput("n_large", label="", min = 1988, max = 2017, value = 2017)
                                                )
                                            ),
                                        ),
                                        mainPanel(
                                            tabsetPanel(type = "tabs",
                                                        tabPanel("Plot", plotOutput("meanPlot")),
                                                        tabPanel("Summary", verbatimTextOutput("histSummary")),
                                                        tabPanel("Table", tableOutput("histTable"))
                                            )
                                        )) 
                 ),
                 useShinyjs()
) 

col_scale <- scale_colour_discrete(limits = list_choices)

# Server
server <- function(input, output) {
    output$plots<- renderPlot({
        if (identical(input$select, "Original series")){
            ggplot(data, aes(t, CO2)) + 
                    geom_line(size=1, color="deepskyblue4") +
                    xlab("Number of months since beginning") + ylab("Monthly CO2 concentration") +
                    theme_stata()
        }
        else if (identical(input$select, "Series without trend")){
            ggplot(data_notrend, aes(t, CO2)) + 
                    geom_line(size=1, color="deepskyblue4") +
                    xlab("Number of months since beginning") + ylab("Monthly CO2 concentration") +
                    theme_stata()
        }
        
        else if (identical(input$select, "Series without trend and stationarity")){
            ggplot(data_notrend_noseasonal, aes(t, CO2)) + 
                    geom_line(size=1, color="deepskyblue4") +
                    xlab("Number of months since beginning") + ylab("Monthly CO2 concentration") +
                    theme_stata()
        }
    })
    

    samples <- reactive({
        n_low <- text=paste(input$n_low)
        n_large <- text=paste(input$n_large)
        interval=seq(min(input$n_low, input$n_large), max(input$n_low, input$n_large))
        aver_int=average[input$n_low-1987:input$n_large-1987]
    })
    
    observe(if(input$auto_low) disable("n_low") else enable("n_low") )
    
    observe(if(input$auto_large) disable("n_large") else enable("n_large") )
    
    output$meanPlot <- renderPlot(
        ggplot(data.frame(t=seq(min(input$n_low, input$n_large), max(input$n_low, input$n_large)),
            aver=aver_year[(min(input$n_low, input$n_large)-1987):(max(input$n_low, input$n_large)-1987)]), 
            aes(t, aver)) +
            geom_point(size=2, color="deepskyblue3")+
            xlab("Year") + ylab("Anual CO2 concentration (ppm)") +
            ggtitle("Average anual CO2 concentration") +
            theme_stata()

    );
    
    output$histSummary <- renderPrint(summary(data))
    output$histTable <- renderTable(data)
  
        }


# Run the application 
shinyApp(ui = ui, server = server)