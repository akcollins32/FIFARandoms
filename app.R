#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
Teams <- read.csv("FIFA Teams.csv")
library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("FIFA Randomizer"),

        # Show a plot of the generated distribution
        mainPanel(
           actionButton("team", "True Random"),
           actionButton("country", "Random Country"),
           actionButton("countryleague", "Random League within Country"),
           actionButton("countryteam", "Team within Country and League"),
           actionButton('league', "Random League"),
           actionButton("leagueteams", "Random Team within League"),
           textOutput("country"),
           textOutput("league"),
           tableOutput("table1")
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
values <- reactiveValues()
randomTeam <- observeEvent(input$team, 
                               {output$table1 <- renderTable(Teams[sample(nrow(Teams),1),])})
    
randomCountry <- observeEvent(input$country, {
      countryrandom <- Teams[sample(nrow(Teams),1),2]
      values$countryrandom <- countryrandom
      output$country <- renderText(countryrandom)
    })

randomLeague <- observeEvent(input$league, {
 
  league.random <- sample(unique(Teams$League),1)
  values$leaguerandom <- league.random
  output$league <- renderText(league.random)
})    

randomLeagueTeam <- observeEvent(input$leagueteams, {
  leagueteams <- Teams%>%filter(League == isolate(values$leaguerandom))
  output$table1 <- renderTable(leagueteams[sample(nrow(leagueteams),1),])
})

randomCountryLeague <- observeEvent(input$countryleague, {
  
  countryleagues <- Teams%>%filter(Country == isolate(values$countryrandom))
  if(length(unique(countryleagues$League))==1){
    values$countryleaguerandom <- unique(countryleagues$League)
}else{
  
  countryleaguerandom <- countryleagues[sample(nrow(countryleagues),1),3]
  values$countryleaguerandom <- countryleaguerandom
  output$league <- renderText(countryleaguerandom)
}
  })

randomCountryTeam <- observeEvent(input$countryteam, {
  countryleagues <- Teams%>%filter(Country == isolate(values$countryrandom))
  
  if(length(unique(countryleagues$League))==1){
    values$countryleaguerandom <- unique(countryleagues$League)
  }
  
  CountryLeagueTeams <- Teams%>%filter(Country == isolate(values$countryrandom) & League == isolate(values$countryleaguerandom))
  
  output$table1 <- renderTable(CountryLeagueTeams[sample(nrow(CountryLeagueTeams),1),])
})
}


# Run the application 
shinyApp(ui = ui, server = server)
