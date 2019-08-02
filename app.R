library(shiny)
library(DT)
library(tidyverse)
library(rvest)
library(yaml)

dat <- NULL # initialize

pinchy_crabs <- read_yaml("teams.yml")$pinchy_crabs
bats <- read_yaml("teams.yml")$bats

dat <- "https://www.basketball-reference.com/leagues/NBA_2019_advanced.html" %>%
  read_html() %>% 
  html_table() %>% 
  getElement(1) 

dat <- dat[, c("Player", "Pos", "MP", "WS")] %>% 
  filter(Player %in% c(pinchy_crabs, bats)) %>% 
  transmute(
    Team = ifelse(Player %in% pinchy_crabs, "Pinchy Crabs", "Ballin' Bats"),
    Player = Player,
    Position = Pos,
    `Minutes Played` = as.numeric(MP),
    `Win Shares` = as.numeric(WS)
  )

ui <- fluidPage(
  
  fluidRow(
    column(6,
      uiOutput("pinchy")
    ),
    column(6,
      uiOutput("bats")
    )
  )
   
)

server <- function(input, output) {
   
  output$pinchy <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == "Pinchy Crabs") %>% 
        select(-Team) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3("Baltimore Pinchy Crabs"),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`),
            ", per 48 Minutes: ",
            round(sum(tmp$`Win Shares`) / sum(tmp$`Minutes Played`) * 48, 3)
          )
        ),
        renderDataTable(
          datatable(
            tmp,
            rownames = FALSE,
            options = list(
              paging = FALSE,
              searching = FALSE,
              bInfo = FALSE
            )
          )
        )
      )
    }
  })
  
  output$bats <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == "Ballin' Bats") %>% 
        select(-Team) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3("Austin Ballin' Bats"),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`),
            ", per 48 Minutes: ",
            round(sum(tmp$`Win Shares`) / sum(tmp$`Minutes Played`) * 48, 3)
          )
        ),
        renderDataTable(
          datatable(
            tmp,
            rownames = FALSE,
            options = list(
              paging = FALSE,
              searching = FALSE,
              bInfo = FALSE
            )
          )
        )
      )
    }
  })
   
}

shinyApp(ui = ui, server = server)
