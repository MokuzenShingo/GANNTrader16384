##############################################################
#  Init
##############################################################
Sys.setenv(TZ = "Asia/Tokyo")

library(shiny)
library(shinydashboard)
library(dplyr)
library(visNetwork)
library(rhandsontable)
library(DT)
library(xts)
library(rpart)
library(plotly)
library(stringr)
library(markdown)
library(PerformanceAnalytics)
library(quantmod)
library(TFX)
library(RcppRoll)
library(nnet)
##############################################################
#  UI
##############################################################
## Header content
header <- dashboardHeader(title = "GANNTrader")

## Sidebar content
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Predict", tabName = "predict", icon = icon("users")),
    menuItem("Analytics", tabName = "analytics", icon = icon("line-chart")),
    menuItem("Source code", icon = icon("file-code-o"), 
             href = "https://github.com/MokuzenShingo/GANNTrader16384/")
  )
)

## Body content
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "predict",
            fluidRow(
              column(width = 12, 
                     h3("USDJPY"),
                     textOutput("predict_update_text"),
                     textOutput("predict_pivot_text"),
                     textOutput("predict_action_text"))),
            fluidRow(
              column(width = 12, plotlyOutput("predict_chart"))),
            hr(),
            fluidRow(
              column(width = 12, dataTableOutput("predict_table")))),
    tabItem(tabName = "analytics",
            fluidRow(
              column(width = 12, 
                     h3("USDJPY"),
                     textOutput("analytics_update_text"))),
            fluidRow(
              column(width = 4, uiOutput("analytics_dates")),
              column(width = 2, checkboxInput("analytics_entry_check", label = "entry", value = T)),
              column(width = 2, checkboxInput("analytics_exit_check",  label = "exit",  value = F))),
            fluidRow(
              column(width = 12, 
                     plotlyOutput("analytics_chart"))
            ),
            hr(),
            fluidRow(
              column(width = 6, 
                     plotlyOutput("analytics_histgram")),
              column(width = 6, 
                     tableOutput("analytics_table"))
            ))
  )
)

##############################################################
#  Server 
##############################################################
server <- function(input, output, session) {
  source("server_set.R")
  source("pivot.R")
  source("return_analysis.R")
  value <- reactiveValues()
  calculateEveryHour <- function(){
    withProgress(value = 0,message = 'File Downloading',{
      download.file(url = "https://github.com/MokuzenShingo/GANNTrader/raw/master/data/realtimeData.RData",
                    "./data/realtimeData.RData")
      load("./data/realtimeData.RData")
      value$realtimeData <- realtimeData[["USDJPYhour"]]["2016-01-01::"]
      price <- na.omit(realtimeData[["USDJPYhour"]][paste0(substr(Sys.time() - 60 * 60 * 24 * 7, 1,10),"::")])
      value$price <- price
    })
    
    withProgress(value = 0,message = 'Now predicting',{
      validResult <- readRDS("./data/USDJPYhour_bestGeneration_16384.rds")
      gtype <- gtype.ini(validResult) 
      
      price_temp <- coredata(price[nrow(price) - 0:23,])
      position.table <- data.frame(
        posi.flag = c(0, rep(1, 24), rep(-1, 24)),
        backdate  = c(0, 1:24, 1:24),
        open      = c(0, price_temp[nrow(price_temp):1, "open"],  price_temp[nrow(price_temp):1, "open"]),
        high      = c(0, price_temp[nrow(price_temp):1, "high"],  price_temp[nrow(price_temp):1, "high"]),
        low       = c(0, price_temp[nrow(price_temp):1, "low"] ,  price_temp[nrow(price_temp):1, "low"]),
        close     = c(0, price_temp[nrow(price_temp):1, "close"], price_temp[nrow(price_temp):1, "close"])
      )
      
      indicators <- get_indicators(price)  # calculate technical indicators
      signal.table <- signal.table.ini(validResult, price,indicators) # calculate market condition signal
      
      ### predict optimum actions
      trade_signal <- apply(position.table, 1, function(position){
        # each classifier predict action
        step.sig <- apply(gtype, 1, function(x){
          ID <- x[length(x)]
          x <- as.numeric(x[-length(x)])
          ind.sig(gtype = x,
                  curr.price  = tail(price, 1),
                  curr.vola   = tail(indicators[[4]][, 1], 1),
                  signal      = tail(signal.table[, ID], 1),
                  posi.price  = position[3:6],
                  posi.flag   = position[1])
        })
        # ensemble predicted actions
        ense.sig <- as.integer(mode(step.sig))
        c(ense.sig, sum(step.sig == -1) / 500, sum(step.sig == 0) / 500, sum(step.sig == 1) / 500)
      })
      trade_signal.t <- t(trade_signal)
      entry_date <- c(tail(index(price) + 3600, 1), tail(index(price), 24)[24:1], tail(index(price), 24)[24:1])
      position <- c("none", rep("long", 24), rep("short", 24) )
      
      hour.df <- data.frame(as.character(entry_date), position,  # index
                            sapply(trade_signal.t[,1],action_text),  # action
                            apply(trade_signal.t[,2:4],2,percent_text))  # probability
      names(hour.df) <- c("EntryDate", "Position", "Action", "short", "hold", "long")
      value$hour.df <- hour.df
      value$time <- Sys.time()
    })
    withProgress(value = 0, message = 'making orderbook',{
      source("getOrderbook.R")
      get_orderbook()
      value$ens <- readRDS("./data/USDJPYhour_16384_orderbook.rds")
    })
  }
  
  calculateEveryHour()
  # observe({
  #   invalidateLater(1000 * 60, session)
  #   if(substr(Sys.time(),15,16) == "02"){
  #     calculateEveryHour()
  #   }
  # })

  output$predict_table <- renderDataTable({
    datatable(value$hour.df, rownames = FALSE,
              options = list( columnDefs = list(list(className = 'dt-body-right',  targets = 3:5),
                                                list(className = 'dt-body-center', targets = 0:2))))
  })
  
  output$predict_update_text <- renderText({
    paste0("last update : ", value$time)
  })
  
  output$predict_pivot_text <- renderText({
    pivot.text <- round(value$pivot,3)
    paste0("today's PIVOT : ",
           "  S2 :", pivot.text[2],
           ",  S1 :", pivot.text[3],
           ",  P :", pivot.text[4],
           ",  B1 :", pivot.text[5],
           ",  B2 :", pivot.text[6])
  })
  
  output$predict_action_text <- renderText({
    paste0("recommend action : ", value$hour.df[1,"Action"])
  })
  
  output$predict_chart <- renderPlotly({
    price.df <- as.data.frame(value$price)
    price.df$date <- index(value$price)
    
    dateCount <- price.df$date
    temp <- ifelse(as.integer(substr(dateCount,12,13)) < 6, 
                   paste0(substr(dateCount,6,8), as.integer(substr(dateCount,9,10)) - 1),
                   substr(dateCount,6,10))
    for(i in 1:length(temp)) if(as.integer(substr(temp[i],4,5)) == 0) temp[i] <- temp[i-7]
    temp <- ifelse(substr(temp,5,5) == "",
                   paste0(substr(temp,1,3), "0", substr(temp,4,4)),
                   as.character(temp))
    price.df$dayCount <- paste0(substr(dateCount,1,8), temp)
    
    price.df.g <- group_by(price.df, dayCount)
    price.df.day <- summarise(price.df.g,
                              dayClose = tail(close, 1),
                              dayHigh = max(high),
                              dayLow = min(low))
    pivot <- apply(price.df.day, 1, function(x) PIVOT(as.numeric(x[2]),as.numeric(x[3]),as.numeric(x[4])))
    pivot.table <- t(pivot)
    pivot.table.temp <- rbind(NA, pivot.table[-nrow(pivot.table),])
    value$pivot <- tail(pivot.table.temp,1)
    price.df.day <- cbind(price.df.day, pivot.table.temp)
    
    dat <- full_join(price.df, price.df.day, "dayCount")
    
    i <- list(line = list(color = '#64A8D7'))
    d <- list(line = list(color = '#262B34'))
    
    plot_ly(data = dat, x = ~date, 
            open = ~open, close = ~close, high = ~high, low = ~low,
            type="candlestick", increasing = i, decreasing = d) %>%
      add_lines(x =~dat$date, y = ~dat$S2, line = list(width = 2, dash = "2px", color = "#5A5F67"), inherit = F, name = "S2") %>%
      add_lines(x =~dat$date, y = ~dat$S1, line = list(width = 1, dash = "2px", color = "#5A5F67"), inherit = F, name = "S1") %>%
      add_lines(x =~dat$date, y = ~dat$P, line = list(width = 1, dash = "2px", color = "#A0BBCE"), inherit = F, name = "P") %>%
      add_lines(x =~dat$date, y = ~dat$B1, line = list(width = 1, dash = "2px", color = "#5A5F67"), inherit = F, name = "B1") %>%
      add_lines(x =~dat$date, y = ~dat$B2, line = list(width = 2, dash = "2px", color = "#5A5F67"), inherit = F, name = "B2") %>%
      layout(xaxis = list(title = "", rangeslider = list(visible = F)),
             yaxis = list(title = ""),
             showlegend = F)
  })
  
  dataInput <- reactive({
    analytic.period <- paste0(as.character(input$analytics_dates[1]), "::",as.character(input$analytics_dates[2]))
    cbind(value$realtimeData[analytic.period], value$ens[analytic.period])
  })
  
  output$analytics_update_text <- renderText({
    paste0("last update : ", value$time)
  })
  output$analytics_dates <- renderUI({
    dateRangeInput("analytics_dates", label = "period",
                   start = Sys.time() - 60 * 60 * 24 * 7,
                   end = Sys.time())
  })
  
  output$analytics_chart <- renderPlotly({
    dat <- dataInput()
    dat.df <- as.data.frame(dat)
    dat.df$date <- index(dat)
    dat.df$return0 <- na.fill(dat.df$return, 0)
    dat.df$return_cumsum0 <- cumsum(dat.df$return0)
    dat.df$entry_long  <- ifelse(dat.df$posi.flag == 1, dat.df$close, NA)
    dat.df$entry_short <- ifelse(dat.df$posi.flag == -1, dat.df$close, NA)
    dat.df$exit_long <- ifelse(dat.df$posi.temp == -1, dat.df$close, NA)
    dat.df$exit_short <- ifelse(dat.df$posi.temp == 1, dat.df$close, NA)
    
    p <- plot_ly(data = dat.df, x = ~date, 
                 open = ~open, close = ~close, high = ~high, low = ~low,
                 type="candlestick", 
                 increasing = list(line = list(color = '#A0BBCE')), 
                 decreasing = list(line = list(color = '#262B34'))) %>%
      layout(xaxis = list(title = "", rangeslider = list(visible = F)),
             yaxis = list(title = ""),
             showlegend = F)
    if(input$analytics_entry_check == T){
      p <- p %>% add_trace(x = ~dat.df$date, y = ~dat.df$entry_long ,
                           type = "scatter", mode = "markers", name = "entry long",
                           marker = list(color = '#0C88D7', size = 10, opacity=0.9)) %>%
        add_trace(x = ~dat.df$date, y = ~dat.df$entry_short,
                  type = "scatter", mode = "markers", name = "entry short",
                  marker = list(color = '#FC393A', size = 10, opacity=0.9))
    }
    
    if(input$analytics_exit_check == T){
      p <- p %>% add_trace(x = ~dat.df$date, y = ~dat.df$exit_long ,
                           type = "scatter", mode = "markers", name = "exit long",
                           marker = list(color = '#0C88D7', size = 10, symbol = "x", opacity=0.5))  %>%
        add_trace(x = ~dat.df$date, y = ~dat.df$exit_short,
                  type = "scatter", mode = "markers", name = "exit short",
                  marker = list(color = '#FC393A', size = 10, symbol = "x",opacity=0.5))
    }
    
    pp <- plot_ly(data = dat.df,x = ~date, y = ~return_cumsum0,
                  type = "scatter", mode = "lines", line = list(color = "#262B34")) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "return_cumsum"),
             showlegend = F)
    
    subplot(p,pp,nrows = 2, shareX = T,heights = c(0.7,0.2))
  })
  output$analytics_histgram <- renderPlotly({
    dat <- dataInput()
    long.return  <- as.data.frame(dat[dat$posi.temp == 1, "return"])
    short.return <- as.data.frame(dat[dat$posi.temp == -1, "return"])
    
    plot_ly(opacity=0.7) %>%
      add_histogram(x = ~long.return$return, name = "long", marker = list(color = '#0C88D7')) %>%
      add_histogram(x = ~short.return$return, name = "short", marker = list(color = '#FC393A')) %>%
      layout(barmode = "overlay",
             xaxis = list(title = ""),
             legend = list( orientation = "h", yanchor = "bottom"),
             title = "return")
  })
  output$analytics_table <- renderTable({
    dat <- dataInput()
    dat <- dat[dat$exit.flag == 1, ]
    total       <- return.analysis(dat[,"return"])
    long_entry  <- return.analysis(dat[dat$posi.temp ==  1, "return"])
    short_entry <- return.analysis(dat[dat$posi.temp == -1, "return"])
    data.frame(total = total, long_entry = long_entry, short_entry = short_entry)
  }, rownames = TRUE)
  
}
##############################################################
#  Bild app
##############################################################
ui <- dashboardPage(header, sidebar, body)
shinyApp(ui, server)
