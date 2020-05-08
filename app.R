rm(list=ls())

suppressMessages(library(shiny))
suppressMessages(library(shinyjs))
suppressMessages(library(shinythemes))
suppressMessages(library(xts))
suppressMessages(library(plyr))
suppressMessages(library(RPushbullet))
suppressMessages(library(RSQLite))
suppressMessages(library(rgdax))
suppressMessages(library(lubridate))
suppressMessages(library(anytime))
suppressMessages(library(stringr))
suppressMessages(library(tcltk))
suppressMessages(library(dplyr))
suppressMessages(library(installr))

source("_limitorderalgo.R")
source("_gamma_algos.R")
source("_coinbase_hist.R")
source("_inputs.R")
source("_busy_indicator.R")
source("_proxyclick.R")
source("_automation.R")
source("_gdax.R")

#######################################################################################################################
                                                   #USER INTERFACE#
#######################################################################################################################
ui <- fluidPage(
  theme = shinytheme("darkly"),
  useShinyjs(),
  #used for busy indicator
  tags$style(appCSS),
  #used for "proxy click"
  tags$head(tags$script(HTML(jscode))),
#=================
#LIMIT ORDER ALGO
#=================
  navlistPanel(
    tabPanel("Limit Order",      
      titlePanel("Crypto Limit Order Algo"),
      selectInput(inputId = "pair_limit","Product",c("BTC-USD","ETH-USD","LTC-USD","ZRX-USD")),
      selectInput(inputId = "side","Side",c("Buy","Sell")),
      tagAppendAttributes(
        textInput(inputId = "splices","# of Orders",""),
        'data-proxy-click' = "submit"
      ),
      tagAppendAttributes(
       textInput(inputId = "quantity","Quantity",""),
        'data-proxy-click' = "submit"
     ),
     checkboxInput("postOnly","Post Only",FALSE),
     withBusyIndicatorUI(
        actionButton("submit","Submit Order",class = "btn-primary")
      ),
     #Print Console Updates to Screen
     textOutput("console")
    ),
#==================
#GAMMA ALGO 
#==================    
    tabPanel("Gamma Algo",      
            titlePanel("Crypto Gamma Order Algo"),
            selectInput(inputId = "pair_gamma","Product",c("BTC-USD")),
            selectInput(inputId = "algo_type","Order Type",c("limit","market")),
            textInput(inputId = "buffer","$ buffer","1"),
            textInput(inputId = "buy_power","Min. Buying Power","10"),

            #used flowLayout to align buttons horizontally
            flowLayout(
              #disable start button when script is running
              withBusyIndicatorUI(
                actionButton("gamma_run","START")),
                actionButton("gamma_end","STOP")
                )
            ),
#==================
#MARKET MAKER ALGO 
#==================  
    tabPanel("Market Maker",
            titlePanel("Crypto Market Maker Algo"),
            selectInput(inputId = "pair_maker","Market",c("ETH-USD","BTC-USD","LTC-USD","ZRX-USD")),
            textInput(inputId = "size","Order Size",".01"),
            selectInput(inputId = "bias","Order Sizing Bias",c("Neutral","Long","Short")),
            checkboxInput(inputId = "autobias","Automatic Order Sizing Bias",FALSE),
            textInput(inputId = "spread","Min Point Spread",".05"),
            textInput(inputId = "minposition","Min Position","10"),
            textInput(inputId = "maxposition","Max Position","120"),
             
            #used flowLayout to align buttons horizontally
            flowLayout(
              #disable start button when script is running
              withBusyIndicatorUI(
                actionButton("market_run","START")),
                actionButton("market_end","STOP")
                )
            ),
#================
#HISTORICAL DATA 
#================     
    tabPanel("Data Download", 
             selectInput(inputId = "pair_data","Market",c("BTC-USD","ETH-USD","ETH-BTC")),
             actionButton("download","Download")
             ),
    #Width of navigation list and tabset respectively
    widths = c(2,10)
  ) 
)
#######################################################################################################################
                                                      #SERVER#
#######################################################################################################################
server <- function(input, output) {
  #debugging tool will print the status of all the inputs and values to identify potential issues
  #observe(cat(str(reactiveValuesToList(input)), "\n"))
#=================
#LIMIT ORDER ALGO
#=================
#LIMIT ALGO ENABLE/DISABLE SUBMIT 
  #Disable clicking of Submit button until a proper quantity and splice value is filled in by the user
  observe({
    if (is.null(input$quantity) || input$quantity == "" || input$quantity == 0 || is.na(as.numeric(input$quantity)) ||
        is.null(input$splices) || input$splices == "" || input$splices == 0 || is.na(as.numeric(input$splices)) 
        ){
      shinyjs::disable("submit")
    } else {
      shinyjs::enable("submit")
    }
  })
#LIMIT ALGO SUBMIT BUTTON
  #press submit button and the below code runs
  trade <- observeEvent(input$submit, {
    withBusyIndicatorServer("submit", {
      #save chosen pair as variable for execution
      pair<<-input$pair_limit
      #save quantity
      order_size_original <<- as.numeric(input$quantity)
      #save # of orders rounded down if fractional
      orders <<- floor(as.numeric(input$splices))
      postonly <<- input$postOnly
      #The proxy click bypassed the disable of the submit button so we need to check again and do nothing if the values are not proper
      if (is.null(input$quantity) || input$quantity == "" || input$quantity == 0 || is.na(as.numeric(input$quantity)) ||
          is.null(input$splices) || input$splices == "" || input$splices == 0 || is.na(as.numeric(input$splices)) 
      ){
        output$console <- renderText({
            "You clicked enter and the inputs weren't properly filled out..."
          })
      }else{
        #send execution based on trade direction
        if (input$side == "Buy"){buy_limit_algo()}else 
        if (input$side == "Sell"){sell_limit_algo()}
      }
    })
  })
  #once an input is changed, clear any text output (if there was any)
  observe({
    input$product
    input$splices
    input$quantity
    output$console <- renderText({""})
  })
#######################################################################################################################  
#=================
#GAMMA ALGO
#=================
  #GAMMA ALGO ENABLE/DISABLE SUBMIT 
  #Disable clicking of Submit button until a proper buffer and buying power value are filled in by the user
  observe({
    if (is.null(input$buffer) || input$buffer == "" || input$buffer == 0 || is.na(as.numeric(input$buffer)) ||
        is.null(input$buy_power) || input$buy_power == "" || input$buy_power == 0 || is.na(as.numeric(input$buy_power)) 
    ){
      shinyjs::disable("gamma_run")
    } else {
      shinyjs::enable("gamma_run")
    }
  })
  #GAMMA ALGO SUBMIT BUTTON
  observe({
    ###START GAMMA ALGO###
    isolate({
      pair<<-input$pair_gamma
      order_type<<-input$algo_type
      buffer<<-as.numeric(input$buffer)
      cash_available <<- as.numeric(input$buy_power)
    })
    
    #Block initial shiny app start up
    if (input$gamma_run == 0){
      shinyjs::disable("gamma_end")
      initial_gamma <<- TRUE
      return()
    }
    
    #Gamma loop
    if (input$gamma_run > input$gamma_end){
      #Send message on startup
      if (initial_gamma){
        cat("Gamma Algo has started...\n")
        initial_gamma<<- FALSE 
        shinyjs::disable("gamma_run")
        shinyjs::enable("gamma_end")
      }
      #run algo function
      gamma()
      invalidateLater(500)
    }else{
      shinyjs::enable("gamma_run")
      shinyjs::disable("gamma_end")
      cat("Gamma Algo has ended...\n")
      #Reset booleans 
      initial_mm<<- TRUE
      
    }
  })
#######################################################################################################################
#==================
#MARKET MAKER ALGO 
#==================  
  observe({
    if (is.null(input$size) || input$size == "" || input$size == 0 || is.na(as.numeric(input$size)) ||
        is.null(input$spread) || input$spread == "" || input$spread == 0 || is.na(as.numeric(input$spread))
    ){
      shinyjs::disable("market_run")
    } else {
      shinyjs::enable("market_run")
    }
  })
  observe({
    if (!input$autobias){
      shinyjs::enable("bias")
      if(input$bias == "Long"){
        s_size <<- 1
        b_size <<- 1.5
      }else
        if(input$bias == "Short"){
          s_size <<- 1.5
          b_size <<- 1
        } else {
          s_size <<- 1
          b_size <<- 1
        }
    }else{
      shinyjs::disable("bias")
      s_size <<- 1
      b_size <<- 1
    }
  })
  observe({
    ###START MARKET MAKER###
    isolate({
      pair<<-input$pair_maker
      size<<-as.numeric(input$size)
      spread<<-as.numeric(input$spread)
      min_position <<- as.numeric(input$minposition)
      max_position <<- as.numeric(input$maxposition)
    })
    #Block initial shiny app start up
    if (input$market_run == 0){
      shinyjs::disable("end")
      inventory_trouble <<- FALSE
      initial_mm <<- TRUE
      return()
    }
    #MM loop
    if (input$market_run > input$market_end){
      #Send message on startup
      if (initial_mm){
        cat("Market Maker has started...\n")
        initial_mm<<- FALSE 
        shinyjs::disable("market_run")
        shinyjs::enable("market_end")
        }
      if (!inventory_trouble){
        myMaker()
        invalidateLater(500)
      }
    }else{
      if (!inventory_trouble){
        #if script is stopped by the end button
        cancel_orders()
        cat("Market Maker has Ended...\n")
      }else {
        #reset the inventory trouble boolean
        inventory_trouble <<- FALSE
      }
      #regardless of how the script is stopped, run the below
      shinyjs::enable("market_run")
      shinyjs::disable("market_end")
      #Reset booleans if there is another start up (don't need to reset inventory_trouble as it would not have triggered)
      initial_mm<<- TRUE
    }
  })
  #######################################################################################################################
  #================
  #HISTORICAL DATA
  #================
  data_download <- observeEvent(input$download, {
    withBusyIndicatorServer("download", {
      pairdata <<- input$pair_data
      pull_hist()
    })
  })
#server
}
# Run the app ----
shinyApp(ui = ui, server = server)