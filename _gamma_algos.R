
gamma <- function () {
  tryCatch(usd <<- round(curr_bal_usd(),2),
           error = function(e){
             usd <<- numeric(0)
           })
  
  tryCatch(iposition <<- round(last_price()*curr_bal(),2),
           error = function(e){
             iposition <<- numeric(0)
           })      
  
  #Sometimes iposition or usd come back black causing a null value
  if (is.empty(usd) || is.empty(iposition)){
    cat('Had trouble pulling account values, trying again...\n')
  }else{
    #USD + iposition
    equity <- iposition + usd
    
    #calculate margin score
    if (usd <= 0) {
      margin_score <- equity/iposition 
    }else{
      margin_score <- 1
    }
    
    #calculate buying power
    if ((equity*3)-iposition-buffer <= 0){
      buying_power <<- 0
    }else{
      buying_power <<- round((equity*3)-iposition,2)-buffer
    }
    
    #execution Logic
    if (buying_power >= cash_available & price_limit > ask_price()){
      
      # notify that an order is being sent
      if(Pushbullet){
        pbPost(type = 'note', title = "Excess Capital Available", body = sprintf("Signal generated at %s", Sys.time()), email = myEmail, apikey = myAPIkey)
      }
      
      # identify which algo to use 
      if (order_type == "limit"){
        gamma_limit_algo()
      }else{
        gamma_market_algo()
      }
      
    }
    
    cat(paste('Buying Power: ',buying_power,'\n'))
  }
  
  cat(paste(Sys.time(),'\n'))
}

gamma_limit_algo <- function(){
  if (!(WorkingOrder)){
    #outside of while loop to only send message once per signal 
    cat('Sending BUY Order...\n')
    WorkingOrder <<- TRUE
    while (WorkingOrder){
      
      order_size <<- floor(buying_power/ask_price()*1000000)/1000000
      
      #this section tries to get an order out into the market while dealing with potential issues
      #if we get past this section, it should mean we have an order in the market that we need to check the status of
      if (no_orders()){ 
        #set variables for below algo
        Order_Price <<- NULL
        attempt <<- 0
        
        while(is.null(Order_Price) && attempt <= 2){
          attempt <<- attempt + 1
          #Send order, if it fails, mute the response
          tryCatch(add_order(product_id = pair, api.key = my_api.key, secret = my_secret, passphrase = my_passphrase,
                             type = "limit", price = bid_price(), side = "b", size = order_size, post_only = TRUE),error = function(e){return(
                               cat('Something went wrong...\n')
                             )})
          
          Sys.sleep(1)
          #pull limit price from response, if no response, it will loop again. This is just a print log to notify that there are errors
          tryCatch(Order_Price <<- as.numeric(as.character(response[1,2])),
                   error = function(e){
                     Order_Price <<- NULL
                     if(attempt == 3){
                       cat('ALERT: Max order attempts tried...\n')
                       break
                     }else { 
                       cat('Order Attempt Failed, trying again...\n')
                     }
                   })
          
        }
        
      }else {#if an order is present, skip the attempt to submit an order
        cat('An order is already present, no new orders can be created...\n')
        cancel_orders()
        cat('Order will trigger again if enough cash is available...\n')
        WorkingOrder <<- FALSE
        break
      }
      
      #time given to work the order
      Sys.sleep(Order_Refresh_Rate)#in seconds
      #starts the while loop to check on the open order (local variable to function)
      check_order <- TRUE
      
      #checking on order has 3 outcomes: Filled, Partial, or no Fill
      while (check_order){
        #True if no orders are present
        order_status <- no_orders()
        
        
        #If there are no orders (ie. order was filled) 
        if (order_status){
          cat('BUY Order Filled...\n')
          #if there are no orders, we assume filled so kill algo and reset booleans
          WorkingOrder <<- FALSE
          check_order <- FALSE
        }else
          #When market gets away from us
          if (bid_price() > Order_Price){
            cancel_orders()
            #Cycle back to the top of the workingOrder loop
            check_order <- FALSE
            #working order is still true here so loop back and create a new updated order
            cat('Adjusting to NBBO Price...\n')
          }else{
            cat('Still Working Order...\n')
            #addtional time given to work the order
            Sys.sleep(Order_Refresh_Rate)#in seconds
            #check_order is still true here so we stay in this loop until either the trade fills or the market runs away from us
          }
        
        #while (check_order)  
      }
      #while (workingOrder)  
    }
    #if (!(WorkingOrder))
  }
  #function()
}

gamma_market_algo <- function(){
  cat('Sending market BUY Order...\n')
  
  attempt <<- 0
  
  while(attempt <= 2){
    
    attempt <<- attempt + 1
    
    order_size <<- floor(buying_power/ask_price()*1000000)/1000000
    
    #Send order, if it fails, mute the response
    tryCatch(add_order(product_id = pair, api.key = my_api.key, secret = my_secret, passphrase = my_passphrase,
                       type = "market", side = "b", size = order_size),error = function(e){return(
                         cat('Something went wrong...\n'))})
    Sys.sleep(1)              
    if (attempt == 3){
      cat('Max attempts tried...\n')
    }
    
  }
}