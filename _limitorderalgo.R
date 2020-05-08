#This is an semi aggressive limit order which will be the the best bid at all times until filled
#GOTCHA - Partial fill then manual cancel order will still give a "Order filled" message
#Gotcha - If price goes up as you buy and there is not enough cash for the size, the algo will stop and tell you your order size is too large
#GOTCHA - Only one order can be processed at a time. Make sure there are no orders ANYWHERE on exchange

buy_limit_algo <- function(){
  #cap # of orders
  if (orders > 10){
    orders <<- 10
    cat('# of Orders is too large, reduced to 10...\n')
  }
  
  #initiate partial fill boolean
  partial_fill <<- FALSE
  kill <<- FALSE
  
  for (i in 1:orders){
    
    if (!(WorkingOrder)){
      
      cat(paste('Sending',order_num[i], 'BUY Order...\n'))
      WorkingOrder <<- TRUE
      
      while (WorkingOrder){
        
        #establish initial cash position before order is created
        initial_cash <<- curr_bal_usd()
        
        
        #determine what sizing needs to be used
        if (partial_fill){
          #Pull the partial size from below
          order_size <<- partial_size
        }else{
          #if partial fill is off use the original value inputed
          order_size <<- order_size_original
        }
        
        if (order_size > (initial_cash/bid_price())){
          #check if order size is too large
          cat('Order Rejected...\n')
          cat('The order you are sending is larger than available cash...\n')
          kill <<- TRUE
          WorkingOrder <<- FALSE
          break
        }
        
        if (no_orders()){ 
          #set variables for below algo
          Order_Price <<- NULL
          order_rejected <<- FALSE
          attempt <<- 0
          
          while((is.null(Order_Price) || order_rejected) && attempt <= 2){
            #reset to false incase looping back from an order rejection
            order_rejected<<-FALSE
            attempt <<- attempt + 1
            #Send order, if it fails, mute the response
            tryCatch(add_order(product_id = pair, api.key = my_api.key, secret = my_secret, passphrase = my_passphrase,
                               type = "limit", price = bid_price(), side = "b", size = order_size, post_only = postonly),error = function(e){return(
                                 cat('Something went wrong...\n')
                               )})
            Sys.sleep(1)
            #pull limit price from response, if no response, loop again
            tryCatch(Order_Price <<- as.numeric(as.character(response[1,2])),
                     error = function(e){
                       Order_Price <<- NULL
                       if(attempt == 3){
                         cat('ALERT: Max order attempts tried...\n')
                         kill <<- TRUE
                       }else { 
                         cat('Order Attempt Failed, trying again...\n')
                       }
                     }
            )
            #this allows to re=attempt the order if there was no connection issue with the exchange, but they rejected the order for some reason
            if (response$status == "rejected"){
              cat("Order Rejected..\n")
              cat("Might have attempted to trade through the market with a post only order\n")
              cat(paste('ReSending',order_num[i], 'BUY Order...\n'))
              order_rejected<<- TRUE
              #If max attempts are reached but its due to order rejection (instead of error) we need to kill the algo
              if (attempt == 3){
                cat("Attempted multiple orders..keep getting rejected\n")
                cat("Check to see what the issue may be\n")
                kill <<- TRUE
                WorkingOrder <<- FALSE
                break
              }
            }
          }
          
        }else {#if an order is present, skip the attempt to submit an order
          cat('An order is already present, no new orders can be created...\n')
          kill <<- TRUE
          WorkingOrder <<- FALSE
          break
        }
        
        #time given to work the order
        Sys.sleep(Order_Refresh_Rate)#in seconds
        #starts the while loop to check on the open order - local variable to function
        check_order <- TRUE
        
        while (check_order){
          #create variables to reduce api requests while checking orders
          my_USD_balance <- curr_bal_usd()
          order_status <- no_orders()
          #check to see if any open orders are present
          if (order_status && (my_USD_balance<initial_cash)){
            cat(paste(order_num[i],'BUY Order Filled...\n'))
            #if there are no orders, and the correct position, end execution and start at orders loop
            partial_fill <<- FALSE
            WorkingOrder <<- FALSE
            check_order <- FALSE
          }else
            #if no orders are present and position did not change
            if (order_status && (my_USD_balance==initial_cash)){
              cat('BUY Order Cancelled...\n')
              #end execution logic because no orders exists
              kill <<- TRUE
              partial_fill <<- FALSE
              WorkingOrder <<- FALSE
              check_order <- FALSE
            }else 
              if (bid_price() > Order_Price){
                
                #used for trycatch error handling
                fill_error<<-FALSE
                #figure out how much is left to trade in case of a partial fill
                #NOTE: it take the first order in the open orders if for some reason there are multiple orders this could get affected
                filled <- tryCatch({as.numeric(check_open_orders()[1,11])},error = function (e){
                  cat("Problem getting fill\n")
                  fill_error<<-TRUE         
                  })
                  
                #run this code if we didn't have any errors in getting the fill value
                if (!fill_error){
                  #If an order was partial filled, figure out how much is left
                  if (filled != 0){
                  cat('Partial fill on existing order...\n')
                  partial_fill <<- TRUE
                  partial_size <<- order_size-filled
                  }else{
                    #If nothing was filled, figure out if the current order was a partial from previous or not
                    if (order_size < order_size_original){
                      partial_fill <<- TRUE
                      partial_size <<- order_size-filled
                    }else{
                      partial_fill <<- FALSE
                    }
                  }
                  cancel_orders()
                  Sys.sleep(1)
                  #Cycle back to the top of the workingOrder loop
                  check_order <- FALSE
                  #working order is still true here so loop back and create a new updated order
                  cat('Adjusting to NBBO Price...\n')
                  
                }else{
                  #Run this if we had an issue checking the fill value (likely order filled while processing fill data which then generates error)
                  #if there are no orders, assume fill and start loop again
                  #reset error handling
                  fill_error<<-FALSE
                  partial_fill <<- FALSE
                  WorkingOrder <<- FALSE
                  check_order <- FALSE
                }
                
              }else{
                cat('Still Working Order...\n')
                #addtional time given to work the order
                Sys.sleep(Order_Refresh_Rate)#in seconds
                #check_order is still true here so we stay in this loop unti either the trade fills or the market runs away from us
              }
          
        #while (check_order)  
        }
        
      #while (workingOrder)  
      }
      
    #if (!(WorkingOrder))
    }
    
    #Stop the for loop (order splices) if interrupted during execution
    if(kill){
      kill <<- FALSE
      break}
    
  #for (i in 1:orders)
  }
  
#function()
}

sell_limit_algo <- function(){
  #cap # of orders 
  if (orders > 10){
    orders <<- 10
    cat('# of Orders is too large, reduced to 10...\n')
  }
  
  #initiate booleans 
  partial_fill <<- FALSE
  kill <<- FALSE
  
  for (i in 1:orders){
    
    if (!(WorkingOrder)){
      
      cat(paste('Sending',order_num[i], 'SELL Order...\n'))
      WorkingOrder <<- TRUE
      
      while (WorkingOrder){
        
        #establish initial position before order is created
        initial_pos <<- curr_bal()
        
        #determine what sizing needs to be used
        if (partial_fill){
          #Pull the partial size from below
          order_size <<- partial_size
        }else{
          #if partial fill is off use the original value inputed
          order_size <<- order_size_original
        }
        
        if (order_size > initial_pos){
          #check if order size is too large
          cat('Order Rejected...\n')
          cat('The order you are sending is larger than available position...\n')
          kill <<- TRUE
          WorkingOrder <<- FALSE
          break
        }
        
        if (no_orders()){
          #set variables for below algo
          Order_Price <<- NULL
          order_rejected <<- FALSE
          attempt <<- 0
          while((is.null(Order_Price) || order_rejected) && attempt <= 2){
            #reset to false incase looping back from an order rejection
            order_rejected<<-FALSE
            attempt <<- attempt + 1
            #Send order, if it fails, mute the response
            tryCatch(add_order(product_id = pair, api.key = my_api.key, secret = my_secret, passphrase = my_passphrase,
                               type = "limit", price = ask_price(), side = "s", size = order_size, post_only = postonly),error = function(e){return(
                                 cat('Something went wrong...\n')
                               )})
            Sys.sleep(1)
            #pull limit price from response, if no response, loop again
            tryCatch(Order_Price <<- as.numeric(as.character(response[1,2])),
                     error = function(e){
                       Order_Price <<- NULL
                       if(attempt == 3){
                         cat('ALERT: Max order attempts tried...\n')
                         kill <<- TRUE
                       }else { 
                         cat('Order Attempt Failed, trying again...\n')
                       }
                     }
            )
            #this allows to re=attempt the order if there was no connection issue with the exchange, but they rejected the order for some reason
            if (response$status == "rejected"){
              cat("Order Rejected..\n")
              cat("Might have attempted to trade through the market with a post only order\n")
              cat(paste('ReSending',order_num[i], 'SELL Order...\n'))
              order_rejected<<- TRUE
              #If max attempts are reached but its do to order rejection (instead of error) we need to kill the algo
              if (attempt == 3){
                cat("Attempted multiple orders..keep getting rejected\n")
                cat("Check to see what the issue may be\n")
                kill <<- TRUE
                WorkingOrder <<- FALSE
                break
              }
            }
          }

        }else {
          cat('Order Rejected...\n')
          cat('An order is already present, no new orders can be created...\n')
          kill <<- TRUE
          WorkingOrder <<- FALSE
          break
        }
        
        #time given to work the order
        Sys.sleep(Order_Refresh_Rate)#in seconds
        #starts the while loop to check on the open order - local variable to function
        check_order <- TRUE
        
        while (check_order){
          #create variables to reduce api requests while checking orders
          my_balance <- curr_bal()
          order_status <- no_orders()
          #check to see if any open orders are present and if the position has changed
          if (order_status && (my_balance<initial_pos)){
            cat(paste(order_num[i],'SELL Order Filled...\n'))
            #if there are no orders, and the correct position, end execution
            partial_fill <<- FALSE
            WorkingOrder <<- FALSE
            check_order <- FALSE
          }else
            #if no orders are present and position did not change
            if (order_status && (my_balance==initial_pos)){
              cat('SELL Order Cancelled...\n')
              kill <<- TRUE
              partial_fill <<- FALSE
              WorkingOrder <<- FALSE
              check_order <- FALSE
            }else 
              #If the market moved away from us 
              if (ask_price() < Order_Price){
        
                #used for trycatch error handling
                fill_error<<-FALSE
                #figure out how much is left to trade in case of a partial fill
                #NOTE: it take the first order in the open orders if for some reason there are multiple orders this could get affected
                filled <- tryCatch({as.numeric(check_open_orders()[1,11])},error = function (e){
                  cat("Problem getting fill\n")
                  fill_error<<-TRUE         
                })
                
                #run this code if we didn't have any errors in getting the fill value
                if (!fill_error){
                  #If an order was partial filled, figure out how much is left
                  if (filled != 0){
                    cat('Partial fill on existing order...\n')
                    partial_fill <<- TRUE
                    partial_size <<- order_size-filled
                  }else{
                    #If nothing was filled, figure out if the current order was a partial from previous or not
                    if (order_size < order_size_original){
                      partial_fill <<- TRUE
                      partial_size <<- order_size-filled
                    }else{
                      partial_fill <<- FALSE
                    }
                  }
                  cancel_orders()
                  Sys.sleep(1)
                  #Cycle back to the top of the workingOrder loop
                  check_order <- FALSE
                  #working order is still true here so loop back and create a new updated order
                  cat('Adjusting to NBBO Price...\n')
  
                }else{
                  #Run this if we had an issue checking the fill value (likely order filled while processing fill data which then generates error)
                  #if there are no orders, assume fill and start loop again
                  #reset error handling
                  fill_error<<-FALSE
                  partial_fill <<- FALSE
                  WorkingOrder <<- FALSE
                  check_order <- FALSE
                }
                
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
    
    #Stop the for loop (order splices) if interrupted during execution
    if(kill){
      kill <<- FALSE
      break}
    
    #for (i in 1:orders)
  }
  
  #function()
}