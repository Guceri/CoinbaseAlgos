#NOTE: 
#Rate limit for public API request is 3/sec and up to 6/sec in bursts
#Rate limit for private API request is 5/sec and up to 10/sec in bursts

#NOTE: Any functions that are having trouble getting an api request should be wrapped in tryCatch and the some value
#needs to be created that can be handled in the code (usually <-NULL is the best right now)

#######################################################################################################################
#                                               PUBLIC API CALLS
#######################################################################################################################
bid_price <- function(){
  error<-FALSE
  orderbook <- tryCatch({public_orderbook(product_id = pair, level = 1)$bids[1]},error = function (e) {
    error<<-TRUE
  })
  if(error){
    bid <- current_theo
    bid
  }else{
    orderbook
  }
}

ask_price <- function(){
  error<-FALSE
  orderbook <- tryCatch({public_orderbook(product_id = pair, level = 1)$asks[1]},error = function (e){
    error<<-TRUE
  })
  if(error){
    ask <- current_theo
    ask
  }else{
    orderbook
  }
}

bid_size <- function(){
  orderbook <- public_orderbook(product_id = pair, level = 1)$bids[2]
  orderbook
}

ask_size <- function(){
  orderbook <- public_orderbook(product_id = pair, level = 1)$asks[2]
  orderbook
}

last_price <- function(){
  last_100_trades <- tail(public_trades(product_id = pair)[3],n=1)[1,1]
  last_100_trades
}

#######################################################################################################################
#                                               AUTH API CALLS
#######################################################################################################################
curr_bal_usd <- function(){
  #stays local to function
  error<-FALSE
  balance <- tryCatch({accounts(api.key = my_api.key, secret = my_secret, passphrase = my_passphrase)},error = function (e){
    error<<-TRUE
    })
  if(error){
    balance<-NULL
    balance
  }else{
    balance <- subset(balance$balance, balance$currency == substr(pair,5,8))
    balance
  }
}

curr_bal <- function(){
  error<-FALSE
  balance <- tryCatch({accounts(api.key = my_api.key, secret = my_secret, passphrase = my_passphrase)},error = function (e){
      error<<-TRUE
      })
  if(error){
    balance<-NULL
    balance
  }else{
    balance <- subset(balance$balance, balance$currency == substr(pair,1,3))
    balance
  }
}


check_open_orders <- function(){
  error<-FALSE
  my_orders<-tryCatch({open_orders(my_api.key, my_secret, my_passphrase)},error = function (e){
    error<<-TRUE
    })
  if(error){
    my_orders<-NULL
  }else{
    my_orders
  }
}

cancel_orders <- function(){
  cat('Cancelling Open Orders...\n')
  cancel_orders <- cancel_order(api.key = my_api.key, secret = my_secret, passphrase = my_passphrase)
}

no_orders <- function(){
  error<-FALSE
  my_order_status <- tryCatch({length(check_open_orders()) == 0},error = function(e){
    error<<-TRUE
    })
  if(error){
    my_order_status<-NULL
    my_order_status
  }else{
    my_order_status
  }
}
