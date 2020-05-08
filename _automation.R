#public API request limit is 3/sec and up to 6/sec in bursts
#account API request limit is 5/sec and up to 10/sec in bursts
myMaker <- function () {
  #heartbeat
  cat(paste(Sys.time(),"\n"))
  #for use with myMaker & limitorderalgo functions
  postonly<<-TRUE
  #total size of orders 
  total_size<<-sum(size*b_size,size*s_size)
  myBalance <<- curr_bal()
  #if there was an error getting the balance, skip MM
  if(!is.null(myBalance)){
    if (myBalance < min_position){
      cat("inventory is too small. Buying to rebalance...\n")
      cancel_orders()
      #pair + postonly is already determined
      orders <<- 1 #splices
      #buy enough to quote ~3 more times in sell direction before retriggering
      order_size_original <<- size*3
      #BROADCAST SIGNAL
      if(Pushbullet){
        pbPost(type = 'note', title = "Low Inventory Signal Generated", body = sprintf("Signal generated at %s", Sys.time()), email = myEmail, apikey = myAPIkey)
      }
      buy_limit_algo()
    }
    if (myBalance > max_position){
      cat("inventory is too large. Selling to rebalance...\n")
      cancel_orders()
      #pair + postonly is already determined
      orders <<- 1 #splices
      #sell enough to quote ~3 more times in buy direction before retriggering
      order_size_original <<- size*3
      #BROADCAST SIGNAL
      if(Pushbullet){
        pbPost(type = 'note', title = "High Inventory Signal Generated", body = sprintf("Signal generated at %s", Sys.time()), email = myEmail, apikey = myAPIkey)
      }
      sell_limit_algo()
    }
    ############################################################################################################################## 
    #NULL if error & no orders will product a list of 0
    istatus<<-check_open_orders()
    #helps identify full or partial order fills; produces a 0 if an error or no orders
    isize<-sum(as.numeric(istatus$size))-sum(as.numeric(istatus$filled_size)) 
    #figure out what theo value is (finds NBBO too)
    current_theo <<- theo_price()
    #if there are no errors in checking open orders and trades do exist and nothing has traded; 
    if(!is.null(istatus) & sum(as.numeric(istatus$size)) != 0 & (total_size == isize)){
      #figure out my best bid & offer
      isells<-subset(istatus,istatus$side == "sell")
      ibuys<-subset(istatus,istatus$side == "buy")
      my_best_offer<<-as.numeric(min(isells$price))
      my_best_bid<<-as.numeric(max(ibuys$price))
      #If trouble getting orderbook, current_theo will be NULL also causing an error
      tryCatch({
          if (current_theo >= (my_best_offer-.8*spread) || current_theo <= (my_best_bid+.8*spread)) {
            cat(paste("no edge vs.theo:",current_theo,"\n"))
            cancel_orders()
          }
      },error = function(e){
        cat("Not able to check theo against my best bid/offer\n")
      })
    }
    #if an order is missing, partial filled, error in check (returns isize <-0), or order dupes, then cancel remaining orders and replace
    if (isize < total_size || isize > total_size) {
      if (isize != 0){
        cancel_orders()
      }
      cat("sending new quotes...\n")
      
      if (!is.null(current_theo)){
        if(current_theo < best_bid){
          ibid <<- current_theo}else{ibid<<-best_bid}
        if(current_theo > best_ask){
          iask <<- current_theo}else{iask<<-best_ask}
      }else{
        ibid<<-best_bid
        iask<<-best_ask
      }
      
      itrades<<-ihistory()
      #GOTCHA- if the last trade is far from current price, it will take a new trade in the opposite direction to reset
      if (!is.null(itrades)){
        if (itrades$Last_Side == "buy" & itrades$Last_Price < ibid){
          ibid<<-itrades$Last_Price
        }
        if (itrades$Last_Side == "sell" & itrades$Last_Price > iask){
          iask<<-itrades$Last_Price
        }
      }
      
    #######################################################################################################################
      tryCatch( add_order(product_id = pair, api.key = my_api.key, secret = my_secret, passphrase = my_passphrase,
              type = "limit", price = round(ibid-spread, 2), side = "b", size = (size*b_size), post_only = postonly), error = function(e){return(
                cat('Something went wrong on the 1st level buy order...\n'))})
      tryCatch(add_order(product_id = pair, api.key = my_api.key, secret = my_secret, passphrase = my_passphrase,
              type = "limit", price = round(iask+spread, 2), side = "s", size = (size*s_size), post_only = postonly), error = function(e){return(
                cat('Something went wrong on the 1st level sell order...\n'))}) 

      #if (isize < total_size) {new order submission section}
      }
  #if(!is.null(myBalance)) {skips the entire market maker function and executes the below code}
  }else{
    cat("Trouble getting balance\n")
    cancel_orders()
  }
#mymaker
}
  
theo_price<- function(){
  error<-FALSE
  ibook<-tryCatch({public_orderbook(pair,level = 2)},error = function(e){
    cat("Problem with getting the orderbook\n")
    error<<-TRUE
    if(Pushbullet){
      pbPost(type = 'note', title = "Problem getting theo_price", body = sprintf("Problem generated at %s", Sys.time()), email = myEmail, apikey = myAPIkey)
    }
  })
  if(!error){
    bid_prices<-round(as.data.frame(ibook[[2]][1:50]),2)
    bid_sizes<-round(as.data.frame(ibook[[2]][51:100]),2)
    best_bid<<-ibook[[2]][1]
    last_bid<-best_bid-2*spread
    ask_prices<-round(as.data.frame(ibook[[3]][1:50]),2)
    ask_sizes<-round(as.data.frame(ibook[[3]][51:100]),2)
    best_ask<<-ibook[[3]][1]
    last_ask<-best_ask+2*spread
    bid_range_prices<-subset(bid_prices,bid_prices[,1]>last_bid)
    bid_range_sizes<-head(bid_sizes,nrow(bid_range_prices))
    ibid_top<-cbind(bid_range_prices,bid_range_sizes)
    ask_range_prices<-subset(ask_prices,ask_prices[,1]<last_ask)
    ask_range_sizes<-head(ask_sizes,nrow(ask_range_prices))
    iask_top<-cbind(ask_range_prices,ask_range_sizes)
    bid_size_sum<-sum(ibid_top[,2])
    ask_size_sum<-sum(iask_top[,2])
    bid_weight<-round(bid_size_sum/(bid_size_sum+ask_size_sum),2)
    ask_weight<-round(ask_size_sum/(bid_size_sum+ask_size_sum),2)
    weighted_mid_pt<-round((last_ask-last_bid)*bid_weight+last_bid,2)
    return(weighted_mid_pt)
  }else{
    return(NULL)
  }
}

ihistory <- function(){
  error<-FALSE
  ifills<-tryCatch({fills(my_api.key,my_secret,my_passphrase,pair)},error = function(e){
    cat("Problem with getting the order history\n")
    error<<-TRUE
    if(Pushbullet){
      pbPost(type = 'note', title = "Problem getting ihistory", body = sprintf("Problem generated at %s", Sys.time()), email = myEmail, apikey = myAPIkey)
    }
  })
  
  if(!error){
    last_trade<-ifills[ifills$created_at == max(ifills$created_at),]
    last_side<-last_trade$side
    fill_price<-last_trade$price
    ibuys<- ifills[ifills$side == "buy",]
    isells<-ifills[ifills$side == "sell",]
    ibuy_dollar_sum<-sum(ibuys$size*ibuys$price)
    isell_dollar_sum<-sum(isells$size*isells$price)
    total_buys<-round(sum(ibuys$size),1)
    total_sells<-round(sum(isells$size),1)
    ibuy_ave<-round(ibuy_dollar_sum/total_buys,2)
    isell_ave<-round(isell_dollar_sum/total_sells,2)
    ilist<-list("Total_Buys"=total_buys,"Buy_Price"=ibuy_ave,"Total_Sells"=total_sells,"Sell_Price"=isell_ave,"Last_Side"=last_side,"Last_Price"=fill_price)
    return(ilist)
  }else{
    return(NULL)
  }
}
