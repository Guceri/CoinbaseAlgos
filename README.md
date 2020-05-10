# CoinbasePro Execution Dashboard
  This is a dashboard written in R, using the Shiny interface to execute common tasks needed for execution and data management
of crypto currency markets on the CoinbasePro exchange.  While there is plenty of excellent backtesting, and charting programs available,
I haven't come across any algorithmic execution tools that can be overlayed onto existing strategies, so I decided to build some that could 
be useful for anyone looking to gain an edge on their execution.

# Getting Started
TODO: 
-list of programs needed to get started (download links)
r, rstudio, sqlite db_browser
-install any dependencies (rgdax, rtools)
-how to download repository onto computer
-update input and database files

Type sample commands in here:
'''
install.packages('devtools')
'''

Graphical flow chart of files and how everything works along with description

## Limit Order Algo
  The limit order algo allows you to be best bid/offer by updating your limit price anytime the market moves away from your current limit price.  Large orders can be broken down into smaller "slices" so that your resting order is not so large that it would look obvious.  In future installations, I will include a randomizer to the slices so that they are not constantly the same size. 
  
## Gamma Order Algo
  Since CoinbasePro has included the ability to leverage BTC trading, I created a "Gamma" algo which keeps track of your buying power and buys additional BTC as price moves up and buying power is made available.  This allows to have a convex potential exposure to BTC which is similar to the effects of Gamma in an options position.  Future installations will create more functionality for limiting how this is used and providing more flexibility for dealing with margin calls and potential exit targets.  

## Market Makeer Algo
  This is an algo that allows you to quote two sided markets at specified widths along with various built in bias which look at the order book as well as current available position/funds.

## Data Download
  Since CoinbasePro only allows you to download limited amount of historical data per request, a sqlite database was created to be able to store all historical data (1 min granularity), as well as the production of a CSV file which can be used for other software programs (such as a backtesting software).  
