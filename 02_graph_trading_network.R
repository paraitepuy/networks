library(visNetwork)
library(tidyverse)
library(igraph)

#########################################################################################################
# Follow this to: 
# GRAPH TRADING RELATIONSHIPS
#########################################################################################################

# We will graph a network with trading relationships, step by step, following an example.

# We are looking into a group of companies exporting from France. Information is in a .csv file.
# (You will find the file for this example in the folder input_output. All data is fictional).
tradingrels <- read.csv("tradingrels.csv", stringsAsFactors = FALSE, fileEncoding="UTF-8-BOM")

# NODES
# Nodes will be the names of the companies shipping and buying goods
nodes <- data.frame(id=c(tradingrels$shippername, tradingrels$buyername),
                    country=c(tradingrels$shippercountry, tradingrels$buyercountry)) %>%
  unique() %>%
  mutate(label=id)


# EDGES
# Edges will show the relationship between shippers (from) and buyers (to)
edges <- data.frame(from=tradingrels$shippername,
                    to=tradingrels$buyername,
                    arrows=c("to"), # Include arrows pointing to "to" (buyer) to show trade direction
                    goods=tradingrels$goods, # Keep other information about the transaction for later
                    valuefob=tradingrels$usdfob,
                    bol=tradingrels$billoflading,
                    date=tradingrels$dateshipped)


# Draw your graph!
visNetwork(nodes = nodes,
           edges = edges)


# More things useful for your visualization.

## 1) Change colors of the nodes
## For example, because you want to draw attention to one of the companies
nodes$color <- ifelse(nodes$id=="Succulent Blackmamba Corp","#F2A918","#4C67BD")

visNetwork(nodes = nodes,
           edges = edges)


## 2) Add titles to nodes
## For example, to include the countries of origin and the reason a company was highlighted
nodes$title <- ifelse(nodes$id=="Succulent Blackmamba Corp",
                      paste("Company listed by law enforcement.<br>", nodes$country),
                      as.character(nodes$country))

visNetwork(nodes = nodes,
           edges = edges)


## 3) Add titles to edges
## For example, to display the goods traded, date and value
edges$title <- paste("Goods: ",edges$goods,
                     "<br>Value: ", edges$valuefob/1000, "k USD", #show in thousands
                     "<br>Date: ", edges$date)

visNetwork(nodes = nodes,
           edges = edges)


## 4) Change size of the nodes, by their number of connections.
## For example, to display as larger the nodes that have more trade transactions.
## For this, we'll use a measure of CENTRALITY.

## Create an igraph object using our edges. Note 'from' and 'to' need to be in the first columns.
graphtrade <- graph_from_data_frame(edges, directed=TRUE)

## Degree centrality counts the number of connections of a node. We calculate it from the graph object.
nodes$degree <- centr_degree(graphtrade, mode = "all")$res

## We want the size of the nodes to correspond to the degree centrality.  
## Here we'll make minimun size = 5 and then proportional to the degree.

nodes$size <- nodes$degree/min(nodes$degree)*5

visNetwork(nodes = nodes,
           edges = edges)

## FOR YOUR INVESTIGATION: Anything noticeable in the graph? 
## In the example, there are 2 large nodes: those 2 companies have more transactions than the rest.
## Transactions are mostly between themselves. 


## 5) Change size of the nodes, by the value of their transactions.
## For example, to display as larger the nodes that traded larger values.

## Find the total traded by company, adding amounts sold and purchased.
nodes <- merge(x = nodes, 
               y = aggregate(tradingrels$usdfob, by = list(tradingrels$shippername), FUN = sum) %>%
                 rbind(aggregate(tradingrels$usdfob, by = list(tradingrels$buyername), FUN = sum)) %>%
                 setNames(c("id","totaltraded")),
               by = "id")

nodes$size <- nodes$totaltraded/max(nodes$totaltraded)*40 #Here we'll make maximum size = 40

visNetwork(nodes = nodes,
           edges = edges)

## Since we are showing the amount traded in the node size, we could display the value in the title.
nodes$title <- paste(nodes$title,"<br>Traded", nodes$totaltraded/1000, "k usd")

## FOR YOUR INVESTIGATION: Anything noticeable in the graph? 
## In the example, one node is larger than the rest. That company trades in total a larger amount.


## 6) Change size of the edges.
## For example, to make the edges thicker if they represent transactions of larger values.

edges$value <- edges$valuefob

visNetwork(nodes = nodes,
           edges = edges) %>%
  visEdges(scaling = list(min=3, max=8)) #Set scaling for nicer visualization, 
# when the range of values for edges is too wide.

## FOR YOUR INVESTIGATION: Anything noticeable in the graph? 
## The company with the largest amount traded has 3 transactions one day exporting medicaments,
## and 3 transactions the following day selling vehicle parts. Transactions have similar amounts.


## 7) Add identifying information and options to explore the graph

## Include information that describes your data: countries involved, dates of the transactions.
about <- paste(c("Exports from:",unique(tradingrels$shippercountry), 
                 "<br>To:", unique(tradingrels$buyercountry), 
                 "<br>During:", min(tradingrels$dateshipped), "-", max(tradingrels$dateshipped)), 
               collapse=" ")

visNetwork(nodes = nodes,
           edges = edges, 
           main = "Trading Relationships",
           submain = about,
           height = 500, # Delimit the size of the graph if you need to
           width = 800) %>%
  visEdges(scaling = list(min=2, max=8)) %>% ## Include options to better explore the graph
  visOptions(highlightNearest = TRUE,
             nodesIdSelection = TRUE, #To select nodes by their id
             selectedBy = "country")  #To select nodes by their country

## Consider adding other attributes that would be useful to select by.
## For example, the type of goods the companies trade. 

## To select by traded goods, the nodes need a column with the traded goods
goodtrd <- data.frame(id = c(tradingrels$shippername,tradingrels$buyername),
                      traded = c(tradingrels$goods,tradingrels$goods)) %>%
  unique()

goodtrd <- aggregate(goodtrd$traded, by=list(goodtrd$id), FUN=paste, collapse = ", ") %>%
  setNames(c("id","goodtraded"))

nodes <- merge(x=nodes, y=goodtrd)

rm(goodtrd)

visNetwork(nodes = nodes,
           edges = edges, 
           main = "Trading Relationships",
           submain = about,
           height = 500,
           width = 800) %>%
  visEdges(scaling = list(min=2, max=9)) %>%
  visOptions(highlightNearest = TRUE,
             nodesIdSelection = TRUE,
             selectedBy = "goodtraded") #To select nodes by the goods they traded


## 8) Add a custom legend
## You'll create data frames for your nodes and edges with the information the legend should show

## Create the labels for the legend
lblsmall <- paste("Amount traded \n",round(quantile(nodes$totaltraded/1000,c(.3))), "k usd")
lbllarge <- paste("Amount traded \n",round(quantile(nodes$totaltraded/1000,c(.99))), "k usd")
valmed <- median(nodes$size)
valsmall <- as.numeric(round(quantile(nodes$size,c(.3))))
vallarge <- as.numeric(round(quantile(nodes$size, c(.99))))

## Create the data frames for the legend describing nodes and edges
lndnodes <- data.frame(label = c("Company", "Target Company", lblsmall, lbllarge),
                       shape = c("icon"),
                       icon = list(face = "FontAwesome", 
                                   code = c("f111"),
                                   color = c("#4C67BD","#F2A918","#A9A9A9", "#A9A9A9"),
                                   size = c(valmed,valmed,valsmall,vallarge)))

lndedges <- data.frame(label = "Width coresponds to \n transaction value",
                       color = "#4C67BD",
                       font = list(align = c("bottom"),
                                   size = 12))

# When your graph is ready, save it as an HTML file so you can share it.

mytradingnetwork <- 
  visNetwork(nodes = nodes,
             edges = edges, 
             main = "Trading Relationships",
             submain = about,
             footer = list(text="Visit https://github.com/paraitepuy/networks",
                           style='color:#696969;font-size:12px;text-align:center;'),
             height = 500,
             width = 800) %>%
  visEdges(scaling = list(min=2, max=9)) %>%
  visOptions(highlightNearest = TRUE,
             nodesIdSelection = TRUE,
             selectedBy = "goodtraded") %>%
  visLegend(useGroups = FALSE,
            position = "right",
            addNodes = lndnodes,
            addEdges = lndedges,
            zoom = FALSE,
            stepY = 70) %>% 
  addFontAwesome() 

mytradingnetwork

visSave(mytradingnetwork, "mynetwork02.html")


# Done!

#########################################################################################################
