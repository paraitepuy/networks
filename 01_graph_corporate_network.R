library(visNetwork)
library(tidyverse)

#########################################################################################################
# Follow this to: 
# GRAPH CORPORATE NETWORKS
#########################################################################################################

# We will graph a corporate network, step by step, following an example.

# You are interested in a company with 5 officers. You have the information in a .csv file.
# (You will find the file for this example in the folder input_output. All data is fictional).
onecompany <- read.csv("onecompany.csv", stringsAsFactors = FALSE)

# To use visNetwork we need two data frames: one for nodes and one for edges.
# In this case: nodes are the names of our companies and people, and edges the relationship between them.

# NODES 
# Nodes need a unique 'id'
nodes <- data.frame(id=c(onecompany$companyname,onecompany$companyofficers)) %>%
  unique()

# EDGES connect two nodes. 
# They need to have 'from' and 'to'
edges <- data.frame(from=onecompany$companyname,
                    to=onecompany$companyofficers)

# You can already draw your first graph...
visNetwork(nodes = nodes,
           edges = edges)

# ... but that graph needs names, aka labels or titles (which show on hover)
nodes$label <- nodes$id 
edges$title <- onecompany$officerposition 

# Draw your graph again: It's done!
visNetwork(nodes = nodes,
           edges = edges)


# More things useful for your visualization.

## 1) Change colours
## For example, because you want to draw attention to one of the directors
nodes$color <- ifelse(nodes$id=="Tani, Monica","#F2A918","#4C67BD")

visNetwork(nodes = nodes,
           edges = edges)

## 2) Add a title to a node
## For example, to clarify why you are drawing attention to the director above
nodes$title <- ifelse(nodes$id=="Tani, Monica","This person is mentioned in a law enforcement case","")

visNetwork(nodes = nodes,
           edges = edges)

## 3) Change the shape and size
## For example, to differenciate between companies (squares) and people (smaller triangles)
nodes$shape <- ifelse(nodes$id %in% onecompany$companyname, "square",
                      ifelse(nodes$id %in% onecompany$companyofficers, "triangle",NA))
nodes$size <- ifelse(nodes$id %in% onecompany$companyname, 24,
                     ifelse(nodes$id %in% onecompany$companyofficers, 15, NA))

visNetwork(nodes = nodes,
           edges = edges)

## 4) Change the shape to an icon
## Start by creating groups of nodes. We'll create 3 groups: 'companies', 'targetofficers' and 'officers'
nodes$group <- ifelse(nodes$id %in% onecompany$companyname, "companies",
                      ifelse(nodes$id %in% onecompany$companyofficers & nodes$id=="Tani, Monica", 
                             "targetofficers",
                             ifelse(nodes$id %in% onecompany$companyofficers,"officers",NA)))

nodes <- select(nodes,-c(color,shape,size)) #remove these columns so the source of the shapes and colors 
#correspond to the new groups we created and not the old colours and shapes.

# Pick the unicode for your icon at: https://fontawesome.com/icons?d=gallery

visNetwork(nodes = nodes,
           edges = edges) %>%
  visGroups(groupname = "companies", shape = "icon",
            icon = list(code="f1ad",color="#4C67BD"))%>%
  visGroups(groupname = "officers", shape="icon",
            icon=list(code="f007",color="#4C67BD")) %>%
  visGroups(groupname = "targetofficers", shape="icon",
            icon=list(code="f007",color="#F2A918")) %>%
  addFontAwesome()

## 5) Make one line dashed
## For example, because one of the 5 officers is no longer active in the company
edges$dashes <- ifelse(edges$to == "Hoffpauir, Jordon", TRUE, FALSE)

visNetwork(nodes = nodes,
           edges = edges) %>%
  visGroups(groupname = "companies", shape = "icon",
            icon = list(code="f1ad",color="#4C67BD"))%>%
  visGroups(groupname = "officers", shape="icon",
            icon=list(code="f007",color="#4C67BD")) %>%
  visGroups(groupname = "targetofficers", shape="icon",
            icon=list(code="f007",color="#F2A918")) %>%
  addFontAwesome()

## 6) Add a title to identify your graph and a footer for additional details
visNetwork(nodes = nodes,
           edges = edges,
           main = "The Corporate Network",
           footer = "Source: Paraitepui.") %>%
  visGroups(groupname = "companies", shape = "icon",
            icon = list(code="f1ad",color="#4C67BD"))%>%
  visGroups(groupname = "officers", shape="icon",
            icon=list(code="f007",color="#4C67BD")) %>%
  visGroups(groupname = "targetofficers", shape="icon",
            icon=list(code="f007",color="#F2A918")) %>%
  addFontAwesome()

## 7) Add a legend for your groups
visNetwork(nodes = nodes,
           edges = edges,
           main = "The Corporate Network",
           footer = "Source: Paraitepui.") %>%
  visGroups(groupname = "companies", shape = "icon",
            icon = list(code="f1ad",color="#4C67BD"))%>%
  visGroups(groupname = "officers", shape="icon",
            icon=list(code="f007",color="#4C67BD")) %>%
  visGroups(groupname = "targetofficers", shape="icon",
            icon=list(code="f007",color="#F2A918")) %>%
  addFontAwesome() %>%
  visLegend(width = 0.04, position = "right")

## 8) Or add a custom legend
## You'll create data frames for your nodes and edges with the information the legend should show
lndnodes <- data.frame(label = c("Company", "Officer", "Target Officer"), 
                       shape = c("icon"),
                       icon = list(face = "FontAwesome", 
                                   code = c("f1ad","f007","f007"),
                                   color = c("#4C67BD","#4C67BD","#F2A918")))

lndedges <- data.frame(label=c("Former officer", "Current officer"),
                       dashes=c(TRUE,FALSE),
                       color=c("#4C67BD"),
                       arrows.to=FALSE)

visNetwork(nodes = nodes,
           edges = edges,
           main = "The Corporate Network",
           footer = "Source: Paraitepui.") %>%
  visGroups(groupname = "companies", shape = "icon",
            icon = list(code="f1ad",color="#4C67BD"))%>%
  visGroups(groupname = "officers", shape="icon",
            icon=list(code="f007",color="#4C67BD")) %>%
  visGroups(groupname = "targetofficers", shape="icon",
            icon=list(code="f007",color="#F2A918")) %>%
  addFontAwesome() %>%
  visLegend(useGroups = FALSE, 
            position = "right",
            addNodes = lndnodes,
            addEdges = lndedges)


# When your graph is ready, save it as an HTML file so you can share it.
mycorporatenetwork <- 
  visNetwork(nodes = nodes,
             edges = edges,
             main = "The Corporate Network",
             footer = "Source: Paraitepui.") %>%
  visGroups(groupname = "companies", shape = "icon",
            icon = list(code="f1ad",color="#4C67BD"))%>%
  visGroups(groupname = "officers", shape="icon",
            icon=list(code="f007",color="#4C67BD")) %>%
  visGroups(groupname = "targetofficers", shape="icon",
            icon=list(code="f007",color="#F2A918")) %>%
  addFontAwesome() %>%
  visLegend(useGroups = FALSE, 
            position = "right",
            addNodes = lndnodes,
            addEdges = lndedges)

visSave(mycorporatenetwork, "mynetwork01.html")

# Now you can send the company report you were working on, with an explorable network :)
