library(visNetwork)
library(tidyverse)

#########################################################################################################
# Use this if you need to: 
# GRAPH CORPORATE NETWORKS
#########################################################################################################

# USE YOUR DATA from a .csv file
# Download the template01.csv available in the input_output folder.
# Delete the example data and copy the data that you would like to show.
# Do not change the column names or the name of the file.
# Go to the bottom of this script if you need details on how to fill each field.

# GRAPH YOUR CORPORATE NETWORK

templatedata <- read.csv("template01.csv", stringsAsFactors = FALSE, fileEncoding="UTF-8-BOM")

# The nodes
nodes <- data.frame(id=c(templatedata$companyname,templatedata$officername),
                    title=c(templatedata$company_keyinformation,templatedata$officer_keyinformation),
                    highlight=c(templatedata$company_highlight,templatedata$officer_highlight)) %>%
  unique() %>%
  mutate(label=id, 
         group=ifelse(id %in% templatedata$companyname & highlight=="YES", "TargetCompany",
                      ifelse(id %in% templatedata$officername & highlight=="YES", "TargetOfficer",
                             ifelse(id %in% templatedata$companyname,"Company",
                                    ifelse (id %in% templatedata$officername, "Officer", NA))))) #We are creating 4 groups

# The edges
edges <- data.frame(from=templatedata$companyname,
                    to=templatedata$officername,
                    title=templatedata$officerposition,
                    source=templatedata$sources)

# The graph
mycorporatenetwork <- 
  visNetwork(nodes = nodes,
           edges = edges,
           main = "Corporate Network",
           footer = list(text="Visit https://github.com/paraitepuy/networks",
                         style='color:#CDC8C4;font-size:12px;text-align:center;')) %>%
           visGroups(groupname = "TargetCompany", shape = "icon",
                     icon = list(code="f1ad",color="#F2A918"))%>%
           visGroups(groupname = "TargetOfficer", shape="icon",
                     icon=list(code="f007",color="#F2A918")) %>%
           visGroups(groupname = "Company", shape="icon",
                     icon=list(code="f1ad",color="#4C67BD")) %>%
           visGroups(groupname = "Officer", shape="icon",
                     icon=list(code="f007",color="#4C67BD")) %>%
           addFontAwesome() %>%
           visLegend(width = 0.04, position = "right")

mycorporatenetwork

visSave(mycorporatenetwork, "corporatenetwork01.html")

# Done!

#########################################################################################################
# HOW TO FILL THE TEMPLATE

# 'companyname' Name of the companies you'd like to include in your graph. Make sure each company name is
# written in the exact same way.

# 'officername' Name of the owners, directors or related people you'll include in your graph. Make sure 
# each individual name is written in the exact same way.

# 'officerposition'	describes the relationship between the company and individual. 

# 'officer_keyinformation' Information about the officer you'd like to display in the graph as text. 
# Include for example "Person we are onboarding", "Sanctioned", "From high risk jurisdiction".

# 'company_keyinformation' Information about the company you'd like to display in the graph as text. 
# Include for example "Company under investigation", "Recently incorporated in secrecy jurisdiction".

# 'officer_highlight'	and 'company_highlight' YES or NO. YES if you'd like to highlight the officer or
# company with a different colour.

# 'sources' make note about where the information about the nodes and edges came from. For audit trail. 
#########################################################################################################
