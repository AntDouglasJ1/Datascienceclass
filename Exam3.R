#clear environment
#set working directory
rm(list=ls(all=TRUE))
setwd("C:/Users/antdo/Documents/R/Exam 3")
#libraries
library(tidycensus)
library(rio)
library(tidyverse)
library(varhandle)
library(ggrepel)
library(geosphere)
library(rgeos)
library(viridis)
library(mapview)
library(rnaturalearth)
library(rnaturalearthdata)
library(devtools)
library(remotes)
library(raster)
library(sp)
library(sf)
library(Imap)
library(data.table)
library(WDI)
library(pdftools)
library(tidyr)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(rvest)
# need to install via github with devtoolds and remotes
library(rnaturalearthhires)# devtools::install_github("ropensci/rnaturalearthhires")
devtools::install_github("ropensci/rnaturalearthhires")
library(ggsflabel)# devtools::install_github("yutannihilation/ggsflabel")
devtools::install_github("yutannihilation/ggsflabel")
#my computer is old and is taking to long to knit document so had to put things in comments because I have been waiting for 15 minutes, my exam is already late because compiling took to long
#https://github.com/AntDouglasJ1/Datascienceclass

# load the census API key 
#census_api_key("0c33ba019dec86a5e736a28abe201c62faddbd21",install = TRUE,overwrite = TRUE)
#Loading census data for 2010 and 2015
#v10 <-load_variables(year = 2010,"acs5")
#View(v10)
#v15 <-load_variables(year = 2015,"acs5")
#View(v15)
#getting gini data from 5-yr ACS data from 2010 and 2015 in USA
#texas_gini_v10 <-get_acs(geography = "state",variables =c(GiniIndex=c("B19083_001")),country = "USA",year = 2010)
#texas_gini_v15 <-get_acs(geography = "state",variables =c(GiniIndex=c("B19083_001")),country = "USA",year = 2015)
#merge into one panel
#suppressMessages(library(bit64))
#inequality_panel <-
#  bind_rows(texas_gini_v10,texas_gini_v15)
#Rename estimate to gini
#setnames(inequality_panel, "NAME", "state")
#setnames(inequality_panel,"estimate","gini")
#sort data by year


#names(inequality_panel)[names(inequality_panel) == "NAME"] <- "state"
#names(inequality_panel)[names(inequality_panel) == "estimate"] <- "gini"

#head(inequality_panel)

#Reshape inequality panel into wide
#inequality_wide <-
  #inequality_panel%>%
  #pivot_wider(id_cols =c("GEOID", "state", "gini"),# unique IDs
              #names_from = "year",# names for new wide vars
             # values_from = "year",# data to put in new wide vars
             # names_prefix = "year_" )# prefix to add before years
#head(inequality_wide)
#convert to long format
#use pivot_longer
#inequality_long <-
 # inequality_wide%>%
  #pivot_longer(cols =starts_with("year"),# use columns starting with "year"
   #            names_to ="year",# name of new column
    #           names_prefix = "year_",# part of string to drop
     #          values_to = "year",# where to put numeric values
      #         values_drop_na = FALSE)%>% # don't drop NAs
#head(inequality_long)
  
#show have same number of observations
#inequality_long=inequality_wide

#collapse inequality_long
#inequality_collapsed <-
  #inequality_long%>%
  #group_by(state, GEOID, gini)%>%
  #summarize(across(where(is.numeric), sum))
#get map of USA
USA <-ne_countries(country ='United States of America', scale = "large", returnclass = "sf")

#create map
#USA_map =ggplot()+
 # geom_sf(data = USA)+
  #geom_sf(data = USA,aes(fill=inequality_collapsed))+
 # theme_void()+
 # scale_fill_viridis(option = "viridis")+
 # theme(legend.position = "right")
#print(USA_map)

#WDI import
gdp_current <- WDI(country = "all",indicator =c("NY.GDP.MKTP.CD"),
                                    start = 2006, end = 2007, extra = FALSE, cache = NULL)
#deflate
library(data.table)
deflator_data =WDI(country = "all", indicator =c("NY.GDP.DEFL.ZS"),
                   start = 2001,
                   end = 2015,
                   extra = FALSE, cache = NULL)
setnames(deflator_data,"NY.GDP.DEFL.ZS", "deflator")
#merge deflator into data_collapsed by year
#deflated_data<-left_join(gdp_current,deflator_data,by=c("year"))
#deflate data
#gdp_deflated<-deflated_data$deflated_amount = deflated_data$current_amount/(deflated_data$deflator/100)
#head(gdp_deflated)

#Three main components in a shiny app are: UI with inputs and outputs, Server with render, and a Call to shinyAPP
#pulling pdf from website
#online
armeniatext=pdf_text(pdf = "https://pdf.usaid.gov/pdf_docs/PA00TNMG.pdf")
#myxtext is a character vector.
armeniatext
#change character vector to a data frame where each page is an observation
armeniatext=as.data.frame(armeniatext, stringsAsFactors=FALSE)
colnames(armeniatext)[which(names(armeniatext)=="armeniatext")] <- "text"#change column name
#in order to tokenize text into words:
armeniatext=armeniatext%>%
  unnest_tokens(word, text)
#in order to get rid of stop words:
data(stop_words)
armeniatext <- armeniatext%>%
  anti_join(stop_words)
#top terms 
armeniatext%>%
  count(word, sort = TRUE)

#Scrape The Billboard Hot 100 page
hot100page <-"https://www.billboard.com/charts/hot-100"
hot100 <-read_html(hot100page)
hot100
str(hot100)
body_nodes <-hot100%>%
  html_node("body")%>%
  html_children()
body_nodes
#see nodes inside nodes
body_nodes%>%
  html_children()
rank <-hot100%>%
  rvest::html_nodes('body')%>%
  xml2::xml_find_all("//span[contains(@class,
                     'chart-element__rank__number')]")%>%
  rvest::html_text()
artist <-hot100%>%
  rvest::html_nodes('body')%>%
  xml2::xml_find_all("//span[contains(@class,
                     'chart-element__information__artist')]")%>%
  rvest::html_text()
title <-hot100%>%
  rvest::html_nodes('body')%>%
  xml2::xml_find_all("//span[contains(@class,
                     'chart-element__information__song')]")%>%
  rvest::html_text()
last_week <-hot100%>%
  rvest::html_nodes('body')%>%
  xml2::xml_find_all("//span[contains(@class,
                     'chart-element__information__delta__text text--last')]")%>%
  rvest::html_text()

#combine into dataframe and view
chart_df <-data.frame(rank, artist, title,last_week)
knitr::kable(
  chart_df%>%head(10))
View(chart_df)

