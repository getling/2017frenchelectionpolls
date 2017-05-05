# Clear workspace
rm(list = ls())
library(tidyverse)
library(magrittr)
library(rvest)
setwd("/Users/getling/Downloads/2017frenchelectionpolls")
# Set base theme
theme_set(theme_minimal())

# Experimental scraping, currently getting old polls due to Pym.js iframe loading
# url <- 'http://www.bbc.com/news/world-europe-39692961'
# 
# poll_tbl <-
#   read_html(url) %>% 
#   html_nodes('.table-container') %>% 
#   html_children() %>% 
#   html_table() %>% 
#   .[[1]]
# 
# poll_tbl %<>% 
#   mutate( polldate = as.Date(Date,
#                              format = "%d/%m/%Y"),
#           org = as.factor(Poll)) #%>%
#  select(-Date, -Poll)
# End experimental

# Load data (from http://www.bbc.com/news/world-europe-39692961)
polls <- read_csv("Frenchpolls.csv", 
         col_types = cols(
           polldate = col_date(format = "%m/%d/%y"),
           org = col_factor(levels = NULL),
           Macron = col_number(),
           LePen = col_number(),
           sample = col_number()
))

library(RColorBrewer)
library(ggthemes)
library(scales)

# Full unweighted average
polls %>%
  gather('Name', 'Proportion', 3:4) %>%
  ggplot(aes( x = polldate, 
              y = Proportion, 
              color = Name)) +
  geom_point(alpha = 0.25) + 
  geom_smooth() +
  coord_cartesian(ylim= c(25, 75)) +
  geom_hline(aes(yintercept = 50), 
             color = 'lightgray', 
             linetype = 'dashed') +
  
  theme_pander() +
  scale_colour_brewer(palette = "Set1") 

# Full weighted average
polls %>%
  gather('Name', 'Proportion', 3:4) %>%
  ggplot(aes( x = polldate, 
              y = Proportion, 
              color = Name,
              size = sample,
              weight = sample)) +
  geom_point(alpha = 0.25) + 
  geom_smooth() +
  coord_cartesian(ylim= c(25, 75)) +
  geom_hline(aes(yintercept = 50), 
             color = 'lightgray', 
             linetype = 'dashed') +
  
  theme_pander() +
  scale_colour_brewer(palette = "Set1") 

# All time, by organization
polls %>%
  filter(org != 'Le Terrain',
         org != 'Kantar Sofres') %>%
  gather('Name', 'Proportion', 3:4) %>%
  ggplot(aes( x = polldate, 
              y = Proportion, 
              color = Name,
              size = sample,
              weight = sample)) +
  geom_point(alpha = 0.25) + 
  geom_smooth() +
  coord_cartesian(ylim= c(0, 100)) +
  facet_wrap( ~ org, 
              ncol = 4) +
  geom_hline(aes(yintercept = 50), 
             color = 'lightgray', 
             linetype = 'dashed') +
  
  theme_pander() +
  scale_colour_brewer(palette = "Set1") + 
  theme(legend.position = c(0.9, 0.25))

# Since 1st Round
polls %>%
  filter(polldate > '2017-04-21') %>%
  gather('Name', 
         'Proportion', 
         3:4) %>%
  ggplot(aes( x = polldate,
              y = Proportion, 
              color = Name,
              size = sample,
              weight = sample)) +
  geom_point(alpha = 0.25) + 
  geom_smooth() +
  geom_hline(aes(yintercept = 50), 
             color = 'lightgray', 
             linetype = 'dashed') +
  coord_cartesian(ylim = c(25, 75)) +
  theme_pander() +
  scale_colour_brewer(palette = "Set1")


# Attempt to use bsts
library(lubridate)
library(bsts)
library(dplyr)
library(ggplot2)
library(tidyquant)

pollts <- as_xts(polls, date_col = polldate)
#Y <- window(pollts, start=c(1949, 1), end=c(1959,12))
