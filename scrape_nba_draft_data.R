library(rvest)
library(tidyverse)

get_draft_data <- function(year){

  draft_url <- paste0("http://www.basketball-reference.com/draft/NBA_", 
                      year, ".html")
  
  draft_site <- read_html(draft_url)
  
  # pull the links to the players' pages from draft table
  draft <- draft_site %>% 
    html_nodes(".left:nth-child(4) a") %>% 
    html_attr("href") %>% 
    data_frame() %>% 
    select(Link = 1) %>% 
    mutate(Year = year)

  stats_url <- paste0("http://www.basketball-reference.com/leagues/NBA_", 
                year + 1, "_advanced.html")
  
  stats_site <- read_html(stats_url)
  
  # get the links to the players' pages from stats table
  stats_links <- stats_site %>% 
    html_nodes("th+ .left a") %>% 
    html_attr("href") %>% 
    data_frame() %>% 
    select(Link = 1)
  
  # get the stats table
  stats <- stats_site %>% 
    html_node("table") %>% 
    html_table(header=TRUE)
  
  # make new data_frame because of problems with unnamed variables
  data_frame(Player = stats$Player,
                       MP = stats$MP,
                       WS = stats$WS) %>% 
    filter(Player != "Player") %>% 
    cbind(stats_links) %>% 
    filter(!duplicated(Link)) %>% 
    mutate(MP = as.numeric(MP),
           WS = as.numeric(WS),
           TotalMP = sum(MP),
           TotalWS = sum(WS)) %>% 
    inner_join(draft, by="Link")
  
}

# pull data 1989 - 2016
nba <- lapply(1989:2016, get_draft_data) %>% 
  bind_rows()
