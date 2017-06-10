library(tidyverse)
library(stringr)
library(rvest)

# get the url for each episode
links <- read_html("http://www.seinology.com/scripts-english.shtml") %>% 
  html_nodes(".spacer2 td:nth-child(1) a") %>% 
  html_attr("href") %>% 
  data_frame() %>% 
  select(url = 1) %>%
  filter(grepl("shtml", url) & !duplicated(url)) %>% 
  mutate(full_url = paste0("http://www.seinology.com/", url))


# function to pull any script return necessary data
pull_script <- function(page_url){
  
  # read in the full script
  script <- read_html(page_url) %>% 
    html_nodes(".spacer2 font") %>% 
    html_text() %>% 
    paste0(collapse = "\n") %>% 
    str_replace_all("\\u0092", "'") %>% 
    str_replace_all("\\u0085", "...")
  
  # get season number
  season <- str_extract(script, "(?i)(?<=season )\\d")
  
  # get episode number
  episode <- str_extract(page_url, "\\d+")
  
  # get episode title
  title <- str_extract(script, "(?<= - ).*")
  
  # get writer
  writer <- str_extract(script, "(?i)(?<=written by(:)?\\s).*")
  
  # get lines
  script_edit <- str_replace_all(script, "\t|\\(.*?\\)|\\[.*?\\]|NOTE:", "")
  
  # regex patterns for pulling speakers and lines
  line_regex <- "(?<=\n[A-Z]{1,20}(\\.)?(\\s{1,20})?([A-Z]{1,20})?:).*"
  
  speaker_regex <- "(?<=\n)[A-Z]+(\\.)?(\\s)?([A-Z]+)?(?=:)"
  
  lines <- unlist(str_extract_all(script_edit, line_regex))
  
  lines <- str_replace_all(lines, "\\u0092", "'")
  
  # get the scenes and the speaker
  if (str_detect(script, "INT\\.|EXT\\.") & episode != 69){
    
    script_df <- data_frame(
      text = unlist(str_split(script, "INT\\.|EXT\\.")),
      scene_num = 0:(length(text) - 1)
    ) %>% 
      # remove episode information
      slice(-1) %>% 
      mutate(text = str_replace_all(text, "\t|\\(.*?\\)|\\[.?\\]|NOTE:", ""),
             speaker = str_extract_all(text, speaker_regex)) %>% 
      unnest(speaker)
  
  } else {
    
    script_df <- data_frame(
      text = unlist(str_split(script, "(?<=\n|\t)\\[.*?\\]|scene:")),
      scene_num = 0:max((length(text) - 1), 1)
    ) %>% 
      # remove episode information
      slice(-1) %>% 
      mutate(text = str_replace_all(text, "\t|\\(.*?\\)|\\[.?\\]|NOTE:", ""),
             speaker = str_extract_all(text, speaker_regex)) %>% 
      unnest(speaker)
  
  } 
  
  if (nrow(script_df) == length(lines)){
    
    dat <- script_df %>% 
      transmute(season = season,
                episode = as.numeric(episode),
                title = title,
                writer = writer,
                scene_num,
                scene = paste0("e", episode, "s", scene_num),
                speaker,
                line = lines)
  } else {
    
    dat <- data_frame(
      season = season,
      episode = as.numeric(episode),
      title = title,
      writer = writer,
      scene_num = NA,
      scene = NA,
      speaker = unlist(str_extract_all(script_edit, speaker_regex)),
      line = lines
    )
    
  }
  dat
}

# run for all episodes
seinfeld <- lapply(links$full_url, pull_script) %>% 
  bind_rows() 

seinfeld <- seinfeld %>% 
  mutate(episode = as.numeric(episode))

seinfeld$scene[seinfeld$episode %in% c(54, 121)] <- NA
seinfeld$scene_num[seinfeld$episode %in% c(54, 121)] <- NA
