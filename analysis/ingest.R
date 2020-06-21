library(tidyverse)
library(jsonlite)
library(fs)
db_info <- fromJSON('https://api.fda.gov/download.json')
d <- db_info$results$drug$event$partitions
d <- d %>% 
  mutate(year = str_extract(display_name, '^\\d{4}')) %>% 
  mutate(year = as.numeric(year))
d %>% filter(year >= 2015) %>% group_by(year) %>% summarize(size=sum(as.numeric(size_mb))) %>% 
  summarize(sum(size))

get_file <- function(u){
  period <- str_extract(u, '\\d{4}q\\d{1}')
  yr <- str_extract(period, '\\d{4}') 
  qtr <- str_extract(u, 'q\\d{1}')
  dest_dir = path('/Volumes/ARAASTAT/Astra', yr, qtr)
  if(!fs::dir_exists(dest_dir)) fs::dir_create(dest_dir)
  dest_file <- fs::path(dest_dir,  basename(u))
  if(fs::file_exists(dest_file)) return('File downloaded')
  download.file(u, destfile = dest_file, mode = 'wb')
  unzip(dest_file, exdir = dest_dir)
}

map(d %>% filter(year==2019) %>% pull(file), get_file)
map(d %>% filter(year==2020) %>% pull(file), get_file)
