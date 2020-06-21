library(tidyverse)
library(openfda)

yrs = seq(2004, 2020)
freq_country <- map(yrs, ~fda_query('/drug/event.json') %>% 
      fda_filter('receivedate',paste0('[',.x,'0101+TO+',.x,'1231]')) %>% 
      fda_count('primarysource.reportercountry.exact') %>% 
      fda_limit(1000) %>% fda_exec() %>% set_names(c('term', paste('count',.x, sep='_'))))
bl = reduce(freq_country, right_join) %>% 
  gather(year, count,-term) %>% 
  mutate(year = as.numeric(str_remove(year, 'count_'))) %>% 
  mutate(term = ifelse(term=='COUNTRY NOT SPECIFIED', NA, term))

top_10 <- bl %>% filter(year==2019, !is.na(term)) %>% slice_max(count, n=10)

ggplot(bl %>% filter(year < 2020, !is.na(term)), aes(x = year, y=count, color=term))+
  geom_line(show.legend=F) + scale_y_log10()

ggplot(bl %>% filter(year < 2020, !is.na(term), term %in% top_10$term), aes(x = year, y=count, color=term))+
  geom_line(show.legend=T) + scale_y_log10()


## What indicators most commonly appear in adverse event reporting?

freq_indications <- map(yrs, 
                        ~fda_query('/drug/event.json') %>% 
                          fda_filter('receivedate', paste0('[', .x, '0101+TO+',.x,'1231]')) %>% 
                          fda_count('patient.drug.drugindication.exact') %>% 
                          fda_limit(1000) %>% fda_exec() %>% set_names(c('term',paste('count', .x, sep='_'))))
blah = reduce(freq_indications, full_join) %>% 
  gather(yr, count, -term) %>% 
  mutate(yr = as.numeric(str_remove(yr, 'count_')))
blah %>% nest(-yr, data=c(term,count)) %>% 
  mutate(data = map(data, slice_max, count, n=10)) %>% 
  unnest() %>% select(-count) %>% 
  janitor::tabyl(term, yr) %>% 
  janitor::adorn_totals('col') %>% 
  arrange(-Total)

blah %>% filter(yr==2020) %>% slice_max(count, n=10)
blah %>% nest(-yr, data=c(term,count)) %>% 
  mutate(data = map(data, slice_max, count, n=10)) %>% 
  filter(yr >= 2015) %>% 
  unnest() %>% select(-count) %>% 
  janitor::tabyl(term, yr) %>% 
  janitor::adorn_totals('col') %>% 
  arrange(-Total)

## Primary sources

### USA
freq_quals <- map(yrs, 
                        ~fda_query('/drug/event.json') %>% 
                          fda_filter('receivedate', paste0('[', .x, '0101+TO+',.x,'1231]')) %>% 
                          fda_filter('primarysource.reportercountry.exact','US') %>% 
                          fda_count('primarysource.qualification') %>% 
                          fda_limit(1000) %>% fda_exec() %>% set_names(c('term',paste('count', .x, sep='_'))))
freq_quals <- reduce(freq_quals, full_join) %>% 
  gather(yr, counts, -term) %>% 
  mutate(yr = as.numeric(str_remove(yr, 'count_')))
quals_key = tribble(~code,~qualification,
                    1, 'Physician',
                    2, 'Pharmacist',
                    3, 'Other health professional',
                    4, 'Lawyer',
                    5, 'Consumer or non-health professional')
freq_quals <- freq_quals %>% left_join(quals_key, by=c('term'='code'))

freq_quals %>% group_by(yr) %>% 
  mutate(perc = counts/sum(counts, na.rm=T)*100) %>% 
  ungroup() %>% 
  ggplot(aes(x = yr, y = counts, fill = qualification)) + 
    geom_bar(stat='identity', position='fill') + 
    scale_y_continuous(labels = scales::percent_format())+
  labs(x = 'Year', y = 'Percent', title = 'Who reports (USA)', caption='Source: openfda.gov') + 
  theme_classic()

### World
### 

freq_quals <- map(yrs, 
                  ~fda_query('/drug/event.json') %>% 
                    fda_filter('receivedate', paste0('[', .x, '0101+TO+',.x,'1231]')) %>% 
                    fda_count('primarysource.qualification') %>% 
                    fda_limit(1000) %>% fda_exec() %>% set_names(c('term',paste('count', .x, sep='_'))))
freq_quals <- reduce(freq_quals, full_join) %>% 
  gather(yr, counts, -term) %>% 
  mutate(yr = as.numeric(str_remove(yr, 'count_')))
quals_key = tribble(~code,~qualification,
                    1, 'Physician',
                    2, 'Pharmacist',
                    3, 'Other health professional',
                    4, 'Lawyer',
                    5, 'Consumer or non-health professional')
freq_quals <- freq_quals %>% left_join(quals_key, by=c('term'='code'))

freq_quals %>% group_by(yr) %>% 
  mutate(perc = counts/sum(counts, na.rm=T)*100) %>% 
  ungroup() %>% 
  ggplot(aes(x = yr, y = counts, fill = qualification)) + 
  geom_bar(stat='identity', position='fill') + 
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = 'Year', y = 'Percent', title = 'Who reports (USA)', caption='Source: openfda.gov') + 
  theme_classic()
