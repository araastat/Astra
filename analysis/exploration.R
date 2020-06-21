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
freq_indications = reduce(freq_indications, full_join) %>% 
  gather(yr, count, -term) %>% 
  mutate(yr = as.numeric(str_remove(yr, 'count_')))
freq_indications %>% nest(-yr, data=c(term,count)) %>% 
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
  labs(x = 'Year', y = 'Percent', title = 'Who reports (World)', caption='Source: openfda.gov') + 
  theme_classic()

### World
### 

freq_quals_world <- map(yrs, 
                  ~fda_query('/drug/event.json') %>% 
                    fda_filter('receivedate', paste0('[', .x, '0101+TO+',.x,'1231]')) %>% 
                    fda_count('primarysource.qualification') %>% 
                    fda_limit(1000) %>% fda_exec() %>% set_names(c('term',paste('count', .x, sep='_'))))
freq_quals_world <- reduce(freq_quals_world, full_join) %>% 
  gather(yr, counts, -term) %>% 
  mutate(yr = as.numeric(str_remove(yr, 'count_')))
quals_key = tribble(~code,~qualification,
                    1, 'Physician',
                    2, 'Pharmacist',
                    3, 'Other health professional',
                    4, 'Lawyer',
                    5, 'Consumer or non-health professional')
freq_quals_world <- freq_quals_world %>% left_join(quals_key, by=c('term'='code'))

freq_quals_world %>% group_by(yr) %>% 
  mutate(perc = counts/sum(counts, na.rm=T)*100) %>% 
  ungroup() %>% 
  ggplot(aes(x = yr, y = counts, fill = qualification)) + 
  geom_bar(stat='identity', position='fill') + 
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = 'Year', y = 'Percent', title = 'Who reports (USA)', caption='Source: openfda.gov') + 
  theme_classic()

## Gender-based reporting
## 
freq_gender <- map(yrs, 
                  ~fda_query('/drug/event.json') %>% 
                    fda_filter('receivedate', paste0('[', .x, '0101+TO+',.x,'1231]')) %>% 
                    # fda_filter('patient.drug.drugindication','multiple') %>% 
                    fda_count('patient.patientsex') %>% 
                    fda_limit(1000) %>% fda_exec() %>% set_names(c('term',paste('count', .x, sep='_'))))

freq_gender <- reduce(freq_gender, full_join) %>% 
  gather(yr, counts, -term) %>% 
  mutate(yr = as.numeric(str_remove(yr, 'count_')))
freq_gender %>% filter(term!= '0') %>% 
  spread(term, count) %>% 
  as_tabyl() %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting()

## Serious outcomes

freq_serious <- map(yrs, 
                    ~fda_query('/drug/event.json') %>% 
                      fda_filter('receivedate', paste0('[', .x, '0101+TO+',.x,'1231]')) %>% 
                      fda_count('serious') %>% 
                      fda_limit(1000) %>% fda_exec() %>% set_names(c('term',paste('count', .x, sep='_'))))

freq_serious <- reduce(freq_serious, full_join) %>% 
  gather(yr, counts, -term) %>% 
  mutate(yr = as.numeric(str_remove(yr, 'count_')))
  
freq_serious <- freq_serious %>% 
  mutate(term = ifelse(term==1, 'serious','non-serious'))
ggplot(freq_serious, aes(x = yr, y = count, fill = as.factor(term)))+
  geom_bar(stat='identity', position='fill') + 
  geom_hline(yintercept = 0.5, linetype=2)+
  scale_x_continuous('Year')+
  scale_y_continuous('Percent', labels = scales::percent_format())+
  scale_fill_manual(values = c('serious'='red', 'non-serious'='skyblue'))+
  theme_classic() + 
  labs(fill = 'Seriousness')


## temporal patterns in frequency
monthly <- as.character(1:12) %>% str_pad(2, side='left', pad='0')
dt <- expand.grid(yrs[-17], monthly) %>% unite('dt',c('Var1','Var2'), sep='', remove=F) %>% arrange(dt) %>% 
  mutate(start_date='01') 
month_ends = tribble(~monthly,~end_date,
                     '01','31',
                     '02','28',
                     '03','31',
                     '04','30',
                     '05','31','06','30','07','31','08','31', '09','30','10','31','11','30','12','31')
dt <- dt %>% left_join(month_ends, by = c('Var2'='monthly')) 
dt <- dt %>% 
  mutate(start_date = paste0(dt, start_date), end_date = paste0(dt, end_date), 
         query_date = glue::glue('[{start_date}+TO+{end_date}]'))
api <- readLines(here('Personal/apikey.txt'))
bl=map(dt$query_date, ~fda_query('/drug/event.json') %>% fda_api_key(api) %>% 
         fda_filter('receivedate', .x) %>% 
      fda_count('patient.patientsex') %>% 
      fda_exec())
dt$n = map_int(bl, ~sum(.x$count))

ggplot(dt, aes(x = as.Date(start_date, '%Y%m%d'), y = n))+
  geom_line() + 
  scale_y_log10('Number of reports', labels = scales::comma_format())+
  scale_x_date('Year', date_breaks='1 year', labels = scales::date_format('%Y')) +
  theme_bw()


save(dt, freq_gender, freq_country,freq_indications, freq_serious, freq_quals, file = here('data','freqs.rda'), compress=T)
