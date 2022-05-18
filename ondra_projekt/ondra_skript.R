#07/10/2021

#libraries
library(tidyverse)
library(fredr)
library(plotly)

# getting the data
fredr_set_key('07d43cd27b8d103658e01cf077124b24')

#inflation
#unemployment
#mean wage
#hypo rate

cz_hypo_rate <- read_delim("../CZ_projekt/cnb_data.csv",
                           delim = "|", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                               grouping_mark = ";"), trim_ws = TRUE)

cz_hypo_rate$date <- seq(as.Date ("2004/01/01"), as.Date("2022/02/02"), "month")
cz_hypo_rate %>% rename(value = rate) -> cz_hypo_rate

cz_inflation <- fredr(series_id = 'CZECPIALLMINMEI')
cz_gdp <- fredr(series_id = 'CLVMNACSCAB1GQCZ')
cz_unemp <- fredr('LRHUTTTTCZM156S')
cz_med_wage <- fredr('LCEAPR03CZQ661S')
cz_houseprice <- fredr('QCZR628BIS')

cz_inflation %>% 
  select(date, value) %>% 
  rename(inflation = 2) %>% 
  left_join(., cz_hypo_rate, by = 'date') %>% 
  rename(hypo_rate = 2) %>% 
  left_join(cz_gdp, by = 'date')

var_list <- list(
  gdp = cz_gdp,
  hypo = cz_hypo_rate,
  inflation = cz_inflation,
  wage_med = cz_med_wage,
  unemp = cz_unemp,
  houseprice = cz_houseprice
)


for (i in 1:length(var_list)) var_list[[i]] <- var_list[[i]][c('date', 'value')]
for(i in 1:length(var_list)) names(var_list[[i]])[2] <- names(var_list)[i]


var_list

join_list <- function(list){
  tbl_res <- list[[1]]
  for(i in 2:length(list)) tbl_res <- inner_join(tbl_res, list[[i]], by = 'date')
  return(tbl_res)
}

tbl_final <- join_list(var_list)

tbl_final %>% 
  mutate(gdp = (gdp/tbl_final$gdp[which(tbl_final$date == '2010-01-01')])*100,
         inflation = (inflation/tbl_final$inflation[which(tbl_final$date == '2010-01-01')])*100,
         wage_med = (wage_med/tbl_final$wage_med[which(tbl_final$date == '2010-01-01')])*100,
         unemp = (unemp/tbl_final$unemp[which(tbl_final$date == '2010-01-01')])*100,
         hypo = (hypo/tbl_final$hypo[which(tbl_final$date == '2010-01-01')])*100) %>% 
  drop_na(.) %>% 
  pivot_longer(data = .,
               cols = -date,
               names_to = 'variable',
               values_to = 'value') %>% 
  ggplot(data = .,
         aes(x = date, y = value, color = variable)) + geom_line() +
  scale_y_continuous(name = 'Index, základ 01/2010 = 100') +
  scale_x_date(date_breaks = '1 years') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) -> plot_index


ggplotly(plot_index)

plotly_indexy <- ggplotly(plot_index)
htmlwidgets::saveWidget(plotly_indexy, "indexy_graf.html")

tbl_final %>% 
  mutate(gdp = (gdp/tbl_final$gdp[which(tbl_final$date == '2010-01-01')])*100,
         inflation = (inflation/tbl_final$inflation[which(tbl_final$date == '2010-01-01')])*100,
         wage_med = (wage_med/tbl_final$wage_med[which(tbl_final$date == '2010-01-01')])*100,
         unemp = (unemp/tbl_final$unemp[which(tbl_final$date == '2010-01-01')])*100,
         hypo = (hypo/tbl_final$hypo[which(tbl_final$date == '2010-01-01')])*100) %>% 
  drop_na() %>% 
  pivot_longer(data = .,
               cols = -date,
               names_to = 'variable',
               values_to = 'value') %>% 
  ggplot(data = .,
         aes(x = date, y = value, color = variable)) + geom_line() +
  scale_y_continuous(name = 'Index, základ 01/2010 = 100') +
  scale_x_date(date_breaks = '1 years') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

tbl_final %>% 
  mutate(gdp = 100*(log(gdp) - lag(log(gdp))),
         inflation = inflation - lag(inflation),
         wage_med = wage_med - lag(wage_med),
         unemp = unemp - lag(unemp),
         hypo = hypo - lag(hypo),
         houseprice = houseprice - lag(houseprice)) %>% 
  drop_na() %>% 
  pivot_longer(data = .,
               cols = -date,
               names_to = 'variable',
               values_to = 'value') %>% 
  ggplot(data = .,
         aes(x = date, y = value, color = variable)) + geom_line() +
  scale_y_continuous(name = 'Rel. zmena, pct.') +
  scale_x_date(date_breaks = '1 years') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle('Relativni zmeny sledovanych velicin') -> plot_rel_change

plotly_rel_change <- ggplotly(plot_rel_change)
htmlwidgets::saveWidget(plotly_rel_change, "relchange_graf.html")


tbl_final %>% 
  mutate(gdp = log(gdp) - lag(log(gdp)),
         inflation = inflation - lag(inflation),
         wage_med = wage_med - lag(wage_med),
         unemp = unemp - lag(unemp),
         hypo = hypo - lag(hypo),
         houseprice = houseprice - lag(houseprice)) %>% 
  drop_na() %>% 
  dplyr::select(-date) %>% 
  VAR(y = ., 
      p = 2,
      type = 'const',
      season = NULL,
      exog = NULL) %>% 
  predict(., n.ahead = 6, CI=0.95) %>% 
  fanchart(., main = c('gdp', 'wage_med', 'hypo', 'unemp', 'inflation', 'houseprice')) -> fanchart_var






var_list %>% names(.)

save(var_list, file = 'ondra_makro_promenne.Rdata')


length(var_prediction$fcst)

var_prediction$fcst[[1]][,1]
extract_predictions <- function(object = var_prediction$fcst){
  ret_tbl = object[[1]][,1]
  for(i in 2:length(object)){
    ret_tbl = cbind(ret_tbl, object[[i]][,1])
  }
  return(as_tibble(ret_tbl))
}


x <- extract_predictions()

x


names(x) <- names(tbl_final)[-1]






