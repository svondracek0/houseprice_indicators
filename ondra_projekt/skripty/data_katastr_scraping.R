#Downloading the Data

library(gdata)
library(tidyverse)
download.file('https://cuzk.cz/Periodika-a-publikace/Statisticke-udaje/Statistiky-podle-roku/2020/Statistiky-2020.aspx', dest = 'data.zip')

years_seq <- seq(from = 2006, to = 2021, by = 1)

for (element in years_seq) x = paste('https://cuzk.cz/Periodika-a-publikace/Statisticke-udaje/Statistiky-podle-roku/', 
                               element, 
                               '/Statistiky-',
                               element,
                               '.aspx', sep = '')




download_data_zip <- function(start, end){
  years_seq <- seq(from = start, to = end, by = 1)
  for (element in years_seq) {
    x = paste(
      'https://cuzk.cz/Periodika-a-publikace/Statisticke-udaje/Statistiky-podle-roku/',
      element,
      '/Statistiky-',
      element,
      '.aspx',
      sep = ''
    )
    download.file(
      x,
      dest = paste('./katastr_data/data', element, '.zip', sep = ''))
  }
  for (year in 2020){
    print(paste('extracting:', 'data', year, '.zip', sep = ''))
    unzip(paste('data', year, '.zip', sep = ''), exdir = paste('./extracted', year, 'data', sep=""))
  }
}


download_data_zip(2020, 2020)













