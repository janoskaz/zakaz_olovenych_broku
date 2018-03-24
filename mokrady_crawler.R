# libraries
library(rvest)
library(tidyverse)

# download page, retrieve information about wetland and return them
process_page=function(url) {
  # download and convert to XML
  page = read_html(url)
  # get name
  name = page %>%  html_node("h1.wetland__title") %>% html_text
  # get code
  code = page %>%  html_node("div.wetland__code") %>% html_text
  # get coordinates
  crds = page %>%  html_node("td[data-th=\"Lokalizace\"]") %>% html_text
  # can be unknown
  if (grepl('\\d',crds)) {
    crds = unlist(strsplit(gsub('[A-Z]','',crds),split = '\\s'))
    x = crds[1]
    y = crds[2]
  } else {
    x = y = NA
  }
  # area
  area = page %>%  html_node("td[data-th=\"Rozloha\"]") %>% html_text
  area = as.numeric(gsub(',','\\.',gsub('[a-zA-Z]','',gsub(pattern = "\\s",'',area))))
  # category
  category = page %>%  html_node("td[data-th=\"Kategorie\"]") %>% html_text
  # append to results
  return(c(name, code, area, category, x, y))
}

# save crawling (handles 404 error)
process_page_possibly=possibly(process_page,otherwise="This page could not be accessed")

# init results vector
res = c()

# try 3000 ids
for (i in 1:3000) {
  url = paste('http://mokrady.ochranaprirody.cz/mokrad/',i,sep="");
  p = process_page_possibly(url)
  # of correct results were returned, append to results
  if (length(p) == 6){
    res = rbind(res,p)
  }
}

# convert to dataframe
res = as.data.frame(res)
# column names
colnames(res) = c('Name','Code','Area','Category','X','Y')
# area as number
res$Area = as.numeric(as.character(res$Area))
# omit NA values
res2 = na.omit(res)
# aggregate
agg = data.frame(aggregate(res2$Area, by=list(res2$Category), FUN=sum))
# ramsar wetlands in ha
ramsar_area = 64673
# local, regional and supraregional
rest_area = sum(agg$x[-3])
# total wetlands area in km
total_area = (ramsar_area + rest_area)/100

# percentage of CR (CR area = 78866)
perc_cr = (total_area/78866)*100
print(perc_cr)
