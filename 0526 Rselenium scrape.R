# Week15: Table and Dynamic Web Page Scraping
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(writexl)
library(jsonlite)
library(RCurl)
library(XML)
library(httr)

setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")

# we should be able to download by our selves
regno <- read_xlsx('registernumber.xlsx')

# Get the link
air <- readLines("https://prtr.epa.gov.tw/FacilityInfo/_DetailAirReport?registrationno=F16A0143")

air_parse <- htmlParse(air, encoding = "UTF-8")

# we want xml Value
tablecontext <- xpathSApply(air_parse, "//tr[@style='display:none;']/td", xmlValue)
tablecontext

# matrix function let us create a data frame orderly.
# 7 columns
temp_table <- matrix(tablecontext, ncol = 7, byrow = T)

# when we do not have any ID
# we think registered number is an unique number
temp_table <- data.frame(regino = "F16A0143", temp_table)

# show me only gaoshung
khregno_air <- regno %>%
  filter(County == "高雄市",
         is.na(AirRegisteredStatus) == FALSE)

# create an empty data frame
air_df <- data.frame()

# We only look for 1 to 10 
for (k in 1:10) {
  url <- paste0("https://prtr.epa.gov.tw/FacilityInfo/_DetailAirReport?registrationno=", khregno_air$RegistrationNo[k])
  air <- readLines(url)
  air_parse <- htmlParse(air, encoding = "UTF-8")
  tablecontext <- xpathSApply(air_parse,"//tr[@style='display:none;']/td", xmlValue)
  
  # some facility does not air pollution data so we use condition 
  ## length(tablecontext) = 420. what we want to do with the code below? 420! = 0?? 
  if (length(tablecontext) != 0) {
    temp_table <- matrix(tablecontext, ncol = 7, byrow = T)
    temp_table <- data.frame(regino = khregno_air$RegistrationNo[k], temp_table)
    air_df <- rbind(air_df, temp_table)
  }
}





# Introduction of Selenium

# Many websites’ content is generated using JavaScript. 
# If we use httr or rvest, only the original page will be loaded and JavaScript won’t be executed.
# Hence some data is only available in browser (Yan 2022).

# https://cimentadaj.github.io/blog/2018-05-25-installing-rjava-on-windows-10/installing-rjava-on-windows-10/

Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.8.0_333/")
library(rJava)
library(RSelenium)

# make sure the port is the same figure, if error occurs, we can change to any number
rsDriver(port = 4445L,
         chromever = "102.0.5005.63",
         geckover = "latest")

# Start a server and browser
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445,
  browser = c("chrome"))


# Open an browser:
remDr$open()

  
# Navigate to an URL:
remDr$navigate("https://webscraper.io/test-sites/e-commerce/allinone/computers/laptops")


# title <- xpathSApply(laptop_parse, "//h4/a[@class='title']", xmlGetAttr, 'title')
item_elem <- remDr$findElements(using = "xpath", value = "//*[@class='title']")

title <- sapply(item_elem, function(x){
  x$getElementAttribute('title')
})


# link <- xpathSApply(laptop_parse, "//h4/a[@href]", xmlGetAttr, 'href')
link_elem <- remDr$findElements(using = "xpath", value = "//h4/a[@href]")

link <- sapply(link_elem, function(x){
  x$getElementAttribute('href')
})


# descri <- xpathSApply(laptop_parse, "//p[@class='description']", xmlValue)
descri_elem <- remDr$findElements(using = "xpath", value = "//p[@class='description']")

descri <- sapply(descri_elem, function(x){
  x$getElementText()
})


#rating <-  xpathSApply(laptop_parse, "//p[@data-rating]", xmlGetAttr, 'data-rating')
rate_elem <- remDr$findElements(using = "xpath", value = "//p[@data-rating]")

rating <- sapply(rate_elem, function(x){
  x$getElementAttribute('data-rating')
})


#review <- xpathSApply()
review_elem <- remDr$findElements(using = "xpath", value = "//p[@class='pull-right']")

review <- sapply(review_elem, function(x){
  x$getElementText()
})


laptoplist <- data.frame(title = unlist(title), description = unlist(descri), rate = unlist(rating), review = unlist(review), link = unlist(link))

# total 117 links
unlist(link) 

# extract the number at the end of the link
id <- substr(unlist(link), as.numeric(nchar(unlist(link)) - 2), as.numeric(nchar(unlist(link))))
id
# 여기를 안 해주고 밑에서 가능한지 함 바바바
laptoplist$id <- id

# this code will direct to the selected link
remDr$navigate(unlist(link[1]))
remDr

#hd <- xpathSApply(sub_parse, "//div[@class='swatches']//button[@value]", xmlValue)
hd_elem <- remDr$findElements(using = "xpath", value = "//div[@class='swatches']//button[@value]")

hd <- sapply(hd_elem, function(x){
  x$getElementText()
})

price_temp <- data.frame()

# Move mouse to the element we selected
remDr$mouseMoveToLocation(webElement = hd_elem[[2]]) 

# Click the right mouse button
remDr$click() 


# price <- xpathSApply(sub_parse, "~~", xmlValue)
price_elem <- remDr$findElement(using = "xpath", value = "//h4[@class='pull-right price']")

price <- price_elem$getElementText() #  get the price 

hp_temp <- data.frame(hd = unlist(hd[2]), prices = unlist(price))

price_temp <- rbind(price_temp, hp_temp)

price_temp$id <- id[1]

price_temp



price_df <- data.frame()

for (y in 1:10) { #replace length(link) to 10
  
  remDr$navigate(unlist(link[y]))
  
  #hd <- xpathSApply(sub_parse, "//div[@class='swatches']//button[@value]", xmlValue)
  
  hd_elem <- remDr$findElements(using = "xpath", value = "//div[@class='swatches']//button[@value]")
  
  hd <- sapply(hd_elem, function(x){
    x$getElementText()
  })
  
  price_temp <- data.frame()
  
  for (i in 1:length(hd)) {
    
    remDr$mouseMoveToLocation(webElement = hd_elem[[i]]) # Move mouse to the element we selected
    remDr$click() # Click the right mouse button
    
    price_elem <- remDr$findElement(using = "xpath", value = "//h4[@class='pull-right price']")
    
    price <- price_elem$getElementText()
    
    hp_temp <- data.frame(hd = unlist(hd[i]), prices = unlist(price))
    
    price_temp <- rbind(price_temp, hp_temp)
    
  }
  
  id <- laptoplist$id[y]
  
  price_temp$id <- id
  
  price_df <- rbind(price_df, price_temp)
  
}

# 여기 해봐 나중에 
laptoplist <- laptopslist %>%
  left_join(price_df, by = "id")





# what is Sys.sleep(30)?
# if we request in a sec, sever will identify we are trying to scrap this website
# we will get penalty such as banning ip...
# This code will delay the scrapping for security
# Also if you didn't set any sleep, when your internet not fast enough, it may have error

# Sys.sleep(runif(1,25,35)) 
# This code will produce a random number and scrap between the range which is 25 sec to 35 sec
# we can make the range much longer like 25 to 50 
# The web sever will not detect you scrapping the website

# Download free pdf via our school library
# https://nccu.primo.exlibrisgroup.com/discovery/fulldisplay?docid=alma991021042005905721&context=L&vid=886NCCU_INST:886NCCU_INST&lang=en&search_scope=MyInst_and_CI&adaptor=Local%20Search%20Engine&tab=Everything&query=any,contains,the%20economist&offset=0&pcAvailability=false


Sys.sleep(runif(1,1,2))

myheader <- c(
  "User-Agent"="Mozilla/5.0(Windows;U;Windows NT 5.1;zh-CN;rv:1.9.1.6",
  "Accept"="text/htmal,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="big5,utf-8;q=0.7,*;q=0.7"
)

rsDriver(port = 4567L,
         chromever = "102.0.5005.63",
         geckover = "latest")

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4567,
  browser = c("chrome"))

remDr$open()

remDr$navigate("https://search.proquest.com/publication/41716?OpenUrlRefId=info:xri/sid:primo")

# 1번 째 page. display_record_indexing_data 다음에 href 가 중요 
web.elem <- remDr$findElement(using = "xpath", value = "//*[@class='display_record_indexing_data']//*[@href]")

# click it 
web.elem$clickElement()

# 2번 째 page. find a button(link) where we can download pdf file 
issue.elem <- remDr$findElements(using = "xpath", value = "//*[@class='addFlashPageParameterformat_fulltextPDF  format_pdf']")

# make a list that includes all the links 
links <- sapply(issue.elem, function(issue.elem){
  issue.elem$getElementAttribute('href')
})
links

# if I want, length(issue.elem)
for (al in 1:5) {
  
  Sys.sleep(10)
  
  # natviage to the link we want to go 
  remDr$navigate(unlist(links[al]))
  
  # this is a link section (a href) for downloading the pdf file
  article.elem <- remDr$findElement(using = "xpath", value = "//*[@class='tool-option-link pdf-download']")
  
  # click the element by iteself
  article.elem$clickElement()
  
  # let me know how the process goes
  print(paste(Sys.time(), "I'm still waiting...", sep = " "))
  
}
