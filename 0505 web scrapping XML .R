library(dplyr)
library(RCurl)
library(XML)
library(readr)
library(readxl)

setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")

parse <- htmlParse("book.html", encoding = "UTF-8")
parse

# point is get the TAG NAME
# we can skip doc and fun
# fun is function's short
# we can think one < is for one / 
# <body> for a / , <h1> for another /

xpathSApply(doc = parse, path = "//h1", fun = xmlValue) # Value O / value X. Get title

xpathSApply(doc = parse, path = "//p", fun = xmlValue) # Get Price

xpathSApply(doc = parse, path = "//p[1]", xmlValue) # Get only the first Price

xpathSApply(doc = parse, path = "//p[contains(text(), 'Price')]", xmlValue)

xpathSApply(doc = parse, path = "//a", xmlValue) # only Value 'link' comes out. We need to get Attributes

xpathSApply(doc = parse, path = "//a", xmlAttrs) # Get Attributes. whole link comes out

xpathSApply(doc = parse, path = "//a", xmlGetAttr, 'href') # Same as the above

title <- xpathSApply(parse, "//h1", xmlValue) 

price <- xpathSApply(parse, "//p[contains(text(), 'Price')]", xmlValue) 
price


nchar(price) # 12
pr <- regexpr("[0-9]", price) # number will appear firstly at 8th
pr

# substr() function to extract the first match in the first string.
# The regexpr() function gives you the (a) index into each string where the match begins and the (b) length of the match for that string.
# regexpr() only gives you the first match of the string (reading left to right).
price <- substr(price, regexpr("[0-9]", price), nchar(price)) # show only numbers
price

link <- xpathSApply(parse, "//a", xmlGetAttr, 'href')

booklist <- data.frame(titile = title, price = price, link = link)





# 실제 URL




# train
# Identify what kinds of information we need: 1. Items' names 2. Links to subpages 3. Prices 4. Description 5. Rates 6. Reviews

# 1. getURL() to get webpages => somehow it does not work so we use readLines() instead.
# 2. htmlParse() to parse html codes 
# 3. xpathSApply() to appoint the data you want

# phone_parse <- htmlParse("Web Scraper Test Sites.html", encoding = "UTF-8") => when we can't use URL, we just download html file and do like this
# phone_parse


phone_parse <- readLines("https://webscraper.io/test-sites/e-commerce/allinone/phones")
phone_parse <- htmlParse(phone_parse, encoding = "UTF-8")

title <- xpathSApply(phone_parse, "//h4/a[@class = 'title']", xmlValue) # class 의 머시기 @, id 는 # 
title

link <- xpathSApply(phone_parse, "//h4/a[@href]", xmlGetAttr, 'href') # link 니까 a 붙이는 거 + value 아니고 Attr
link

descri <- xpathSApply(phone_parse, "//p[@class = 'description']", xmlValue)
descri

price <- xpathSApply(phone_parse, "//h4[@class = 'pull-right price']", xmlValue) # <h4 class = "pull-right price">$489.99</h4> 
price

rating <- xpathSApply(phone_parse, "//p[@data-rating]", xmlGetAttr, 'data-rating') # <p data-rating = "3> so it's attribute
rating

review <- xpathSApply(phone_parse,"//p[@class='pull-right']", xmlValue) # how many reviews?
review

phonelist <- data.frame(title = title, description = descri, price = price, rate = rating, review = review, link = link) # name you want = real var
phonelist


# We want to enter every subpages to get color data

phonelist$link

color_df <- data.frame()


for (i in 1:3) {
  sub <- readLines(paste0("https://webscraper.io/", phonelist$link[i]))
  
 # sub <- htmlParse("nokia", encoding = "UTF-8", phonelist$link[i])
  
  sub_parse <- htmlParse(sub, encoding = "UTF-8")
  
  color <- xpathSApply(sub_parse, "//select[@aria-label = 'color']//option[@value]", xmlValue) # <select aria-label = "color"> <option value = "Gold">Gold</option> ~~ </select>
  
  # How to save your color data?

  color <- color[2:length(color)] # "Gold" "White" "Black"
  
  color <- toString(color) # "Gold, White, Black"
  
  temp <- data.frame(color = color)
  
  # rbind for row-bind
  # that can be used to combine several vectors, matrices and/or data frames by rows.
  color_df <- rbind(color_df, temp) 
}

# The cbind function – short for column bind – is a merge function 
# that can be used to combine two data frames with the same number of multiple rows into a single data frame.

phonelist <- cbind(phonelist, color_df)
phonelist


# practice

laphtml <- readLines("https://webscraper.io/test-sites/e-commerce/allinone/computers/laptops")
laphtml

laptop_parse <- htmlParse(laphtml, encoding = "UTF-8")
laptop_parse

# title <- xpathSApply(laptop_parse, "//h4/a[@class = 'title']", xmlValue) # check the website. 
title <- xpathSApply(laptop_parse, "//h4/a[@class = 'title']", xmlGetAttr, 'title')
title

link <- xpathSApply(laptop_parse, "//h4/a[@href]", xmlGetAttr, 'href')
link

descri <- xpathSApply(laptop_parse, "//p[@class = 'description']", xmlValue)
descri

price <- xpathSApply(laptop_parse,  "//h4[@class = 'pull-right price']", xmlValue)
price

rating <-  xpathSApply(laptop_parse, "//p[@data-rating]", xmlGetAttr, 'data-rating') # class 없으니 그냥 쓴다.
rating

review <- xpathSApply(laptop_parse, "//p[@class='pull-right']", xmlValue)
review

laptoplist <- data.frame(title = title, description = descri, price = price, rate = rating, review = review, link = link)


laptoplist$link

hd_df <- data.frame()

for (i in 1:length(laptoplist$link)) {
  
  sub <- readLines(paste0("https://webscraper.io/", laptoplist$link[i]))
  
  sub_parse <- htmlParse(sub, encoding = "UTF-8")
  
  # hd <- xpathSApply(sub_parse, "//button[@type = 'button']", xmlValue) # this way also work but below is prof's code
  hd <- xpathSApply(sub_parse, "//div[@class = 'swatches']//button[@value]", xmlValue)
  
  # hd <- hd[1:length(hd)] # we don't need to cut it 
  
  hd <- toString(hd)
  
  temp <- data.frame(hd = hd)
  
  hd_df <- rbind(hd_df, temp) 
}


laptoplist <- cbind(laptoplist, hd_df)
laptoplist

# The limitation is that we cannot change the price for each hdd. 
# we can't get a data from HTML since dynamic lang (JS) is used.
