#Class: Week 15
#Course: Big Data and Social Analysis
#Semester: Spring 2021
#Lesson: Table and Dynamic Web Page Scraping
#Instructor: Chung-pei Pien
#Organization: ICI, NCCU

### Student Information --------

#Chinese Name: 辛鐘成
#English First Name: Jongsung Shin
#UID: 110ZU1038
#E-mail: sjongsung97@gmail.com

### Questions --------

library(dplyr)
library(RCurl)
library(XML)
library(readr)
library(readxl)
library(httr)
library(rvest)

#Q1: Please read the html document of http://books.toscrape.com/ into a R object, (2 points)

# get url
url = 'http://books.toscrape.com/'
data = GET(url)
book <- htmlParse(data, encoding = "UTF-8")

#Q2: Get the titles, links and rates (convert rates into digit) of books in this page (20 books). (5 points)

title <- xpathSApply(book, "//h3/a[@href]", xmlValue) # get title which is included by a href 
title

link <-  xpathSApply(book, "//h3/a[@href]", xmlGetAttr, 'href') # get Attribute of href
link

ratinglist <- xpathSApply(book, "//p[@class]", xmlGetAttr, 'class') # Due to the class name, we need to remove the others we don't need
ratinglist

# I was thinking to use gsup, or substr but I will try stopwords 

stopwords = c("star-rating","price_color", "instock", "availability")

rating <- unlist(strsplit(ratinglist, " ")) # split the words
rating

rating <- rating[!rating %in% stopwords] # remove all stopwords
rating

# Change written number to number 
rating <- case_when(
  rating == "One" ~ 1,
  rating == "Two" ~ 2,
  rating == "Three" ~ 3,
  rating == "Four" ~ 4,
  TRUE ~ 5
) 
rating

#Q3: Create a dataframe called book_df to store title, link and rate data. (2 points)

book_df <- data.frame(title = title,link = link, rate = rating) # create a df, name you want = real var
book_df

#Q4: Go to the webpage of the first book (A Light in the Attic). (2 points)
#Please read the html document of A Light in the Attic into a R object, 
Attic <- readLines("http://books.toscrape.com/catalogue/a-light-in-the-attic_1000/index.html")
Attic <- htmlParse(Attic, encoding = "UTF-8")
Attic

#Q5: Get Product Description of this book. (2 points)

descri <- xpathSApply(Attic, "//p", xmlValue) # description belongs to p
descri

# Experiment
#Product_Type <- xpathSApply(Attic, "//tr/td", xmlValue)	
#Product_Type
#a <- append(descri[4], Product_Type)
#a
#table[nrow(table) + 1,] <- a
#table <- rbind(table,Product_Type) 


#Q6: Get Product Information (This is a table) of this book.
#Create a data frame called table that has the following columns: 
#UPC, Product Type, Price (excl. tax), Price (incl. tax), Tax, Availability, Number of reviews.
#(6 points)

# create data frame with 0 rows and 7 columns
table <- data.frame(matrix(ncol = 7, nrow = 0))

# provide column names
colnames(table) <- c('UPC', 'Product Type', 'Price (excl. tax)', 'Price (incl. tax)', 'Tax','Availability','Number of reviews')

#Q7: Create another column to store Product Description data in to object table. (1 points)

library(tibble) # use this library for below code
table <- table %>%
  # Creating an empty column:
  add_column(Description = NA, .after="Number of reviews")
head(table)

# adding column in below way occurs error so I used above
# table$Description <- Description
# Error in `$<-.data.frame`(`*tmp*`, new, value = 1) : 
# replacement has 1 row, data has 0


#Q8: Create a loop and paste and adjust the above codes into the loop to get all 20 book's description and product information.
#(8 points)

for (i in 1:20) {
  
  # I have to use the paste0 like below since a link only shows the link without "http://books.toscrape.com/"
  
  Each_link <- readLines(paste0("http://books.toscrape.com/", book_df$link[i]))
  
  Each_link_parse <- htmlParse(Each_link, encoding = "UTF-8")
  
  descri <- xpathSApply(Each_link_parse, "//p", xmlValue)
  
  Product_Information <- xpathSApply(Each_link_parse, "//tr/td", xmlValue) # product info belongs to tr/td
  
  Info <- append(Product_Information, descri[4]) # descri[0~3] has another value so take only [4]
  
  table[nrow(table) + 1,] <- Info
}

#Q9: Merge book_df and the table created by the above loop. (2 points)

Complete_table <- cbind(table, book_df) # combine together

