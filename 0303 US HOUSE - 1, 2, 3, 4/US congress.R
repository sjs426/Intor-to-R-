library(readxl)

# set the folder. I can go to Session -> Set working directory
# I can load the package I need once I set it
setwd("C:/Users/sung/Desktop/Big data with R/0303 US HOUSE - 1, 2, 3, 4")

house_115 <- read.csv("house_115.csv")
trumpvote <- read_xlsx("trumpscore.xlsx") # have to install readxl fist to read xlsx file


# Check the observations and variables:
nrow(house_115)
colnames(house_115)

# Don't want to key in this in the future? house_115$date_of_birth

# You can change the column's names: 
# by the way, c is a vector function
colnames(house_115)[7] <- c('birth')
colnames(house_115)

# change multiple column names?
# more than 3 so we use c
colnames(house_115)[c(7,10:11)] <- c('brith','twitter','facebook')
colnames(house_115)

# Select columns you want: 1 + 5~7 columns
# matrix is [i,j] if i is empty, it is all
house_a <- house_115[,c(1,5:7)]

# select rows and columns you want: 100~200 list of house_115, 1 + 5~7 columns
house_b <- house_115[100:200,c(1,5:7)]

# select row you want:
# create df only involving female lawmaker's data:
# range is all row which is F and every columns
# [i,j] so have to put , in [] even if it's empty
house_f <- house_115[house_115$gender == 'F',]


# Since house_115 involves vacating, success, and non-voting lawmakers, 18 left + 15 successor + 6 non-voting lawmakers
# so the real num of lawmakers 435 doesn't match with our observation => nrow(house_115) which is 456.
# There were 18 lawmakers who left the positions from 2016-2018. However, only 15  districts were reelected, which is successor (15)
# A table represents 2016's lawmakers => All observations - (successor == 1) - (non_voting == 1) = 456 - (15+6) = 435
# A table represents lawmakers before the 2018 election: All observations - (vacate == 1) - (non-voting == 1) = 432
house_115_ex <- house_115[,c(45:47)] # column 45~47 only with all the rows
house_115_ex1 <- house_115_ex[house_115_ex$vacate == '1',] # 18 left /  row vacate = 1 with all the column
nrow(house_115_ex1)

house_115_ex2 <- house_115_ex[house_115_ex$successor == '1',] # 15 successor /  row successor = 1 with all the column
nrow(house_115_ex2)

house_115_ex3 <- house_115_ex[house_115_ex$non_voting == '1',] # 6 non-voting lawmakers /  row non_voting = 1 with all the column
nrow(house_115_ex3)

house_115_2016 <- house_115[!(house_115$successor == 1) & !(house_115$non_voting == 1),] # except 1, show others. it is ([i,j])
nrow(house_115_2016)
nrow(house_115) - nrow(house_115_2016)

house_115_2018 <- house_115[!(house_115$vacate == 1) & !(house_115$non_voting == 1),]
nrow(house_115_2018)
nrow(house_115) - nrow(house_115_2018)

# Ratio for gender
# nrow(x) means the number of rows; nrow(x) is the same but treats a vector as a one row matrix
# [i,j] if inside [] means matrix so have to put , 
# can choose categories (columns) by using $
colnames(house_115_2016)
M_gender <- nrow(house_115_2016[house_115_2016$gender == 'M',]) # row = M with all column
F_gender <- nrow(house_115_2016[house_115_2016$gender == 'F',]) # row = F with all column
M_gender / F_gender # 4.240964
# Male ratio is 4.240964 times higher than Female ratio

# https://appdividend.com/2019/11/16/how-to-find-element-in-list-in-python/
# In python, The index() method takes a single argument, 
# which is the element, and it returns its position in the list.
# M_gender = house_115_2016[gender].values
# Num_M_gender = M_gender.index('M')
# print(Num_M_gender)

# Ratio for party
unique(house_115_2016$party) # check if there is any party rather than R and D
M_party <- nrow(house_115_2016[house_115_2016$party == 'R',])
F_party <- nrow(house_115_2016[house_115_2016$party == 'D',])
M_party/F_party # 1.242268
# republican party ratio is 1.242268 times higher than democratic party ratio


# Create new variables based on your demands

# 1. names: Combine first name and last name
# paste two variable for name and give it a space
# have to give it a space between sep= ' ' otherwise no space 
house_115_2016$name <- paste(house_115_2016$first_name, house_115_2016$last_name,sep=' ') # or paste0

# 2. sex: Female is 1 and male is 0
unique(house_115_2016$gender)
house_115_2016$sex <- 0 # give it all 0 first for a new var  
house_115_2016$sex[house_115_2016$gender == "F"] <- 1 # and then only give F 1 

# 3. party_cate: R is 1, D is 2, and other is 0
unique(house_115_2016$party)
house_115_2016$party_cate <- 2
house_115_2016$party_cate[house_115_2016$party == 'R'] <- 1

# 4. year: Lawmaker's birth year
# The regexpr function is used to identify where a pattern is within a character vector, where each element is searched separately.
minus <- regexpr("-", house_115_2016$brith) # identify where "-" is, which is 5. if there are two, use "--" to find where the dashline is
minus
house_115_2016$year <- substr(house_115_2016$brith, 1, minus-1) # 51 번 째에 있음

# 5. name
# nchar is the total length of an element. We don't need to count it 
nchar(house_115_2016$name)
minus_space <- regexpr(' ', house_115_2016$name) # find ' ' this, which is different in each element.
minus_space
house_115_2016$L_name <- substr(house_115_2016$name, minus_space+1, nchar(house_115_2016$name)) # substr(x,initiating number, end of number)
minus_space1 <- regexpr(' ', house_115_2016$L_name) # sort out Middle name 
minus_space1
house_115_2016$L_name <- substr(house_115_2016$L_name, minus_space1+1, nchar(house_115_2016$L_name))

