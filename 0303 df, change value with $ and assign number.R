donate_df <- data.frame(code = c("001", "002", "003", "004", "005"),
                        gender = c("M", "F", "F", "M", "F"),
                        value = c(500, 300, 1000, 500, 300))

# Take all data in a column: Using$ to extract a column£ºdonate_df$value
money <- donate_df$value

#2 ways of calculating the sum and mean(money = donate_df$value)
sum(money)
sum(donate_df$value)
mean(money)
mean(donate_df$value)

#Male & Female donation 
# gender == M in the value
# M in donate_df$gender & donate_df$value 
donate_df$value[donate_df$gender == "M"]
donate_df$value[donate_df$gender == "F"]
sum(donate_df$value[donate_df$gender == "F"])

#Then we can  use it to get other info
sum(donate_df$value[donate_df$gender == "M"])
mean(donate_df$value[donate_df$gender == "F"])

# 1. change value to usd
donate_df$usd <- donate_df$value/28

# 2. create a variable called sex: Female is 1 and male is 0
donate_df$sex <- 0
donate_df$sex[donate_df$gender == 'F'] <- 1

# 3. Create a variable called donate: value >= 500 is 2, and value smaller than 500 is 1
donate_df$donate <- 1
donate_df$donate[donate_df$value >= 500] <- 2

# 4. Create a table called male_donate: row is only male with all the column (6)
male_donate <- donate_df[donate_df$gender == 'M',]

# only female observations with 5 columns
# -1 means taking out the first column
f_donate <- donate_df[donate_df$gender == 'F', -1]

# x[i,j] , x[c(1,3),] , x[c(, 2:3)], x[1:2,]
# why using c? -> if we want to use range not only 1~2 then we need to. like over 3 arrange
# first and third row, empty means all columns
ddd <- donate_df[c(1,3),]
eee <- donate_df[c(1,3),-2]
