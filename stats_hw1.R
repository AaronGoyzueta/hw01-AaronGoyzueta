# Part 1
(1/3) + (1/4) # 1
2^10 +1 # 2
f <- 440
1127*log(1 + (f/700)) # 3
a <- 2
b <- 4
c <- -4
(-b + sqrt((b^2) - 4*a*c))/(2*a) # 4

# Part 2
df1 <- read.csv("NYC.csv", header = TRUE)
df_fourth <- df1[df1$word=="fouRth", ]
xtabs(~r + emphasis + store, data=df_fourth)
# Answer: Klein's employees used r 6 times, Macy's 13 times, and Saks 16 times. Total = 35

df_klein <- df1[df1$store=="Klein's", ]
df_klein_floor <- df_klein[df_klein$word=="flooR", ]
floor_xtabs <- xtabs(~r, data=df_klein_floor)
floor_xtabs/sum(floor_xtabs)
# Answer: 11.54% of the time

# Stretch goal
store_xtabs <- xtabs(~store + r, data=df1)
100 * prop.table(store_xtabs, 1)
# Klein's has the highest rate of r-lessness at 90.28%. Saks is lowest at 52.54%

emp_xtabs <- xtabs(~emphasis + r, data=df1)
100 * prop.table(emp_xtabs, 1)
# There is a 5% drop in the rate of r-lessness from normal emphasis to emphatic

word_xtabs <- xtabs(~word + r, data=df1)
100 * prop.table(word_xtabs, 1)
# The rate of r-lessness in 'fourth' is 77.23%, which is 18.44% higher than in 'floor'

# Part 3
df_2 <- read.table("VOT.tsv", sep="\t", header=TRUE)
sorted_vot <- sort(df_2$vot)
quantile(sorted_vot)
# Quartiles for VOT: 1st = -17.975, median = 13.825, 3rd = 27.365

df_esp <- df_2[df_2$language=="spanish", ]
mean(df_esp$vot)
# Mean of spanish speakers' VOTs = -24.313

df_eng <- df_2[df_2$language=="english", ]
sd(df_eng$vot)
# sample standard deviation of English speakers' VOT = 19.865

# Stretch goal
sample_mean <- mean(df_eng$vot)
numerator <- sum((df_eng$vot-sample_mean)^2)
s = sqrt(numerator/(length(df_eng$vot)-1))
s
# sample standard deviation = 19.865