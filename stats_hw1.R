# R commands go here

# Part 1
(1/3) + (1/4)
2^10 +1
f <- 440
1127*log(1 + (f/440))
a <- 2
b <- 4
c <- -4
(-b + sqrt((b^2) - 4*a*c)/2*a)

# Part 2
df <- read.csv("NYC.csv", header = TRUE)
df_fourth <- df[df$word=="fouRth", ]
xtabs(~r + emphasis + store, data=df_fourth)
# Answer: Klein's employees used 

df_klein <- df[df$store=="Klein's", ]
df_klein_floor <- df_klein[df_klein$word=="flooR", ]
floor_xtabs <- xtabs(~r, data=df_klein_floor)
floor_xtabs/sum(floor_xtabs)
