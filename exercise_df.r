rm(list = ls())


df <- read.csv("./data/firms_final_txt.txt", sep = ";", header = TRUE)

df[df$east == "west", "east"] <- "FALSE"
df$east <- as.numeric(as.logical(df$east))

print(table(complete.cases(df)))

df <- na.omit(df)

print(head(df, 20))


df_small <- df[1:6,]

print(head(df_small, 6))

print(df[2, "size"])

firms_east <- subset(df, east == 1)
firms_west <- subset(df, east == 0)

sales_per_employee <- df$sales / df$size

df2 <- cbind(df, sales_per_employee)

print(head(df2))

print(cor(df2$investment, df2$sales_per_employee))