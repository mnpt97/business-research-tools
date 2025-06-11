rm(list = ls())


x1 <- 15

x1_sq <- sqrt(x1 + 2)

x1_sq_rounded <- round(x1_sq, 1)

print(x1_sq_rounded)

print(ls())


is_equal <- 5 == 6
is_unequal <- 5 != 6


print(is_equal)
print(is_unequal)

is_equal_num <- as.numeric(is_equal)
is_unequal_num <- as.numeric(is_unequal)

print(is_equal_num)
print(is_unequal_num)