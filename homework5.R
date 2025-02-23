
# homework 5

## question 1

n = 9

for (i in 1:n) {
  cat(1:i, "\n")
}


## question 2

n = 9

for (i in 1:n) {
  cat(strrep(i, i), "\n")
}


## question 3

### input data
out.daily = read.csv('~/Desktop/Modeling for ID/idm_week5/out.daily.csv')

out.daily

### aggregate into weeks
out.daily$Week <- ceiling(out.daily$Day / 7)
weekly_cases <- tapply(out.daily$Cases, out.daily$Week, sum)
weekly_data <- data.frame(Week = names(weekly_cases), Weekly_Cases = weekly_cases)
print(weekly_data)


## question 4

n = 5
q4matrix <- matrix("",nrow = n, ncol = n)
q4matrix[cbind(1:n, rev(1:n))] = "o"
diag(q4matrix) = "x"
q4matrix







