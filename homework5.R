
# homework 5

## question 1

q1 <- function(n) {
  
  for (i in 1:n) {
  cat(1:i, "\n")
  } 
}

q1(9)

## question 2

q2 <- function(n) {
  
  for (i in 1:n) {
  cat(strrep(i, i), "\n")
  }
}

q2(9)

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


## question 5

library(deSolve)

SIR = function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    dS = -beta * S * I / N
    dI = beta * S * I / N - gamma * I
    
    list(c(dS, dI))
  })
}

N = 1e5       
I0 = 10       
S0 = N - I0   
gamma = 0.8  
beta_vec = seq(0.5, 2, by = 0.1)  
times = seq(1, 365, by = 1)    

# define final epidemic size and R0
final_sizes <- numeric(length(beta_vec))  
R0_values <- beta_vec / gamma           

# write for loop
for (i in seq_along(beta_vec)) {
  beta <- beta_vec[i]
  
  state <- c(S = S0, I = I0)
  parameters <- c(beta = beta, gamma = gamma)
  
  out <- ode(y = state, times = times, func = SIR, parms = parameters)
  
  # final number of susceptibles
  final_S <- out[nrow(out), "S"]
  
  # final epidemic size (total infected = N - final_S)
  final_sizes[i] <- N - final_S
}

plot(R0_values, final_sizes, type = "b", pch = 16, col = "blue",
     xlab = expression(R[0]), ylab = "Final Epidemic Size",
     main = "Impact of R0 on Epidemic Size")
grid()




