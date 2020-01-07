## Hadley's Book Chpater 21 Loops. 

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

#Loop Example 
output <- vector(mode = "double", ncol(df))

for (i in seq_along(df)) {
  output[[i]] <- median(df[[i]])
}

output

#### Chapter 21: Iteration 21.2.1 Exercises

#1.1
View(mtcars)
output <- vector("double", ncol(mtcars))

for (i in seq_along(mtcars)) {
  output[[i]] <- mean(mtcars[[i]])
}

output

#1.2
View(nycflights13::flights)

output <- vector("double", ncol(nycflights13::flights))

for (i in seq_along(nycflights13::flights)) {
  output[[i]]<- class(nycflights13::flights[[i]])
}

output

#1.3
View(iris)

output <- vector(mode = 'numeric', ncol(iris))

for (i in seq_along(iris)) {
  output[[i]] <- length(unique(iris[[i]]))
}

output

#1.4

output <- list()

for (i in seq_along(c(-10,0,10, 100))) {
 output[[i]] <- rnorm (20, mean = i)
}

output

#2

# out <- ""
# for (x in letters) {
#   out <- stringr::str_c(out, x)
# }
# out
out_2 <- str_c(letters, collapse = "") 

# x <- sample(100)
# sd <- 0
# for (i in seq_along(x)) {
#   sd <- sd + (x[i] - mean(x)) ^ 2
# }
# sd <- sqrt(sd / (length(x) - 1))

sd2 <- sd(sample(100))


x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}

out



