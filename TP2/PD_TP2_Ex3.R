p = function(x) pnorm(x, 0, 1, lower.tail = TRUE)
g = function(x) pnorm(x, 0, 1, lower.tail = FALSE)

p(0.41)
p(-0.2)
g(-1.54)

p(1.2)-p(-0.63)

summary(qnorm(0.75, 0, 1))

test = rnorm(100,0,1)
hist(test)

mean(test <= 0.41)
mean(test < -0.2)
mean(test >= -1.54)

#mean(-0.63 <= test <= 1.2)
