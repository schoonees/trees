## Mixture of bivariate Gaussians

require(plotly)
require(mvtnorm)
npts <- 100
xseq <- seq(from = -2, to = 2, length = npts)
yseq <- seq(from = -2, to = 2, length = npts)

mns1 <- c(-1, 1)
mns2 <- c(1, -1)
sig1 <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)
sig2 <- matrix(c(1, 0.25, 0.25, 1), nrow = 2, ncol = 2)

mix_prop <- 0.6

plot_fun <- function(x, y) {
  mix_prop * dmvnorm(cbind(x, y), mean = mns1, sigma = sig1) +
  (1 - mix_prop) * dmvnorm(cbind(x, y), mean = mns2, sigma = sig2)
}

z <- outer(xseq, yseq, plot_fun)

p <- plot_ly(x = xseq, y = yseq, z = z) %>% 
  add_surface(opacity = 0.65, showlegend = FALSE)
p

## Regression tree approximation
xy <- expand.grid(x = xseq, y = yseq)
xyz <- xy
xyz$z <- plot_fun(x = xyz$x, y = xyz$y)


## Add trees
require(rpart)
depth <- 4
t_fit <- rpart(z ~ x + y, data = xyz, 
               control = rpart.control(maxdepth = depth, xval = 0, 
                                       minsplit = 2, cp = 0))
t_df <- xy
t_df$z <- predict(t_fit, newdata = xy)
p %>% add_surface(x = xseq, y = yseq, z = matrix(t_df$z, npts, npts))

t3 <- rpart(z ~ x + y, data = xyz, control = rpart.control(maxdepth = 2, xval = 0))
