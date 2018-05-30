require(plotly)
require(mvtnorm)
require(rpart)

## Set up function to approximate
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


## Set up basic plot
p <- plot_ly(x = xseq, y = yseq, z = z) %>% 
  add_surface(opacity = 0.65, showlegend = FALSE)


## Data for regression tree
xy <- expand.grid(x = xseq, y = yseq)
xyz <- xy
xyz$z <- plot_fun(x = xyz$x, y = xyz$y)

shinyServer(function(input, output, session) {
  
  # idx  <- NULL
  # dmat <- NULL
  # 
  # ## ID the point clicked on 
  # xy  <- reactive(c(input$click_plot$x, input$click_plot$y))
  # id <- observe({
  #   if (!is.null(xy())) {
  #     dmat <- as.matrix(dist(rbind(xy(), train)))
  #     idx <<- which.min(dmat[1, -1])
  #     dmat <<- dmat[-1, -1]
  #   }
  # })
  
output$plot1 <- renderPlotly({
    
  t_fit <- rpart(z ~ x + y, data = xyz, 
                 control = rpart.control(maxdepth = input$k, xval = 0, 
                                         minsplit = 2, cp = 0))
  t_df <- xy
  t_df$z <- predict(t_fit, newdata = xy)
  p %>% add_surface(x = xseq, y = yseq, z = matrix(t_df$z, npts, npts))

  })
  
})
