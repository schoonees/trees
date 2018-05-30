shinyUI(pageWithSidebar(
  headerPanel('Regression Tree Function Approximation'),
  sidebarPanel(
    sliderInput('k', 'Select the tree depth', value = 2, min = 1, max = 8)
  ),
  mainPanel(
    plotlyOutput('plot1')
  )
))
