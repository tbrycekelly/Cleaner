ui = fluidPage(
  
  titlePanel("Data CLEANER"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("upload", "Upload a file"),
      selectInput("xvar", label = "X Variable", choices =c("x")),
      selectInput("yvar", label = "Y Variable", choices =c("y")),
      selectInput("zvar", label = "Z Variable", choices =c("z")),
      sliderInput("xrange", label = "X Range", min = 0, max = 0, value = c(0,0)),
      sliderInput("yrange", label = "Y Range", min = 0, max = 0, value = c(0,0)),
      selectInput("pal", label = "Palette", choices =c("inferno", 'ocean.deep', 'ocean.thermal', 'ocean.curl', 'ocean.amp', 'alphabet')),
      downloadButton("download")
    ),
    
    mainPanel(
      plotOutput("dataPlot", click = "pointPicker")
    )
  )
)