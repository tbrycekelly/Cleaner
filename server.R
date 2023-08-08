server = function(input, output, session){
  dat = data.frame(x = runif(10), y = runif(10), z = 1)
  saveRDS(dat, 'state.rds')
  
  ## Read uploaded data set, save local copy, set variable sliders
  observeEvent(input$upload, {
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    tmp = switch(ext,
           csv = read.csv(input$upload$datapath, sep = ","),
           tsv = read.csv(input$upload$datapath, sep = "\t"),
           xlsx = openxlsx::read.xlsx(input$upload$datapath),
           validate("Invalid file; Please upload a .csv, .tsv, or xlsx file")
    )
    
    for (i in 1:ncol(tmp)) {
      if ("character" %in% typeof(tmp[[i]])) {
        tmp[[i]] = factor(tmp[[i]])
      }
    }
    
    saveRDS(tmp, 'state.rds')
    
    message('Updating variable sliders.')
    updateSelectInput(inputId = 'xvar', choices = names(tmp))
    updateSelectInput(inputId = 'yvar', choices = names(tmp))
    updateSelectInput(inputId = 'zvar', choices = names(tmp))
    
  })
  
  
  
  output$dataPlot = renderPlot({
    tmp = local.data()
    pal = eval(parse(text = input$pal))
    
    if (!input$xvar %in% names(tmp)) {
      tmp[[input$xvar]] = 0
    }
    if (!input$yvar %in% names(tmp)) {
      tmp[[input$yvar]] = 0
    }
    
    plot(x = tmp[,input$xvar],
         y = tmp[,input$yvar],
         cex = tmp[,input$zvar],
         bg = pal(max(tmp[,input$zvar], na.rm = T))[round(tmp[,input$zvar])],
         xlab = input$xvar,
         ylab = input$yvar, 
         pch = 21)
  })
  
  ## Update Sliders
  observeEvent(input$xvar, {
    tmp = local.data()
    message('Upadting xrange slider.')
    updateSliderInput(inputId = 'xrange',
                      min = min(pretty(tmp[,input$xvar])),
                      max = max(pretty(tmp[,input$xvar])))
  })
  
  observeEvent(input$yvar, {
    tmp = local.data()
    message('Upadting yrange slider.')
    updateSliderInput(inputId = 'yrange',
                      min = min(pretty(tmp[,input$yvar])),
                      max = max(pretty(tmp[,input$yvar])))
  })
  
  
  local.data = reactive({
    dat = readRDS('state.rds')
    
    selPts = nearPoints(dat,
                         input$pointPicker, "x", "y",
                         threshold = 25, maxpoints = 1, allRows = TRUE)
    if(sum(selPts[,"selected_"]) > 0){
      dat[which(selPts[, "selected_", ]), input$zvar] = dat[which(selPts[, "selected_", ]), input$zvar] + 1
    }
    saveRDS(dat, 'state.rds')
    
    dat
  })
  
  
  ## File export
  output$download <- downloadHandler(
    filename = function() {
      "output.xlsx"
    },
    content = function(file) {
      openxlsx::write.xlsx(local.data(), file)
    }
  )
  
  
}