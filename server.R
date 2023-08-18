server = function(input, output, session){
  #tmp = data.frame(x = runif(10), y = runif(10), z = 1)
  #saveRDS(tmp, 'state.rds')
  
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
    colnames(tmp) = make.names(colnames(tmp))
    
    for (i in 1:ncol(tmp)) {
      if ("character" %in% typeof(tmp[[i]])) {
        tmp[[i]] = factor(tmp[[i]])
      }
    }
    
    saveRDS(tmp, 'state.rds')
    
    message('Updating variable sliders.')
    l = rep(T, ncol(tmp))
    for (i in 1:ncol(tmp)) {
      if (!is.numeric(tmp[1,i])) { l[i] = F}
    }
    updateSelectInput(inputId = 'xvar', choices = colnames(tmp)[l])
    updateSelectInput(inputId = 'yvar', choices = colnames(tmp)[l])
    updateSelectInput(inputId = 'zvar', choices = colnames(tmp)[l])
    
  })
  
  
  
  output$dataPlot = renderPlot({
    tmp = local.data()
    pal = eval(parse(text = input$pal))
    
    if (!input$xvar %in% colnames(tmp)) {
      message('No X ', input$xvar)
      #tmp[[input$xvar]] = 0
    }
    if (!input$yvar %in% colnames(tmp)) {
      message('No Y')
      #tmp[[input$yvar]] = 0
    }
    if (!input$zvar %in% colnames(tmp)) {
      message('No Z')
      message(paste(colnames(tmp), collapse = ', '))
      #tmp[[input$zvar]] = 0
    }
    
    
    plot(x = tmp[,input$xvar],
         y = tmp[,input$yvar],
         cex = tmp[,input$zvar],
         bg = pal(max(tmp[,input$zvar], na.rm = T))[round(tmp[,input$zvar])],
         xlab = input$xvar,
         ylab = input$yvar, 
         xlim = input$xrange,
         ylim = input$yrange,
         pch = 21)
  })
  
  ## Update Sliders
  observeEvent(input$xvar, {
    tmp = local.data()
    
    if (input$xvar %in% colnames(tmp)) {
      message('Upadting xrange slider.')
      updateSliderInput(inputId = 'xrange',
                        min = min(pretty(tmp[,input$xvar])),
                        max = max(pretty(tmp[,input$xvar])))
    }
  })
  
  observeEvent(input$yvar, {
    tmp = local.data()
    
    if (input$yvar %in% colnames(tmp)) {
      message('Upadting yrange slider.')
      updateSliderInput(inputId = 'yrange',
                        min = min(pretty(tmp[,input$yvar])),
                        max = max(pretty(tmp[,input$yvar])))
    }
  })
  
  observeEvent(input$refresh,
               {
                 tmp = readRDS('state.rds')
                 message('Updating variable sliders.')
                 l = rep(T, ncol(tmp))
                 for (i in 1:ncol(tmp)) {
                   if (!is.numeric(tmp[1,i])) { l[i] = F}
                 }
                 updateSelectInput(inputId = 'xvar', choices = colnames(tmp)[l])
                 updateSelectInput(inputId = 'yvar', choices = colnames(tmp)[l])
                 updateSelectInput(inputId = 'zvar', choices = colnames(tmp)[l])
               }
  )
  
  local.data = reactive({
    dat = readRDS('state.rds')
    
    selPts = nearPoints(df = dat, coordinfo = input$pointPicker, xvar = input$xvar, yvar = input$yvar,
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