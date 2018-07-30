library("rhandsontable")
library("antibioticR")

data(micdata)
DF <- micdata
DF$log2conc <- log2(DF$conc)
DF <- DF[c("conc", "log2conc", "freq")]
lastClrBtn <- 0

shinyServer(function(input, output, session) {

  get_analysis <- reactive({

    df <- hot_to_r(input$hot)
    df2 <- na.omit(df)

    if (input$bxAutomatic) {
      pstart <- ecoffinder_startpar(df2$log2conc, df2$freq)
      updateNumericInput(session, "inpMean", value = unname(pstart["mean"]))
      updateNumericInput(session, "inpSd", value = unname(max(pstart["sd"], 0.2))) # heuristic: min sd 0.2
      updateNumericInput(session, "inpK", value = unname(pstart["K"]))
      print(pstart)
    } else {
      pstart <- c(
        mean = input$inpMean,
        sd = input$inpSd,
        K = input$inpK
      )
    }

    ecoffinder_nls(df$log2conc, df$freq, startpar = pstart)
  })

  output$hot = renderRHandsontable({
    if(input$clrBtn) {
      print(lastClrBtn)
      DF$freq <- as.numeric(NA) # as.numeric to avoid boolean
    } else {
      if (!is.null(input$hot)) {
        DF <- hot_to_r(input$hot)
      } else {
        print("startup")
      }
    }

    rhandsontable(DF) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_col(c("conc"), type="numeric", format="0[.]00000", readOnly = TRUE) %>%
      hot_col(c("log2conc"), type="numeric", format="0.0", readOnly = TRUE) %>%
      hot_col("freq", type="numeric")
  })

  output$figure <- renderPlot({
    input$runBtn
    isolate({
      if (!is.null(input$hot)) {
        analysis <- get_analysis()
        plot(analysis, xlab="Log2MIC")
      } else {
        # do nothing
      }
    })
  })

  output$density <- renderPlot({
    input$runBtn
    isolate({
      if (!is.null(input$hot)) {
        analysis <- get_analysis()
        plot(analysis, cumulative=FALSE, fits="best", xlab="Log2MIC")
      } else {
        # do nothing
      }
    })
  })


  # output$results <- renderTable({
  #   input$runBtn
  #   isolate({
  #     if (!is.null(input$hot)) {
  #       df <- hot_to_r(input$hot)
  #       data.frame(Parameter=c("mean", "sd", "cv"),
  #                  Value=c(mean(df$log2conc), sd(df$conc), "123"))
  #     }
  #   })
  # })

  output$summary <- renderPrint({
    input$runBtn
    isolate({
      if (!is.null(input$hot)) {
        #df <- hot_to_r(input$hot)
        analysis <- get_analysis()
        p("ECOFF values")
        summary(analysis)

       ## configurabe summary
       # with(analysis, {
       #   print(summary(models[[i_best]]))
       #   cat("---\n")
       #   cat("ecoff quantiles:\n")
       #   ecoff
       # })
     }
    })
  })


})
