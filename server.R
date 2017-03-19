source("functions.R")

shinyServer(
  function(input, output, session) {
    
    # from UI - button
    uniform <- reactive({ input$uniform })
    # from UI - numeric input
    n.levels <- reactive({ input$n.levels })
    n.steps <- reactive({ input$n.steps })
    # from UI - sliders
    lr <- reactive({ input$lr })        # between 0 and 1
    mean1 <- reactive({ input$mean1 })   # between 0 and 2
    upper1 <- reactive({ input$upper1 }) # between 100 and 200
    
    # auto/fixed:
    size <- reactive({ rep(100, n.levels()) })
    loss.rate <- reactive({ rep(lr(), n.levels()) })
    staff.init <- reactive({
      lapply(1:n.levels(), function(i){
        n <- floor(size()[i]/n.groups)
        c(sapply(1:n.groups, function(j) rep(j,n)))
      })
    })
    
    # evaluation distribution (normal)
    eval.mean <- reactive({ c(mean1(), 0) })
    eval.sd <- c(1,1)
    
    # evaluation distribution (uniform)
    eval.lower <- c(1,1)
    eval.upper <- reactive({ c(upper1(), 100) })
    
    data <- reactive({ 
              input$simulate
              lapply(1:n.expts, function(i) experiment(staff.init(), 
                                                       n.steps(), 
                                                       n.levels(),
                                                       loss.rate(),
                                                       eval.mean(),
                                                       eval.sd,
                                                       eval.upper(),
                                                       eval.lower,
                                                       uniform=uniform()))
      
            })
    
    # plot outputs:
    output$appraisal_plot <- renderPlot({
      if(uniform()){
        show.unif.scores(eval.lower, eval.upper())
      } else {
        show.normal.scores(eval.mean(), eval.sd)
      }
    })
    output$profile_plot <- renderPlot({
      show.profile(data(), n.levels())
    })
    
  }
)