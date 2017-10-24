library(shiny)
library(ggplot2)
library(pwr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Planning
  # power
  
  # t-test
  pwr_t <- reactive({pwr <- pwr.t.test(n = NULL, d = input$td, sig.level = input$alpha, 
                                       power = input$pwr, 
                                        type = input$ttype,
                                        alternative = input$talternative)
             
                    paste("For ", pwr$power*100, "% power to detect an effect of d = ", pwr$d, 
                    " or greater at an alpha of p <", 
                    pwr$sig.level, " will require ", ceiling(pwr$n), 
                    ifelse(input$ttype=="two.sample", 
                           paste(" participants per group, or ", 2* ceiling(pwr$n), " participants total.", 
                                 " To account for missingness or incomplete data, a 10% oversample would take that to ",
                                 2* ceiling(pwr$n*1.1), ".", sep=""),
                           ifelse(input$ttype=="one.sample", 
                                  paste(" participants.", 
                                        " To account for missingness or incomplete data, a 10% oversample would take that to ",
                                        ceiling(pwr$n*1.1), ".", sep=""),
                                  paste(" pairs of participants, or ", 2* ceiling(pwr$n), 
                                        " participants total.",
                                        " To account for missingness or incomplete data, a 10% oversample would take that to ",
                                        2* ceiling(pwr$n*1.1), ".", sep=""))),
                    sep="")
                    
  })
  
  output$tpower <- renderText({pwr_t()})
  
  # 1-Way ANOVA
  
  pwr_ANOVA <- reactive({pwr <- pwr.anova.test(n = NULL, 
                                               k = input$anova_k, 
                                               f = input$anova_f,
                                               sig.level = input$alpha, 
                                               power = input$pwr
                                               )
  
  paste("For ", pwr$power*100, "% power to detect an effect of f = ", pwr$d, 
        " or greater at an alpha of p <", 
        pwr$sig.level, " will require ", ceiling(pwr$n), " participants per group, or ", 
        input$anova_k * ceiling(pwr$n), " participants total.",
        " To account for missingness r incomplete data, a 10% oversample would take that to ",
        input$anova_k * ceiling(pwr$n*1.1), ".", sep="")
        
  })
  
  output$ANOVApower <- renderText({pwr_ANOVA()})
  
  # correlation
  pwr_corr <- reactive({pwr <- pwr.r.test(n = NULL,
                                          r = input$r,
                                          alternative = input$ralternative,
                                          sig.level = input$alpha,
                                          power = input$pwr
  )

  paste("For ", pwr$power*100, "% power to detect an effect of r = ", pwr$r,
        " or greater at an alpha of p < ",
        pwr$sig.level, " will require ", ceiling(pwr$n), " participants in total.", 
        " To account for missingness r incomplete data, a 10% oversample would take that to ",
        ceiling(pwr$n*1.1),  ".", sep="")

  })
  
  output$corrpower <- renderText({pwr_corr()})
  
  # GLM
  pwr_GLM <- reactive({pwr <- pwr.f2.test(v = NULL, # v= n - u - 1
                                          f2 = input$GLM_r2 / (1-input$GLM_r2),
                                          u = input$GLM_u,
                                          sig.level = input$alpha,
                                          power = input$pwr
  )


  paste("For ", pwr$power*100, "% power to detect an effect of r-squared = ", pwr$f2/(pwr$f2+1),
        " or greater at an alpha of p < ",
        pwr$sig.level, " will require ", ceiling(pwr$v+1+pwr$u), " participants in total.", 
        " To account for missingness r incomplete data, a 10% oversample would take that to ", 
        ceiling(pwr$v+1+pwr$u*1.1), "." , sep="")
  })
  output$GLMpower <- renderText({pwr_GLM()})
  
  #IF prop
  # 
  #pwr.p.test(h= ES.h(.42,.20),n=30, power =NULL, alternative = "greater")
  pwr_prop <- reactive({pwr <- pwr.p.test(n = NULL, 
                                          h =  ES.h(input$prop1, input$prop2),
                                          alternative = "two.sided",
                                          sig.level = input$alpha,
                                          power = input$pwr
  )
  
  
  paste("For ", pwr$power*100, "% power to detect an effect of h = ", round(pwr$h, 2),
        " or greater at an alpha of p < ",
        pwr$sig.level, " will require ", ceiling(pwr$n), " participants in total.", 
        " To account for missingness r incomplete data, a 10% oversample would take that to ",
        ceiling(pwr$n* 1.1), ".", sep="")
  
  })
  output$proppower <- renderText({pwr_prop()})
  
  
  # Sampling rate
  # Generate plot 
  output$plan_plot <- renderPlot({

    df <- data.frame(Date = c(input$Plan_Start_date, input$Plan_Target_date),
                     n = c(0, input$Plan_target_n)
    )

    p <- ggplot(df, aes(Date, n)) + 
      geom_hline(yintercept = input$Plan_target_n, linetype = "dotted", colour= "red") +
      theme_classic() + 
      coord_cartesian(ylim = c(0, 20 + input$Plan_target_n))
    if (as.integer(input$Plan_Target_date - input$Plan_Start_date)>0) {
    p + geom_line(cex = 1, linetype = "dashed", col = "Blue")
    
    } else {
        p
      }
    
  })
  
  # Rate summary
  output$plan_text <-  renderText({
    plan_days_to_target <- (as.integer(input$Plan_Target_date - input$Plan_Start_date))/7*5
    plan_rate <- input$Plan_target_n/ plan_days_to_target
    if(is.finite(plan_rate)) {
    c("This sampling plan requires an effective recruitment rate of", round(plan_rate, 1), "participants per work day (not accounting for leave or illness).")
    }
  })

  
  # Progress
  # power
   
   # t-test
   prog_pwr_t <- reactive({
     if (input$solveFor=="pwr"){
       n <- ifelse(input$prog_ttype== "one.sample", input$Prog_target_n, input$Prog_target_n/2)
       pwr <- pwr.t.test(n = n, d = input$prog_td, sig.level = input$prog_alpha, power = NULL, 
                         type = input$prog_ttype,
                         alternative = input$prog_talternative)
       
     }
     else if(input$solveFor=="eff") {
       n <- ifelse(input$prog_ttype== "one.sample", input$Prog_target_n, input$Prog_target_n/2)
       pwr <- pwr.t.test(n = n, d = NULL, sig.level = input$prog_alpha, power = input$prog_pwr, 
                         type = input$prog_ttype,
                         alternative = input$prog_talternative)
     }
   
     
   paste("With ", input$Prog_target_n, " participants, ", 
                "this study has", round(pwr$power, 2)*100, "% power to detect an effect of d = ", round(pwr$d,2), 
                 "at alpha = ", 
                pwr$sig.level, ".")
   })
   
  output$prog_tpower <- renderText({prog_pwr_t()})

  # 1-Way ANOVA

  prog_pwr_ANOVA <- reactive({
                      if (input$solveFor=="pwr"){
                      
                        n <- input$Prog_target_n / input$prog_anova_k
                        pwr <- pwr.anova.test(n = n,
                                            k = input$prog_anova_k,
                                            f = input$prog_anova_f,
                                            sig.level = input$prog_alpha,
                                            power = NULL
                      )
                      }
    
                      if (input$solveFor=="eff"){
                        n <- input$Prog_target_n / input$prog_anova_k
                        pwr <- pwr.anova.test(n = n,
                                            k = input$prog_anova_k,
                                            f = NULL,
                                            sig.level = input$prog_alpha,
                                            power = input$prog_pwr
                      )
                      }
  paste("With ", input$Prog_target_n, " participants, ",
        "this study has", round(pwr$power, 2)*100, "% power to detect an effect of f = ",
        round(pwr$f, 2), "at alpha = ", pwr$sig.level, ".")
 })
   
  output$prog_ANOVApower <- renderText({prog_pwr_ANOVA()})

  # correlation
  prog_pwr_corr <- reactive({
    if (input$solveFor=="pwr"){
      pwr <- pwr.r.test(n = input$Prog_target_n,
                        r = input$prog_r,
                        alternative = input$prog_ralternative,
                        sig.level = input$prog_alpha,
                        power = NULL
                        )
    }
    
    if (input$solveFor=="eff"){
      pwr <- pwr.r.test(n = input$Prog_target_n,
                        r = NULL,
                        alternative = input$prog_ralternative,
                        sig.level = input$prog_alpha,
                        power = input$prog_pwr
                        )
    }
    
    paste("With ", input$Prog_target_n, " participants, ",
          "this study has", round(pwr$power, 2)*100, "% power to detect an effect of r = ",
          round(pwr$r, 2), "at alpha = ", pwr$sig.level, ".")
  })

   output$prog_corrpower <- renderText({prog_pwr_corr()})

  # GLM
  prog_pwr_GLM <- reactive({
    if (input$solveFor=="pwr"){
      pwr <- pwr.f2.test(v = input$Prog_target_n - input$prog_GLM_u - 1, # v= n - u - 1 
                         f2 = input$prog_GLM_r2 / (1-input$prog_GLM_r2),
                         u = input$prog_GLM_u,
                         sig.level = input$prog_alpha,
                         power = NULL
                         )
    }
    
    if (input$solveFor=="eff"){
      pwr <- pwr.f2.test(v = input$Prog_target_n - input$prog_GLM_u - 1, # v= n - u - 1
                         f2 = NULL,
                         u = input$prog_GLM_u,
                         sig.level = input$prog_alpha,
                         power = input$prog_pwr
                         )
      }

    paste("With ", input$Prog_target_n, " participants, ",
          "this study has", round(pwr$power, 2)*100, "% power to detect an effect of r-squared = ",
          round(pwr$f2 / (pwr$f2 + 1), 2), "at alpha = ", pwr$sig.level, ".")
    })
  
   output$prog_GLMpower <- renderText({prog_pwr_GLM()})
   
  # prop

  prog_pwr_prop <- reactive({
    if (input$solveFor=="pwr"){
      pwr <- pwr.p.test(n = input$Prog_target_n, 
                       h =  ES.h(input$prog_prop1, input$prog_prop2),
                       alternative = "two.sided",
                       sig.level = input$prog_alpha,
                       power = NULL
                       ) 
     }
    
    if (input$solveFor=="eff"){
      pwr <- pwr.p.test(n = input$Prog_target_n, 
                      h =  NULL,
                      alternative = "two.sided",
                      sig.level = input$prog_alpha,
                      power = input$prog_pwr
                      )
    }
    
    paste("With ", input$Prog_target_n, " participants, ",
          "this study has", round(pwr$power, 2)*100, "% power to detect an effect of h = ",
          round(pwr$h, 2), "at alpha = ", pwr$sig.level, ".")
    })
  output$prog_proppower <- renderText({prog_pwr_prop()})


  # Progress Sampling rate
  
  prog <- reactive({
    days_active <- (as.integer(Sys.Date() - input$Start_date))/7*5
    days_to_target <- (as.integer(input$Prog_target_date - Sys.Date()))/7*5
    eff_rate <- input$Prog_current_n / days_active
    req_rate <- (input$Prog_target_n - input$Prog_current_n) / days_to_target
    proj_date <- ((input$Prog_target_n - input$Prog_current_n) / (eff_rate/5*7)) + Sys.Date()
    schedule <- ceiling(proj_date - input$Prog_target_date)
    
    list(days_active = days_active,
        days_to_target = days_to_target,
        eff_rate = eff_rate,
        req_rate = req_rate,
        proj_date = proj_date,
        schedule = schedule
                         )
    
  })
  
  # Generate plot
  output$prog_plot <- renderPlot({

    df <- data.frame(Series = factor(c("Current", "Current", "Target", "Target", "Projected", "Projected"), levels = c("Current", "Target", "Projected")),
                     Date = c(input$Start_date, Sys.Date(), input$Start_date, input$Prog_target_date, Sys.Date(), prog()$proj_date),
                     n = c(0, input$Prog_current_n, 0, input$Prog_target_n, input$Prog_current_n, input$Prog_target_n)
    )
    
    ggplot(df, aes(Date, n, col=Series, linetype=Series)) + geom_line(cex = 1) +
      scale_linetype_manual(values = c("solid", "dotted", "dashed")) + 
      scale_colour_manual(values = c("Black","Blue", "Red")) +
      geom_hline(yintercept = input$Prog_target_n, linetype="dashed") +
      theme_classic() 
    
  })

  # Rate summary
  output$prog_text <-  renderText({
    
    if(is.finite(prog()$eff_rate) & is.finite(prog()$req_rate)) {
      out <- 
        ifelse(prog()$eff_rate == 0, 
               "Please use the Planning panel to estimate required rate.",
        ifelse(prog()$schedule > 0,
      paste("The current sampling rate is ", round(prog()$eff_rate, 1), 
        " participants per work day (not accounting for leave or illness).", 
        "Maintaining the current rate of recruitment will lead to a completion date of ",
        prog()$proj_date, ".", "That's a projection of ",  prog()$schedule, 
        " days over the target. The rate would have to increase to ",
        round(prog()$req_rate, 1), " participants per work day to meet the target completion date."),
        
      paste("The current sampling rate is ", round(prog()$eff_rate, 1), 
            " participants per work day (not accounting for leave or illness).",
            " Maintaining the current rate of recruitment will lead to a completion date of ",  
            prog()$proj_date, ".", " Placing the project ",  abs(prog()$schedule), 
            " days ahead of target.", sep="")
      ))
      out
    } else{ paste("Set recruitment start and target dates to begin.")}
  })
  
# } else{
#   paste()
#   
  
  # dateInput("Start_date", "Start date:", 
  #           startview = "decade", format = "dd/mm/yyyy"),  
  # dateInput("ProgTarget_date", "Target date:", 
  #           startview = "decade", format = "dd/mm/yyyy"),  
  # numericInput("Prog_target_n", "Target Sample size", value = 85),
  # numericInput("Prog_current_n", "Current Sample size", value = 85),
  
})
