library(shiny)

navbarPage("Planning and Progress Dashboard",
          
# Plannning Inputs --------------------------------------------------------

           
           tabPanel("Planning",
                    tags$head(includeHTML(("g_analytics.html"))), # Google Analytics
                    sidebarLayout(
                      sidebarPanel(
                      h3("Power"),
                      selectInput("test", "Analysis type:", 
                                   choices = c("t-test", "1-way ANOVA", "Correlation", "GLM", "Proportions test")),
                      conditionalPanel("input.test == 't-test'", 
                                       numericInput("td", "Std. Mean difference (d):", value = .2, step = .05),
                                       em("small = .2, med = .5, large = .8"),
                                       radioButtons("ttype", "Type:", choiceNames = c("Independent samples", "One sample", "Paired samples"),
                                                    choiceValues = c("two.sample", "one.sample", "paired")),
                                       radioButtons("talternative", "Tails:", choiceNames = c("Two tailed", "Upper"), 
                                                    choiceValues = c("two.sided", "greater"))
                                       ),
                      
                      conditionalPanel("input.test == '1-way ANOVA'", 
                                       numericInput("anova_k", "Number of Groups:", value = 3),
                                       numericInput("anova_f", "Effect size (Cohen's f):", value = .1, step = .05),
                                       em("small = .1 ,med = .25, large = .4")
                      ),
                      
                      conditionalPanel("input.test == 'Correlation'", 
                                       numericInput("r", "Correlation coefficient (r):", value = .1, min = 0, max = 1, step = .05),
                                       em("small = .1, med = .3, large = .5"),
                                       radioButtons("ralternative", "Tails:", choiceNames= c("Two-tailed", "one-tailed"),
                                                    choiceValues = c("two.sided", "greater"))
                                       
                      ),
                      conditionalPanel("input.test == 'GLM'", 
                                       numericInput("GLM_u", "Number of Parameters / predictors:", value = 3),
                                       numericInput("GLM_r2", "Variance explained:", value = .1, min = 0, max = 1, step = .05) # r squared  
                      ),
                      conditionalPanel("input.test == 'Proportions test'", 
                                       numericInput("prop1", "Proportion group 1:", value = .4, min = 0, max = 1, step = .05),
                                       numericInput("prop2", "Proportion group 2:", value = .6, min = 0, max = 1, step = .05)
                                       # This needs to be fed to ES.h() before input into the power calc.
                                       ),
                      
                      sliderInput("pwr", "Target Power:", value = .8, min = .7, max = 1),
                      sliderInput("alpha", "Alpha (Type I error rate):", value = .05, min = 0, max = .1),
                      hr(),
                      
                      h3("Sampling Rate"),

                      numericInput("Plan_target_n", "Target Sample size", value = 85),
                      dateInput("Plan_Start_date", "Start date:", 
                                startview = "month", format = "dd/mm/yyyy"),
                      
                      radioButtons("planSolveFor", "Solve for:",
                                   choiceNames = c("Sampling Rate", "Completion Date"), 
                                   choiceValues = c("rate", "date")
                      ),
                      
                      conditionalPanel("input.planSolveFor == 'rate'", 
                        dateInput("Plan_Target_date", "Target completion date:", 
                                  startview = "month", format = "dd/mm/yyyy")
                      ),

                      conditionalPanel("input.planSolveFor == 'date'", 
                        numericInput("estRate", "Participants / Obs. per week:", 
                                     value = 5, min = 1, max = 300, step = 1)
                      )
                      
                      
                      ),
                      

                      
                      
                      

# Planning Main -----------------------------------------------------------


                      mainPanel(
                        fluidRow(h3("Power"),
                                 conditionalPanel("input.test == 't-test'",
                                                  textOutput("tpower")),
                                 conditionalPanel("input.test == '1-way ANOVA'",
                                                  textOutput("ANOVApower")),
                                 conditionalPanel("input.test == 'Correlation'",
                                                  textOutput("corrpower")),
                                 conditionalPanel("input.test == 'GLM'", 
                                                  textOutput("GLMpower")),
                                 conditionalPanel("input.test == 'Proportions test'",
                                                  textOutput("proppower"))
                                 
                                 ),
                        fluidRow(h3("Sampling Rate"),
                                 plotOutput("plan_plot")),
                        fluidRow(textOutput("plan_text"))         
                                          )
                    )

           ),
           

# Progress inputs ---------------------------------------------------------


           tabPanel("Progress",
                    sidebarLayout(
                      sidebarPanel(
                        h3("Sampling Rate"), 
                        dateInput("Start_date", "Start date:", 
                                  startview = "month", format = "dd/mm/yyyy"),  
                        dateInput("Prog_target_date", "Target completion date:", 
                                  startview = "month", format = "dd/mm/yyyy"),  
                        numericInput("Prog_target_n", "Target Sample size", value = 85),
                        numericInput("Prog_current_n", "Current Sample size", value = 0),
                        hr(),
                        h3("Power"),
                        radioButtons("solveFor", "Solve for:", 
                                     choiceNames = c("Power to detect", "Smallest Effect"), 
                                     choiceValues = c("pwr", "eff")
                                     ),
                        selectInput("prog_test", "Analysis type:", 
                                    choices = c("t-test", "1-way ANOVA", "Correlation", "GLM", "Proportions test")
                                    ),
                        
                        conditionalPanel("input.prog_test == 't-test'", 
                                         conditionalPanel("input.solveFor == 'pwr'", 
                                            numericInput("prog_td", "Std. Mean difference (d):", value = .2, step =.05),
                                            em("small = .2, med = .5, large = .8")
                                            ),
                                         radioButtons("prog_ttype", "Type:", choiceNames = c("Independent samples", "One sample", "Paired samples"),
                                                      choiceValues = c("two.sample", "one.sample", "paired")),
                                         radioButtons("prog_talternative", "Tails:", choiceNames = c("Two tailed",  "Upper"), 
                                                      choiceValues = c("two.sided", "greater"))
                        ),
                        
                        conditionalPanel("input.prog_test == '1-way ANOVA'", 
                                         numericInput("prog_anova_k", "Number of Groups:", value = 3),
                                         conditionalPanel("input.solveFor == 'pwr'",
                                            numericInput("prog_anova_f", "Effect size (Cohen's f):", value = .1, step =.05),
                                            em("small = .1 ,med = .25, large = .4")
                                            )
                        ),
                        
                        conditionalPanel("input.prog_test == 'Correlation'", 
                                         conditionalPanel("input.solveFor == 'pwr'",
                                            numericInput("prog_r", "Correlation coefficient (r):", value = .1, min = 0, max = 1, step = .05),
                                            em("small = .1, med = .3, large = .5")
                                            )
                        ),
                        conditionalPanel("input.prog_test == 'GLM'", 
                                         numericInput("prog_GLM_u", "Number of Parameters / predictors:", value = 3),
                                         conditionalPanel("input.solveFor == 'pwr'",
                                            numericInput("prog_GLM_r2", "Variance explained:", value = .1, min = 0, max = 1, step = .05)
                                            )
                        ),
                        conditionalPanel("input.prog_test == 'Proportions test'", 
                                         conditionalPanel("input.solveFor == 'pwr'",
                                            numericInput("prog_prop1", "Proportion group 1:", value = .5, min = 0, max = 1, step = .05),
                                            numericInput("prog_prop2", "Proportion group 2:", value = .5, min = 0, max = 1, step = .05)
                                            )
                                         ),
                        conditionalPanel("input.solveFor == 'eff'",
                            sliderInput("prog_pwr", "Target Power:", value = .8, min = .7, max = 1)),
                        sliderInput("prog_alpha", "Alpha (Type I error rate):", value = .05, min = 0, max = .1)
                      ),
                      

# Progress main -----------------------------------------------------------

                      mainPanel(
                        
                        fluidRow(h3("Sampling Rate"),
                                  plotOutput("prog_plot")
                        ),
                         
                        fluidRow(textOutput("prog_text")
                                  ),
                         
                         fluidRow(h3("Power"),
                                  conditionalPanel("input.prog_test == 't-test'",
                                                   textOutput("prog_tpower")),
                                 conditionalPanel("input.prog_test == '1-way ANOVA'",
                                                  textOutput("prog_ANOVApower")),
                                 conditionalPanel("input.prog_test == 'Correlation'",
                                                  textOutput("prog_corrpower")),
                                 conditionalPanel("input.prog_test == 'GLM'",
                                                  textOutput("prog_GLMpower")),
                                 conditionalPanel("input.prog_test == 'Proportions test'",
                                                  textOutput("prog_proppower"))
                         )        
                      )
                    )
           ),
           

# About Panel -------------------------------------------------------------


           tabPanel("About",
                    fluidPage("This was built by Mathew Ling", 
                              a("(@lingtax)", href="https://twitter.com/lingtax"),
                              " to promote better planned research.",
                                                                 p(),
                              "It is powered by ", strong("R"), " and the ", strong("pwr"), " and ", strong("ggplot2"), 
                              " packages, and is built in ", strong("Shiny"), ".",
                              
                              p(),
                              
                              "Cohen's f is the standard deviation of the population means divided by their common standard 
                              deviation. For equal population sizes, f is equal to the square root of Eta squared divided 
                              by one minus eta squared.",
                              p(),
                              
                              "For bug reports and feature requests, please raise an issue at ", 
                              a("this github repository", href = "https://github.com/Lingtax/PlanningProgressDashboard/issues/new"))
           )
)
