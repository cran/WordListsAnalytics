
tab_t_c_a_calculator <- tabPanel("Root Calculation and Plotting",
                                 # Sidebar layout
                                 sidebarLayout(

                                   # Left panel: Coverage slider
                                   sidebarPanel(
                                     # coverage: fraction of the total incidence probabilities of the reported properties that are in the reference sample
                                     sliderInput("C_target", "C_target:", min = 0, max = 0.99, value = 0.5, step = 0.01),
                                     sliderInput("s_avg", "s_avg:", min = 1, max = 20, value = 5, step = 0.1),
                                     selectInput("concept_tca", "Type of Concept:", choices = c("Concrete", "Abstract")),

                                     textOutput("T_uncorrected"),
                                     textOutput("T_corrected"),

                                   ),
                                   # Right panel: Table showing Concept T_star S_hat_star and Warning
                                   mainPanel(

                                     plotOutput("plot1"),
                                     plotOutput("plot2")
                                   )
                                 )
)
