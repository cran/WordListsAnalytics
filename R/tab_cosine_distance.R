tab_cosine_distance <- tabPanel("Cosine Distance",
                            mainPanel(

                              # Table output for estimation table
                              h2("Cosine distance matrix"),
                              # Download button for cosine matrix
                              tags$div(style = "height:20px;"),
                              downloadButton("download_cosine_matrix", "Download Cosine Matrix"),
                              tabPanel("CosineDistance", tableOutput("cosine_distance_table"))
                            )
)
