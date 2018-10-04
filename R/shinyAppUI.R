#' Shiny app server object
#'
#' @importFrom shiny fluidPage
#' @importFrom DT dataTableOutput
#' @import shiny




shinyAppUI = fluidPage(
  titlePanel("Principal Component Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Upload your CSV File',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      htmlOutput("varselect"),
      htmlOutput("varSupQuant"),
      htmlOutput("varSupQuali"),
      htmlOutput("ncp_main"),
      htmlOutput("scale"),
      tags$div(class="header", checked=NA,
               tags$h4("This Shiny app is powered by"),
               tags$h4("Dhafer Malouche"),
               tags$a(href="http://dhafermalouche.net", "http://dhafermalouche.net")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Active Data",tableOutput('table')),
        tabPanel("Scree plot",
                 tabsetPanel(
                   tabPanel("The plot",
                            column(4,wellPanel("",htmlOutput("ncp"))),
                            column(4,wellPanel("",htmlOutput("addlabels"))),
                            downloadButton('downloadScreeplot', 'Download Plot'),
                            plotOutput("screeplot1", height="800px")
                   ),
                   tabPanel("Table",
                            tableOutput("pca"))
                 )
        ),
        tabPanel("Individual results",
                 tabsetPanel(
                   tabPanel("Coordinates",
                            column(6,
                                   downloadButton('download_coord_pc', 'Download result (csv)')),
                            DT::dataTableOutput('coord_pc')),
                   tabPanel("Cos2",
                            column(6,
                                   downloadButton('download_coord_cos2_pc', 'Download result (csv)')),
                            DT::dataTableOutput("coord_cos2_pc")),
                   tabPanel("Contribution",
                            column(6,
                                   downloadButton('download_coord_cont_pc', 'Download result (csv)')),
                            DT::dataTableOutput("coord_cont_pc")),
                   tabPanel("Plot of Individuals",
                            column(6,wellPanel(h2("Costumizing the graph"),
                                               htmlOutput("axes1_ind"),
                                               htmlOutput("axes2_ind"),
                                               htmlOutput("geom_ind"),
                                               htmlOutput("repel_ind"),
                                               htmlOutput("hab_ind"))),
                            column(6,wellPanel("",
                                               htmlOutput(("varsSupQuali_s1")),
                                               htmlOutput("ell_ind"),
                                               htmlOutput("alpha_ind"),
                                               htmlOutput("select_ind"))),
                            column(4,downloadButton('downloadIndplot', 'Download Plot')),
                            plotOutput("ind_plot1", height="800px"))
                 )),
        tabPanel("Variable results",
                 tabsetPanel(
                   tabPanel("Coordinates",
                            column(6,
                                   downloadButton('download_coord_var_pc', 'Download result (csv)')),
                            DT::dataTableOutput('coord_var_pc')),
                   tabPanel("Cos2",
                            column(6,
                                   downloadButton('download_cos2_var_pc', 'Download result (csv)')),
                            DT::dataTableOutput("coord_cos2_var_pc")),
                   tabPanel("Contribution",
                            column(6,
                                   downloadButton('download_cont_var_pc', 'Download result (csv)')),
                            DT::dataTableOutput("coord_cont_var_pc")),
                   tabPanel("Correlation",
                            column(6,
                                   downloadButton('download_cor_var_pc', 'Download result (csv)')),
                            DT::dataTableOutput("coord_cor_var_pc")),
                   tabPanel("Plot of Variables",
                            column(6,wellPanel("",
                                               htmlOutput("varsSup_s1"),
                                               htmlOutput("SupVar"),
                                               downloadButton("downloadVarplot","Download Plot"))),
                            column(6,wellPanel(h2("Costumizing the graph"),
                                               htmlOutput("axes1_var"),
                                               htmlOutput("axes2_var"),
                                               htmlOutput("geom_var"),
                                               htmlOutput("repel_var"),
                                               htmlOutput("select_var"))),
                            plotOutput("var_plot1",height = "800px"))
                 )),
        tabPanel("Biplot",
                 column(6,wellPanel("",
                                    htmlOutput("varsSupQuali_s2"),
                                    htmlOutput("axes1_bi"),
                                    htmlOutput("axes2_bi")
                 )),
                 column(6,wellPanel("",
                                    selectInput(inputId = "biplot",label = "Biplot",choices = c(T,F),selected = T,
                                                multiple = F),
                                    selectInput(inputId = "convex",label = "Convex Hull",choices = c(T,F),selected = T,
                                                multiple = F),
                                    htmlOutput("geom_var2"),
                                    htmlOutput("geom_ind2"),
                                    htmlOutput("alpha"),
                                    downloadButton("downloadBiplot","Download Plot")
                 )),
                 plotOutput("biplot1",height = "800px"))
      )
    )
  ))
