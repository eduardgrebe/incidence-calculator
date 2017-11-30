# Copyright (C) 2017 Individual contributors
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your option)
# any later version.  This program is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
# Public License for more details.  You should have received a copy of the GNU
# General Public License along with this program.  If not, see
# <http://www.gnu.org/licenses/>.


library(shiny)
# Define UI
fluidPage(
  titlePanel("Prevalence and incidence calculator (UNAIDS RG) [beta, 30/11/2017]"),
  fluidRow(
    tabsetPanel(id = "tabset", type = "tabs",
                tabPanel("Estimate incidence",
                         br(),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    #h3("Prevalence estimate format"),
                                    radioButtons("single_multiple", label = h4("Enter HIV prevalence and prevalence of recency:"),
                                                 c("via a form (one survey at a time)" = 1,
                                                   "via a file (one or more surveys)" = 2
                                                 ),
                                                 selected = 1)
                                  )  
                                  
                           )
                         ),
                         fluidRow(
                           column(12,
                                  conditionalPanel(
                                    condition = "input.single_multiple == 1",
                                           column(3,
                                                  wellPanel(
                                                    radioButtons("data_type", label = h3("Data type:"),
                                                                 c("Sample proportions" = 1,
                                                                   "Sample counts" = 2
                                                                 ),
                                                                 selected = 1)
                                                  ),
                                                  wellPanel(
                                                    h3("Recency test"),
                                                    numericInput("MDRI","MDRI (days):", value = 180, step = 1, min = 0, max = 730),
                                                    numericInput("SE_MDRI","SE on MDRI:", value = 18, step = 0.5, min = 0, max = 100),
                                                    numericInput("FRR","False-Recent Rate (%):", value = 0.5, step = 0.1, min = 0, max = 100),
                                                    numericInput("SE_FRR","SE on FRR:", value = 0.125, step = 0.005, min = 0, max = 100),
                                                    sliderInput("BigT", "Time cutoff T (days):", min = 180, max = 1095, value = 730, step = 5)
                                                  )
                                                  
                                           ),
                                           column(4,
                                                  
                                                  conditionalPanel(
                                                    condition = "input.data_type == 1",
                                                    wellPanel(
                                                      h3("Survey data"),
                                                      em("Pre-processed data:"),
                                                      em(tags$ul(
                                                        tags$li("Use standard complex survey methods"),
                                                        tags$li("Estimate correlation between prevalence and prop. recent")
                                                      )),
                                                      numericInput("PrevH",
                                                                   label = h5("Prevalence (%)"),
                                                                   value = 20.000, step = 0.1, min=0, max = 100),
                                                      numericInput("SE_PrevH",
                                                                   label = h5("SE on prevalence (%)"),
                                                                   value = 0.693, 
                                                                   step = 0.1, min=0, max = 100),
                                                      numericInput("PrevR",
                                                                   label = h5("Proportion recent | + (%)"), value = 5, step = 0.1, min=0, max = 100),
                                                      numericInput("SE_PrevR",
                                                                   label = h5("SE on prop. recent (%)"),
                                                                   value = 0.844,
                                                                   step = 0.1, min=0, max = 100),
                                                      numericInput("cor_PrevH_PrevR",
                                                                   label = h5("Corr prev. & prop. recent"),
                                                                   value = 0.200,
                                                                   min = -1, max = 1, step = 0.01)
                                                    )
                                                  ),
                                                  conditionalPanel(
                                                    condition = "input.data_type == 2",
                                                    wellPanel(
                                                      h3("Survey data"),
                                                      em("Assumptions:"),
                                                      em(tags$ul(
                                                        tags$li("Non-SRS, DEs calculated"),
                                                        tags$li("Corr prevalence and prop.recent estimated")
                                                      )),
                                                      numericInput("N", 
                                                                   label = "Sample size",
                                                                   value = 5000,
                                                                   min = 1,
                                                                   step = 1),
                                                      numericInput("N_H", 
                                                                   label = "N HIV-positive",
                                                                   value = 1000, 
                                                                   min = 1,
                                                                   step = 1),
                                                      numericInput("N_testR", 
                                                                   label = "HIV-positives tested for recency",
                                                                   value = 1000, 
                                                                   min = 1,
                                                                   step = 1),
                                                      numericInput("N_R", 
                                                                   label = "N recent",
                                                                   value = 50, 
                                                                   min = 1,
                                                                   step = 1 ),
                                                      numericInput("DE_H", 
                                                                   label = "Design effect HIV prevalence",
                                                                   value = 1.5,
                                                                   min = 1,
                                                                   step = 0.1),
                                                      numericInput("DE_R",
                                                                   label = "Design effect prop. recent | +",
                                                                   value = 1.5,
                                                                   min = 1,
                                                                   step = 0.1),
                                                      numericInput("cor_PrevH_PrevR",
                                                                   label = h5("Corr prev. & prop. recent"),
                                                                   value = 0.200,
                                                                   min = -1, max = 1, step = 0.01)
                                                    )
                                                  ),
                                                  wellPanel(
                                                    numericInput("n_bootstraps",
                                                                 label = h5("Bootstrapping iterations"),
                                                                 value = 100000,
                                                                 step = 1000, min = 10000, max = 500000)
                                                  )
                                           ),
                                    column(5,
                                           wellPanel(
                                             h2("Incidence estimates"),
                                             fluidRow(
                                               conditionalPanel(
                                                 condition = "input.single_multiple == 1",
                                                 wellPanel(
                                                   tableOutput("incidence_table")
                                                 )
                                               )
                                             )
                                           )
                                    )
                                    ),
              
                                  conditionalPanel(
                                    condition = "input.single_multiple == 2",
                                    column(12,
                                           wellPanel(
                                             h2("Upload survey results"),
                                             fluidRow(
                                               br(),
                                               fileInput("input_file", "Choose CSV File",
                                                                        multiple = FALSE,
                                                                        accept = c("text/csv",
                                                                                   "text/comma-separated-values,text/plain",
                                                                                   ".csv"))
                                             )
                                             ),
                                             wellPanel(
                                             h2("Input data"),
                                             fluidRow(
                                                 tableOutput("data_table")
                                             )
                                           ),
                                           
                                             wellPanel(
                                               h2("Incidence estimates"),
                                                     tableOutput("incidence_table_multiple"),
                                               downloadButton("incidence_table_download", label = "Download estimates")
                                            
                                           )
                                           )
                                  )
                         )
                         )
                
                ),
                
                tabPanel("Help", 
                         #value = "example",
                         wellPanel(
                           includeHTML("www/Help.html")
                         )
                ),
                
                tabPanel("Technical documentation",
                         #value = "techdoc",
                         wellPanel(
                           includeHTML("www/TechnicalDocumentation.html")  
                         )
                ),
                
                tabPanel("About", 
                         #value = "about",
                         #value='tab4_val', id = 'tab4',
                         wellPanel( p(""),
                                    p(HTML("Calculates HIV incidence from prevalence survey data that include biomarkers of recent infection.")),
                                    p("Authors:"),
                                    tags$ul(
                                      tags$li("Eduard Grebe (SACEMA, Stellenbosch University)"),
                                      tags$li("Alex Welte (SACEMA, Stellenbosch University)")
                                    ),
                                    p("Contributors:"),
                                    tags$ul(
                                      tags$li("Jeffrey Eaton (Imperial College London)")
                                    ),
                                    p(em("Built using", a(strong("inctools"), href = "https://github.com/SACEMA/inctools", target = "_blank"))),
                                    br(),
                                    
                                    p("This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.  You should have received a copy of the GNU General Public License along with this program.  If not, see http://www.gnu.org/licenses/.")
                         ),
                         br(),
                         img(src='SACEMA_logo.jpg', align = "right")
                )
)
)
)

