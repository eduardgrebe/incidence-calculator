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

fluidPage(
  titlePanel("Prevalence and Incidence Calculator (UNAIDS RG) [beta 3, 30/11/2017]"),
  fluidRow(
    tabsetPanel(id = "tabset", type = "tabs",
                tabPanel("Estimate Incidence",
                         br(),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    #h3("Prevalence estimate format"),
                                    radioButtons("single_multiple", label = h4("Enter HIV Prevalence and Proportion Recent:"),
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
                                                    radioButtons("data_type", label = h3("Data Type:"),
                                                                 c("Estimated Prevalences" = 1,
                                                                   "Sample Counts" = 2
                                                                 ),
                                                                 selected = 1)
                                                  ),
                                                  wellPanel(
                                                    h3("Recency Test"),
                                                    numericInput("MDRI","MDRI (days):", value = 180, step = 1, min = 0, max = 730),
                                                    numericInput("SE_MDRI","SE on MDRI:", value = 18, step = 0.5, min = 0, max = 100),
                                                    numericInput("FRR","False-Recent Rate (%):", value = 0.5, step = 0.1, min = 0, max = 100),
                                                    numericInput("SE_FRR","SE on FRR (percentage points):", value = 0.125, step = 0.005, min = 0, max = 100),
                                                    sliderInput("BigT", "Time Cutoff T (days):", min = 180, max = 1095, value = 730, step = 5)
                                                  )
                                                  
                                           ),
                                           column(4,
                                                  
                                                  conditionalPanel(
                                                    condition = "input.data_type == 1",
                                                    wellPanel(
                                                      h3("Survey Data"),
                                                      em("Pre-processed Data:"),
                                                      em(tags$ul(
                                                        tags$li("Use standard complex survey methods"),
                                                        tags$li("Estimate correlation between prevalence and prop. recent")
                                                      )),
                                                      numericInput("PrevH",
                                                                   label = h5("Prevalence (%):"),
                                                                   value = 20.000, step = 0.1, min=0, max = 100),
                                                      numericInput("SE_PrevH",
                                                                   label = h5("SE on Prevalence (percentage points):"),
                                                                   value = 0.693, 
                                                                   step = 0.1, min=0, max = 100),
                                                      numericInput("PrevR",
                                                                   label = h5("Proportion of HIV+ that test Recent (%):"), value = 5, step = 0.1, min=0, max = 100),
                                                      numericInput("SE_PrevR",
                                                                   label = h5("SE on Prop. Recent (percentage points):"),
                                                                   value = 0.844,
                                                                   step = 0.1, min=0, max = 100),
                                                      numericInput("cor_PrevH_PrevR",
                                                                   label = h5("Corr (Prev., Prop. Recent):"),
                                                                   value = 0.200,
                                                                   min = -1, max = 1, step = 0.01)
                                                    )
                                                  ),
                                                  conditionalPanel(
                                                    condition = "input.data_type == 2",
                                                    wellPanel(
                                                      h3("Survey Data"),
                                                      # em("Assumptions:"),
                                                      # em(tags$ul(
                                                      #   tags$li("Non-SRS, DEs calculated"),
                                                      #   tags$li("Corr prevalence and prop.recent estimated")
                                                      # )),
                                                      numericInput("N", 
                                                                   label = "Sample Size:",
                                                                   value = 5000,
                                                                   min = 1,
                                                                   step = 1),
                                                      numericInput("N_H", 
                                                                   label = "N HIV Positive:",
                                                                   value = 1000, 
                                                                   min = 1,
                                                                   step = 1),
                                                      numericInput("N_testR", 
                                                                   label = "HIV Positives Tested for Recency:",
                                                                   value = 1000, 
                                                                   min = 1,
                                                                   step = 1),
                                                      numericInput("N_R", 
                                                                   label = "N Recent:",
                                                                   value = 50, 
                                                                   min = 1,
                                                                   step = 1 ),
                                                      numericInput("DE_H", 
                                                                   label = "Design Effect, HIV Prevalence:",
                                                                   value = 1.5,
                                                                   min = 1,
                                                                   step = 0.1),
                                                      numericInput("DE_R",
                                                                   label = "Design Effect, Prop. HIV+ who test Recent:",
                                                                   value = 1.5,
                                                                   min = 1,
                                                                   step = 0.1)
                                                      # ,
                                                      # numericInput("cor_PrevH_PrevR",
                                                      #              label = h5("Corr prev. & prop. recent"),
                                                      #              value = 0.200,
                                                      #              min = -1, max = 1, step = 0.01)
                                                    )
                                                  ),
                                                  wellPanel(
                                                    numericInput("n_bootstraps",
                                                                 label = h5("Bootstrapping Iterations:"),
                                                                 value = 100000,
                                                                 step = 1000, min = 10000, max = 500000)
                                                  )
                                           ),
                                    column(5,
                                           wellPanel(
                                             h2("Incidence Estimates"),
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
                                             h2("Upload Survey Results"),
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
                                             h2("Uploaded Data"),
                                             fluidRow(
                                                 tableOutput("data_table")
                                             )
                                           ),
                                           
                                             wellPanel(
                                               h2("Incidence Estimates"),
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
                
                tabPanel("Technical Documentation",
                         #value = "techdoc",
                         wellPanel(
                           includeHTML("www/TechnicalDocumentation.html")  
                         )
                ),
                
                tabPanel("About", 
                         #value = "about",
                         #value='tab4_val', id = 'tab4',
                         wellPanel( p(""),
                                    p(em("Calculates HIV incidence from prevalence survey data that include biomarkers of recent infection.")),
                                    p(strong("Authors:")),
                                    tags$ul(
                                      tags$li(a("Eduard Grebe (SACEMA, Stellenbosch University)", href = "mailto:eduardgrebe@sun.ac.za")),
                                      tags$li(a("Alex Welte (SACEMA, Stellenbosch University)", href = "mailto:alexwelte@sun.ac.za"))
                                    ),
                                    p(strong("Project lead:")),
                                    tags$ul(
                                      tags$li(a("Jeffrey Eaton (Imperial College London)", href = "mailto:jeffrey.eaton@imperial.ac.uk"))
                                    ),
                                    br(),
                                    p("Re-uses some code under the GPL from:"),
                                    tags$ul(
                                      tags$li("Lamin Juwara (McMaster University)"),
                                      tags$li("Stefano Ongarello (FIND)")
                                    ),
                                    br(),
                                    p(em(strong("Built using", a(strong("inctools"), href = "https://github.com/SACEMA/inctools", target = "_blank")))),
                                    br(),
                                    p(strong("Licence: GPL-3")),
                                    p("This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.  You should have received a copy of the GNU General Public License along with this program.  If not, see http://www.gnu.org/licenses/.")
                         ),
                         br(),
                         img(src='SACEMA_logo.jpg', align = "right")
                )
)
)
)

