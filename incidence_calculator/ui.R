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
  titlePanel("Prevalence and incidence calculator (UNAIDS RG 2017) [alpha]"),
  fluidRow(
    tabsetPanel(id = "tabs", type = "tabs",
                tabPanel("Single survey",
                         br(),
                         fluidRow(
                           column(2,
                                  wellPanel(
                                    h3("Recency test"),
                                    numericInput("MDRI","MDRI (days):", value = 180, step = 1, min = 0, max = 730),
                                    numericInput("RSE_MDRI","RSE on MDRI (%):", value = 10, step = 0.5, min = 0, max = 100),
                                    numericInput("FRR","False-Recent Rate (%):", value = 0.5, step = 0.1, min = 0, max = 100),
                                    numericInput("RSE_FRR","RSE on FRR (%):", value = 25, step = 0.5, min = 0, max = 100),
                                    sliderInput("BigT", "Time cutoff T (days):", min = 180, max = 1095, value = 730, step = 5)
                                  )
                                  
                           ),
                           column(3,
                                  wellPanel(
                                    radioButtons("data_type", label = h3("Data dype:"),
                                                 c("Sample proportions" = 1,
                                                   "Sample counts" = 2
                                                 ),
                                                 selected = 1)
                                  ),
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
                                                   value = 20, step = 0.1, min=0, max = 100),
                                      numericInput("RSE_PrevH",
                                                   label = h5("RSE on prevalence (%)"),
                                                   value = 2.828, 
                                                   step = 0.1, min=0, max = 100),
                                      numericInput("PrevR",
                                                   label = h5("Proportion recent | + (%)"), value = 5, step = 0.1, min=0, max = 100),
                                      numericInput("RSE_PrevR",
                                                   label = h5("RSE on prop. recent (%)"),
                                                   value = 13.78,
                                                   step = 0.1, min=0, max = 100),
                                      numericInput("cor_PrevH_PrevR",
                                                   label = h5("Cor prev. & prop. recent"),
                                                   value = 0,
                                                   min = -1, max = 1, step = 0.01)
                                    )
                                  ),
                                  conditionalPanel(
                                    condition = "input.data_type == 2",
                                    wellPanel(
                                      h3("Survey data"),
                                      em("Assumptions:"),
                                      em(tags$ul(
                                        tags$li("Simple random sampling"),
                                        tags$li("Prevalence and prop. recent uncorrelated")
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
                                                   value = 1,
                                                   min = 1,
                                                   step = 0.1),
                                      numericInput("DE_R",
                                                   label = "Design effect prop. recent | +",
                                                   value = 1,
                                                   min = 1,
                                                   step = 0.1)
                                    )
                                  ),
                                  wellPanel(
                                    numericInput("n_bootstraps",
                                                 label = h5("Bootstrapping iterations"),
                                                 value = 100000,
                                                 step = 1000, min = 10000, max = 500000)
                                  )
                           ),
                           column(7,
                                  wellPanel(
                                    h2("Incidence estimates"),
                                    fluidRow(
                                      h3("Incidence"),
                                      tableOutput("incidence_table")
                                      # ,
                                      # br(),
                                      # h3("Annual risk of infection"),
                                      # tableOutput("ari_table"),
                                      # br(),
                                      # h3("Test properties"),
                                      # br(),
                                      # h5("MDRI"),
                                      # tableOutput("mdri_table"),
                                      # h5("FRR"),
                                      # tableOutput("frr_table")
                                    )
                                  )
                           )
                         )
                ),
                tabPanel("Multiple Surveys"
                         
                ),
                
                tabPanel("Technical Documentation", 
                         wellPanel(
                           includeHTML("www/TechnicalDocumentation.html")  
                         )
                ),
                
                tabPanel("Worked Example", 
                         wellPanel(
                           includeHTML("www/WorkedExample.html")
                         )
                ),
                
                tabPanel("About", 
                         #value='tab4_val', id = 'tab4',
                         wellPanel( p(""),
                                    p(HTML("Calculates incidence and annual risk of infection from survey data with biomarkers for recent infection.")),
                                    p("Authors:"),
                                    tags$ul(
                                      tags$li("Eduard Grebe"),
                                      tags$li("Alex Welte")
                                    ),
                                    p("Contributors:"),
                                    tags$ul(   
                                      tags$li("Lamin Juwara"),
                                      tags$li("Stefano Ongarello")
                                    ),
                                    p(em("Built using", a(strong("inctools"), href = "https://cran.r-project.org/web/packages/inctools/index.html", target = "_blank"))),
                                    br(),
                                    p("This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.  You should have received a copy of the GNU General Public License along with this program.  If not, see http://www.gnu.org/licenses/.")
                         )
                )
    )
  )
  
)
