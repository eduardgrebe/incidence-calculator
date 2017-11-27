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
  titlePanel("Prevalence and incidence calculator (UNAIDS RG 2017)"),
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
    column(2,
           wellPanel(
             h3("Survey data"),
             numericInput("PrevH",
                           label = h5("Prevalence (%)"),
                           value = 20, step = 0.1, min=0, max = 100),
             numericInput("RSE_PrevH",
                          label = h5("RSE on prevalence (%)"),
                          value = 2.8, 
                          step = 0.1, min=0, max = 100),
             numericInput("PrevR",
                          label = h5("Proportion recent | + (%)"), value = 10, step = 0.1, min=0, max = 100),
             numericInput("RSE_PrevR",
                          label = h5("RSE on prop. recent (%)"),
                          value = 9.8,
                          step = 0.1, min=0, max = 100),
             numericInput("cor_PrevH_PrevR",
                          label = h5("Cor prev. & prop. recent"),
                          value = 0,
                          min = 0, max = 1, step = 0.01)
           ),
           wellPanel(
             numericInput("n_bootstraps",
                          label = h5("Bootstrapping iterations"),
                          value = 100000,
                          step = 1000, min = 10000, max = 500000)
           )
    ),
    column(8,
           tabsetPanel(id = "tabs", type = "tabs",
                       tabPanel("Incidence estimates",
                                 br(""),
                                 # fluidRow(column(12,
                                 #                 downloadButton('downloadData1', 'Download Estimates'))
                                 # ),
                                 # br(""),
                                fluidRow(
                                  h3("Incidence"),
                                  tableOutput("incidence_table"),
                                  br(),
                                  h3("Annual risk of infection"),
                                  tableOutput("ari_table"),
                                  br(),
                                  h3("Test properties"),
                                  br(),
                                  h5("MDRI"),
                                  tableOutput("mdri_table"),
                                  h5("FRR"),
                                  tableOutput("frr_table")
                                 )
                                # ,
                                # fluidRow(
                                #   h3("Annual risk of infection"),
                                #   tableOutput("ari_table")
                                #   ),
                                # fluidRow(
                                #   h3("Recent infection test properties"),
                                #   verbatimTextOutput("everything"),
                                #   tableOutput("mdri_table")#,
                                #   #tableOutput("frr_table")
                                # )
                                ),
                       
                       tabPanel("Documentation", 
                                #value='tab4_val', id = 'tab4',
                                wellPanel(p("The documentation will go here"),
                                          p(HTML("
                                            <p> This is an HTML page </p>
                                          ")))
                                           
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
  
)
  