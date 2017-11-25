# Copyright (C) 2017 Alex Welte, Eduard Grebe, Lamin Juwara
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
  titlePanel("Incidence calculator"),
  fluidRow(
    column(3,
           wellPanel(
             h3("Sample Proportions"),
             numericInput("PrevH",
                           label = h5("Prevalence of HIV infection (%)"),
                           value = 20, step = 0.1, min=0, max = 100),
             numericInput("RSE_PrevH",
                          label = h5("RSE of Prevalence HIV infection (%)"),
                          value = 2.8, 
                          step = 0.1, min=0, max = 100),
             numericInput("PrevR",
                          label = h5("Prevalence of recent infections among positives (%)"), value = 10, step = 0.1, min=0, max = 100),
             numericInput("RSE_PrevR",
                          label = h5("RSE of Prevalence of recent infections among positives (%)"),
                          value = 9.8,
                          step = 0.1, min=0, max = 100),
             numericInput("cov_PrevH_PrevR",
                          label = h5("Covariance of Prevalence and Prevalence of recent infection"),
                          value = 0)
           ),
           wellPanel(
             h4("Recency test"),
             numericInput("MDRI","Mean Duration of Recent Infection (days):", value = 180, step = 1, min = 0, max = 730),
             numericInput("RSE_MDRI","Relative Standard Error on MDRI (%):", value = 10, step = 0.5, min = 0, max = 100),
             numericInput("FRR","False-Recent Rate (%):", value = 0.5, step = 0.1, min = 0, max = 100),
             numericInput("RSE_FRR","Relative Standard Error on FRR (%):", value = 25, step = 0.5, min = 0, max = 100),
             sliderInput("BigT", "Time cutoff T (days):", min = 180, max = 1096, value = 730, step = 1)
           )
    ),
    column(9,
           tabsetPanel(id = "tabs", type = "tabs",
                       tabPanel("Incidence Estimates",
                                 br(""),
                                 # fluidRow(column(12,
                                 #                 downloadButton('downloadData1', 'Download Estimates'))
                                 # ),
                                 # br(""),
                                fluidRow(
                                 tableOutput("incest_tab"))
                                # ,
                                # fluidRow(
                                #   tableOutput("incest_rest"))
                                ),
                       
                       tabPanel("Documentation", 
                                #value='tab4_val', id = 'tab4',
                                wellPanel( p("The documentation will go here"))
                                           
                       ),
                       tabPanel("About", 
                                #value='tab4_val', id = 'tab4',
                                wellPanel( p(""),
                                           p(HTML("Calculates the point estimate and confidence interval for incidence, prevalence and
                                              annual risk of infection.")),
                                           p("Contributors:"),
                                           tags$ul(
                                             tags$li("Eduard Grebe"),
                                             tags$li("Alex Welte"),
                                             tags$li("Lamin Juwara"),
                                             tags$li("Stefano Ongarello")
                                           ),
                                           p(em("Built using", a(strong("inctools"), href = "https://cran.r-project.org/web/packages/inctools/index.html", target = "_blank")))
                                )
                       )
           )
    )
  )
  
)
  