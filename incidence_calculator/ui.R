## Copyright (C) 2017 Individual contributors
## This program is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as published by the
## Free Software Foundation, either version 3 of the License, or (at your option)
## any later version.  This program is distributed in the hope that it will be
## useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
## Public License for more details.  You should have received a copy of the GNU
## General Public License along with this program.  If not, see
## <http://www.gnu.org/licenses/>.

library(shiny)

fluidPage(
  img(src='2012_logo_example1_en.jpg', align = "right", width=350,
      style="border: 25px solid white;"),
  titlePanel("Prevalence and Incidence Calculator (UNAIDS RG) [beta 6, 02/12/2017]"),
  fluidRow(
    tabsetPanel(id = "tabset", type = "tabs",
                tabPanel("Estimate Incidence",
                         br(),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                        #h3("Prevalence estimate format"),
                                    radioButtons("single_multiple", label = h3("Enter survey data about HIV stautus and recency:"),
                                                 c("via a form (one population at a time)" = 1,
                                                   "via a file (multiple populations/surveys)" = 2
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
                                                          c("Prevalence and proportion recent" = 1,
                                                            "Population proportions" = 4,
                                                            "Population totals" = 3,
                                                            "Raw sample counts" = 2,
                                                            "Prevalence and Incidence" = 5
                                                            ),
                                                          selected = 1)
                                           ),
                                           wellPanel(
                                             h3("Recency Test"),
                                             textInput("MDRI",
                                                       label = h5("MDRI (days):"),
                                                       value = 180),
                                             textInput("SE_MDRI",
                                                       label = h5("SE of MDRI:"),
                                                       value = 18),
                                             textInput("FRR",
                                                       label = h5("False-Recent Rate (%):"),
                                                       value = 0.5),
                                             textInput("SE_FRR",
                                                       label = h5("SE of FRR (percentage points):"),
                                                       value = 0.125),
                                             textInput("BigT",
                                                       label = h5("Time Cutoff T (days):"),
                                                       value = 730)
                                           )
                                           ),
                                    column(4,

                                           conditionalPanel(
                                             condition = "input.data_type == 1",
                                             wellPanel(
                                               h3("Prevalences"),
                                               em("Pre-processed Data:"),
                                               em(tags$ul(
                                                         tags$li("Use standard complex survey methods"),
                                                         tags$li("Estimate correlation between prevalence and prop. recent")
                                                       )),
                                               textInput("PrevH",
                                                         label = h5("Prevalence (%):"),
                                                         value = 20),
                                               textInput("SE_PrevH",
                                                         label = h5("SE of Prevalence (percentage points):"),
                                                         value = 0.7),
                                               textInput("PrevR",
                                                         label = h5("Prevalence of Recency amongst HIV+ (%):"),
                                                         value = 5),
                                               textInput("SE_PrevR",
                                                         label = h5("SE of Prev. Recency (percentage points):"),
                                                         value = 0.9),
                                               textInput("cor_PrevH_PrevR",
                                                         label = h5("Corr (Prev., Prop. Recent):"),
                                                         value = 0.12)
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.data_type == 2",
                                             wellPanel(
                                               h3("Raw Sample Counts"),
                                        # em("Assumptions:"),
                                        # em(tags$ul(
                                        #   tags$li("Non-SRS, DEs calculated"),
                                        #   tags$li("Corr prevalence and prop.recent estimated")
                                        # )),
                                               textInput("N",
                                                         label = h5("Sample Size:"),
                                                         value = 5000),
                                               textInput("N_H",
                                                         label = h5("N HIV Positive:"),
                                                         value = 1000),
                                               textInput("N_testR",
                                                         label = h5("HIV Positives Tested for Recency:"),
                                                         value = 900),
                                               textInput("N_R",
                                                         label = h5("N Recent:"),
                                                         value = 45),
                                               textInput("DE_H",
                                                         label = h5("Design Effect, HIV Prevalence:"),
                                                         value = 1),
                                               textInput("DE_R",
                                                         label = h5("Design Effect, Prop. HIV+ who test Recent:"),
                                                         value = 1)
                                        # ,
                                        # textInput("cor_PrevH_PrevR",
                                        #              label = h5("Corr prev. & prop. recent"),
                                        #              value = 0.200,
                                        #              min = -1, max = 1, step = 0.01)
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.data_type == 3",
                                             wellPanel(
                                               h3("Population Totals"),
                                               em("If all positives were tested for recency, input 'N HIV+ and not tested' to 0."),
                                        # em(tags$ul(
                                        #   tags$li("Non-SRS, DEs calculated"),
                                        #   tags$li("Corr prevalence and prop.recent estimated")
                                        # )),
                                               h4("Totals:"),
                                               textInput("N_Re",
                                                         label = h5("N HIV+ and Recent:"),
                                                         value = 31.331),

                                               textInput("N_nonR",
                                                         label = h5("N HIV+ and Not Recent:"),
                                                         value = 954.639),
                                               textInput("N_notT",
                                                         label = h5("N HIV+ and Not Tested for Recency:"),
                                                         value = 110.514),

                                               textInput("N_Neg",
                                                         label = h5("N HIV Negative:"),
                                                         value = 10903.516),
                                               h4("Variance-Covariance:"),
                                               textInput("Var_N_R",
                                                         label = h5("Var(N Recent):"),
                                                         value = 32.4219032),
                                               textInput("Var_N_nonR",
                                                         label = h5("Var(N Not Recent):"),
                                                         value = 1784.879919),
                                               textInput("Var_N_notT",
                                                         label = h5("Var(N Not Tested):"),
                                                         value = 122.1672748),
                                               textInput("Var_N_Neg",
                                                         label = h5("Var(N HIV Negative):"),
                                                         value = 29790.404358),
                                               textInput("Cov_R_NR",
                                                         label = h5("Cov(N Recent, N Not Recent):"),
                                                         value = -0.695587),
                                               textInput("Cov_R_notT",
                                                         label = h5("Cov(N Recent, N Not Tested):"),
                                                         value = -0.9200922),
                                               textInput("Cov_R_Neg",
                                                         label = h5("Cov(N Recent, N HIV Negative):"),
                                                         value = -5.589968),
                                               textInput("Cov_NR_notT",
                                                         label = h5("Cov(N Not Recent, N Not Tested):"),
                                                         value = 67.5996290),
                                               textInput("Cov_NR_Neg",
                                                         label = h5("Cov(N Not Recent, N HIV Negative):"),
                                                         value = 1283.403260),
                                               textInput("Cov_NotT_Neg",
                                                         label = h5("Cov(N Not Tested, N HIV Negative):"),
                                                         value = 291.981347)
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.data_type == 4",
                                             wellPanel(
                                               h3("Population Proportions"),

                                               textInput("P_Re",
                                                         label = h5("Proportion HIV+ and Recent:"),
                                                         value = 0.0026109),
                                               textInput("P_nonR",
                                                         label = h5("Proportion HIV+ and Not Recent:"),
                                                         value = 0.0795532),
                                               textInput("P_notT",
                                                         label = h5("Proportion HIV+ and Not Tested for Recency:"),
                                                         value = 0.0092095),
                                               textInput("P_Neg",
                                                         label = h5("Proportion HIV Negative:"),
                                                         value = 0.9086263),
                                               h4("Variance-Covariance:"),
                                               textInput("Var_P_R",
                                                         label = h5("Var(Prop. Recent):"),
                                                         value = 2.258946e-07),
                                               textInput("Var_P_nonR",
                                                         label = h5("Var(Prop. Not Recent):"),
                                                         value = 1.046920e-05),
                                               textInput("Var_P_notT",
                                                         label = h5("Var(Prop. Not Tested):"),
                                                         value = 8.074968e-07),
                                               textInput("Var_P_Neg",
                                                         label = h5("Var(Prop. HIV Negative):"),
                                                         value = 1.179336e-05),
                                               textInput("Cov_R_NRp",
                                                         label = h5("Cov(Prop. Recent, Prop. Not Recent):"),
                                                         value = -2.512008e-08),
                                               textInput("Cov_R_notTp",
                                                         label = h5("Cov(Prop. Recent, Prop. Not Tested):"),
                                                         value = -1.087571e-08),
                                               textInput("Cov_R_Negp",
                                                         label = h5("Cov(Prop. Recent, Prop. HIV Negative):"),
                                                         value = -1.898988e-07),
                                               textInput("Cov_NR_notTp",
                                                         label = h5("Cov(Prop. Not Recent, Prop. Not Tested):"),
                                                         value = 1.813772e-07),
                                               textInput("Cov_NR_Negp",
                                                         label = h5("Cov(Prop. Not Recent, Prop. HIV Negative):"),
                                                         value = -1.062546e-05),
                                               textInput("Cov_NotT_Negp",
                                                         label = h5("Cov(Prop. Not Tested, Prop. HIV Negative):"),
                                                         value = -9.779983e-07)
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.data_type == 5",
                                             wellPanel(
                                               h3("Prevalence and Incidence"),
                                               em("Pre-calculated Prevalence and Incidence. Assume Prevalence and Prevalence of Recency uncorrelated."),
                                               textInput("PrevH",
                                                         label = h5("Prevalence (%):"),
                                                         value = 20),
                                               textInput("SE_PrevH",
                                                         label = h5("SE of Prevalence (percentage points):"),
                                                         value = 0.7),
                                               textInput("Inc",
                                                         label = h5("Incidence (% p.a.):"),
                                                         value = 2.33),
                                               textInput("SE_Inc",
                                                         label = h5("SE of Incidence (percentage points):"),
                                                         value = 0.56)
                                             )
                                           )
                                           ),
                                    column(5,
                                           wellPanel(
                                             h2("Incidence Estimates"),
                                             fluidRow(
                                               conditionalPanel(
                                                 condition = "input.single_multiple == 1",
                                                 wellPanel(
                                                   tableOutput("incidence_table"),
                                                   br(),
                                                   p("*per annum")
                                                 )
                                               )
                                             )
                                           ),
                                           wellPanel(
                                             numericInput("n_bootstraps",
                                                          label = h5("Bootstrapping Iterations:"),
                                                          value = 100000,
                                                          step = 10000,
                                                          min = 10000,
                                                          max = 500000)
                                           )
                                           )
                                  )
                                  )
                         ),
                         fluidRow(
                           conditionalPanel(
                             condition = "input.single_multiple == 2",

                             br(),
                             column(12,

                                    wellPanel(
                                      h3("Upload Survey Results"),
                                      br(),
                                      fileInput("input_file", "Choose CSV File",
                                                multiple = FALSE,
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv"))
                                    ),
                                    br(),


                                    wellPanel(
                                      h3("Uploaded Data"),
                                      fluidRow(
                                        tableOutput("data_table")
                                      )
                                    ),
                                    br(),
                                    wellPanel(
                                      h2("Incidence Estimates"),
                                      tableOutput("incidence_table_multiple"),
                                      downloadButton("incidence_table_download", label = "Download estimates")

                                    ),
                                    br(),
                                    wellPanel(
                                      numericInput("n_bootstraps",
                                                   label = h5("Bootstrapping Iterations:"),
                                                   value = 100000,
                                                   step = 1000,
                                                   min = 10000,
                                                   max = 500000)
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
                                          tags$li(a("Alex Welte (SACEMA, Stellenbosch University)", href = "mailto:alexwelte@sun.ac.za")),
                                          tags$li(a("Jeffrey Eaton (Imperial College London)", href = "mailto:jeffrey.eaton@imperial.ac.uk"))
                                        ),
                                   br(),
                                   p("Re-uses some code under the GPL from:"),
                                   tags$ul(
                                          tags$li("Lamin Juwara (McGill University)"),
                                          tags$li("Stefano Ongarello (FIND)")
                                        ),
                                   br(),
                                   p(em(strong("Built using", a(strong("inctools"), href = "https://github.com/SACEMA/inctools", target = "_blank")))),
                                   br(),
                                   p(strong("Licence: GPL-3")),
                                   p("This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.  You should have received a copy of the GNU General Public License along with this program.  If not, see http://www.gnu.org/licenses/.")
                                   ),
                         br(),
                         img(src='MRC_COAM_colour_web.png', align = "right",
                             style="height: 80px; border: 10px solid white;"),
                         img(src='imp_ml_1cs_ps_056486_001.jpg', align = "right",
                             style="height: 80px; border: 10px solid white;"),
                         img(src='SACEMA_logo.jpg', align = "right",
                             style="height: 100px; border: 10px solid white;")
                         )
                )
  )
)
