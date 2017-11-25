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

#server.R

library(shiny)
library(ggplot2)
library(scales)
library(plyr)
library(dplyr)
library(grid)
source('incidence.R')


shinyServer(function(input, output){
  incidence_calc <- reactive({
    validate(
      need(input$PrevH >= 0, 'Please provide a valid value for HIV prevalence'),
      need(input$PrevH <= 100, 'Please provide a valid HIV prevalence'),
      need(!(input$PrevH == "" ), 'Please provide a value HIV prevalence'),
      need(input$RSE_PrevH >= 0, 'Please provide a valid RSE for HIV prevalence'),
      need(input$RSE_PrevH <= 100, 'Please provide a valid RSE for Hiv prevalence'),
      need(!(input$RSE_PrevH == "" ), 'Please provide a value for RSE for HIV prevalence'),
      need(input$PrevR >= 0, 'Please provide a valid value for recency among HIV prevalence'),
      need(input$PrevR <= 100, 'Please provide a valid recency among HIV prevalence'),
      need(!(input$PrevR == "" ), 'Please provide a value recency among HIV prevalence'),
      need(input$RSE_PrevR >= 0, 'Please provide a valid RSE for recency of HIV positives'),
      need(input$RSE_PrevR <= 100, 'Please provide a valid RSE for recency of Hiv prevalence'),
      need(!(input$RSE_PrevR == "" ), 'Please provide a value RSE for recency of HIV prevalence'),
      need(input$RSE_FRR >= 0, 'Please provide a valid RSE for FRR'),
      need(input$RSE_FRR <= 100, 'Please provide a valid RSE for FRR'),
      need(!(input$RSE_FRR == "" ), 'Please provide a value for RSE_FRR'),
      need(input$RSE_MDRI >= 0, 'Please provide a valid RSE for MDRI'),
      need(input$RSE_MDRI <= 100, 'Please provide a valid RSE for MDRI'),
      need(!(input$RSE_MDRI == "" ), 'Please provide a value for RSE_MDRI'),
      need(input$MDRI >= 0, 'Please provide a valid value for MDRI'),
      need(input$FRR >= 0, 'Please provide a valid value for FRR'),
      need(input$FRR <= 100, 'Please provide a valid value for FRR'),
      need(input$BigT, 'Please provide a value for the cut-off time'),
      need(input$BigT > 120, 'Please provide a valid value for the cut-off time (>120)'),
      need(!is.na(input$cov_PrevH_PrevR), "Please provide a valid covariance for PrevH and PrevR (default: 0)"),
      need(input$n_bootstraps >= 10000, "Bootstrapping iterations must be in the range [10,000,500,000]"),
      need(input$n_bootstraps <= 500000, "Bootstrapping iterations must be in the range [10,000,500,000]")
    )
    incprops(PrevH = input$PrevH/100, RSE_PrevH = input$RSE_PrevH/100,
             PrevR = input$PrevR/100, RSE_PrevR = input$RSE_PrevR/100,
             MDRI = input$MDRI, RSE_MDRI = input$RSE_MDRI/100,
             FRR = input$FRR/100, RSE_FRR = input$RSE_FRR/100,
             BigT = input$BigT,
             Boot = TRUE,
             BS_Count = input$n_bootstraps,
             Covar_HR = input$cov_PrevH_PrevR)
  }) 
  
    output$incidence_table <- renderTable(digits = 6, {
      incidence_calc()$Incidence
    })

  output$ari_table <- renderTable(digits = 6, {
    incidence_calc()$Annual.Risk.Infection
  })

  output$mdri_table <- renderTable(digits = 2, {
    incidence_calc()$MDRI.CI
  })

  output$frr_table <- renderTable(digits = 4, {
    incidence_calc()$FRR.CI
  })
  
})
