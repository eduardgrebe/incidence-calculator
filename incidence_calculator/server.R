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
    if (input$data_type == 1) {
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
        need(!is.na(input$cor_PrevH_PrevR), "Please provide a valid correlation for PrevH and PrevR (default: 0)"),
        need(input$n_bootstraps >= 10000, "Bootstrapping iterations must be in the range [10,000,500,000]"),
        need(input$n_bootstraps <= 500000, "Bootstrapping iterations must be in the range [10,000,500,000]")
      )
      temp <- incprops(PrevH = input$PrevH/100, 
                       RSE_PrevH = input$RSE_PrevH/100,
                       PrevR = input$PrevR/100, 
                       RSE_PrevR = input$RSE_PrevR/100,
                       MDRI = input$MDRI, 
                       RSE_MDRI = input$RSE_MDRI/100,
                       FRR = input$FRR/100, 
                       RSE_FRR = input$RSE_FRR/100,
                       BigT = input$BigT,
                       Boot = TRUE,
                       BS_Count = input$n_bootstraps,
                       cor_HR = input$cor_PrevH_PrevR)
      
      inc_df <- dplyr::data_frame(
        `%HIV+` = input$PrevH * 100 / 100, #cluge to make trailing zeros appear
        `%HIV+ SE` = input$PrevH/100 * input$RSE_PrevH/100 * 100,
        `%Inc` = temp$Incidence$Incidence * 100,
        `%Inc SE` = temp$Incidence$RSE.I * temp$Incidence$Incidence * 100,
        Corr = temp$Incidence$Cor.PrevH.I
      )
      
      return(inc_df)
      
    } else if (input$data_type == 2) {
      validate(
        need(input$N>0,"Please enter a valid total population sample size"),
        need(input$N>=input$N_H,"HIV-positive subjects should be less than total sample size"),
        need(input$N_H>=input$N_testR,"HIV-positive subjects tested for recency should be less than or equal to HIV-positive subjects among total sample size"),
        need(input$N_testR>=input$N_R,"The number of recent HIV cases should be less than or equal to HIV-positive subjects tested for recency"),
        need(input$DE_H >= 1, "Design effect on HIV prevalence must be >= 1"),
        need(input$DE_R >= 1, "Design effect on Prop. recent | + must be >= 1"),
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
        need(input$BigT > 120, 'Please provide a valid value for the cut-off time (>120)')
      )
      
      temp <- inccounts(N = input$N, 
                        N_H = input$N_H, 
                        N_testR = input$N_testR, 
                        N_R = input$N_R, 
                        DE_H = input$DE_H, 
                        DE_R = input$DE_R, 
                        MDRI = input$MDRI, 
                        RSE_MDRI = input$RSE_MDRI/100,
                        FRR = input$FRR/100, 
                        RSE_FRR = input$RSE_FRR/100,
                        BigT = input$BigT,
                        Boot = TRUE,
                        BS_Count = input$n_bootstraps,
                        Covar_HR = 0, 
                        cor_HR = 0)
      
      temp_prev <- prevcounts(N = input$N, 
                              N_H = input$N_H, 
                              N_testR = input$N_testR, 
                              N_R = input$N_R,
                              DE_H = input$DE_H, 
                              DE_R = input$DE_R)
      
      inc_df <- dplyr::data_frame(
        `%HIV+` = temp_prev$PrevH * 100,
        `%HIV+ SE` = temp_prev$RSE_PrevH * temp_prev$PrevH * 100,
        `%Inc` = temp$Incidence$Incidence * 100,
        `%Inc SE` = temp$Incidence$RSE.I * temp$Incidence$Incidence * 100,
        Corr = temp$Incidence$Cor.PrevH.I
      )
      
      return(inc_df)
    }
  }) 
  
  output$incidence_table <- renderTable(digits = 3, {
    incidence_calc()
  })
  
})
