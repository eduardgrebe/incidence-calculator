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
library(readxl)
library(readr)
source('incidence.R')

shinyServer(function(input, output){

  determine_tab <- reactive({
    active_tab <- input$tabs
    return(active_tab)
  })
  
  incidence_calc <- reactive({
    #browser()
    
    if (input$single_multiple == 1) {
      validate(
        need(input$SE_FRR >= 0, 'Please provide a valid SE for FRR'),
        #need(input$SE_FRR <= 100, 'Please provide a valid SE for FRR'),
        need(!(input$SE_FRR == "" ), 'Please provide a value for SE_FRR'),
        need(input$SE_MDRI >= 0, 'Please provide a valid SE for MDRI'),
        #need(input$SE_MDRI <= 100, 'Please provide a valid SE for MDRI'),
        need(!(input$SE_MDRI == "" ), 'Please provide a value for SE_MDRI'),
        need(input$MDRI >= 0, 'Please provide a valid value for MDRI'),
        need(input$FRR >= 0, 'Please provide a valid value for FRR'),
        need(input$FRR <= 100, 'Please provide a valid value for FRR'),
        need(input$BigT, 'Please provide a value for the cut-off time'),
        need(input$BigT > 120, 'Please provide a valid value for the cut-off time (>120)'),
        need(input$n_bootstraps >= 10000, "Bootstrapping iterations must be in the range [10,000,500,000]"),
        need(input$n_bootstraps <= 500000, "Bootstrapping iterations must be in the range [10,000,500,000]")
      )
      
      RSE_MDRI <- input$SE_MDRI / input$MDRI
      MDRI <- input$MDRI
      
      FRR <- input$FRR / 100
      RSE_FRR  <- input$SE_FRR / input$FRR
      
      
      if (input$data_type == 1) {
        validate(
          need(input$PrevH >= 0, 'Please provide a valid value for HIV prevalence'),
          need(input$PrevH <= 100, 'Please provide a valid HIV prevalence'),
          need(!(input$PrevH == "" ), 'Please provide a value HIV prevalence'),
          need(input$SE_PrevH >= 0, 'Please provide a valid SE for HIV prevalence'),
          #need(input$SE_PrevH <= 10, 'Please provide a valid SE for Hiv prevalence'),
          need(!(input$SE_PrevH == "" ), 'Please provide a value for SE for HIV prevalence'),
          need(input$PrevR >= 0, 'Please provide a valid value for recency among HIV prevalence'),
          need(input$PrevR <= 100, 'Please provide a valid recency among HIV prevalence'),
          need(!(input$PrevR == "" ), 'Please provide a value recency among HIV prevalence'),
          need(input$SE_PrevR >= 0, 'Please provide a valid SE for recency of HIV positives'),
          #need(input$SE_PrevR <= 10, 'Please provide a valid SE for recency of Hiv prevalence'),
          need(!(input$SE_PrevR == "" ), 'Please provide a value SE for recency of HIV prevalence'),
          need(!is.na(input$cor_PrevH_PrevR), "Please provide a valid correlation for PrevH and PrevR (default: 0)")
        )
        
        #browser()
        
        RSE_Prop_Pos <- input$SE_PrevH / input$PrevH
        Prop_Pos <- input$PrevH / 100
        RSE_Prop_R <- input$SE_PrevR / input$PrevR
        Prop_R <- input$PrevR / 100
        
        temp <- incprops(PrevH = Prop_Pos, 
                         RSE_PrevH = RSE_Prop_Pos,
                         PrevR = Prop_R, 
                         RSE_PrevR = RSE_Prop_R,
                         MDRI = MDRI, 
                         RSE_MDRI = RSE_MDRI,
                         FRR = FRR, 
                         RSE_FRR = RSE_FRR,
                         BigT = input$BigT,
                         Boot = TRUE,
                         BS_Count = input$n_bootstraps,
                         cor_HR = input$cor_PrevH_PrevR)
        
        inc_df <- dplyr::data_frame(
          `%HIV+` = round(input$PrevH, 3), #  * 100 / 100 cluge to make trailing zeros appear
          `%HIV+ SE` = round(input$SE_PrevH, 3),
          `%Inc` = round(temp$Incidence$Incidence * 100, 3),
          `%Inc SE` = round(temp$Incidence$RSE.I * temp$Incidence$Incidence * 100, 3),
          Corr = round(temp$Incidence$Cor.PrevH.I,3)
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
          need(!is.na(input$cor_PrevH_PrevR), "Please provide a valid correlation for PrevH and PrevR (default: 0)")
        )
        
        temp <- inccounts(N = input$N, 
                          N_H = input$N_H, 
                          N_testR = input$N_testR, 
                          N_R = input$N_R, 
                          DE_H = input$DE_H, 
                          DE_R = input$DE_R, 
                          MDRI = MDRI, 
                          RSE_MDRI = RSE_MDRI,
                          FRR = FRR, 
                          RSE_FRR = RSE_FRR,
                          BigT = input$BigT,
                          Boot = TRUE,
                          BS_Count = input$n_bootstraps,
                          Covar_HR = 0, 
                          cor_HR = input$cor_PrevH_PrevR)
        
        temp_prev <- prevcounts(N = input$N, 
                                N_H = input$N_H, 
                                N_testR = input$N_testR, 
                                N_R = input$N_R,
                                DE_H = input$DE_H, 
                                DE_R = input$DE_R)
        
        inc_df <- dplyr::data_frame(
          `%HIV+` = temp_prev$PrevH * 100,
          `%HIV+ SE` = temp_prev$RSE_PrevH * temp_prev$PrevH * 100,
          `%Inc` = round(temp$Incidence$Incidence * 100, 3),
          `%Inc SE` = round(temp$Incidence$RSE.I * temp$Incidence$Incidence * 100, 3),
          Corr = round(temp$Incidence$Cor.PrevH.I, 3)
        )
        
        return(inc_df)
      }
    }
  }) 
  
  
  read_data <- reactive({
    input_file_df <- readr::read_csv(input$input_file$datapath)
    return(input_file_df)
  })
  
  
  incidence_calc_multiple <- reactive({
    
    survey_data <- read_data()
    n_s <- nrow(survey_data) 
    
    inc_df <- dplyr::data_frame(
      Year = rep(NA,n_s),
      `%HIV+` = rep(NA,n_s),
      `%HIV+ SE` = rep(NA,n_s),
      `%Inc` = rep(NA,n_s),
      `%Inc SE` = rep(NA,n_s),
      Corr = rep(NA,n_s)
    )
    
    for (i in 1:n_s) {
      RSE_Prop_Pos <- survey_data$SE_Prevalence[i] / survey_data$Prevalence_percent[i]
      Prop_Pos <- survey_data$Prevalence_percent[i] / 100
      RSE_Prop_R <- survey_data$SE_PropRecent[i] / survey_data$PropRecent_percent[i]
      Prop_R <- survey_data$PropRecent_percent[i] / 100
      RSE_MDRI <- survey_data$SE_MDRI[i] / survey_data$MDRI_days[i]
      MDRI <- survey_data$MDRI_days[i]
      RSE_FRR  <- survey_data$SE_FRR[i] / survey_data$FRR_percent[i]
      FRR <- survey_data$FRR_percent[i] / 100
      
      temp <- incprops(PrevH = Prop_Pos, 
               RSE_PrevH = RSE_Prop_Pos,
               PrevR = Prop_R, 
               RSE_PrevR = RSE_Prop_R,
               MDRI = MDRI, 
               RSE_MDRI = RSE_MDRI,
               FRR = FRR, 
               RSE_FRR = RSE_FRR,
               BigT = survey_data$BigT[i],
               Boot = TRUE,
               BS_Count = 100000,
               cor_HR = survey_data$Corr_Prev_PropRecent[i])
      
      inc_df$Year[i] <- survey_data$Year[i]
      inc_df$`%HIV+`[i] <- survey_data$Prevalence_percent[i]
        inc_df$`%HIV+ SE`[i] <- survey_data$SE_Prevalence[i]
        inc_df$`%Inc`[i] <- round(temp$Incidence$Incidence * 100, 3)
        inc_df$`%Inc SE`[i] <- round(temp$Incidence$RSE.I * temp$Incidence$Incidence * 100, 3)
        inc_df$Corr[i] <- round(temp$Incidence$Cor.PrevH.I, 3)
    }
    
    return(inc_df)
    
  })
  
  
  
  output$incidence_table <- renderTable(digits = 3, {
    incidence_calc()
  })
  
  
  output$data_table <- renderTable(digits = 3, {
    if (!is.null(input$input_file)) {
      read_data()
    }
  })
  
  output$incidence_table_multiple <- renderTable(digits = 3, {
    if (!is.null(input$input_file)) {
    incidence_calc_multiple()
    }
  })
  
  output$incidence_table_download <- downloadHandler(
      filename = function() {
        paste('estimates-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        readr::write_csv(incidence_calc_multiple(), con)
      }
    )
  
})
