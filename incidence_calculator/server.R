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
source('incidence.R')

num <- as.numeric # improve code readibility

shinyServer(function(input, output){
  
  determine_tab <- reactive({
    active_tab <- input$tabs
    return(active_tab)
  })
  
  incidence_calc <- reactive({
    #browser()
    
    RSE_MDRI <- num(input$SE_MDRI) / num(input$MDRI)
    MDRI <- num(input$MDRI)
    
    FRR <- num(input$FRR) / 100
    RSE_FRR  <- ifelse(num(input$FRR) == 0, 0, num(input$SE_FRR) / num(input$FRR))
    FRR<- ifelse(FRR==0,0.0000000001,FRR) # cheating
    
    if (input$single_multiple == 1) {
      validate(
        need(num(input$SE_FRR) >= 0, 'Please provide a valid SE for FRR'),
        #need(num(input$SE_FRR) <= 100, 'Please provide a valid SE for FRR'),
        need(!(num(input$SE_FRR) == "" ), 'Please provide a value for SE_FRR'),
        need(num(input$SE_MDRI) >= 0, 'Please provide a valid SE for MDRI'),
        #need(num(input$SE_MDRI) <= 100, 'Please provide a valid SE for MDRI'),
        need(!(num(input$SE_MDRI) == "" ), 'Please provide a value for SE_MDRI'),
        need(num(input$MDRI) >= 0, 'Please provide a valid value for MDRI'),
        need(num(input$FRR) >= 0, 'Please provide a valid value for FRR'),
        need(num(input$FRR) <= 100, 'Please provide a valid value for FRR'),
        need(num(input$BigT), 'Please provide a value for the cut-off time'),
        need(num(input$BigT) > 120, 'Please provide a valid value for the cut-off time (>120)'),
        need(input$n_bootstraps >= 10000, "Bootstrapping iterations must be in the range [10,000,500,000]"),
        need(input$n_bootstraps <= 500000, "Bootstrapping iterations must be in the range [10,000,500,000]")
      )
      
      RSE_MDRI <- num(input$SE_MDRI) / num(input$MDRI)
      MDRI <- num(input$MDRI)
      
      FRR <- num(input$FRR) / 100
      RSE_FRR  <- ifelse(num(input$FRR) == 0, 0, num(input$SE_FRR) / num(input$FRR))
      FRR<- ifelse(FRR==0,0.0000000001,FRR) # cheating
      
      
      if (input$data_type == 1) {
        validate(
          need(num(input$PrevH) >= 0, 'Please provide a valid value for HIV prevalence'),
          need(num(input$PrevH) <= 100, 'Please provide a valid HIV prevalence'),
          need(!(num(input$PrevH) == "" ), 'Please provide a value HIV prevalence'),
          need(num(input$SE_PrevH) >= 0, 'Please provide a valid SE for HIV prevalence'),
          #need(num(input$SE_PrevH) <= 10, 'Please provide a valid SE for Hiv prevalence'),
          need(!(num(input$SE_PrevH) == "" ), 'Please provide a value for SE for HIV prevalence'),
          need(num(input$PrevR) >= 0, 'Please provide a valid value for recency among HIV prevalence'),
          need(num(input$PrevR) <= 100, 'Please provide a valid recency among HIV prevalence'),
          need(!(num(input$PrevR) == "" ), 'Please provide a value recency among HIV prevalence'),
          need(num(input$SE_PrevR) >= 0, 'Please provide a valid SE for recency of HIV positives'),
          #need(num(input$SE_PrevR) <= 10, 'Please provide a valid SE for recency of Hiv prevalence'),
          need(!(num(input$SE_PrevR) == "" ), 'Please provide a value SE for recency of HIV prevalence'),
          need(!is.na(num(input$cor_PrevH_PrevR)), "Please provide a valid correlation for PrevH and PrevR (default: 0)")
        )
        
        #browser()
        
        RSE_Prop_Pos <- num(input$SE_PrevH) / num(input$PrevH)
        Prop_Pos <- num(input$PrevH) / 100
        RSE_Prop_R <- num(input$SE_PrevR) / num(input$PrevR)
        Prop_R <- num(input$PrevR) / 100
        
        temp <- incprops(PrevH = Prop_Pos, 
                         RSE_PrevH = RSE_Prop_Pos,
                         PrevR = Prop_R, 
                         RSE_PrevR = RSE_Prop_R,
                         MDRI = MDRI, 
                         RSE_MDRI = RSE_MDRI,
                         FRR = FRR, 
                         RSE_FRR = RSE_FRR,
                         BigT = num(input$BigT),
                         Boot = TRUE,
                         BS_Count = input$n_bootstraps,
                         cor_HR = num(input$cor_PrevH_PrevR))
        
        inc_df <- dplyr::data_frame(
          `Prev (%)` = round(num(input$PrevH), 3), #  * 100 / 100 cluge to make trailing zeros appear
          `Prev SE` = round(num(input$SE_PrevH), 3),
          `Inc (%)*` = round(temp$Incidence$Incidence * 100, 3),
          `Inc SE` = round(temp$Incidence$RSE.I * temp$Incidence$Incidence * 100, 3),
          Corr = round(temp$Incidence$Cor.PrevH.I,3)
        )
        
        return(inc_df)
        
      } else if (input$data_type == 2) {
        validate(
          need(num(input$N)>0,"Please enter a valid total population sample size"),
          need(num(input$N)>=num(input$N_H),"HIV-positive subjects should be less than total sample size"),
          need(num(input$N_H)>=num(input$N_testR),"HIV-positive subjects tested for recency should be less than or equal to HIV-positive subjects among total sample size"),
          need(num(input$N_testR)>=num(input$N_R),"The number of recent HIV cases should be less than or equal to HIV-positive subjects tested for recency"),
          need(num(input$DE_H) >= 1, "Design effect on HIV prevalence must be >= 1"),
          need(num(input$DE_R) >= 1, "Design effect on Prop. recent | + must be >= 1")
          #need(!is.na(num(input$cor_PrevH_PrevR)), "Please provide a valid correlation for PrevH and PrevR (default: 0)")
        )
        
        temp <- inccounts(N = num(input$N), 
                          N_H = num(input$N_H), 
                          N_testR = num(input$N_testR), 
                          N_R = num(input$N_R), 
                          DE_H = num(input$DE_H), 
                          DE_R = num(input$DE_R), 
                          MDRI = MDRI, 
                          RSE_MDRI = RSE_MDRI,
                          FRR = FRR, 
                          RSE_FRR = RSE_FRR,
                          BigT = num(input$BigT),
                          Boot = TRUE,
                          BS_Count = input$n_bootstraps,
                          Covar_HR = 0, 
                          cor_HR = 0)
        
        temp_prev <- prevcounts(N = num(input$N), 
                                N_H = num(input$N_H), 
                                N_testR = num(input$N_testR), 
                                N_R = num(input$N_R),
                                DE_H = num(input$DE_H), 
                                DE_R = num(input$DE_R))
        
        inc_df <- dplyr::data_frame(
          `Prev (%)` = temp_prev$PrevH * 100,
          `Prev SE` = temp_prev$RSE_PrevH * temp_prev$PrevH * 100,
          `Inc (%)*` = round(temp$Incidence$Incidence * 100, 3),
          `Inc SE` = round(temp$Incidence$RSE.I * temp$Incidence$Incidence * 100, 3),
          Corr = round(temp$Incidence$Cor.PrevH.I, 3)
        )
        
        return(inc_df)
        
      } else if (input$data_type == 3) {
        #browser()
        
        prev <- (num(input$N_Re) + num(input$N_nonR) + num(input$N_notT)) / (num(input$N_Re) + num(input$N_nonR) + num(input$N_notT) + num(input$N_Neg))
        prevR <- num(input$N_Re) / (num(input$N_Re) + num(input$N_nonR))
        
        vcovmat <- matrix(nrow=4,ncol = 4)
        vcovmat[1,1] <- num(input$Var_N_R)
        vcovmat[2,2] <- num(input$Var_N_nonR)
        vcovmat[3,3] <- num(input$Var_N_notT)
        vcovmat[4,4] <- num(input$Var_N_Neg)
        vcovmat[2,1] <- vcovmat[1,2] <- num(input$Cov_R_NR)
        vcovmat[3,1] <- vcovmat[1,3] <- num(input$Cov_R_notT)
        vcovmat[4,1] <- vcovmat[1,4] <- num(input$Cov_R_Neg)
        vcovmat[2,3] <- vcovmat[3,2] <- num(input$Cov_NR_notT)
        vcovmat[3,4] <- vcovmat[4,3] <- num(input$Cov_NotT_Neg)
        vcovmat[4,2] <- vcovmat[2,4] <- num(input$Cov_NR_Neg)
        
        vars <- cbind(c(num(input$N_Neg), num(input$N_Neg), num(input$N_Neg), -(num(input$N_Re) + num(input$N_nonR) + num(input$N_notT))) / (num(input$N_Re) + num(input$N_nonR) + num(input$N_notT) + num(input$N_Neg))^2,
                      c(num(input$N_nonR), -num(input$N_Re), 0, 0) / (num(input$N_Re) + num(input$N_nonR))^2)
        
        propvars <- t(vars) %*% vcovmat %*% vars
        
        SEs <- sqrt(diag(propvars))
        
        corr <- cov2cor(propvars)[1,2]
        
        temp <- incprops(PrevH = prev, 
                         RSE_PrevH = SEs[1]/prev,
                         PrevR = prevR, 
                         RSE_PrevR = SEs[2]/prevR,
                         MDRI = MDRI, 
                         RSE_MDRI = RSE_MDRI,
                         FRR = FRR, 
                         RSE_FRR = RSE_FRR,
                         BigT = num(input$BigT),
                         Boot = TRUE,
                         BS_Count = input$n_bootstraps,
                         cor_HR = corr)
        
        inc_df <- dplyr::data_frame(
          `Prev (%)` = round(prev * 100, 3),
          `Prev SE` =  round(SEs[1] * 100, 3),
          `Inc (%)*` = round(temp$Incidence$Incidence * 100, 3),
          `Inc SE` = round(temp$Incidence$RSE.I * temp$Incidence$Incidence * 100, 3),
          Corr = round(temp$Incidence$Cor.PrevH.I, 3)
        )
        return(inc_df)
        
      } else if (input$data_type == 4) {
        prev <- num(input$P_Re)+num(input$P_nonR)+num(input$P_notT)
        prevR <- num(input$P_Re) / (num(input$P_Re) + num(input$P_nonR))
        
        vcovmat <- matrix(nrow=3, ncol = 3)
        vcovmat[1,1] <- num(input$Var_P_R)
        vcovmat[2,2] <- num(input$Var_P_nonR)
        vcovmat[3,3] <- num(input$Var_P_notT)
        vcovmat[2,1] <- vcovmat[1,2] <- num(input$Cov_R_NRp)
        vcovmat[3,1] <- vcovmat[1,3] <- num(input$Cov_R_notTp)
        vcovmat[2,3] <- vcovmat[3,2] <- num(input$Cov_NR_notTp)
        
        vars <- cbind(c(1, 1, 1),
                      c(num(input$P_nonR), -num(input$P_Re), 0) / (num(input$P_Re) + num(input$P_nonR))^2)
        
        propvars <- t(vars) %*% vcovmat %*% vars
        
        SEs <- sqrt(diag(propvars))
        
        corr <- cov2cor(propvars)[1,2]
        
        temp <- incprops(PrevH = prev, 
                         RSE_PrevH = SEs[1]/prev,
                         PrevR = prevR, 
                         RSE_PrevR = SEs[2]/prevR,
                         MDRI = MDRI, 
                         RSE_MDRI = RSE_MDRI,
                         FRR = FRR, 
                         RSE_FRR = RSE_FRR,
                         BigT = num(input$BigT),
                         Boot = TRUE,
                         BS_Count = input$n_bootstraps,
                         cor_HR = corr)
        
        inc_df <- dplyr::data_frame(
          `Prev (%)` = round(prev * 100, 3),
          `Prev SE` =  round(SEs[1] * 100, 3),
          `Inc (%)*` = round(temp$Incidence$Incidence * 100, 3),
          `Inc SE` = round(temp$Incidence$RSE.I * temp$Incidence$Incidence * 100, 3),
          Corr = round(temp$Incidence$Cor.PrevH.I, 3)
        )
        return(inc_df)
        
      } else if (input$data_type == 5) {
        
        prev <- num(input$PrevH)/100 
        prev_se <- num(input$SE_PrevH)/100
        inc <- num(input$Inc)/100
        inc_se <- num(input$SE_Inc)/100
        
        inc_df <- dplyr::data_frame(
          `Prev (%)` = round(num(input$PrevH),3),
          `Prev SE` = num(input$SE_PrevH),
          `Inc (%)*` = num(input$Inc),
          `Inc SE` = num(input$SE_Inc),
          Corr = inc / ((1-prev)*prev) * prev_se / inc_se
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
      `Prev (%)` = rep(NA,n_s),
      `Prev SE` = rep(NA,n_s),
      `Inc (%)` = rep(NA,n_s),
      `Inc SE` = rep(NA,n_s),
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
                       BS_Count = input$n_bootstraps,
                       cor_HR = survey_data$Corr_Prev_PropRecent[i])
      
      inc_df$Year[i] <- survey_data$Year[i]
      inc_df$`Prev (%)`[i] <- survey_data$Prevalence_percent[i]
      inc_df$`Prev SE`[i] <- survey_data$SE_Prevalence[i]
      inc_df$`Inc (%)`[i] <- round(temp$Incidence$Incidence * 100, 3)
      inc_df$`Inc SE`[i] <- round(temp$Incidence$RSE.I * temp$Incidence$Incidence * 100, 3)
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
