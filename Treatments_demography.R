
library(shiny)
library(ggplot2)
library(officer)
library(readxl)
library(cowplot)
library(scales)
library(shinyWidgets)
library(png)
library(tidyverse)
library(shinydashboard)
library(gridExtra)
library(plotly)

###If you want to try and get tooltips - change the lines that are commented out around the ###HERE###

#############################################################################
##Create CI function
#############################################################################

get_percentages_and_confidence_intervals = function(numerators, denominators){
    if (length(numerators) == 0){ #This address data incompleteness for surgery in the "Other" site
        estimate = 0 * denominators
        lcis = 0 * denominators
        ucis = 0 * denominators
    } else {
        estimate = numerators / denominators
        add_or_subtract = 1.96 * sqrt (( 1.96 ^ 2 + 4 * numerators * (1 - estimate)))
        other_num_bit = 2 * numerators + 1.96^2
        denom_bit = 2 * (denominators + (1.96^2))
        lcis = (other_num_bit + add_or_subtract)/denom_bit
        ucis = (other_num_bit - add_or_subtract)/denom_bit
        lcis = pmax(lcis, 0)
        ucis = pmin(ucis, 1)}
    return(list(ratio = estimate, lowers = lcis, uppers = ucis))
}

#############################################################################
##Import data 
#############################################################################

data<- read.csv("demography.csv", stringsAsFactors = TRUE)

### INPUT for years of diagnosis the app is covering
years<-c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")

#Set years levels so that combined years is always first
data$year<- factor(data$year, levels=c(paste0(min(years),"-",max(years)), years))

#############################################################################
# Define UI for application 
#############################################################################


ui <- fluidPage(
    
    # Application title
    #  titlePanel("Treatment Breakdown, 2013-2020"),
    
    titlePanel(
        fluidRow(
            column(9, paste0("Treatment Breakdown, ",min(years),"-",max(years))), 
            column(3, img(src='ndrs_cruk.png', align = "right", width=250, units="px" ))
        ), windowTitle = "Cancer Treatments"
    ),
    
    sidebarLayout(
        sidebarPanel(
            pickerInput(inputId = "cancer",
                        label = "Select Cancer Type",
                        choices = as.list(levels(data$cancergroup)),
                        # choices = as.list(factor(data2$cancergroup, levels = unique(data2$cancergroup))),
                        #factor(X, levels = unique(X)).
                        multiple = FALSE),
            #selected = as.list(levels(data$cancergroup))),
            pickerInput(inputId = "factor",
                        label = "Select Factor of Interest",
                        choices = as.list(levels(data$factor)),
                        multiple = FALSE),
            pickerInput(inputId = "year",
                        label = "Select Year of Interest",
                        choices = as.list(levels(data$year)),
                        multiple = FALSE),
            #selected = as.list(levels(data$factor))),
            strong(paste0("This website presents the numbers and percentages of tumours diagnosed in England in ",min(years),"-",max(years)," recorded as receiving radiotherapy, chemotherapy or tumour resection. The results are presented by cancer site, year of diagnosis, stage at diagnosis, age, gender, deprivation, ethnicity, and comorbidities.")),
            br(),
            br(),
            p("The methodology is described in the standard operating procedure ", a("CAS-SOP #4.8 Linking treatment tables - chemotherapy, tumour resections and radiotherapy.", href="https://digital.nhs.uk/ndrs/our-work/ncras-work-programme/treatment-data"), em("(Please note this link may not load in all browsers).")),
            p("Analytical notes:"),
            p("Other care represents the group of patients who had no record of chemotherapy, tumour resection, or radiotherapy in the time frame assessed. This may include patients who received other treatments (such as hormonal therapy or management of symptoms), treatment outside of the time frame assessed, treatment in a private setting, or there may be data missing from the datasets used."),
            p("The patient's Charlson comorbidity score was derived from Hospital Episodes Statistics (HES) and Cancer Registry data combined, and looks back at the time period between 27 months to 3 months before the patient's cancer diagnosis."),
            p("Tumour resections have been defined for 28 malignant sites and 4 benign sites."),
            br(),
            strong("Click below to download a copy of the data, and for TIFF outputs of the graphs:"),
            br(),
            downloadLink('downloadData4', 'Download Data for Overall Treatment Graph'),
            br(),
            downloadLink('downloadData', 'Download Data for Independent Treatments Graph'),
            br(),
            downloadLink('downloadData2', 'Download Data for Treatment Combination Graphs'),
            br(),
            downloadLink('downloadData3', 'Download Raw Data for all Cancers and all years'),
            br(),
            downloadLink('downloadGraph4', 'Export Overall Treatment Graph'), 
            br(),
            downloadLink('downloadGraph', 'Export Graph for Independent Treatments'),   
            br(),
            downloadLink('downloadGraph2', 'Export Graph for Stacked Treatment Combination'),
            br(),
            downloadLink('downloadGraph3', 'Export Graph for Treatment Combination'),
            br(),
            br(),
            em("This work has been produced as part of the Cancer Research UK - NHS England Partnership."),
            br(),
            em("This work uses data collected by the NHS, as part of the care and support of cancer patients.")
            
            
        ),
        
        mainPanel(
            #plotOutput("distPlot1"), 
            #plotOutput("distPlot2"),
            #plotOutput("distPlot3")
            
            ##    img(src='phe.png', align = "right", width=150, units="px" ),
            tabsetPanel(
                tabPanel("Overall", plotOutput("distPlot4")),
                tabPanel("Independent Treatment", plotOutput("distPlot1", width="100%")),
                #tabPanel("Stacked Treatment Combinations", plotlyOutput("distPlot2")),
                ###HERE###
                tabPanel("Stacked Treatment Combinations", plotOutput("distPlot2")),
                tabPanel("Treatment Combinations", plotOutput("distPlot3")),
                tabPanel("Information", htmlOutput("text"))
            )
            
        )
    )
)

#############################################################################
# Create functions to draw graphs 
#############################################################################


#####Function for overall graphs

shovegraphintofunction4 = function(input) {
    
    ##Firstly create independent treatment graph  
    
    # Create the subset of the data based on drop-downs of cancer type    
  
    tempdata = data[data$cancergroup == input$cancer,]
    tempdata = tempdata[tempdata$factor == "Gender",]
    tempdata = tempdata[tempdata$year == input$year,]
    
    tempdata$dummyvar <- 1
    
    # Aggregate the data to calculate the percentages and CIs  
    
    data_by_year = tempdata
    ##Need to subset the data again for some reason
    #data_by_year = data_by_year[data_by_year$cancergroup == input$cancer,]
    #data_by_year = data_by_year[data_by_year$factor == "Gender",]
    
    chemo_data = aggregate(tumour_count ~ dummyvar + ct_flag, data = data_by_year, FUN = 'sum', drop = FALSE)
    chemo_numerators = chemo_data[chemo_data$ct_flag == "Yes",]$tumour_count
    chemo_totals = aggregate(tumour_count ~ dummyvar, data = data_by_year, FUN = 'sum', drop = FALSE)
    chemo_denominators = chemo_totals$tumour_count
    chemo_frame = c(chemo_totals, get_percentages_and_confidence_intervals(chemo_numerators, chemo_denominators))
    chemo_frame$Treatment = "Chemotherapy"
    
    radio_data = aggregate(tumour_count ~ dummyvar + rt_flag, data = data_by_year, FUN = 'sum', drop = FALSE)
    radio_numerators = radio_data[radio_data$rt_flag == "Yes",]$tumour_count
    radio_totals = aggregate(tumour_count ~ dummyvar, data = data_by_year, FUN = 'sum', drop = FALSE)
    radio_denominators = radio_totals$tumour_count
    radio_frame = c(radio_totals, get_percentages_and_confidence_intervals(radio_numerators, radio_denominators))
    radio_frame$Treatment = "Radiotherapy"
    
        surgery_data = aggregate(tumour_count ~ dummyvar + sg_flag, data = data_by_year, FUN = 'sum', drop = FALSE)
        surgery_numerators = surgery_data[surgery_data$sg_flag == "Yes",]$tumour_count
        surgery_totals = aggregate(tumour_count ~dummyvar, data = data_by_year, FUN = 'sum', drop = FALSE)
        surgery_denominators = surgery_totals$tumour_count
        surgery_frame = c(surgery_totals, get_percentages_and_confidence_intervals(surgery_numerators, surgery_denominators))
        surgery_frame$Treatment = "Tumour resection"   

  if (input$cancer == "Other: Malignant neoplasms"| input$cancer == "Other: Non-malignant neoplasms"){
    plottingdata = rbind(as.data.frame(chemo_frame), as.data.frame(radio_frame))
    
  }
        else{
            
    plottingdata = rbind(as.data.frame(chemo_frame), as.data.frame(surgery_frame), as.data.frame(radio_frame))
        }

      #plottingdata$value = factor(plottingdata$value, levels = factor_levels)
        #plottingdata[is.na(plottingdata)] <- 0
    plottingdata[plottingdata$ratio == 0, c("lowers", "uppers")] = NA
    
    plottingdata$tempvar <- as.integer(plottingdata$ratio * 100)
    plottingdata$chemolab <- plottingdata[plottingdata$Treatment=="Chemotherapy",]$tempvar
    
    plottingdata$Treatment<- factor(plottingdata$Treatment, ordered = TRUE, levels = c("Chemotherapy", "Tumour resection", "Radiotherapy")) #Create ordered factor

    #Independent plot code (with a loop to fix the all cancers label):
    if (input$cancer == "Other: Malignant neoplasms") {
      plot1 = ggplot(plottingdata, aes(Treatment, ratio, fill = Treatment)) +
        geom_bar(stat = "identity", position = "dodge") + 
        geom_errorbar(position = position_dodge(0.9), aes(ymin = lowers, ymax = uppers), width = 0.3) + 
        labs(x = "Overall",title = paste(input$cancer,input$year,": Independent Treatments"), caption = "This work has been produced as part of the Cancer Research UK - NHS England Partnership") + 
        scale_y_continuous(labels = scales::percent, breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), name = "Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
        #scale_x_discrete(labels = NULL) + 
        scale_fill_manual(name = "", values = c("Chemotherapy" = "#92D050", "Tumour resection" = "#00B0F0", "Radiotherapy" = "#FFC000")) +
        theme_classic(base_size = 15) +
        theme(legend.position="bottom", legend.text = element_text(size = 12) ) +
        theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
        theme(legend.position="bottom") + 
        theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())+
        guides(fill=guide_legend(ncol=1, nrow=3))}
    
    else if (input$cancer == "Skin: BCC (experimental)" | input$cancer == "Skin: cSCC (experimental)") {
      plot1 = ggplot(plottingdata, aes(dummyvar, ratio, fill = Treatment)) +
        geom_bar(stat = "identity", position = "dodge") + 
        geom_errorbar(position = position_dodge(0.9), aes(ymin = lowers, ymax = uppers), width = 0.3) + 
        labs(x = "Overall",title = paste(input$cancer,input$year,": Independent Treatments"), caption = "This work has been produced as part of the Cancer Research UK - NHS England Partnership \n Figures for non-melanoma skin cancers are currently experimental and likely undercount surgical tumour resections") + 
        scale_y_continuous(labels = scales::percent, breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), name = "Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
       # scale_x_discrete(labels = NULL) + 
        scale_fill_manual(name = "", values = c("Chemotherapy" = "#92D050", "Tumour resection" = "#00B0F0", "Radiotherapy" = "#FFC000")) +
        theme_classic(base_size = 15) +
        theme(legend.position="bottom", legend.text = element_text(size = 12) ) +
        theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
        theme(legend.position="bottom") + 
        theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())+
        guides(fill=guide_legend(ncol=1, nrow=3))}
    
    else {
    
        plot1 = ggplot(plottingdata, aes(dummyvar, ratio, fill = Treatment)) +
            geom_bar(stat = "identity", position = "dodge") + 
            geom_errorbar(position = position_dodge(0.9), aes(ymin = lowers, ymax = uppers), width = 0.3) + 
            labs(x = "Overall",title = paste(input$cancer, "cancer ",input$year,": \nTreatments are presented independently"), caption = "This work has been produced as part of the Cancer Research UK - NHS England Partnership") + 
            scale_y_continuous(labels = scales::percent, breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), name = "Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
            #scale_x_discrete(labels = NULL) + 
            scale_fill_manual(name = "", values = c("Chemotherapy" = "#92D050", "Tumour resection" = "#00B0F0", "Radiotherapy" = "#FFC000")) +
            theme_classic(base_size = 15) +
            theme(legend.position="bottom", legend.text = element_text(size = 12) ) +
            theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
            theme(legend.position="bottom") + 
            theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())+
            guides(fill=guide_legend(ncol=1, nrow=3))
    }
    
    ##Secondly create stacked bar chart, using the same data as the base 
    
    #Data processing:
    

        treatment_interaction_data = data_by_year
        ##Need to subset the data again for some reason
        treatment_interaction_data = treatment_interaction_data[treatment_interaction_data$cancergroup == input$cancer,]
        treatment_interaction_data = treatment_interaction_data[treatment_interaction_data$factor == "Gender",]

    
    
    treatment_interaction_data$treatment_combinations = interaction(treatment_interaction_data$ct_flag, treatment_interaction_data$rt_flag, treatment_interaction_data$sg_flag)
    
    treatment_interaction_data$treatment_combinations = factor(treatment_interaction_data$treatment_combinations, levels = 
                                                                   c("No.No.No", "Yes.No.No", "No.No.Yes","No.Yes.No", "Yes.Yes.No", "Yes.No.Yes", "No.Yes.Yes", "Yes.Yes.Yes"))
    
    levels(treatment_interaction_data$treatment_combinations) = list(
        "Other care" = "No.No.No", "Chemotherapy only" = "Yes.No.No", "Tumour resection only" = "No.No.Yes", "Radiotherapy only" = "No.Yes.No",
        "Chemotherapy and radiotherapy" = "Yes.Yes.No", "Tumour resection and chemotherapy" = "Yes.No.Yes",
        "Tumour resection and radiotherapy" = "No.Yes.Yes", "Tumour resection, radiotherapy and chemotherapy" = "Yes.Yes.Yes")
    
    aggregate_denominators = aggregate(tumour_count ~ dummyvar, data = treatment_interaction_data, FUN = 'sum')
    
    colnames(aggregate_denominators)[colnames(aggregate_denominators) == "tumour_count"] = "denominator"
    
    treatment_interaction_data2 = aggregate(tumour_count ~ dummyvar + treatment_combinations , data = treatment_interaction_data, FUN = 'sum', drop = FALSE)
    
    treatment_interaction_data3 = merge(treatment_interaction_data2, aggregate_denominators)
    
    
    
    treatments_and_ratios = as.data.frame(c(treatment_interaction_data3, 
                                            get_percentages_and_confidence_intervals(treatment_interaction_data3$tumour_count, treatment_interaction_data3$denominator)))
    
    treatments_and_ratios = treatments_and_ratios[treatments_and_ratios$ratio > 0,]  
    
    # Stacked plot code(with a loop to fix the all cancers label):
    
    plot2 = ggplot(treatments_and_ratios, aes(dummyvar, ratio, fill = treatment_combinations)) +
        geom_bar(position = position_stack(reverse = TRUE),stat="identity", aes(fill = treatment_combinations)) +
        labs(x = "Overall", title = paste(input$cancer, "cancer ",input$year,": Treatments are presented in combinations as a stacked bar chart"), caption = "This work has been produced as part of the Cancer Research UK - NHS England Partnership") +
        scale_y_continuous(labels = scales::percent, name = " Proportion of tumours", limits = c(0,1)) +
        #scale_x_discrete(limits = treatments_and_ratios$value) +
        scale_fill_manual(name = "", values = c("Other care" = "#BFBFBF", "Chemotherapy only" = "#92D050",
                                                "Tumour resection only" = "#00B0F0", "Radiotherapy only" = "#FFC000",
                                                "Chemotherapy and radiotherapy" = "#FF33CC", "Tumour resection and chemotherapy" = "#002060",
                                                "Tumour resection and radiotherapy" = "#FFFF00", "Tumour resection, radiotherapy and chemotherapy" = "#7030A0")) +
        theme_classic(base_size = 15) +
        theme(legend.position="bottom", legend.text = element_text(size = 12) ) +
        theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
        guides(fill=guide_legend(reverse = TRUE), ncol=2) +
        theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())
    
    
    # Multiple treatments plot code:
    
    
    plot3 = ggplot(treatments_and_ratios, aes(dummyvar, ratio, fill = treatment_combinations)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(position = position_dodge(0.9), aes(ymin = lowers, ymax = uppers), width = 0.3) +
        
        labs(x = "Overall", title = "\nTreatments are presented in combinations") +
        scale_y_continuous(labels = scales::percent, name = "", limits = c(0, 1), breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1)) +
        scale_fill_manual(name = "", values = c("Other care" = "#BFBFBF", "Chemotherapy only" = "#92D050",
                                                "Tumour resection only" = "#00B0F0", "Radiotherapy only" = "#FFC000",
                                                "Chemotherapy and radiotherapy" = "#FF33CC", "Tumour resection and chemotherapy" = "#002060",
                                                "Tumour resection and radiotherapy" = "#FFFF00", "Tumour resection, radiotherapy and chemotherapy" = "#7030A0")) +
        theme_classic(base_size = 15) +
        theme(legend.position="bottom", legend.text = element_text(size = 12) ) +
        theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
        guides(fill=guide_legend(reverse = TRUE, ncol=2, nrow=4)) +
        theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())
   
    
     if (input$cancer=="Other: Malignant neoplasms"| input$cancer=="Other: Non-Malignant neoplasms"){
         
         output = grid.arrange(plot1, ncol=1, nrow=1)
     }
     else {
        output = grid.arrange(plot1,plot3, ncol=2, nrow=1)
    }
    return(output)
    ###HERE###
    #    ggplotly(output)
}


#####Function for independent treatments graph

shovegraphintofunction1 = function(input) {
    
    # Create the subset of the data based on drop-downs    
    
    tempdata = data[data$cancergroup == input$cancer,]
    tempdata = tempdata[tempdata$factor == input$factor,]
    tempdata = tempdata[tempdata$year == input$year,]
    
    
    # Re-order the levels of age and ethnicity    
    
    if (input$factor == "Age at cancer diagnosis") {
        tempdata$value <- factor(tempdata$value, levels=c("U50", "50-59","60-69","70-79","80+" ))
    }
    else if (input$factor == "Ethnic group") {
        tempdata$value <- factor(tempdata$value, levels=c("Asian", "Black", "White", "Other","Unknown"))
    } 
    else if (input$factor == "Charlson comorbidity index") {
        tempdata$value <- factor(tempdata$value, levels=c("0", "1", "2", "3+"))
    } 
    else if (input$factor == "Deprivation (Index of Multiple Deprivation)") {				
        tempdata$value <- factor(tempdata$value, levels=c("1 - most deprived", "2", "3", "4", "5 - least deprived"))
    } 
    else if (input$factor == "Gender") {
        tempdata$value <- factor(tempdata$value, levels=c("Male", "Female"))
    } 
    else if (input$factor == "Stage at cancer diagnosis") {
        tempdata$value <- factor(tempdata$value, levels=c("Stage 1", "Stage 2", "Stage 3", "Stage 4", "Unknown Stage"))
    } 
    else if (input$factor == "Year of cancer diagnosis") {
        tempdata$value <- factor(tempdata$value, levels=c(years))
    } 
    else {
        tempdata$value <- tempdata$value
    }
    
    # Aggregate the data to calculate the percentages and CIs  
    
    factor_levels = levels(tempdata$value)
    data_by_year = tempdata
    
    chemo_data = aggregate(tumour_count ~ value + ct_flag, data = data_by_year, FUN = 'sum', drop = FALSE)
    chemo_numerators = chemo_data[chemo_data$ct_flag == "Yes",]$tumour_count
    chemo_totals = aggregate(tumour_count ~ value, data = data_by_year, FUN = 'sum', drop = FALSE)
    chemo_denominators = chemo_totals$tumour_count
    chemo_frame = c(chemo_totals, get_percentages_and_confidence_intervals(chemo_numerators, chemo_denominators))
    chemo_frame$Treatment = "Chemotherapy"
    
    radio_data = aggregate(tumour_count ~ value + rt_flag, data = data_by_year, FUN = 'sum', drop = FALSE)
    radio_numerators = radio_data[radio_data$rt_flag == "Yes",]$tumour_count
    radio_totals = aggregate(tumour_count ~ value, data = data_by_year, FUN = 'sum', drop = FALSE)
    radio_denominators = radio_totals$tumour_count
    radio_frame = c(radio_totals, get_percentages_and_confidence_intervals(radio_numerators, radio_denominators))
    radio_frame$Treatment = "Radiotherapy"
    
        surgery_data = aggregate(tumour_count ~ value + sg_flag, data = data_by_year, FUN = 'sum', drop = FALSE)
        surgery_numerators = surgery_data[surgery_data$sg_flag == "Yes",]$tumour_count
        surgery_totals = aggregate(tumour_count ~value, data = data_by_year, FUN = 'sum', drop = FALSE)
        surgery_denominators = surgery_totals$tumour_count
        surgery_frame = c(surgery_totals, get_percentages_and_confidence_intervals(surgery_numerators, surgery_denominators))
        surgery_frame$Treatment = "Tumour resection" 

    plottingdata = rbind(as.data.frame(chemo_frame), as.data.frame(surgery_frame), as.data.frame(radio_frame))

    plottingdata$ratio[is.na(plottingdata$ratio)] <- 0
    plottingdata[plottingdata$ratio == 0, c("lowers", "uppers")] = NA
    plottingdata$Treatment<- factor(plottingdata$Treatment, ordered = TRUE, levels = c("Chemotherapy", "Tumour resection", "Radiotherapy")) #Create ordered factor
    
    #Filter years to only single year if years is selected
    if (input$factor == "Year of cancer diagnosis"&!(input$year==paste0(min(years),"-",max(years)))) {
      plottingdata = plottingdata[plottingdata$value == input$year,]
    }
      
    ##Put If Statements so you can manually over-ride the x-labels for Gender etc.
    
     if (input$factor == "Gender" & input$cancer %in% c("Uterine", "Cervical", "Ovary", "Vulva")) {
         output = ggplot(plottingdata, aes(value, ratio, fill = Treatment)) +
             geom_bar(stat = "identity", position = "dodge") +
             geom_errorbar(position = position_dodge(0.9), aes(ymin = lowers, ymax = uppers), width = 0.3) +
             labs(x = input$factor, title = paste(input$cancer, "cancer ",input$year,": Treatments are presented independently"), caption = "This work has been produced as part of the Cancer Research UK - NHS England Partnership") +
             scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = "Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
             scale_x_discrete(labels = c("Male", "Female")) +
             scale_fill_manual(name = "", values = c("Chemotherapy" = "#92D050", "Tumour resection" = "#00B0F0", "Radiotherapy" = "#FFC000")) +
             theme_classic(base_size = 15) +
             theme(legend.position="bottom", legend.text = element_text(size = 12) ) +
             theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
             theme(legend.position="bottom")
     }

     else if (input$factor == "Gender") {
         output = ggplot(plottingdata, aes(value, ratio, fill = Treatment)) +
             geom_bar(stat = "identity", position = "dodge") +
             geom_errorbar(position = position_dodge(0.9), aes(ymin = lowers, ymax = uppers), width = 0.3) +
             labs(x = input$factor, title = ifelse(input$cancer=="All defined malignant neoplasms"| input$cancer=="Other: Malignant neoplasms",paste(input$cancer,input$year,": Treatments are presented independently"), paste(input$cancer, "cancer ",input$year,": Treatments are presented independently")), 
                  caption = ifelse(input$cancer %in% c("Skin: BCC (experimental)", "Skin: cSCC (experimental)"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership \n Figures for non-melanoma skin cancers are currently experimental and likely undercount surgical tumour resections"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership"))) +
             scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = "Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
             scale_x_discrete(labels = c("Male", "Female")) +
             scale_fill_manual(name = "", values = c("Chemotherapy" = "#92D050", "Tumour resection" = "#00B0F0", "Radiotherapy" = "#FFC000")) +
             theme_classic(base_size = 15) +
             theme(legend.position="bottom", legend.text = element_text(size = 12) ) +
             theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
             theme(legend.position="bottom")
         
     }
   # else if (input$factor == "Charlson comorbidity index" & input$cancer %in% c("All non-malignant neoplasms", "Benign Endocrine", "Brain: Non-malignant", "Other: Non-malignant neoplasms", "Skin: BCC (experimental)", "Skin: cSCC (experimental)", "Skin: Rare")) {
   #      output = ggplot(plottingdata, aes(value, ratio, fill = Treatment)) +
   #          labs(x = "", title = "Independent treatments cannot be produced for this cancer site", caption = "This work has been produced as part of the Cancer Research UK - NHS England Partnership") +
   #                   scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = " Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
   #                   theme_classic(base_size = 15) +
   #                   scale_x_discrete(labels = NULL) +
   #                   geom_blank()
   #  }
    
    
     else if (input$factor == "Charlson comorbidity index") {
         output = ggplot(plottingdata, aes(value, ratio, fill = Treatment)) +
             geom_bar(stat = "identity", position = "dodge") +
             geom_errorbar(position = position_dodge(0.9), aes(ymin = lowers, ymax = uppers), width = 0.3) +
             labs(x = input$factor, title = ifelse(input$cancer=="All defined malignant neoplasms"| input$cancer=="Other: Malignant neoplasms",paste(input$cancer,input$year,": Treatments are presented independently"), paste(input$cancer, "cancer ",input$year,": Treatments are presented independently")), 
                  caption = ifelse(input$cancer %in% c("Skin: BCC (experimental)", "Skin: cSCC (experimental)"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership \n Figures for non-melanoma skin cancers are currently experimental and likely undercount surgical tumour resections"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership"))) + 
             scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = "Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
             scale_x_discrete(labels = c("0", "1", "2", "3+", "Unknown")) +
             scale_fill_manual(name = "", values = c("Chemotherapy" = "#92D050", "Tumour resection" = "#00B0F0", "Radiotherapy" = "#FFC000")) +
             theme_classic(base_size = 15) +
             theme(legend.position="bottom", legend.text = element_text(size = 12) ) +
             theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
             theme(legend.position="bottom")
     }

     else if (input$factor == "Stage at cancer diagnosis") {
         output = ggplot(plottingdata, aes(value, ratio, fill = Treatment)) +
             geom_bar(stat = "identity", position = "dodge") + 
             geom_errorbar(position = position_dodge(0.9), aes(ymin = lowers, ymax = uppers), width = 0.3) + 
             labs(x = input$factor, title = ifelse(input$cancer=="All defined malignant neoplasms"| input$cancer=="Other: Malignant neoplasms",paste(input$cancer,input$year,": Treatments are presented independently"), paste(input$cancer, "cancer ",input$year,": Treatments are presented independently")), 
                  caption = ifelse(input$cancer %in% c("Skin: BCC (experimental)", "Skin: cSCC (experimental)"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership \n Figures for non-melanoma skin cancers are currently experimental and likely undercount surgical tumour resections"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership"))) + 
             scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = "Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
             scale_x_discrete(labels = c("Stage 1", "Stage 2", "Stage 3", "Stage 4", "Unknown Stage")) + 
             scale_fill_manual(name = "", values = c("Chemotherapy" = "#92D050", "Tumour resection" = "#00B0F0", "Radiotherapy" = "#FFC000")) +
             theme_classic(base_size = 15) +
             theme(legend.position="bottom", legend.text = element_text(size = 12) ) +
             theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
             theme(legend.position="bottom")    
     }
    
     else {
        output = ggplot(plottingdata, aes(value, ratio, fill = Treatment)) +
            geom_bar(stat = "identity", position = "dodge") + 
            geom_errorbar(position = position_dodge(0.9), aes(ymin = lowers, ymax = uppers), width = 0.3) + 
            labs(x = input$factor, title = ifelse(input$cancer=="All defined malignant neoplasms"| input$cancer=="Other: Malignant neoplasms",paste(input$cancer,input$year,": Treatments are presented independently"), paste(input$cancer, "cancer ",input$year,": Treatments are presented independently")), caption = ifelse(input$cancer %in% c("Skin: BCC (experimental)", "Skin: cSCC (experimental)"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership \n Figures for non-melanoma skin cancers are currently experimental and likely undercount surgical tumour resections"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership"))) + 
            scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = "Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
            scale_x_discrete(labels = plottingdata$value) + 
            scale_fill_manual(name = "", values = c("Chemotherapy" = "#92D050", "Tumour resection" = "#00B0F0", "Radiotherapy" = "#FFC000")) +
            theme_classic(base_size = 15) +
            theme(legend.position="bottom", legend.text = element_text(size = 12) ) +
            theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
            theme(legend.position="bottom")
     }

    
    return(output)
}


#####Function for stacked treatment combinations graph

shovegraphintofunction2 = function(input) {
    
    # Create the subset of the data based on drop-downs  
    
    tempdata = data[data$cancergroup == input$cancer,]
    tempdata = tempdata[tempdata$factor == input$factor,]
    tempdata = tempdata[tempdata$year == input$year,]
    
    # Re-order the levels of age and ethnicity    
    
    if (input$factor == "Age at cancer diagnosis") {
        tempdata$value <- factor(tempdata$value, levels=c("U50", "50-59","60-69","70-79","80+" ))
    }
    else if (input$factor == "Ethnic group") {
        tempdata$value <- factor(tempdata$value,  levels=c("Asian", "Black", "White", "Other","Unknown"))
    }
    else if (input$factor == "Charlson comorbidity index") {
            tempdata$value <- factor(tempdata$value, levels=c("0", "1", "2", "3+"))    
    }     
    else if (input$factor == "Gender"){
      tempdata$value <- factor(tempdata$value, levels=c("Male", "Female"))
    } 
    else {
        tempdata$value <- tempdata$value
    }
    
    factor_levels = levels(tempdata$value)
    
    # Aggregate the data to calculate the percentages and CIs  
    data_by_year = tempdata
    
    
    ## Create the combinations variable
    
        treatment_interaction_data = data_by_year
    
    treatment_interaction_data$treatment_combinations = interaction(treatment_interaction_data$ct_flag, treatment_interaction_data$rt_flag, treatment_interaction_data$sg_flag)
    
    treatment_interaction_data$treatment_combinations = factor(treatment_interaction_data$treatment_combinations, levels = 
                                                                   c("No.No.No", "Yes.No.No", "No.No.Yes","No.Yes.No", "Yes.Yes.No", "Yes.No.Yes", "No.Yes.Yes", "Yes.Yes.Yes"))
    
    levels(treatment_interaction_data$treatment_combinations) = list(
        "Other care" = "No.No.No", "Chemotherapy only" = "Yes.No.No", "Tumour resection only" = "No.No.Yes", "Radiotherapy only" = "No.Yes.No",
        "Chemotherapy and radiotherapy" = "Yes.Yes.No", "Tumour resection and chemotherapy" = "Yes.No.Yes",
        "Tumour resection and radiotherapy" = "No.Yes.Yes", "Tumour resection, radiotherapy and chemotherapy" = "Yes.Yes.Yes")
    
    aggregate_denominators = aggregate(tumour_count ~ value, data = treatment_interaction_data, FUN = 'sum')
    
    colnames(aggregate_denominators)[colnames(aggregate_denominators) == "tumour_count"] = "denominator"
    treatment_interaction_data = merge(treatment_interaction_data, aggregate_denominators)
    treatments_and_ratios = as.data.frame(c(treatment_interaction_data, 
                                            get_percentages_and_confidence_intervals(treatment_interaction_data$tumour_count, treatment_interaction_data$denominator)))
    
    ##Put If Statements so you can manually over-ride the x-labels for Gender etc.
    
    if (input$cancer == "Other: Malignant neoplasms"| input$cancer == "Other: Non-malignant neoplasms") {
        
        stacked_treatments_plot = ggplot(treatments_and_ratios, aes(value, ratio, fill = treatment_combinations)) +
            labs(x = "", title = "Treatment combinations cannot be produced for this cancer site", caption = "This work has been produced as part of the Cancer Research UK - NHS England Partnership") +
            scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = " Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
            theme_classic(base_size = 15) +
            scale_x_discrete(labels = NULL) + 
            geom_blank()
    }
    
    else if (input$factor == "Gender" & input$cancer %in% c("Uterine", "Cervical", "Ovary", "Vulva")) {
        treatments_and_ratios = treatments_and_ratios[treatments_and_ratios$ratio > 0,]  
        stacked_treatments_plot = ggplot(treatments_and_ratios, aes(value, ratio, fill = treatment_combinations)) +
            geom_bar(position = position_stack(reverse = TRUE),stat="identity", aes(fill = treatment_combinations)) +
            labs(x = input$factor, title = paste(input$cancer, "cancer ",input$year,": Treatments are presented in combinations as a stacked bar chart"), caption = "This work has been produced as part of the Cancer Research UK - NHS England Partnership") +
            scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = " Proportion of tumours", limits = c(0, 1)) +
            scale_x_discrete(labels = c("Male", "Female")) + 
            scale_fill_manual(name = "", values = c("Other care" = "#BFBFBF", "Chemotherapy only" = "#92D050",
                                                    "Tumour resection only" = "#00B0F0", "Radiotherapy only" = "#FFC000",
                                                    "Chemotherapy and radiotherapy" = "#FF33CC", "Tumour resection and chemotherapy" = "#002060",
                                                    "Tumour resection and radiotherapy" = "#FFFF00", "Tumour resection, radiotherapy and chemotherapy" = "#7030A0")) +
            theme_classic(base_size = 15) +
            theme(legend.position="right", legend.text = element_text(size = 12) ) +
            theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
            guides(fill=guide_legend(reverse = TRUE))   
    }
    
    else if (input$factor == "Gender") {
        treatments_and_ratios = treatments_and_ratios[treatments_and_ratios$ratio > 0,]  
        stacked_treatments_plot = ggplot(treatments_and_ratios, aes(value, ratio, fill = treatment_combinations)) +
            geom_bar(position = position_stack(reverse = TRUE),stat="identity", aes(fill = treatment_combinations)) +
            labs(x = input$factor, title = ifelse(input$cancer=="All defined malignant neoplasms"| input$cancer=="Other: Malignant neoplasms",paste(input$cancer," ",input$year,": Treatments are presented in combinations as a stacked bar chart"), paste(input$cancer, "cancer ",input$year,": Treatments are presented in combinations as a stacked bar chart")), 
                 caption = ifelse(input$cancer %in% c("Skin: BCC (experimental)", "Skin: cSCC (experimental)"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership \n Figures for non-melanoma skin cancers are currently experimental and likely undercount surgical tumour resections"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership")))+
            scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = " Proportion of tumours", limits = c(0, 1)) +
            scale_x_discrete(labels = c("Male","Female")) + 
            scale_fill_manual(name = "", values = c("Other care" = "#BFBFBF", "Chemotherapy only" = "#92D050",
                                                    "Tumour resection only" = "#00B0F0", "Radiotherapy only" = "#FFC000",
                                                    "Chemotherapy and radiotherapy" = "#FF33CC", "Tumour resection and chemotherapy" = "#002060",
                                                    "Tumour resection and radiotherapy" = "#FFFF00", "Tumour resection, radiotherapy and chemotherapy" = "#7030A0")) +
            theme_classic(base_size = 15) +
            theme(legend.position="right", legend.text = element_text(size = 12) ) +
            theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
            guides(fill=guide_legend(reverse = TRUE))  
        
    }
  #   else if (input$factor == "Charlson comorbidity index" & input$cancer %in% c("All non-malignant neoplasms", "Benign Endocrine", "Brain: Non-malignant", "Other: Non-malignant neoplasms", "Skin: BCC (experimental)", "Skin: cSCC (experimental)", "Skin: Rare")) {
  #       stacked_treatments_plot = ggplot(treatments_and_ratios, aes(value, ratio, fill = treatment_combinations)) +
  #           labs(x = "", title = "Treatment combinations cannot be produced for this cancer site", caption = "This work has been produced as part of the Cancer Research UK - NHS England Partnership") +
  #           scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = " Proportion of tumours", limits = c(0, 1)) +
  #           scale_x_discrete(labels = NULL) + 
  #           theme_classic(base_size = 15)
  #           geom_blank()
  # }

    else if (input$factor == "Charlson comorbidity index") {
        treatments_and_ratios = treatments_and_ratios[treatments_and_ratios$ratio > 0,]  
        stacked_treatments_plot = ggplot(treatments_and_ratios, aes(value, ratio, fill = treatment_combinations)) +
            geom_bar(position = position_stack(reverse = TRUE),stat="identity", aes(fill = treatment_combinations)) +
            labs(x = input$factor, title = ifelse(input$cancer=="All defined malignant neoplasms"| input$cancer=="Other: Malignant neoplasms",paste(input$cancer,input$year,": Treatments are presented in combinations as a stacked bar chart"), paste(input$cancer, "cancer ",input$year,": Treatments are presented in combinations as a stacked bar chart")), 
                 caption = ifelse(input$cancer %in% c("Skin: BCC (experimental)", "Skin: cSCC (experimental)"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership \n Figures for non-melanoma skin cancers are currently experimental and likely undercount surgical tumour resections"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership"))) + 
            scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = " Proportion of tumours", limits = c(0, 1)) +
            scale_x_discrete(labels = c("0", "1", "2", "3+", "Unknown")) + 
            scale_fill_manual(name = "", values = c("Other care" = "#BFBFBF", "Chemotherapy only" = "#92D050",
                                                    "Tumour resection only" = "#00B0F0", "Radiotherapy only" = "#FFC000",
                                                    "Chemotherapy and radiotherapy" = "#FF33CC", "Tumour resection and chemotherapy" = "#002060",
                                                    "Tumour resection and radiotherapy" = "#FFFF00", "Tumour resection, radiotherapy and chemotherapy" = "#7030A0")) +
            theme_classic(base_size = 15) +
            theme(legend.position="right", legend.text = element_text(size = 12) ) +
            theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
            guides(fill=guide_legend(reverse = TRUE)) 
    }
    else if (input$factor == "Stage at cancer diagnosis") {  
        treatments_and_ratios = treatments_and_ratios[treatments_and_ratios$ratio > 0,]  
        stacked_treatments_plot = ggplot(treatments_and_ratios, aes(value, ratio, fill = treatment_combinations)) +
            geom_bar(position = position_stack(reverse = TRUE),stat="identity", aes(fill = treatment_combinations)) +
            labs(x = input$factor, title = ifelse(input$cancer=="All defined malignant neoplasms"| input$cancer=="Other: Malignant neoplasms",paste(input$cancer,input$year,": Treatments are presented in combinations as a stacked bar chart"), paste(input$cancer, "cancer ",input$year,": Treatments are presented in combinations as a stacked bar chart")), 
                 caption = ifelse(input$cancer %in% c("Skin: BCC (experimental)", "Skin: cSCC (experimental)"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership \n Figures for non-melanoma skin cancers are currently experimental and likely undercount surgical tumour resections"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership")))+
            scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = " Proportion of tumours", limits = c(0, 1)) +
            scale_x_discrete(labels = c("Stage 1", "Stage 2", "Stage 3", "Stage 4", "Unknown Stage")) + 
            scale_fill_manual(name = "", values = c("Other care" = "#BFBFBF", "Chemotherapy only" = "#92D050",
                                                    "Tumour resection only" = "#00B0F0", "Radiotherapy only" = "#FFC000",
                                                    "Chemotherapy and radiotherapy" = "#FF33CC", "Tumour resection and chemotherapy" = "#002060",
                                                    "Tumour resection and radiotherapy" = "#FFFF00", "Tumour resection, radiotherapy and chemotherapy" = "#7030A0")) +
            theme_classic(base_size = 15) +
            theme(legend.position="right", legend.text = element_text(size = 12) ) +
            theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
            guides(fill=guide_legend(reverse = TRUE)) 
    } 
    else {
        treatments_and_ratios = treatments_and_ratios[treatments_and_ratios$ratio > 0,]  
        stacked_treatments_plot = ggplot(treatments_and_ratios, aes(value, ratio, fill = treatment_combinations)) +
            geom_bar(position = position_stack(reverse = TRUE),stat="identity", aes(fill = treatment_combinations)) +
            labs(x = input$factor, title = ifelse(input$cancer=="All defined malignant neoplasms" | input$cancer=="Other: Malignant neoplasms",paste(input$cancer,input$year,": Treatments are presented in combinations as a stacked bar chart"), paste(input$cancer, "cancer ",input$year,": Treatments are presented in combinations as a stacked bar chart")), 
                 caption = ifelse(input$cancer %in% c("Skin: BCC (experimental)", "Skin: cSCC (experimental)"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership \n Figures for non-melanoma skin cancers are currently experimental and likely undercount surgical tumour resections"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership"))) +
            scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = " Proportion of tumours", limits = c(0, 1)) +
            #scale_x_discrete(limits = treatments_and_ratios$value) +
            scale_fill_manual(name = "", values = c("Other care" = "#BFBFBF", "Chemotherapy only" = "#92D050",
                                                    "Tumour resection only" = "#00B0F0", "Radiotherapy only" = "#FFC000",
                                                    "Chemotherapy and radiotherapy" = "#FF33CC", "Tumour resection and chemotherapy" = "#002060",
                                                    "Tumour resection and radiotherapy" = "#FFFF00", "Tumour resection, radiotherapy and chemotherapy" = "#7030A0")) +
            theme_classic(base_size = 15) +
            theme(legend.position="right", legend.text = element_text(size = 12) ) +
            theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
            guides(fill=guide_legend(reverse = TRUE))     
    }  
    
    
    return(stacked_treatments_plot)
}


#####Function for treatment combinations graph 

shovegraphintofunction3 = function(input) {
    
    # Create the subset of the data based on drop-downs  
    
    tempdata = data[data$cancergroup == input$cancer,]
    tempdata = tempdata[tempdata$factor == input$factor,]
    tempdata = tempdata[tempdata$year == input$year,]
    
    # Re-order the levels of age and ethnicity    
    
    if (input$factor == "Age at cancer diagnosis") {
        tempdata$value <- factor(tempdata$value, levels=c("U50", "50-59","60-69","70-79","80+" ))
    }
    else if (input$factor == "Ethnic group") {
        tempdata$value <- factor(tempdata$value,  levels=c("Asian", "Black", "White", "Other","Unknown"))
    } 
    else if (input$factor == "Charlson comorbidity index"){
        tempdata$value <- factor(tempdata$value, levels = c("0", "1", "2", "3+"))
    }
    else if (input$factor == "Gender"){
      tempdata$value <- factor(tempdata$value, levels=c("Male", "Female"))
    }
    else {
        tempdata$value <- tempdata$value
    }
    
    factor_levels = levels(tempdata$value)
    
    # Aggregate the data to calculate the percentages and CIs  
    data_by_year = tempdata
    
    
    ## Create the combinations variable

    treatment_interaction_data = data_by_year
    
    treatment_interaction_data$treatment_combinations = interaction(treatment_interaction_data$ct_flag, treatment_interaction_data$rt_flag, treatment_interaction_data$sg_flag)
    
    treatment_interaction_data$treatment_combinations = factor(treatment_interaction_data$treatment_combinations, levels = 
                                                                   c("No.No.No", "Yes.No.No", "No.No.Yes","No.Yes.No", "Yes.Yes.No", "Yes.No.Yes", "No.Yes.Yes", "Yes.Yes.Yes"))
    
    levels(treatment_interaction_data$treatment_combinations) = list(
        "Other care" = "No.No.No", "Chemotherapy only" = "Yes.No.No", "Tumour resection only" = "No.No.Yes", "Radiotherapy only" = "No.Yes.No",
        "Chemotherapy and radiotherapy" = "Yes.Yes.No", "Tumour resection and chemotherapy" = "Yes.No.Yes",
        "Tumour resection and radiotherapy" = "No.Yes.Yes", "Tumour resection, radiotherapy and chemotherapy" = "Yes.Yes.Yes")
    
    aggregate_denominators = aggregate(tumour_count ~ value, data = treatment_interaction_data, FUN = 'sum')
    
    colnames(aggregate_denominators)[colnames(aggregate_denominators) == "tumour_count"] = "denominator"
    treatment_interaction_data = merge(treatment_interaction_data, aggregate_denominators)
    treatments_and_ratios = as.data.frame(c(treatment_interaction_data, 
                                            get_percentages_and_confidence_intervals(treatment_interaction_data$tumour_count, treatment_interaction_data$denominator)))
    treatments_and_ratios = treatments_and_ratios[treatments_and_ratios$ratio > 0,]  
    
    
    ##Put If Statements so you can manually over-ride the x-labels for Gender etc. 
    
    
    if (input$cancer == "Other: Malignant neoplasms"| input$cancer == "Other: Non-malignant neoplasms") {
        
        multiple_treatments_plot = ggplot(treatments_and_ratios, aes(value, ratio, fill = treatment_combinations)) +
            labs(x = "", title = "Treatment combinations cannot be produced for this cancer site", caption = "This work has been produced as part of the Cancer Research UK - NHS England Partnership") +
            scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = " Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
            theme_classic(base_size = 15) +
            scale_x_discrete(labels = NULL) + 
            geom_blank()
    }
    else if (input$factor == "Gender" & input$cancer %in% c("Uterine", "Cervical", "Ovary", "Vulva")) {
        
        multiple_treatments_plot = ggplot(treatments_and_ratios, aes(value, ratio, fill = treatment_combinations)) +
            geom_bar(stat = "identity", position = "dodge") +
            geom_errorbar(position = position_dodge(0.9), aes(ymin = lowers, ymax = uppers), width = 0.3) +
            
            labs(x = input$factor, title = paste(input$cancer, "cancer ",input$year,": Treatments are presented in combinations"), caption = "This work has been produced as part of the Cancer Research UK - NHS England Partnership") +
            scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = " Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
            scale_fill_manual(name = "", values = c("Other care" = "#BFBFBF", "Chemotherapy only" = "#92D050",
                                                    "Tumour resection only" = "#00B0F0", "Radiotherapy only" = "#FFC000",
                                                    "Chemotherapy and radiotherapy" = "#FF33CC", "Tumour resection and chemotherapy" = "#002060",
                                                    "Tumour resection and radiotherapy" = "#FFFF00", "Tumour resection, radiotherapy and chemotherapy" = "#7030A0")) +
            scale_x_discrete(labels = c("Male","Female")) + 
            theme_classic(base_size = 15) +
            theme(legend.position="right", legend.text = element_text(size = 12) ) +
            theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
            guides(fill=guide_legend(reverse = TRUE))  
        
    }
    
    else if (input$factor == "Gender") {
        
        multiple_treatments_plot = ggplot(treatments_and_ratios, aes(value, ratio, fill = treatment_combinations)) +
            geom_bar(stat = "identity", position = "dodge") +
            geom_errorbar(position = position_dodge(0.9), aes(ymin = lowers, ymax = uppers), width = 0.3) +
            
            labs(x = input$factor, title = ifelse(input$cancer=="All defined malignant neoplasms"| input$cancer=="Other: Malignant neoplasms",paste(input$cancer,input$year,": Treatments are presented in combinations"),paste(input$cancer, "cancer ",input$year,": Treatments are presented in combinations")), 
                 caption = ifelse(input$cancer %in% c("Skin: BCC (experimental)", "Skin: cSCC (experimental)"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership \n Figures for non-melanoma skin cancers are currently experimental and likely undercount surgical tumour resections"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership")))+
            scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = " Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
            scale_fill_manual(name = "", values = c("Other care" = "#BFBFBF", "Chemotherapy only" = "#92D050",
                                                    "Tumour resection only" = "#00B0F0", "Radiotherapy only" = "#FFC000",
                                                    "Chemotherapy and radiotherapy" = "#FF33CC", "Tumour resection and chemotherapy" = "#002060",
                                                    "Tumour resection and radiotherapy" = "#FFFF00", "Tumour resection, radiotherapy and chemotherapy" = "#7030A0")) +
            scale_x_discrete(labels = c("Male", "Female")) + 
            theme_classic(base_size = 15) +
            theme(legend.position="right", legend.text = element_text(size = 12) ) +
            theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
            guides(fill=guide_legend(reverse = TRUE)) 
        
    }
    # else if (input$factor == "Charlson comorbidity index"& input$cancer %in% c("All non-malignant neoplasms", "Benign Endocrine", "Brain: Non-malignant", "Other: Non-malignant neoplasms", "Skin: BCC (experimental)", "Skin: cSCC (experimental)", "Skin: Rare")) {
    #     multiple_treatments_plot = ggplot(treatments_and_ratios, aes(value, ratio, fill = treatment_combinations)) +
    #         labs(x = "", title = "Treatment combinations cannot be produced for this cancer site", caption = "This work has been produced as part of the Cancer Research UK - NHS England Partnership") +
    #         scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = " Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
    #         theme_classic(base_size = 15) +
    #         scale_x_discrete(labels = NULL) + 
    #         geom_blank()
    # }
    else if (input$factor == "Charlson comorbidity index") {
        multiple_treatments_plot = ggplot(treatments_and_ratios, aes(value, ratio, fill = treatment_combinations)) +
            geom_bar(stat = "identity", position = "dodge") +
            geom_errorbar(position = position_dodge(0.9), aes(ymin = lowers, ymax = uppers), width = 0.3) +
            
            labs(x = input$factor, title = ifelse(input$cancer=="All defined malignant neoplasms"| input$cancer=="Other: Malignant neoplasms",paste(input$cancer,input$year,": Treatments are presented in combinations"),paste(input$cancer, "cancer ",input$year,": Treatments are presented in combinations")), 
                 caption = ifelse(input$cancer %in% c("Skin: BCC (experimental)", "Skin: cSCC (experimental)"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership \n Figures for non-melanoma skin cancers are currently experimental and likely undercount surgical tumour resections"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership"))) + 
            scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = " Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
            scale_fill_manual(name = "", values = c("Other care" = "#BFBFBF", "Chemotherapy only" = "#92D050",
                                                    "Tumour resection only" = "#00B0F0", "Radiotherapy only" = "#FFC000",
                                                    "Chemotherapy and radiotherapy" = "#FF33CC", "Tumour resection and chemotherapy" = "#002060",
                                                    "Tumour resection and radiotherapy" = "#FFFF00", "Tumour resection, radiotherapy and chemotherapy" = "#7030A0")) +
            scale_x_discrete(labels = c("0", "1", "2", "3+", "Unknown")) + 
            theme_classic(base_size = 15) +
            theme(legend.position="right", legend.text = element_text(size = 12) ) +
            theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
            guides(fill=guide_legend(reverse = TRUE)) 
    }
    
    else if (input$factor == "Stage at cancer diagnosis") {  
        multiple_treatments_plot = ggplot(treatments_and_ratios, aes(value, ratio, fill = treatment_combinations)) +
            geom_bar(stat = "identity", position = "dodge") +
            geom_errorbar(position = position_dodge(0.9), aes(ymin = lowers, ymax = uppers), width = 0.3) +
            
            labs(x = input$factor, title = ifelse(input$cancer=="All defined malignant neoplasms"| input$cancer=="Other: Malignant neoplasms",paste(input$cancer,input$year,": Treatments are presented in combinations"),paste(input$cancer, "cancer ",input$year,": Treatments are presented in combinations")), 
                 caption = ifelse(input$cancer %in% c("Skin: BCC (experimental)", "Skin: cSCC (experimental)"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership \n Figures for non-melanoma skin cancers are currently experimental and likely undercount surgical tumour resections"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership")))+
            scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = " Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
            scale_fill_manual(name = "", values = c("Other care" = "#BFBFBF", "Chemotherapy only" = "#92D050",
                                                    "Tumour resection only" = "#00B0F0", "Radiotherapy only" = "#FFC000",
                                                    "Chemotherapy and radiotherapy" = "#FF33CC", "Tumour resection and chemotherapy" = "#002060",
                                                    "Tumour resection and radiotherapy" = "#FFFF00", "Tumour resection, radiotherapy and chemotherapy" = "#7030A0")) +
            scale_x_discrete(labels = c("Stage 1", "Stage 2", "Stage 3", "Stage 4", "Unknown Stage")) + 
            theme_classic(base_size = 15) +
            theme(legend.position="right", legend.text = element_text(size = 12) ) +
            theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
            guides(fill=guide_legend(reverse = TRUE)) 
        
    } 
    
    else {
        multiple_treatments_plot = ggplot(treatments_and_ratios, aes(value, ratio, fill = treatment_combinations)) +
            geom_bar(stat = "identity", position = "dodge") +
            geom_errorbar(position = position_dodge(0.9), aes(ymin = lowers, ymax = uppers), width = 0.3) +
            
            labs(x = input$factor, title = ifelse(input$cancer=="All defined malignant neoplasms"| input$cancer=="Other: Malignant neoplasms",paste(input$cancer,input$year,": Treatments are presented in combinations"),paste(input$cancer, "cancer ",input$year,": Treatments are presented in combinations")), 
                 caption = ifelse(input$cancer %in% c("Skin: BCC (experimental)", "Skin: cSCC (experimental)"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership \n Figures for non-melanoma skin cancers are currently experimental and likely undercount surgical tumour resections"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership"))) +
            scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = " Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
            scale_fill_manual(name = "", values = c("Other care" = "#BFBFBF", "Chemotherapy only" = "#92D050",
                                                    "Tumour resection only" = "#00B0F0", "Radiotherapy only" = "#FFC000",
                                                    "Chemotherapy and radiotherapy" = "#FF33CC", "Tumour resection and chemotherapy" = "#002060",
                                                    "Tumour resection and radiotherapy" = "#FFFF00", "Tumour resection, radiotherapy and chemotherapy" = "#7030A0")) +
            theme_classic(base_size = 15) +
            theme(legend.position="right", legend.text = element_text(size = 12) ) +
            theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
            guides(fill=guide_legend(reverse = TRUE)) 
    }   
    
    
    return(multiple_treatments_plot)
}

#############################################################################
### Define server logic required to output the graphs, and run the exports
#############################################################################

server <- function(input, output) {
    
    ##Display overall graph:  
    
    output$distPlot4 <- renderPlot({
        
        shovegraphintofunction4(input)
    }, height=813, units="px")
    
    
    ##Display independent treatment graph:
    
    output$distPlot1 <- renderPlot({
        
        shovegraphintofunction1(input)
    }, height=813, units="px")
    
    
    ##Display stacked treatment graph:  
    
    # output$distPlot2 <- renderPlotly({
    ###HERE###
    output$distPlot2 <- renderPlot({
        
        shovegraphintofunction2(input)
        #})
        ###HERE###
    }, height=813, units="px")
    
    ##Display treatment combination graph:  
    
    output$distPlot3 <- renderPlot({
        
        shovegraphintofunction3(input)
    }, height=813, units="px")
    
    ##Create explanatory text for explanation tab
    
    output$text <- renderUI({
        str1 <- p("This work aims to provide basic information on the percentage of tumours receiving chemotherapy, radiotherapy and / or a surgical tumour resection as part of the primary course of treatment following diagnosis. The proportion of tumours receiving each treatment is presented, unadjusted for any factors that may affect treatment such as stage, age, gender, deprivation, ethnicity, and comorbidities.")
        str2 <- p("The method used to ascertain treatment information is described in the standard operating procedure CAS-SOP 4.8 Linking treatment tables - chemotherapy, tumour resections and radiotherapy. Slide deck summaries by cancer site are also available.")
        str3 <- strong("Datasets:")
        str4 <- p("Datasets used to capture treatment information include cancer registration data, the Systemic Anti-Cancer Therapy dataset (SACT), RadioTherapy DataSet (RTDS), and inpatient Hospital Episode Statistics (HES).")
        str5 <- p("If a patient has been diagnosed with only one cancer or with multiple cancers diagnosed over 18 months apart, then all of the above datasets are used to identify treatment data. However,  if the patient has been diagnosed with multiple cancers within 18 months then only the cancer registration data has been used. This is because cancer registration staff have linked tumours to specific treatments, whereas the other datasets are at the patient level. Presence of the three treatment types was flagged, without regard to the order of treatments, or whether treatments were delivered together.")
       # str6 <- p("For a small number of patients whose follow up period for radiotherapy extended past April 2016, data was supplemented from a new radiotherapy dataset that is still being tested. A sensitivity analysis that included and excluded this dataset showed that the impact of any errors would affect results by up to 0.5 percentage points for all malignant cancers combined (including up to 1.5 and 1.2 percentage points for breast and prostate cancers, respectively). Radiotherapy figures are likely to be an underestimate as there is underreporting of teletherapy in both RTDS datasets, and data may be incomplete for selected NHS Trusts.")
        str7 <- strong("Treatment definitions:")
        str8 <- p("A tumour resection is an attempt to surgically remove the whole of the primary tumour. These have been identified using OPCS-4 codes  through consultation with site-specific clinicians.  For the following cancer sites, some procedures (for example, endoscopic resections) have been identified as tumour resections in early stage disease only: colon, rectum, cervical, bladder, stomach, oesophagus and liver.")
        str9 <- p("Surgical tumour resections are not defined for all cancer sites. Only sites which have a defined list of tumour resection procedures will be grouped. Tumour resections have not been defined for any of the cancer sites included in the \"Other: Malignant neoplasms\" or \"Other: Non-malignant neoplasms\" categories. However, some of these tumours could have been treated with surgery.")
        str10 <- p("Radiotherapy includes both curative and palliative teletherapy procedures, and excludes brachytherapy and contact radiotherapy.")
        str11 <- p("Chemotherapy includes both curative and palliative chemotherapy, and excludes hormonal therapy, and other supportive drugs such as zoledronic acid, pamidronate, and denosumab.")
        str12 <- p("The tumours in the \"Other care\" category may have received treatment other than chemotherapy, radiotherapy and tumour resection (such as hormonal therapy or management of symptoms); treatment outside of the time frame assessed; treatment in a private setting; or there may be data missing from the datasets used.")
        str13 <- strong("Cohort:")
        str14 <- p(paste0("All patients diagnosed with malignant cancer in England in ",min(years),"-",max(years),", excluding males with gynaecological cancer and females with prostate cancer. Death certificate only registrations are included (0.1% of the cohort with a known Route to Diagnosis)."))
        str15 <- p("The Ethnic groups 'Mixed' and 'Other' were combined into an ‘Other’ ethnicity group.")
        str16 <- strong("Time period:")
        str17 <- p(paste0("Treatments occurring in the period from 1 month before diagnosis to either  6, 9, 12, 15 or 18 months after diagnosis are displayed for tumours diagnosed in ",min(years)," to ",max(years),". The time period within which the majority of first course of treatments occurred varies by cancer site and treatment type. Therefore, an appropriate time period for each cancer site has been chosen using a data-driven approach in consultation with clinicians. Where a time period of 18 months has been used, some tumours diagnosed in ",max(years)," will not yet have surgery data recorded in HES, so the percentage receiving a tumour resection may be an underestimate. For more information, and a sensitivity analysis showing the effect of varying the time periods, see the CAS-SOP 4.8."))
        str18 <- strong("Staging:")
        str19 <- p("Stage breakdowns use TNM staging, except gynaecological cancers which use Figo staging. For ovarian, uterine and vulval cancers, TNM stage has been used where Figo stage was unknown. Figo substages were collated into Figo stages 1, 2, 3, 4, and unknown.  The final recorded stage of a tumour is derived by the registration service using all information available up to 3 months after diagnosis. For this reason, the tumour stage shown in this data may be different to the stage originally available to the clinician when deciding a course of treatment, as it may have been subsequently updated following removal of the tumour and pathology results.")
        str20 <- strong("Known issues:")
        str21 <- p("For pancreatic tumours, a much lower proportion of early stage tumours are recorded to have been resected than expected. Feedback from clinical experts highlighted that this does not fit with clinical experience, so further investigation is needed to understand whether all resections are being captured by the data and methodology.")
        str22 <- p("Figures for non-melanoma skin cancers (NMSC) are currently experimental and likely to be undercount surgical tumour resections.")
        str23 <- p("Between April 2023 and November 2023, the 'Male' and 'Female' graph labels were incorrect for the 'Stacked Treatment Combinations' and 'Treatment Combinations' graphs, for the gender demographic breakdown. This has now been corrected. The underlying and downloadable data were unaffected by this error.")
        HTML(paste(str1, str3, str4, str5, str7, str8, str9, str10, str11, str12, str13, str14, str15, str16, str17, str18, str19, str20, str21, str22, str23, sep = ' '))
        
    })
    
    ##Download overall treatment data:  
    
    output$downloadData4 <- downloadHandler(
      filename = function() {
        if (input$cancer=="All defined malignant neoplasms" | input$cancer == "Other: Malignant neoplasms" | input$cancer == "All defined non-malignant neoplasms" | input$cancer == "Lung: Non-small cell lung cancer" | input$cancer == "Lung: Small cell lung cancer" | input$cancer == "Other: Non-malignant neoplasms" | input$cancer == "Skin: BCC (experimental)" | input$cancer == "Skin: cSCC (experimental)" | input$cancer == "Skin: Rare") {
          
          paste('NCRAS-CRUK - Overall treatment data for ', sapply(input$cancer, tolower),' ',input$year,' ', Sys.Date(), '.csv', sep='') %>%
            str_remove(., ":")
        }
        else {
          paste('NCRAS-CRUK - Overall treatment data for ', sapply(input$cancer, tolower), ' cancer ',input$year,' ', Sys.Date(), '.csv', sep='') %>%
            str_remove(., ":")
          
        }
      },
      content = function(con) {
        
          
        tempdata = data[data$cancergroup == input$cancer,]
        tempdata = tempdata[tempdata$factor == "Gender",]
        tempdata = tempdata[tempdata$year == input$year,]
        
        tempdata$dummyvar <- 1
        
        #tempdata            
           
        # Aggregate the data to calculate the percentages and CIs  
        
        data_by_year = tempdata
            
        chemo_data = aggregate(tumour_count ~ dummyvar + ct_flag, data = data_by_year, FUN = 'sum', drop = FALSE)
        chemo_numerators = chemo_data[chemo_data$ct_flag == "Yes",]$tumour_count
        chemo_totals = aggregate(tumour_count ~ dummyvar, data = data_by_year, FUN = 'sum', drop = FALSE)
        chemo_denominators = chemo_totals$tumour_count
        chemo_frame = c(chemo_totals, get_percentages_and_confidence_intervals(chemo_numerators, chemo_denominators))
        chemo_frame$Treatment = "Chemotherapy"
        
        radio_data = aggregate(tumour_count ~ dummyvar + rt_flag, data = data_by_year, FUN = 'sum', drop = FALSE)
        radio_numerators = radio_data[radio_data$rt_flag == "Yes",]$tumour_count
        radio_totals = aggregate(tumour_count ~ dummyvar, data = data_by_year, FUN = 'sum', drop = FALSE)
        radio_denominators = radio_totals$tumour_count
        radio_frame = c(radio_totals, get_percentages_and_confidence_intervals(radio_numerators, radio_denominators))
        radio_frame$Treatment = "Radiotherapy"
        
        surgery_data = aggregate(tumour_count ~ dummyvar + sg_flag, data = data_by_year, FUN = 'sum', drop = FALSE)
        surgery_numerators = surgery_data[surgery_data$sg_flag == "Yes",]$tumour_count
        surgery_totals = aggregate(tumour_count ~dummyvar, data = data_by_year, FUN = 'sum', drop = FALSE)
        surgery_denominators = surgery_totals$tumour_count
        surgery_frame = c(surgery_totals, get_percentages_and_confidence_intervals(surgery_numerators, surgery_denominators))
        surgery_frame$Treatment = "Tumour resection"
           
        
        if (input$cancer == "Other: Malignant neoplasms"| input$cancer == "Other: Non-malignant neoplasms"){
          plottingdata = rbind(as.data.frame(chemo_frame), as.data.frame(radio_frame))
          plottingdata = merge(plottingdata, radio_denominators)
          
        }
        else{
          
          plottingdata = rbind(as.data.frame(chemo_frame), as.data.frame(surgery_frame), as.data.frame(radio_frame))
          plottingdata = merge(plottingdata, surgery_denominators)
        }


        names(plottingdata)[names(plottingdata) == 'y'] <- 'denominator'
        
        
        plottingdata[plottingdata$ratio == 0, c("lowers", "uppers")] = NA
        
        plottingdata$dummyvar<- NULL
           
        ## Create the combinations variable
        
        treatment_interaction_data = data_by_year
        ##Need to subset the data again for some reason
        treatment_interaction_data = treatment_interaction_data[treatment_interaction_data$cancergroup == input$cancer,]
        treatment_interaction_data = treatment_interaction_data[treatment_interaction_data$factor == "Gender",]
        treatment_interaction_data = treatment_interaction_data[treatment_interaction_data$year == input$year,]
             
        treatment_interaction_data$treatment_combinations = interaction(treatment_interaction_data$ct_flag, treatment_interaction_data$rt_flag, treatment_interaction_data$sg_flag)
        
        treatment_interaction_data$treatment_combinations = factor(treatment_interaction_data$treatment_combinations, levels = 
                                                                     c("No.No.No", "Yes.No.No", "No.No.Yes","No.Yes.No", "Yes.Yes.No", "Yes.No.Yes", "No.Yes.Yes", "Yes.Yes.Yes"))
           
        levels(treatment_interaction_data$treatment_combinations) = list(
          "Other care" = "No.No.No", "Chemotherapy only" = "Yes.No.No", "Tumour resection only" = "No.No.Yes", "Radiotherapy only" = "No.Yes.No",
          "Chemotherapy and radiotherapy" = "Yes.Yes.No", "Tumour resection and chemotherapy" = "Yes.No.Yes",
          "Tumour resection and radiotherapy" = "No.Yes.Yes", "Tumour resection, radiotherapy and chemotherapy" = "Yes.Yes.Yes")

        
        aggregate_denominators = aggregate(tumour_count ~ dummyvar, data = treatment_interaction_data, FUN = 'sum')
        
        colnames(aggregate_denominators)[colnames(aggregate_denominators) == "tumour_count"] = "denominator"
        
        treatment_interaction_data2 = aggregate(tumour_count ~ dummyvar + treatment_combinations , data = treatment_interaction_data, FUN = 'sum', drop = FALSE)
        
        treatment_interaction_data3 = merge(treatment_interaction_data2, aggregate_denominators) %>%
          filter(!(is.na(tumour_count)))
        
        
        treatments_and_ratios = as.data.frame(c(treatment_interaction_data3, 
                                                get_percentages_and_confidence_intervals(treatment_interaction_data3$tumour_count, treatment_interaction_data3$denominator)))
        
        treatments_and_ratios = treatments_and_ratios[treatments_and_ratios$ratio > 0,]   
        
        treatments_and_ratios$dummyvar<- NULL
        treatments_and_ratios$Treatment<- treatments_and_ratios$treatment_combinations
        treatments_and_ratios$treatment_combinations<- NULL
        #   treatments_and_ratios$denominator<- NULL
        
        ##Append the two datasets
          
        overalldata = rbind(plottingdata, treatments_and_ratios)
        
        # if (input$cancer == "Other: Malignant neoplasms" | input$cancer == "Other: Non-malignant neoplasms") {
        #   overalldata$Type <- c("Independent", "Independent", "Combination", "Combination", "Combination", "Combination")
        #   
        # }
        # else {
        #   overalldata$Type <- c("Independent", "Independent", "Independent", "Combination", "Combination", "Combination", "Combination", "Combination", "Combination", "Combination", "Combination")
        # }
        
        overalldata <- overalldata%>% mutate(Type=case_when(
          Treatment=="Chemotherapy" ~ "Independent",
          Treatment=="Radiotherapy" ~ "Independent",
          Treatment=="Tumour resection" ~ "Independent",
          Treatment=="Other care" ~ "Combination",
          Treatment=="Chemotherapy only" ~ "Combination",
          Treatment=="Tumour resection only" ~ "Combination",
          Treatment=="Radiotherapy only" ~ "Combination",
          Treatment=="Chemotherapy and radiotherapy" ~ "Combination",
          Treatment=="Tumour resection and chemotherapy" ~ "Combination",
          Treatment=="Tumour resection and radiotherapy" ~ "Combination",
          Treatment=="Tumour resection, radiotherapy and chemotherapy" ~ "Combination",
        ))
        

        overalldata$cancer <- input$cancer
        overalldata$year <- input$year
        
        ## Rename the variable names for the output 
        
        names(overalldata)[names(overalldata) == 'ratio'] <- 'Proportion of tumours'
        names(overalldata)[names(overalldata) == 'uppers'] <- 'Lower confidence interval'
        names(overalldata)[names(overalldata) == 'lowers'] <- 'Upper confidence interval'
        names(overalldata)[names(overalldata) == 'treatment'] <- 'Treatment modality'
        names(overalldata)[names(overalldata) == 'Type'] <- 'Treatment grouping'
        names(overalldata)[names(overalldata) == 'denominator'] <- 'Number of tumours diagnosed'
        names(overalldata)[names(overalldata) == 'tumour_count'] <- 'Number of tumours treated'
        names(overalldata)[names(overalldata) == 'cancer'] <- 'Cancer type'
        names(overalldata)[names(overalldata) == 'year'] <- 'Year of diagnosis'
        
        overalldata <- overalldata[, c(8,9,5,7,6,1,2,4,3)]
        
        overalldata$'Number of tumours treated' <- NULL
        
        
        write.csv(overalldata, con, row.names=FALSE)
      }
    )    
    
    
    
    ##Download independent data 
    
      output$downloadData <- downloadHandler(
        filename = function() {
          if (input$cancer=="All defined malignant neoplasms" | input$cancer == "Other: Malignant neoplasms" | input$cancer == "All defined non-malignant neoplasms" | input$cancer == "Lung: Non-small cell lung cancer" | input$cancer == "Lung: Small cell lung cancer" | input$cancer == "Other: Non-malignant neoplasms" | input$cancer == "Skin: BCC (experimental)" | input$cancer == "Skin: cSCC (experimental)" | input$cancer == "Skin: Rare") {
            paste('NCRAS-CRUK - Independent treatment data by ', input$factor, ' for ',input$cancer, ' ', input$year, ' ', Sys.Date(), '.csv', sep='') %>%
              str_remove(., ":")
          }  
          
          else {
            paste('NCRAS-CRUK - Independent treatment data by ', input$factor, ' for ', input$cancer, ' cancer', ' ', input$year, ' ', Sys.Date(), '.csv', sep='') %>%
              str_remove(., ":")
          }
        },

      content = function(con) {
        
        tempdata = data[data$cancergroup == input$cancer,]
        tempdata = tempdata[tempdata$factor == input$factor,]
        tempdata = tempdata[tempdata$year == input$year,]
        #factor_levels = levels(tempdata$value)
        factor_levels = levels(droplevels(tempdata$value)) #Need droplevels, else will keep all levels including those filtered out 
        
        data_by_year = tempdata
        
        chemo_data = aggregate(tumour_count ~ factor(value) + ct_flag, data = data_by_year, FUN = 'sum', drop = FALSE)
        chemo_numerators = chemo_data[chemo_data$ct_flag == "Yes",]$tumour_count
        chemo_totals = aggregate(tumour_count ~ factor(value), data = data_by_year, FUN = 'sum', drop = FALSE)
        chemo_denominators = chemo_totals$tumour_count
        chemo_frame = c(chemo_totals, get_percentages_and_confidence_intervals(chemo_numerators, chemo_denominators))
        chemo_frame$Treatment = "Chemotherapy"
        
        radio_data = aggregate(tumour_count ~ factor(value) + rt_flag, data = data_by_year, FUN = 'sum', drop = FALSE)
        radio_numerators = radio_data[radio_data$rt_flag == "Yes",]$tumour_count
        radio_totals = aggregate(tumour_count ~ factor(value), data = data_by_year, FUN = 'sum', drop = FALSE)
        radio_denominators = radio_totals$tumour_count
        radio_frame = c(radio_totals, get_percentages_and_confidence_intervals(radio_numerators, radio_denominators))
        radio_frame$Treatment = "Radiotherapy"
        
        surgery_data = aggregate(tumour_count ~ factor(value) + sg_flag, data = data_by_year, FUN = 'sum', drop = FALSE)
        surgery_numerators = surgery_data[surgery_data$sg_flag == "Yes",]$tumour_count
        surgery_totals = aggregate(tumour_count ~factor(value), data = data_by_year, FUN = 'sum', drop = FALSE)
        surgery_denominators = surgery_totals$tumour_count
        surgery_frame = c(surgery_totals, get_percentages_and_confidence_intervals(surgery_numerators, surgery_denominators))
        surgery_frame$Treatment = "Tumour resection"
        
        if (input$cancer == "Other: Malignant neoplasms" | input$cancer == "Other: Non-malignant neoplasms"){        
        plottingdata = rbind(as.data.frame(chemo_frame), as.data.frame(radio_frame))
        } else {
        plottingdata = rbind(as.data.frame(chemo_frame), as.data.frame(surgery_frame), as.data.frame(radio_frame))
        }
        names(plottingdata)[names(plottingdata) == "factor.value."] <- "value" #Rename column to value
        plottingdata$value = factor(plottingdata$value, levels = factor_levels)
        plottingdata$ratio[is.na(plottingdata$ratio)] <- 0
        plottingdata[plottingdata$ratio == 0, c("lowers", "uppers")] = NA
        
        ##Sort out the labelling of Gender and comorbidity in the data download 
        
        if (input$factor == "Gender" & input$cancer %in% c("Uterine", "Cervical", "Ovary", "Vulva")) {
          plottingdata$value <- factor(plottingdata$value, levels = c("Female"), labels = c("Female"))
        }  
        else if (input$factor == "Gender") {  
          plottingdata$value <- factor(plottingdata$value, levels = c("Male","Female"), labels = c("Male", "Female"))
        }
        else if (input$factor == "Age at cancer diagnosis") {  
          plottingdata$value <- factor(plottingdata$value, levels = c("U50","50-59","60-69","70-79","80+"), labels = c("Under 50", "50-59", "60-69", "70-79","80+"))
          plottingdata$value <- relevel(plottingdata$value,"Under 50")
          plottingdata <- plottingdata[order(plottingdata$Treatment,plottingdata$value),]
        }
        
        else if (input$factor == "Charlson comorbidity index"){
          plottingdata$value <- factor(plottingdata$value, levels = c("0","1","2","3+"), labels = c("0", "1", "2", "3+"))
        }
        
        
        else {
          plottingdata$value = factor(plottingdata$value, levels = factor_levels) 
        }
        
        ## Rename the variable names for the output 
        
        plottingdata$cancer <- input$cancer
        plottingdata$year <- input$year
        
        names(plottingdata)[names(plottingdata) == 'tumour_count'] <- 'Number of tumours diagnosed'
        names(plottingdata)[names(plottingdata) == 'ratio'] <- 'Proportion of tumours'
        names(plottingdata)[names(plottingdata) == 'uppers'] <- 'Lower confidence interval'
        names(plottingdata)[names(plottingdata) == 'lowers'] <- 'Upper confidence interval'
        names(plottingdata)[names(plottingdata) == 'Treatment'] <- 'Treatment modality'
        names(plottingdata)[names(plottingdata) == 'value'] <- paste('Levels of ',sapply(input$factor, tolower))
        names(plottingdata)[names(plottingdata) == 'cancer'] <- 'Cancer type'
        names(plottingdata)[names(plottingdata) == 'year'] <- 'Year of Diagnosis'
        
        # Re order columns
        plottingdata <- plottingdata[, c(7,8,6, 1, 2, 3, 5, 4)]
        
        write.csv(plottingdata, con, row.names=FALSE)
      }
    )
    
    ##Download treatment combination data:  
    
    output$downloadData2 <- downloadHandler(
      filename = function() {
        if (input$cancer=="All defined malignant neoplasms" | input$cancer == "Other malignant neoplasms") {
          paste('NCRAS-CRUK - Treatment combination data by ', sapply(input$factor,tolower), ' for ', sapply(input$cancer, tolower), ' ', input$year, ' ',Sys.Date(), '.csv', sep='') %>%
            str_remove(., ":")
        }
        else {
          paste('NCRAS-CRUK - Treatment combination data by ', sapply(input$factor,tolower), ' for ', sapply(input$cancer, tolower), ' cancer ', input$year, ' ',Sys.Date(), '.csv', sep='')%>%
            str_remove(., ":")
        }
      },
      content = function(con) {
        
        tempdata = data[data$cancergroup == input$cancer,]
        tempdata = tempdata[tempdata$factor == input$factor,]
        tempdata = tempdata[tempdata$year == input$year,]
        factor_levels = levels(droplevels(tempdata$value))
        data_by_year = tempdata
        
        chemo_data = aggregate(tumour_count ~ factor(value) + ct_flag, data = data_by_year, FUN = 'sum', drop = FALSE)
        chemo_numerators = chemo_data[chemo_data$ct_flag == 1,]$tumour_count
        chemo_totals = aggregate(tumour_count ~ factor(value), data = data_by_year, FUN = 'sum', drop = FALSE)
        chemo_denominators = chemo_totals$tumour_count
        chemo_frame = c(chemo_totals, get_percentages_and_confidence_intervals(chemo_numerators, chemo_denominators))
        chemo_frame$Treatment = "Chemotherapy"
        
        radio_data = aggregate(tumour_count ~ factor(value) + rt_flag, data = data_by_year, FUN = 'sum', drop = FALSE)
        radio_numerators = radio_data[radio_data$rt_flag == 1,]$tumour_count
        radio_totals = aggregate(tumour_count ~ factor(value), data = data_by_year, FUN = 'sum', drop = FALSE)
        radio_denominators = radio_totals$tumour_count
        radio_frame = c(radio_totals, get_percentages_and_confidence_intervals(radio_numerators, radio_denominators))
        radio_frame$Treatment = "Radiotherapy"
        
        surgery_data = aggregate(tumour_count ~ factor(value) + sg_flag, data = data_by_year, FUN = 'sum', drop = FALSE)
        surgery_numerators = surgery_data[surgery_data$sg_flag == 1,]$tumour_count
        surgery_totals = aggregate(tumour_count ~factor(value), data = data_by_year, FUN = 'sum', drop = FALSE)
        surgery_denominators = surgery_totals$tumour_count
        surgery_frame = c(surgery_totals, get_percentages_and_confidence_intervals(surgery_numerators, surgery_denominators))
        surgery_frame$Treatment = "Tumour resection"
        
        plottingdata = rbind(as.data.frame(chemo_frame), as.data.frame(surgery_frame), as.data.frame(radio_frame))
        
        names(plottingdata)[names(plottingdata) == "factor.value."] <- "value"
        
        if (input$factor == "Gender") {  
          plottingdata$value <- factor(plottingdata$value, levels = c("Male","Female"), labels = c("Male", "Female"))
        }
        else {
          plottingdata$value = factor(plottingdata$value, levels = factor_levels) 
        }
        
        plottingdata[plottingdata$ratio == 0, c("lowers", "uppers")] = NA
        
        treatment_interaction_data = data_by_year
        treatment_interaction_data$treatment_combinations = interaction(treatment_interaction_data$ct_flag, treatment_interaction_data$rt_flag, treatment_interaction_data$sg_flag)
        treatment_interaction_data$treatment_combinations = factor(treatment_interaction_data$treatment_combinations, levels = 
                                                                     c("No.No.No", "Yes.No.No", "No.No.Yes","No.Yes.No", "Yes.Yes.No", "Yes.No.Yes", "No.Yes.Yes", "Yes.Yes.Yes"))
        
        
        levels(treatment_interaction_data$treatment_combinations) = list(
          "Other care" = "No.No.No", "Chemotherapy only" = "Yes.No.No", "Tumour resection only" = "No.No.Yes", "Radiotherapy only" = "No.Yes.No",
          "Chemotherapy and radiotherapy" = "Yes.Yes.No", "Tumour resection and chemotherapy" = "Yes.No.Yes",
          "Tumour resection and radiotherapy" = "No.Yes.Yes", "Tumour resection, radiotherapy and chemotherapy" = "Yes.Yes.Yes")
        
        aggregate_denominators = aggregate(tumour_count ~ value, data = treatment_interaction_data, FUN = 'sum')
        
        colnames(aggregate_denominators)[colnames(aggregate_denominators) == "tumour_count"] = "denominator"
        treatment_interaction_data = merge(treatment_interaction_data, aggregate_denominators)
        treatments_and_ratios = as.data.frame(c(treatment_interaction_data, 
                                                get_percentages_and_confidence_intervals(treatment_interaction_data$tumour_count, treatment_interaction_data$denominator)))
        treatments_and_ratios = treatments_and_ratios[treatments_and_ratios$ratio > 0,]  
        
        ## Rename the variable names for the output 
        
        treatments_and_ratios$ct_flag <- NULL
        treatments_and_ratios$rt_flag <- NULL
        treatments_and_ratios$sg_flag <- NULL
        
        ##Sort out the labelling of Gender and comorbidity in the data download 
        
        if (input$factor == "Gender" & input$cancer %in% c("Uterine", "Cervical", "Ovary", "Vulva")) {
          treatments_and_ratios$value <- factor(treatments_and_ratios$value, levels = c("Female"), labels = c("Female"))
        }  
        else if (input$factor == "Gender") {  
          treatments_and_ratios$value <- factor(treatments_and_ratios$value, levels = c("Male","Female"), labels = c("Male", "Female"))
        }
        else if (input$factor == "Charlson comorbidity index") {  
          treatments_and_ratios$value <- factor(treatments_and_ratios$value, levels = c("0", "1", "2", "3+"), labels = c("0", "1", "2", "3+"))
        }
        else if (input$factor == "Age at cancer diagnosis") {  
          treatments_and_ratios$value <- factor(treatments_and_ratios$value, levels = c("U50","50-59","60-69","70-79","80+"), labels = c("Under 50", "50-59", "60-69", "70-79","80+"))
          treatments_and_ratios$value <- relevel(treatments_and_ratios$value,"Under 50")
          treatments_and_ratios <- treatments_and_ratios[order(treatments_and_ratios$treatment_combinations,treatments_and_ratios$value),]
        }
        else {
          treatments_and_ratios$value = factor(treatments_and_ratios$value, levels = factor_levels) 
        }
        
        # Sort out the column names 
        
        names(treatments_and_ratios)[names(treatments_and_ratios) == 'tumour_count'] <- 'Number of tumours treated'
        names(treatments_and_ratios)[names(treatments_and_ratios) == 'ratio'] <- 'Proportion of tumours'
        names(treatments_and_ratios)[names(treatments_and_ratios) == 'uppers'] <- 'Lower confidence interval'
        names(treatments_and_ratios)[names(treatments_and_ratios) == 'lowers'] <- 'Upper confidence interval'
        names(treatments_and_ratios)[names(treatments_and_ratios) == 'treatment_combinations'] <- 'Treatment modality'
        names(treatments_and_ratios)[names(treatments_and_ratios) == 'value'] <- paste('Levels of ',sapply(input$factor, tolower))   
        names(treatments_and_ratios)[names(treatments_and_ratios) == 'cancergroup'] <- 'Cancer type'
        names(treatments_and_ratios)[names(treatments_and_ratios) == 'year'] <- 'Year of Diagnosis'
        names(treatments_and_ratios)[names(treatments_and_ratios) == 'denominator'] <- 'Number of tumours diagnosed'
        names(treatments_and_ratios)[names(treatments_and_ratios) == 'factor'] <- 'Factor of interest'
        
        # Re order columns
        treatments_and_ratios <- treatments_and_ratios[, c(2,4,3,1,6,7,5,8,10,9)]
        
        if (input$cancer == "Other: Malignant neoplasms"| input$cancer == "Other: Non-malignant neoplasms") {
          
          treatments_and_ratios$'Number of tumours treated' <- NULL
          treatments_and_ratios$'Proportion of tumours' <- NULL
          treatments_and_ratios$'Lower confidence interval' <- NULL
          treatments_and_ratios$'Upper confidence interval' <- NULL
          treatments_and_ratios$'Treatment modality' <- NULL
          treatments_and_ratios$'Number of tumours diagnosed' <- NULL
          treatments_and_ratios$'Factor of interest' <- NULL
          treatments_and_ratios$'Cancer type' <- NULL
          treatments_and_ratios$'Year of Diagnosis' <- NULL
          treatments_and_ratios[1] <- NULL
          
          write.csv(treatments_and_ratios, con, row.names=FALSE)
        }
        else {
          write.csv(treatments_and_ratios, con, row.names=FALSE)
        }
      }        
        
    )  
    
    ##Download all raw data   
    
    output$downloadData3 <- downloadHandler(
        filename = function() {
            paste('NCRAS-CRUK - Treatment raw data for all malignant cancers ',min(years),"-",max(years),' ',Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            
            tempdata <- data
            
 #Don't want to include Charlson comorbidity index data where index is unknown            
           # tempdata<- tempdata %>% 
          #      filter(!(cancergroup %in% c("All non-malignant neoplasms", "Benign Endocrine", "Brain: Non-malignant", "Other: Non-malignant neoplasms", "Skin: BCC (experimental)", "Skin: cSCC (experimental)", "Skin: Rare") & factor == "Charlson comorbidity index"))

            names(tempdata)[names(tempdata) == 'tumour_count'] <- 'Number of tumours treated'
            names(tempdata)[names(tempdata) == 'ratio'] <- 'Proportion of tumours'
            names(tempdata)[names(tempdata) == 'rt_flag'] <- 'Radiotherapy (Y/N)'
            names(tempdata)[names(tempdata) == 'ct_flag'] <- 'Chemotherapy (Y/N)'
            names(tempdata)[names(tempdata) == 'sg_flag'] <- 'Tumour Resection (Y/N)'
            names(tempdata)[names(tempdata) == 'factor'] <- 'Factor of interest'
            names(tempdata)[names(tempdata) == 'cancergroup'] <- 'Cancer type'
            names(tempdata)[names(tempdata) == 'value'] <- 'Levels of factor'  
            names(tempdata)[names(tempdata) == 'year'] <- 'Year of Diagnosis'  
            
            tempdata <- tempdata[, c(1,4,2,3,5,6,7,8)]

            write.csv(tempdata, con, row.names=FALSE)
        }
    )  
    
    output$downloadGraph <- downloadHandler(
        filename = function() {
            if (input$cancer=="All defined malignant neoplasms" | input$cancer == "Other: Malignant neoplasms") {
                paste('NCRAS-CRUK - Independent treatment graph by ', sapply(input$factor,tolower), ' for ', sapply(input$cancer, tolower), ' ', input$year, ' ',Sys.Date(), '.tiff', sep='') %>%
                str_remove(., ":")
                
            }
            else {
                #paste('Independent treatment graph by ', input$factor, ' For ', paste(input$cancer), ' ', Sys.Date(), '.tiff', sep='')
                paste('NCRAS-CRUK - Independent treatment graph by ', sapply(input$factor,tolower), ' for ', sapply(input$cancer, tolower), ' cancer ', input$year, ' ',Sys.Date(), '.tiff', sep='') %>%
                str_remove(., ":")
            }
        },
        content = function(file) {
            
            tempdata = data[data$cancergroup == input$cancer,]
            tempdata = tempdata[tempdata$factor == input$factor,]
            tempdata = tempdata[tempdata$year == input$year,]
            
            ggsave(file,shovegraphintofunction1(input), dpi=300,width = 45, height = 25, units = "cm")
        }
    )
    
    output$downloadGraph2 <- downloadHandler(
        filename = function() {
            if (input$cancer=="All defined malignant neoplasms" | input$cancer == "Other: Malignant neoplasms") {
                paste('NCRAS-CRUK - Stacked treatment combination graph by ', sapply(input$factor,tolower), ' for ', sapply(input$cancer, tolower), ' ',input$year, ' ', Sys.Date(), '.tiff', sep='') %>%
                str_remove(., ":")
                
            }
            else {
                paste('NCRAS-CRUK - Stacked treatment combination graph by ', sapply(input$factor,tolower), ' for ', sapply(input$cancer, tolower), ' cancer ',input$year, ' ', Sys.Date(), '.tiff', sep='') %>%
                str_remove(., ":")
            }
        },
        content = function(file) {
            
            tempdata = data[data$cancergroup == input$cancer,]
            tempdata = tempdata[tempdata$factor == input$factor,]
            tempdata = tempdata[tempdata$year == input$year,]
            
            ggsave(file,shovegraphintofunction2(input), dpi=300,width = 45, height = 25, units = "cm")
        }
    )  
    
    output$downloadGraph3 <- downloadHandler(
        filename = function() {
            if (input$cancer=="All defined malignant neoplasms" | input$cancer == "Other: Malignant neoplasms") {
                paste('NCRAS-CRUK - Treatment combination graph by ', sapply(input$factor,tolower), ' for ', sapply(input$cancer, tolower), ' ',input$year, ' ', Sys.Date(), '.tiff', sep='') %>%
                str_remove(., ":")
                
            }
            else {
                paste('NCRAS-CRUK - Treatment combination graph by ', sapply(input$factor,tolower), ' for ', sapply(input$cancer, tolower), ' cancer ',input$year, ' ', Sys.Date(), '.tiff', sep='') %>%
                str_remove(., ":")
            }
        },
        content = function(file) {
            
            tempdata = data[data$cancergroup == input$cancer,]
            tempdata = tempdata[tempdata$factor == input$factor,]
            
            ggsave(file,shovegraphintofunction3(input), dpi=300,width = 45, height = 25, units = "cm")
        }
    )  
    
    output$downloadGraph4 <- downloadHandler(
        filename = function() {
            if (input$cancer=="All defined malignant neoplasms" | input$cancer == "Other: Malignant neoplasms") {
                paste('NCRAS-CRUK - Overall treatment graph for ', sapply(input$cancer, tolower), ' ',input$year, ' ', Sys.Date(), '.tiff', sep='') %>%
                str_remove(., ":")
                
            }
            else {
                paste('NCRAS-CRUK - Overall treatment graph for ', sapply(input$cancer, tolower), ' cancer ',input$year, ' ', Sys.Date(), '.tiff', sep='') %>%
                str_remove(., ":")
            }
        },
        content = function(file) {
            
            tempdata = data[data$cancergroup == input$cancer,]
            tempdata = data[data$factor == "Gender",]
            
            tempdata$dummyvar <- 1
            
            ggsave(file,shovegraphintofunction4(input), dpi=300,width = 45, height = 25, units = "cm")
        }
    )  
    
}

#############################################################################
# Run the application 
#############################################################################
shinyApp(ui = ui, server = server)