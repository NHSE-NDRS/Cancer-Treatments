#calncv17nm_rename changed to alliance
#hash out "Other" data 
#Change 1 to Yes and 0 to No

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

data<- read.csv("geography.csv", stringsAsFactors = TRUE)
### INPUT for years of diagnosis the app is covering
years<-c("2013", "2014", "2015", "2016", "2017", "2018","2019","2020","2021")


#Function to create 3 year rolling years as used in geog app.
year_range_three = function(min_year, max_year){
  min_rolling_years <- c(min_year:(as.numeric(max_year)-2))
  max_rolling_years <- c((as.numeric(min_year)+2):(max_year))
  rolling_years <- paste0 (min_rolling_years,'-', max_rolling_years)
  return (rolling_years)
}

rolling_years<-year_range_three(min(years),max(years))

#Set years levels so that combined years is always first
data$year_range<- factor(data$year_range, levels=c(paste0(min(years),"-",max(years)), rolling_years))

#############################################################################
# Define UI for application 
#############################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(
    fluidRow(
      column(9, paste0("Treatment Breakdown by Alliance, ",min(years),"-",max(years))), 
      column(3, img(src='ndrs_cruk.png', align = "right", width=250, units="px" ))
    ), windowTitle = "Cancer Treatments"
  ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      pickerInput(inputId = "cancer",
                  label = "Select Cancer Type",
                  choices = as.list(levels(data$cancergroup)),
                  selected = "All malignant neoplasms with surgery defined",
                  multiple = FALSE),
      pickerInput(inputId = "alliance",
                  label = "Select Alliance of Interest",
                  choices = as.list(levels(data$alliance)),
                  multiple = FALSE),
      
      pickerInput(inputId = "year",
                  label = "Select Year Range of Interest",
                  choices = as.list(levels(data$year_range)),
                  multiple = FALSE),
      
      strong(paste0("This website presents the proportion of tumours diagnosed in England in ",min(years),"-",max(years)," recorded as receiving radiotherapy, chemotherapy or tumour resection as part of the primary course of treatment following diagnosis.")),
      br(),
      p("Please select from the drop-down lists above to view graphs for a specific cancer site, year of diagnosis, and cancer alliance."),
      br(),
      p("Select each of the tabs to view graphs for the treatments independently or combined."),
      br(),
      strong(paste0("Please see the ‘Information’ tab for a more detailed description of the data presented.")),
      br(),
      br(),
      p("The methodology is described in the standard operating procedure ", a("CAS-SOP v4.9 Linking treatment tables: chemotherapy, tumour resections, and radiotherapy.", href="https://digital.nhs.uk/ndrs/data/data-outputs/cancer-data-hub/cancer-treatments"), em("(Please note this link may not load in all browsers).")),
      br(),
      strong("Click below to download a copy of the data, and for TIFF outputs of the graphs:"),
      br(),
      downloadLink('downloadData1', 'Download Data for Independent Treatments'),
      br(),
      downloadLink('downloadData2', 'Download Data for Treatment Combination'),
      br(),
      downloadLink('downloadData3', 'Download Raw Data for all Cancers and years'),
      br(),
      downloadLink('downloadGraph1', 'Export Graph for Independent Treatments'),   
      br(),
      downloadLink('downloadGraph2', 'Export Graph for Treatment Combination'),
      br(),
      br(),
      em("This tool is produced by the National Disease Registration Service (NDRS), as part of the Cancer Research UK - NHS England Partnership."),
      br(),
      br(),
      em("This work uses data that has been provided by patients and collected by the NHS as part of their care and support. The data is collated, maintained and quality assured by the National Disease Registration Service, which is part of NHS England.")
      ),  
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Independent Treatment", plotOutput("distPlot1")),
        tabPanel("Stacked Treatment Combinations", plotOutput("distPlot2")),
        tabPanel("Information", htmlOutput("text"))
      )  
    )
  )
)


#############################################################################
# Create functions to draw graphs 
#############################################################################

#####Function for independent treatments graph

shovegraphintofunction1 = function(input) {
  
  ###Create dataset with percentages for alliance of interest  
  
  ###Create dataset with percentages for alliance of interest  
  # Create the subset of the data based on drop-downs of cancer type and alliance    
  adata_by_year = data[data$cancergroup == input$cancer,]
  #adata_by_year = data[data$cancergroup == "Bladder",]
  adata_by_year = adata_by_year[adata_by_year$alliance == input$alliance,]
  #adata_by_year = adata_by_year[adata_by_year$alliance == "West Yorkshire",]
  adata_by_year = adata_by_year[adata_by_year$year_range == input$year,]
  adata_by_year$dummyvar <- 1
  
  
  # Calculate the percentages for the treatment types
  
  achemo_data = aggregate(tumour_count ~ dummyvar + ct_flag, data = adata_by_year, FUN = 'sum', drop = FALSE)
  achemo_numerators = achemo_data[achemo_data$ct_flag == "Yes",]$tumour_count
  achemo_totals = aggregate(tumour_count ~ dummyvar, data = adata_by_year, FUN = 'sum', drop = FALSE)
  achemo_denominators = achemo_totals$tumour_count
  achemo_frame = c(achemo_totals, get_percentages_and_confidence_intervals(achemo_numerators, achemo_denominators))
  achemo_frame$Treatment = "Chemotherapy"
  
  aradio_data = aggregate(tumour_count ~ dummyvar + rt_flag, data = adata_by_year, FUN = 'sum', drop = FALSE)
  aradio_numerators = aradio_data[aradio_data$rt_flag == "Yes",]$tumour_count
  aradio_totals = aggregate(tumour_count ~ dummyvar, data = adata_by_year, FUN = 'sum', drop = FALSE)
  aradio_denominators = aradio_totals$tumour_count
  aradio_frame = c(aradio_totals, get_percentages_and_confidence_intervals(aradio_numerators, aradio_denominators))
  aradio_frame$Treatment = "Radiotherapy"

    asurgery_data = aggregate(tumour_count ~ dummyvar + sg_flag, data = adata_by_year, FUN = 'sum', drop = FALSE)
    asurgery_numerators = asurgery_data[asurgery_data$sg_flag == "Yes",]$tumour_count
    asurgery_totals = aggregate(tumour_count ~dummyvar, data = adata_by_year, FUN = 'sum', drop = FALSE)
    asurgery_denominators = asurgery_totals$tumour_count
    asurgery_frame = c(asurgery_totals, get_percentages_and_confidence_intervals(asurgery_numerators, asurgery_denominators))
    asurgery_frame$Treatment = "Tumour resection"
    
  if (input$cancer == "Other: Malignant neoplasms"| input$cancer == "Other: Non-malignant neoplasms" 
      | input$cancer == "All haematological malignancies (experimental)"
      | input$cancer == "Haematological malignancy: Acute Lymphoblastic Leukaemia (ALL) (experimental)" 
      | input$cancer == "Haematological malignancy: Acute Myeloid Leukaemia (AML) (experimental)" 
      | input$cancer == "Haematological malignancy: Chronic Lymphocytic Leukaemia (CLL) or Small Lymphocytic Lymphoma (SLL) (experimental)" 
      | input$cancer == "Haematological malignancy: Chronic Myeloid Leukaemia (CML) (experimental)" 
      | input$cancer == "Haematological malignancy: Diffuse Large B-Cell Lymphoma (DLBCL) and other high grade mature B-cell neoplasms (experimental)" 
      | input$cancer == "Haematological malignancy: Follicular Lymphoma (experimental)" 
      | input$cancer == "Haematological malignancy: Hodgkin Lymphoma (experimental)" 
      | input$cancer == "Haematological malignancy: Mantle Cell Lymphoma (MCL) (experimental)"
      | input$cancer == "Haematological malignancy: Myeloma (experimental)" 
      | input$cancer == "Haematological malignancy: Other haematological malignancies (experimental)"){    
    alliancedata = rbind(as.data.frame(achemo_frame), as.data.frame(aradio_frame))    
  }
    else {alliancedata = rbind(as.data.frame(achemo_frame), as.data.frame(asurgery_frame), as.data.frame(aradio_frame))}

  alliancedata[is.na(alliancedata)] <- 0
  alliancedata[alliancedata$ratio == 0, c("lowers", "uppers")] = NA
  
  # Create a variable which records that this is the alliance data 
  alliancedata$displayvar <- "Alliance"
  
  ###Create dataset with percentages for England 
  
  # Create the subset of the data based on drop-downs of cancer type alone -- aggregating across England   
  
  edata_by_year = data[data$cancergroup == input$cancer,]
  edata_by_year = edata_by_year[edata_by_year$year_range == input$year,]
  #edata_by_year = data[data$cancergroup == "Bladder",]
  edata_by_year$dummyvar <- 1

  
  # Calculate the percentages for the treatment types
  
  echemo_data = aggregate(tumour_count ~ dummyvar + ct_flag, data = edata_by_year, FUN = 'sum', drop = FALSE)
  echemo_numerators = echemo_data[echemo_data$ct_flag == "Yes",]$tumour_count
  echemo_totals = aggregate(tumour_count ~ dummyvar, data = edata_by_year, FUN = 'sum', drop = FALSE)
  echemo_denominators = echemo_totals$tumour_count
  echemo_frame = c(echemo_totals, get_percentages_and_confidence_intervals(echemo_numerators, echemo_denominators))
  echemo_frame$Treatment = "Chemotherapy"
  
  eradio_data = aggregate(tumour_count ~ dummyvar + rt_flag, data = edata_by_year, FUN = 'sum', drop = FALSE)
  eradio_numerators = eradio_data[eradio_data$rt_flag == "Yes",]$tumour_count
  eradio_totals = aggregate(tumour_count ~ dummyvar, data = edata_by_year, FUN = 'sum', drop = FALSE)
  eradio_denominators = eradio_totals$tumour_count
  eradio_frame = c(eradio_totals, get_percentages_and_confidence_intervals(eradio_numerators, eradio_denominators))
  eradio_frame$Treatment = "Radiotherapy"

    esurgery_data = aggregate(tumour_count ~ dummyvar + sg_flag, data = edata_by_year, FUN = 'sum', drop = FALSE)
    esurgery_numerators = esurgery_data[esurgery_data$sg_flag == "Yes",]$tumour_count
    esurgery_totals = aggregate(tumour_count ~dummyvar, data = edata_by_year, FUN = 'sum', drop = FALSE)
    esurgery_denominators = esurgery_totals$tumour_count
    esurgery_frame = c(esurgery_totals, get_percentages_and_confidence_intervals(esurgery_numerators, esurgery_denominators))
    esurgery_frame$Treatment = "Tumour resection"
    
if (input$cancer == "Other: Malignant neoplasms"| input$cancer == "Other: Non-malignant neoplasms"
    | input$cancer == "All haematological malignancies (experimental)"
    | input$cancer == "Haematological malignancy: Acute Lymphoblastic Leukaemia (ALL) (experimental)" 
    | input$cancer == "Haematological malignancy: Acute Myeloid Leukaemia (AML) (experimental)" 
    | input$cancer == "Haematological malignancy: Chronic Lymphocytic Leukaemia (CLL) or Small Lymphocytic Lymphoma (SLL) (experimental)" 
    | input$cancer == "Haematological malignancy: Chronic Myeloid Leukaemia (CML) (experimental)" 
    | input$cancer == "Haematological malignancy: Diffuse Large B-Cell Lymphoma (DLBCL) and other high grade mature B-cell neoplasms (experimental)" 
    | input$cancer == "Haematological malignancy: Follicular Lymphoma (experimental)" 
    | input$cancer == "Haematological malignancy: Hodgkin Lymphoma (experimental)" 
    | input$cancer == "Haematological malignancy: Mantle Cell Lymphoma (MCL) (experimental)"
    | input$cancer == "Haematological malignancy: Myeloma (experimental)" 
    | input$cancer == "Haematological malignancy: Other haematological malignancies (experimental)"){        
  englanddata = rbind(as.data.frame(echemo_frame), as.data.frame(eradio_frame))
} else {
  englanddata = rbind(as.data.frame(echemo_frame), as.data.frame(esurgery_frame), as.data.frame(eradio_frame))
}
  #  plottingdata$value = factor(plottingdata$value, levels = factor_levels)
  englanddata[englanddata$ratio == 0, c("lowers", "uppers")] = NA
  
  # Create a variable which records that this is the alliance data 
  englanddata$displayvar <- "England"
  
  ## Append the two datasets together  
  plottingdata = rbind(alliancedata, englanddata)  
  plottingdata$displayvar <- factor(plottingdata$displayvar, levels=c("Alliance", "England"))
  
  plottingdata$Treatment<- factor(plottingdata$Treatment, ordered = TRUE, levels = c("Chemotherapy", "Tumour resection", "Radiotherapy")) #Create ordered factor
  
  plottingdata$dummyvar <- NULL
  
  ##Now plot:

output = ggplot(plottingdata, aes(displayvar, ratio, fill = Treatment)) +
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(position = position_dodge(0.9), aes(ymin = lowers, ymax = uppers), width = 0.3) + 
    labs(x = "Geography", 
         title = ifelse((input$cancer=="All malignant neoplasms with surgery defined"
                        | input$cancer=="All non-malignant neoplasms with surgery defined"
                        | input$cancer == "Other: Malignant neoplasms"
                        | input$cancer == "Other: Non-malignant neoplasms"
                        | input$cancer == "All haematological malignancies (experimental)"
                        | input$cancer == "Haematological malignancy: Acute Lymphoblastic Leukaemia (ALL) (experimental)" 
                        | input$cancer == "Haematological malignancy: Acute Myeloid Leukaemia (AML) (experimental)" 
                        | input$cancer == "Haematological malignancy: Chronic Lymphocytic Leukaemia (CLL) or Small Lymphocytic Lymphoma (SLL) (experimental)" 
                        | input$cancer == "Haematological malignancy: Chronic Myeloid Leukaemia (CML) (experimental)" 
                        | input$cancer == "Haematological malignancy: Diffuse Large B-Cell Lymphoma (DLBCL) and other high grade mature B-cell neoplasms (experimental)" 
                        | input$cancer == "Haematological malignancy: Follicular Lymphoma (experimental)" 
                        | input$cancer == "Haematological malignancy: Hodgkin Lymphoma (experimental)" 
                        | input$cancer == "Haematological malignancy: Mantle Cell Lymphoma (MCL) (experimental)"
                        | input$cancer == "Haematological malignancy: Myeloma (experimental)" 
                        | input$cancer == "Haematological malignancy: Other haematological malignancies (experimental)"
                        | input$cancer == "Cholangiocarcinoma (experimental)"), 
                        paste(paste0(strwrap(gsub("Haematological malignancy: ","",input$cancer),60),collapse="\n"),"\n",input$alliance,", ",input$year, "\nTreatments are presented independently"),
                        paste0(input$cancer, " cancer","\n",input$alliance, ", ", input$year, "\nTreatments are presented independently")), 
         caption = ifelse(input$cancer %in% c("Skin: BCC (experimental)", "Skin: cSCC (experimental)"), 
                          paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership \n Figures for non-melanoma skin cancers are currently experimental and likely undercount surgical tumour resections"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership")))+
    scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = "Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
    #    scale_x_discrete(labels = c("Alliance", "England")) + 
    scale_fill_manual(name = "", values = c("Chemotherapy" = "#92D050", "Tumour resection" = "#00B0F0", "Radiotherapy" = "#FFC000")) +
    theme_classic(base_size = 15) +
    theme(legend.position="bottom", legend.text = element_text(size = 12) ) +
    theme(plot.caption=element_text(size=11, face="italic", hjust=0)) +
    theme(legend.position="bottom")
  
  return(output)
    

  }


#####Function for stacked treatment combinations graph

shovegraphintofunction2 = function(input) {
  
  ###Create dataset with percentages for alliance of interest based on drop-downs of cancer type and alliance    
  atreatment_interaction_data = data[data$cancergroup == input$cancer,]
  #atreatment_interaction_data = data[data$cancergroup == "Bladder",]
  atreatment_interaction_data = atreatment_interaction_data[atreatment_interaction_data$alliance == input$alliance,]
  #atreatment_interaction_data = atreatment_interaction_data[atreatment_interaction_data$alliance == "West Yorkshire",]
  atreatment_interaction_data = atreatment_interaction_data[atreatment_interaction_data$year_range == input$year,]
  #atreatment_interaction_data$dummyvar <- 1

  
  ##Create the combination variable
  
  atreatment_interaction_data$treatment_combinations = interaction(atreatment_interaction_data$ct_flag, atreatment_interaction_data$rt_flag, atreatment_interaction_data$sg_flag)
  
  atreatment_interaction_data$treatment_combinations = factor(atreatment_interaction_data$treatment_combinations, levels = 
                                                                c("No.No.No", "Yes.No.No", "No.No.Yes","No.Yes.No", "Yes.Yes.No", "Yes.No.Yes", "No.Yes.Yes", "Yes.Yes.Yes"))
  
  levels(atreatment_interaction_data$treatment_combinations) = list(
    "Other care" = "No.No.No", "Chemotherapy only" = "Yes.No.No", "Tumour resection only" = "No.No.Yes", "Radiotherapy only" = "No.Yes.No",
    "Chemotherapy and radiotherapy" = "Yes.Yes.No", "Tumour resection and chemotherapy" = "Yes.No.Yes",
    "Tumour resection and radiotherapy" = "No.Yes.Yes", "Tumour resection, radiotherapy and chemotherapy" = "Yes.Yes.Yes")
  
  #aggregate_denominators = aggregate(tumour_count ~ dummyvar, data = atreatment_interaction_data, FUN = 'sum')
  aggregate_denominators = aggregate(tumour_count ~ year_range, data = atreatment_interaction_data, FUN = 'sum')
  
  colnames(aggregate_denominators)[colnames(aggregate_denominators) == "tumour_count"] = "denominator"
  
  atreatment_interaction_data = merge(atreatment_interaction_data, aggregate_denominators)
  
  atreatments_and_ratios = as.data.frame(c(atreatment_interaction_data, 
                                           get_percentages_and_confidence_intervals(atreatment_interaction_data$tumour_count, atreatment_interaction_data$denominator)))
  atreatments_and_ratios = atreatments_and_ratios[atreatments_and_ratios$ratio > 0,]
  
  # Create a variable which records that this is the alliance data 
  #atreatments_and_ratios$displayvar <- "Alliance"
  #atreatments_and_ratios$dummyvar <- NULL
  #atreatments_and_ratios$rt_flag <- NULL
  #atreatments_and_ratios$ct_flag <- NULL
  #atreatments_and_ratios$sg_flag <- NULL
  #atreatments_and_ratios$alliance <- NULL
  
  atreatments_and_ratios = atreatments_and_ratios %>%
    select(year_range, treatment_combinations, cancergroup, tumour_count, denominator, ratio, uppers, lowers) %>%
    mutate(displayvar = "Alliance")
  
  ###Create dataset with percentages for England based on drop-downs of cancer type     
  etreatment_interaction_data = data[data$cancergroup == input$cancer,]
  #etreatment_interaction_data = data[data$cancergroup == "Bladder",]
  etreatment_interaction_data = etreatment_interaction_data[etreatment_interaction_data$year_range == input$year,]
  etreatment_interaction_data$dummyvar <- 1
  
  etreatment_interaction_data$treatment_combinations = interaction(etreatment_interaction_data$ct_flag, etreatment_interaction_data$rt_flag, etreatment_interaction_data$sg_flag)
  
  etreatment_interaction_data$treatment_combinations = factor(etreatment_interaction_data$treatment_combinations, levels = 
                                                                c("No.No.No", "Yes.No.No", "No.No.Yes","No.Yes.No", "Yes.Yes.No", "Yes.No.Yes", "No.Yes.Yes", "Yes.Yes.Yes"))
  
  levels(etreatment_interaction_data$treatment_combinations) = list(
    "Other care" = "No.No.No", "Chemotherapy only" = "Yes.No.No", "Tumour resection only" = "No.No.Yes", "Radiotherapy only" = "No.Yes.No",
    "Chemotherapy and radiotherapy" = "Yes.Yes.No", "Tumour resection and chemotherapy" = "Yes.No.Yes",
    "Tumour resection and radiotherapy" = "No.Yes.Yes", "Tumour resection, radiotherapy and chemotherapy" = "Yes.Yes.Yes")
  
  #etreatment_interaction_data2 = aggregate(tumour_count ~ treatment_combinations + cancergroup, data = etreatment_interaction_data, FUN='sum' )
  etreatment_interaction_data2 = aggregate(tumour_count ~ treatment_combinations + cancergroup + year_range, data = etreatment_interaction_data, FUN='sum' )
  
  #aggregate_denominators = aggregate(tumour_count ~ dummyvar, data = etreatment_interaction_data, FUN = 'sum')
  aggregate_denominators = aggregate(tumour_count ~ year_range, data = etreatment_interaction_data, FUN = 'sum')
  
  colnames(aggregate_denominators)[colnames(aggregate_denominators) == "tumour_count"] = "denominator"
  
  etreatment_interaction_data = merge(etreatment_interaction_data2, aggregate_denominators)
  
  etreatments_and_ratios = as.data.frame(c(etreatment_interaction_data, 
                                           get_percentages_and_confidence_intervals(etreatment_interaction_data$tumour_count, etreatment_interaction_data$denominator)))
  
  etreatments_and_ratios = etreatments_and_ratios[etreatments_and_ratios$ratio > 0,]
  
  # Create a variable which records that this is the alliance data 
  etreatments_and_ratios$displayvar <- "England"
  etreatments_and_ratios$dummyvar <- NULL  
  
  ## Append the two datasets together  
  treatments_and_ratios = rbind(atreatments_and_ratios, etreatments_and_ratios)  
  treatments_and_ratios$displayvar <- factor(treatments_and_ratios$displayvar, levels=c("Alliance", "England"))
  
  treatments_and_ratios$dummyvar <- NULL
  
  if (input$cancer == "Other: Malignant neoplasms"| input$cancer == "Other: Non-malignant neoplasms") {
    
    stacked_treatments_plot = ggplot(treatments_and_ratios, aes(displayvar, ratio, fill = treatment_combinations)) +
      labs(x = "", title = "Treatment combinations cannot be produced for this cancer site", caption = "This work has been produced as part of the Cancer Research UK - NHS England Partnership") +
      scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = " Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
      theme_classic(base_size = 15) +
      scale_x_discrete(labels = NULL) + 
      geom_blank()
  }
  else {
    
    stacked_treatments_plot = ggplot(treatments_and_ratios, aes(displayvar, ratio, fill = treatment_combinations)) +
      geom_bar(position = position_stack(reverse = TRUE),stat="identity", aes(fill = treatment_combinations)) +
      labs(x="Geography", title = ifelse((input$cancer=="All malignant neoplasms with surgery defined"
                                         | input$cancer=="All non-malignant neoplasms with surgery defined"
                                         | input$cancer == "Other: Malignant neoplasms"
                                         | input$cancer == "Other: Non-malignant neoplasms"
                                         | input$cancer == "All haematological malignancies (experimental)"
                                         | input$cancer == "Haematological malignancy: Acute Lymphoblastic Leukaemia (ALL) (experimental)" 
                                         | input$cancer == "Haematological malignancy: Acute Myeloid Leukaemia (AML) (experimental)" 
                                         | input$cancer == "Haematological malignancy: Chronic Lymphocytic Leukaemia (CLL) or Small Lymphocytic Lymphoma (SLL) (experimental)" 
                                         | input$cancer == "Haematological malignancy: Chronic Myeloid Leukaemia (CML) (experimental)" 
                                         | input$cancer == "Haematological malignancy: Diffuse Large B-Cell Lymphoma (DLBCL) and other high grade mature B-cell neoplasms (experimental)" 
                                         | input$cancer == "Haematological malignancy: Follicular Lymphoma (experimental)" 
                                         | input$cancer == "Haematological malignancy: Hodgkin Lymphoma (experimental)" 
                                         | input$cancer == "Haematological malignancy: Mantle Cell Lymphoma (MCL) (experimental)"
                                         | input$cancer == "Haematological malignancy: Myeloma (experimental)" 
                                         | input$cancer == "Haematological malignancy: Other haematological malignancies (experimental)"
                                         | input$cancer == "Cholangiocarcinoma (experimental)"),
                         paste(paste0(strwrap(gsub("Haematological malignancy: ","",input$cancer),60),collapse="\n"),"\n",input$alliance,", ",input$year, "\nTreatments are presented in combinations as a stacked bar chart"),
                         paste0(input$cancer, " cancer","\n",input$alliance, ", ", input$year, "\nTreatments are presented in combinations as a stacked bar chart")), 
             caption = ifelse(input$cancer %in% c("Skin: BCC (experimental)", "Skin: cSCC (experimental)"), 
                            paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership \n Figures for non-melanoma skin cancers are currently experimental and likely undercount surgical tumour resections"), paste("This work has been produced as part of the Cancer Research UK - NHS England Partnership")))+
      scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9, 1), labels = scales::percent, name = "Proportion of tumours (95% confidence interval)", limits = c(0, 1)) +
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




#############################################################################
### Define server logic required to output the graphs, and run the exports
#############################################################################


server <- function(input, output) {
  
  ##Display independent treatment graph:
  
  output$distPlot1 <- renderPlot({
    
    shovegraphintofunction1(input)
  }, height=813, units="px")
  
  ##Display combination treatment graph:
  
  output$distPlot2 <- renderPlot({
    
    shovegraphintofunction2(input)
  }, height=813, units="px")
  
  ##Download independent data 
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      if (input$cancer=="All malignant neoplasms with surgery defined" | 
          input$cancer == "All non-malignant neoplasms with surgery defined" | 
          input$cancer == "All defined non-malignant neoplasms" |
          input$cancer == "All malignant neoplasms with surgery defined, excluding BCC, cSSC and rare skin cancers" | 
          input$cancer == "Lung: Non-small cell lung cancer" | 
          input$cancer == "Lung: Small cell lung cancer" | 
          input$cancer == "Other: Malignant neoplasms" | 
          input$cancer == "Other: Non-malignant neoplasms" | 
          input$cancer == "Skin: BCC (experimental)" | 
          input$cancer == "Skin: cSCC (experimental)" | 
          input$cancer == "Skin: Rare" 
          | input$cancer == "Haematological malignancy: Follicular Lymphoma (experimental)"
          | input$cancer == "Haematological malignancy: Hodgkin Lymphoma (experimental)" 
          | input$cancer == "Haematological malignancy: Myeloma (experimental)" 
          | input$cancer == "Haematological malignancy: Other haematological malignancies (experimental)"
          | input$cancer == "Cholangiocarcinoma (experimental)") {
          paste('NCRAS-CRUK - Independent treatment data for ', sapply(gsub("Haematological malignancy: ", "",input$cancer),tolower), ' ',input$alliance, ' ',input$year,' ', Sys.Date(), '.csv', sep='') %>%
          str_remove(., ":")
      }  
      
      else if (input$cancer == "Haematological malignancy: Acute Lymphoblastic Leukaemia (ALL) (experimental)") {
        paste('NCRAS-CRUK - Independent treatment data for ', 'ALL (experimental)', ' ',input$alliance, ' ',input$year,' ', Sys.Date(), '.csv', sep='') %>%
        str_remove(., ":")
      }

      else if (input$cancer == "Haematological malignancy: Acute Myeloid Leukaemia (AML) (experimental)") {
        paste('NCRAS-CRUK - Independent treatment data for ', 'AML (experimental)', ' ',input$alliance, ' ',input$year,' ', Sys.Date(), '.csv', sep='') %>%
          str_remove(., ":")
      }
      
      else if (input$cancer == "Haematological malignancy: Chronic Lymphocytic Leukaemia (CLL) or Small Lymphocytic Lymphoma (SLL) (experimental)") {
        paste('NCRAS-CRUK - Independent treatment data for ', 'CLL or SLL (experimental)', ' ',input$alliance, ' ',input$year,' ', Sys.Date(), '.csv', sep='') %>%
          str_remove(., ":")
      }
      
      else if (input$cancer == "Haematological malignancy: Chronic Myeloid Leukaemia (CML) (experimental)") {
        paste('NCRAS-CRUK - Independent treatment data for ', 'CML (experimental)', ' ',input$alliance, ' ',input$year,' ', Sys.Date(), '.csv', sep='') %>%
          str_remove(., ":")
      }
      
      else if (input$cancer == "Haematological malignancy: Diffuse Large B-Cell Lymphoma (DLBCL) and other high grade mature B-cell neoplasms (experimental)") {
        paste('NCRAS-CRUK - Independent treatment data for ', 'DLBCL and other high grade mature B-cell neoplasms (experimental)', ' ',input$alliance, ' ',input$year,' ', Sys.Date(), '.csv', sep='') %>%
          str_remove(., ":")
      }
      
      else if (input$cancer == "Haematological malignancy: Mantle Cell Lymphoma (MCL) (experimental)") {
        paste('NCRAS-CRUK - Independent treatment data for ', 'MCL (experimental)', ' ',input$alliance, ' ',input$year,' ', Sys.Date(), '.csv', sep='') %>%
          str_remove(., ":")
      }
      
      else {
        paste('NCRAS-CRUK - Independent treatment data for ', sapply(input$cancer,tolower), ' cancer ',input$alliance, ' ',input$year,' ', Sys.Date(), '.csv', sep='') %>%
          str_remove(., ":")
      }
    },
    content = function(con) {
      
      ###Create dataset with percentages for alliance of interest  
      # Create the subset of the data based on drop-downs of cancer type and alliance    
      adata_by_year = data[data$cancergroup == input$cancer,]
      #adata_by_year = data[data$cancergroup == "Bladder",]
      adata_by_year = adata_by_year[adata_by_year$alliance == input$alliance,]
      #adata_by_year = adata_by_year[adata_by_year$alliance == "West Yorkshire",]
      adata_by_year = adata_by_year[adata_by_year$year_range == input$year,]
      adata_by_year$dummyvar <- 1

      
      # Calculate the percentages for the treatment types
      achemo_data = aggregate(tumour_count ~ dummyvar + ct_flag, data = adata_by_year, FUN = 'sum', drop = FALSE)
      achemo_numerators = achemo_data[achemo_data$ct_flag == "Yes",]$tumour_count
      achemo_totals = aggregate(tumour_count ~ dummyvar, data = adata_by_year, FUN = 'sum', drop = FALSE)
      achemo_denominators = achemo_totals$tumour_count
      achemo_frame = c(achemo_totals, get_percentages_and_confidence_intervals(achemo_numerators, achemo_denominators))
      achemo_frame$Treatment = "Chemotherapy"
      
      aradio_data = aggregate(tumour_count ~ dummyvar + rt_flag, data = adata_by_year, FUN = 'sum', drop = FALSE)
      aradio_numerators = aradio_data[aradio_data$rt_flag == "Yes",]$tumour_count
      aradio_totals = aggregate(tumour_count ~ dummyvar, data = adata_by_year, FUN = 'sum', drop = FALSE)
      aradio_denominators = aradio_totals$tumour_count
      aradio_frame = c(aradio_totals, get_percentages_and_confidence_intervals(aradio_numerators, aradio_denominators))
      aradio_frame$Treatment = "Radiotherapy"

       asurgery_data = aggregate(tumour_count ~ dummyvar + sg_flag, data = adata_by_year, FUN = 'sum', drop = FALSE)
        asurgery_numerators = asurgery_data[asurgery_data$sg_flag == "Yes",]$tumour_count
        asurgery_totals = aggregate(tumour_count ~dummyvar, data = adata_by_year, FUN = 'sum', drop = FALSE)
        asurgery_denominators = asurgery_totals$tumour_count
        asurgery_frame = c(asurgery_totals, get_percentages_and_confidence_intervals(asurgery_numerators, asurgery_denominators))
        asurgery_frame$Treatment = "Tumour resection"
      
        if (input$cancer == "Other: Malignant neoplasms" | input$cancer == "Other: Non-malignant neoplasms"
            | input$cancer == "All haematological malignancies (experimental)"
            | input$cancer == "Haematological malignancy: Acute Lymphoblastic Leukaemia (ALL) (experimental)" 
            | input$cancer == "Haematological malignancy: Acute Myeloid Leukaemia (AML) (experimental)" 
            | input$cancer == "Haematological malignancy: Chronic Lymphocytic Leukaemia (CLL) or Small Lymphocytic Lymphoma (SLL) (experimental)" 
            | input$cancer == "Haematological malignancy: Chronic Myeloid Leukaemia (CML) (experimental)" 
            | input$cancer == "Haematological malignancy: Diffuse Large B-Cell Lymphoma (DLBCL) and other high grade mature B-cell neoplasms (experimental)" 
            | input$cancer == "Haematological malignancy: Follicular Lymphoma (experimental)" 
            | input$cancer == "Haematological malignancy: Hodgkin Lymphoma (experimental)" 
            | input$cancer == "Haematological malignancy: Mantle Cell Lymphoma (MCL) (experimental)"
            | input$cancer == "Haematological malignancy: Myeloma (experimental)" 
            | input$cancer == "Haematological malignancy: Other haematological malignancies (experimental)"){        
          alliancedata = rbind(as.data.frame(achemo_frame), as.data.frame(aradio_frame))
        } else {
          alliancedata = rbind(as.data.frame(achemo_frame), as.data.frame(asurgery_frame), as.data.frame(aradio_frame))
        }  
          
      alliancedata[is.na(alliancedata)] <- 0
      alliancedata[alliancedata$ratio == 0, c("lowers", "uppers")] = NA
      # Create a variable which records that this is the alliance data 
      alliancedata$displayvar <- "Alliance"
      
      ###Create dataset with percentages for England 
      # Create the subset of the data based on drop-downs of cancer type alone -- aggregating across England   
      edata_by_year = data[data$cancergroup == input$cancer,]
      #edata_by_year = data[data$cancergroup == "Bladder",]
      edata_by_year = edata_by_year[edata_by_year$year_range == input$year,]
      edata_by_year$dummyvar <- 1

      
      # Calculate the percentages for the treatment types
      echemo_data = aggregate(tumour_count ~ dummyvar + ct_flag, data = edata_by_year, FUN = 'sum', drop = FALSE)
      echemo_numerators = echemo_data[echemo_data$ct_flag == "Yes",]$tumour_count
      echemo_totals = aggregate(tumour_count ~ dummyvar, data = edata_by_year, FUN = 'sum', drop = FALSE)
      echemo_denominators = echemo_totals$tumour_count
      echemo_frame = c(echemo_totals, get_percentages_and_confidence_intervals(echemo_numerators, echemo_denominators))
      echemo_frame$Treatment = "Chemotherapy"
      
      eradio_data = aggregate(tumour_count ~ dummyvar + rt_flag, data = edata_by_year, FUN = 'sum', drop = FALSE)
      eradio_numerators = eradio_data[eradio_data$rt_flag == "Yes",]$tumour_count
      eradio_totals = aggregate(tumour_count ~ dummyvar, data = edata_by_year, FUN = 'sum', drop = FALSE)
      eradio_denominators = eradio_totals$tumour_count
      eradio_frame = c(eradio_totals, get_percentages_and_confidence_intervals(eradio_numerators, eradio_denominators))
      eradio_frame$Treatment = "Radiotherapy"
      
        esurgery_data = aggregate(tumour_count ~ dummyvar + sg_flag, data = edata_by_year, FUN = 'sum', drop = FALSE)
        esurgery_numerators = esurgery_data[esurgery_data$sg_flag == "Yes",]$tumour_count
        esurgery_totals = aggregate(tumour_count ~dummyvar, data = edata_by_year, FUN = 'sum', drop = FALSE)
        esurgery_denominators = esurgery_totals$tumour_count
        esurgery_frame = c(esurgery_totals, get_percentages_and_confidence_intervals(esurgery_numerators, esurgery_denominators))
        esurgery_frame$Treatment = "Tumour resection"
#      }
        
        if (input$cancer == "Other: Malignant neoplasms" | input$cancer == "Other: Non-malignant neoplasms"
            | input$cancer == "All haematological malignancies (experimental)"
            | input$cancer == "Haematological malignancy: Acute Lymphoblastic Leukaemia (ALL) (experimental)" 
            | input$cancer == "Haematological malignancy: Acute Myeloid Leukaemia (AML) (experimental)" 
            | input$cancer == "Haematological malignancy: Chronic Lymphocytic Leukaemia (CLL) or Small Lymphocytic Lymphoma (SLL) (experimental)" 
            | input$cancer == "Haematological malignancy: Chronic Myeloid Leukaemia (CML) (experimental)" 
            | input$cancer == "Haematological malignancy: Diffuse Large B-Cell Lymphoma (DLBCL) and other high grade mature B-cell neoplasms (experimental)" 
            | input$cancer == "Haematological malignancy: Follicular Lymphoma (experimental)" 
            | input$cancer == "Haematological malignancy: Hodgkin Lymphoma (experimental)" 
            | input$cancer == "Haematological malignancy: Mantle Cell Lymphoma (MCL) (experimental)"
            | input$cancer == "Haematological malignancy: Myeloma (experimental)" 
            | input$cancer == "Haematological malignancy: Other haematological malignancies (experimental)"){        
          englanddata = rbind(as.data.frame(echemo_frame), as.data.frame(eradio_frame))
        } else {
          englanddata = rbind(as.data.frame(echemo_frame), as.data.frame(esurgery_frame), as.data.frame(eradio_frame))
        }         

      
      englanddata[englanddata$ratio == 0, c("lowers", "uppers")] = NA
      # Create a variable which records that this is the alliance data 
      englanddata$displayvar <- "England"
      
      ## Append the two datasets together  
      plottingdata = rbind(alliancedata, englanddata)  
      plottingdata$displayvar <- factor(plottingdata$displayvar, levels=c("Alliance", "England"))
      
      plottingdata$dummyvar <- NULL
      
      ## Rename the variable names for the output 
      
      plottingdata$cancer <- input$cancer
      plottingdata$year_range <- input$year
      
      names(plottingdata)[names(plottingdata) == 'tumour_count'] <- 'Number of tumours diagnosed'
      names(plottingdata)[names(plottingdata) == 'ratio'] <- 'Proportion of tumours'
      names(plottingdata)[names(plottingdata) == 'uppers'] <- 'Lower confidence interval'
      names(plottingdata)[names(plottingdata) == 'lowers'] <- 'Upper confidence interval'
      names(plottingdata)[names(plottingdata) == 'Treatment'] <- 'Treatment modality'
      names(plottingdata)[names(plottingdata) == 'displayvar'] <- 'Geography'
      names(plottingdata)[names(plottingdata) == 'cancer'] <- 'Cancer type'
      names(plottingdata)[names(plottingdata) == 'year_range'] <- 'Year of diagnosis'
      # names(plottingdata)[names(plottingdata) == 'value'] <- paste('Levels of ',sapply(input$factor, tolower))
      
      plottingdata <- plottingdata[, c(7,8,5,6,1,2,4,3)]
      
      write.csv(plottingdata, con, row.names=FALSE)
    }
  )
  
  ##Download treatment combination data 
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      #if (input$cancer=="All malignant neoplasms" | input$cancer == "Other: Malignant neoplasms") {
      if (input$cancer=="All malignant neoplasms with surgery defined" | 
          input$cancer == "All non-malignant neoplasms with surgery defined" | 
          input$cancer == "All defined non-malignant neoplasms" | 
          input$cancer == "All malignant neoplasms with surgery defined, excluding BCC, cSSC and rare skin cancers" | 
          input$cancer == "Lung: Non-small cell lung cancer" | 
          input$cancer == "Lung: Small cell lung cancer" | 
          input$cancer == "Other: Malignant neoplasms" | 
          input$cancer == "Other: Non-malignant neoplasms" | 
          input$cancer == "Skin: BCC (experimental)" | 
          input$cancer == "Skin: cSCC (experimental)" | 
          input$cancer == "Skin: Rare"
          | input$cancer == "Haematological malignancy: Follicular Lymphoma (experimental)"
          | input$cancer == "Haematological malignancy: Hodgkin Lymphoma (experimental)" 
          | input$cancer == "Haematological malignancy: Myeloma (experimental)" 
          | input$cancer == "Haematological malignancy: Other haematological malignancies (experimental)"
          | input$cancer == "Cholangiocarcinoma (experimental)") {
        paste('NCRAS-CRUK - Treatment combination data for ', sapply(gsub("Haematological malignancy: ", "",input$cancer),tolower), ' ',input$alliance, ' ',input$year,' ', Sys.Date(), '.csv', sep='') %>%
          str_remove(., ":")
        
      }
      
      else if (input$cancer == "Haematological malignancy: Acute Lymphoblastic Leukaemia (ALL) (experimental)") {
        paste('NCRAS-CRUK - Treatment combination data for ', 'ALL (experimental)', ' ',input$alliance, ' ',input$year,' ', Sys.Date(), '.csv', sep='') %>%
          str_remove(., ":")
      }
      
      else if (input$cancer == "Haematological malignancy: Acute Myeloid Leukaemia (AML) (experimental)") {
        paste('NCRAS-CRUK - Treatment combination data for ', 'AML (experimental)', ' ',input$alliance, ' ',input$year,' ', Sys.Date(), '.csv', sep='') %>%
          str_remove(., ":")
      }
      
      else if (input$cancer == "Haematological malignancy: Chronic Lymphocytic Leukaemia (CLL) or Small Lymphocytic Lymphoma (SLL) (experimental)") {
        paste('NCRAS-CRUK - Treatment combination data for ', 'CLL or SLL (experimental)', ' ',input$alliance, ' ',input$year,' ', Sys.Date(), '.csv', sep='') %>%
          str_remove(., ":")
      }
      
      else if (input$cancer == "Haematological malignancy: Chronic Myeloid Leukaemia (CML) (experimental)") {
        paste('NCRAS-CRUK - Treatment combination data for ', 'CML (experimental)', ' ',input$alliance, ' ',input$year,' ', Sys.Date(), '.csv', sep='') %>%
          str_remove(., ":")
      }
      
      else if (input$cancer == "Haematological malignancy: Diffuse Large B-Cell Lymphoma (DLBCL) and other high grade mature B-cell neoplasms (experimental)") {
        paste('NCRAS-CRUK - Treatment combination data for ', 'DLBCL and other high grade mature B-cell neoplasms (experimental)', ' ',input$alliance, ' ',input$year,' ', Sys.Date(), '.csv', sep='') %>%
          str_remove(., ":")
      }
      
      else if (input$cancer == "Haematological malignancy: Mantle Cell Lymphoma (MCL) (experimental)") {
        paste('NCRAS-CRUK - Treatment combination data for ', 'MCL (experimental)', ' ',input$alliance, ' ',input$year,' ', Sys.Date(), '.csv', sep='') %>%
          str_remove(., ":")
      }
      
      
      else {
        paste('NCRAS-CRUK - Treatment combination data for ', sapply(input$cancer,tolower), ' cancer ',input$alliance, ' ',input$year,' ', Sys.Date(), '.csv', sep='') %>%
          str_remove(., ":")
      }
    },
    content = function(con) {
      
      
      ###Create dataset with percentages for alliance of interest based on drop-downs of cancer type and alliance    
      atreatment_interaction_data = data[data$cancergroup == input$cancer,]
      #atreatment_interaction_data = data[data$cancergroup == "Bladder",]
      atreatment_interaction_data = atreatment_interaction_data[atreatment_interaction_data$alliance == input$alliance,]
      #atreatment_interaction_data = atreatment_interaction_data[atreatment_interaction_data$alliance == "West Yorkshire",]
      atreatment_interaction_data = atreatment_interaction_data[atreatment_interaction_data$year_range == input$year,]
      atreatment_interaction_data$dummyvar <- 1
      
      
      atreatment_interaction_data$treatment_combinations = interaction(atreatment_interaction_data$ct_flag, atreatment_interaction_data$rt_flag, atreatment_interaction_data$sg_flag)
      
      atreatment_interaction_data$treatment_combinations = factor(atreatment_interaction_data$treatment_combinations, levels = 
                                                                    c("No.No.No", "Yes.No.No", "No.No.Yes","No.Yes.No", "Yes.Yes.No", "Yes.No.Yes", "No.Yes.Yes", "Yes.Yes.Yes"))
      
      levels(atreatment_interaction_data$treatment_combinations) = list(
        "Other care" = "No.No.No", "Chemotherapy only" = "Yes.No.No", "Tumour resection only" = "No.No.Yes", "Radiotherapy only" = "No.Yes.No",
        "Chemotherapy and radiotherapy" = "Yes.Yes.No", "Tumour resection and chemotherapy" = "Yes.No.Yes",
        "Tumour resection and radiotherapy" = "No.Yes.Yes", "Tumour resection, radiotherapy and chemotherapy" = "Yes.Yes.Yes")
      
      aggregate_denominators = aggregate(tumour_count ~ dummyvar, data = atreatment_interaction_data, FUN = 'sum')
      
      colnames(aggregate_denominators)[colnames(aggregate_denominators) == "tumour_count"] = "denominator"
      
      atreatment_interaction_data = merge(atreatment_interaction_data, aggregate_denominators)
      
      atreatments_and_ratios = as.data.frame(c(atreatment_interaction_data, 
                                               get_percentages_and_confidence_intervals(atreatment_interaction_data$tumour_count, atreatment_interaction_data$denominator)))
      atreatments_and_ratios = atreatments_and_ratios[atreatments_and_ratios$ratio > 0,]
      
      # Create a variable which records that this is the alliance data 
      atreatments_and_ratios$displayvar <- "Alliance"
      atreatments_and_ratios$dummyvar <- NULL
      atreatments_and_ratios$rt_flag <- NULL
      atreatments_and_ratios$ct_flag <- NULL
      atreatments_and_ratios$sg_flag <- NULL
      atreatments_and_ratios$alliance <- NULL
      
      ###Create dataset with percentages for England based on drop-downs of cancer type     
      etreatment_interaction_data = data[data$cancergroup == input$cancer,]
      #etreatment_interaction_data = data[data$cancergroup == "Bladder",]
      etreatment_interaction_data = etreatment_interaction_data[etreatment_interaction_data$year_range == input$year,]
      etreatment_interaction_data$dummyvar <- 1
      
      
      etreatment_interaction_data$treatment_combinations = interaction(etreatment_interaction_data$ct_flag, etreatment_interaction_data$rt_flag, etreatment_interaction_data$sg_flag)
      
      etreatment_interaction_data$treatment_combinations = factor(etreatment_interaction_data$treatment_combinations, levels = 
                                                                    c("No.No.No", "Yes.No.No", "No.No.Yes","No.Yes.No", "Yes.Yes.No", "Yes.No.Yes", "No.Yes.Yes", "Yes.Yes.Yes"))
      
      levels(etreatment_interaction_data$treatment_combinations) = list(
        "Other care" = "No.No.No", "Chemotherapy only" = "Yes.No.No", "Tumour resection only" = "No.No.Yes", "Radiotherapy only" = "No.Yes.No",
        "Chemotherapy and radiotherapy" = "Yes.Yes.No", "Tumour resection and chemotherapy" = "Yes.No.Yes",
        "Tumour resection and radiotherapy" = "No.Yes.Yes", "Tumour resection, radiotherapy and chemotherapy" = "Yes.Yes.Yes")
      
      etreatment_interaction_data2 = aggregate(tumour_count ~ treatment_combinations + cancergroup, data = etreatment_interaction_data, FUN='sum' )
      
      aggregate_denominators = aggregate(tumour_count ~ dummyvar, data = etreatment_interaction_data, FUN = 'sum')
      
      colnames(aggregate_denominators)[colnames(aggregate_denominators) == "tumour_count"] = "denominator"
      
      etreatment_interaction_data = merge(etreatment_interaction_data2, aggregate_denominators)
      
      etreatments_and_ratios = as.data.frame(c(etreatment_interaction_data, 
                                               get_percentages_and_confidence_intervals(etreatment_interaction_data$tumour_count, etreatment_interaction_data$denominator)))
      
      etreatments_and_ratios = etreatments_and_ratios[etreatments_and_ratios$ratio > 0,]
      
      # Create a variable which records that this is the alliance data 
      etreatments_and_ratios$displayvar <- "England"
      etreatments_and_ratios$dummyvar <- NULL  
      etreatments_and_ratios$year_range<-input$year
      
      ## Append the two datasets together  
      treatments_and_ratios = rbind(atreatments_and_ratios, etreatments_and_ratios)  
      treatments_and_ratios$displayvar <- factor(treatments_and_ratios$displayvar, levels=c("Alliance", "England"))
      #treatments_and_ratios$year_range <- input$year
      
      ## Rename the variable names for the output 
      
      names(treatments_and_ratios)[names(treatments_and_ratios) == 'tumour_count'] <- 'Number of tumours treated'
      names(treatments_and_ratios)[names(treatments_and_ratios) == 'denominator'] <- 'Number of tumours diagnosed'
      names(treatments_and_ratios)[names(treatments_and_ratios) == 'ratio'] <- 'Proportion of tumours'
      names(treatments_and_ratios)[names(treatments_and_ratios) == 'uppers'] <- 'Lower confidence interval'
      names(treatments_and_ratios)[names(treatments_and_ratios) == 'lowers'] <- 'Upper confidence interval'
      names(treatments_and_ratios)[names(treatments_and_ratios) == 'treatment_combinations'] <- 'Treatment modality'
      names(treatments_and_ratios)[names(treatments_and_ratios) == 'cancergroup'] <- 'Cancer type'
      names(treatments_and_ratios)[names(treatments_and_ratios) == 'displayvar'] <- 'Geography'
      names(treatments_and_ratios)[names(treatments_and_ratios) == 'year_range'] <- 'Year of diagnosis'
      
      treatments_and_ratios <- treatments_and_ratios[, c(1,9,3,4,5,2,6,8,7)]
      
      if (input$cancer == "Other: Malignant neoplasms" | input$cancer == "Other: Non-malignant neoplasms") {
        
        treatments_and_ratios$'Number of tumours treated' <- NULL
        treatments_and_ratios$'Number of tumours diagnosed' <- NULL
        treatments_and_ratios$'Proportion of tumours' <- NULL
        treatments_and_ratios$'Lower confidence interval' <- NULL
        treatments_and_ratios$'Upper confidence interval' <- NULL
        treatments_and_ratios$'Treatment modality' <- NULL
        treatments_and_ratios$'Cancer type' <- NULL
        treatments_and_ratios$'Geography' <- NULL
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
      paste("NCRAS-CRUK - Treatment raw data for all malignant cancers ",min(years),"-",max(years),' ', Sys.Date(), ".csv", sep='') 
    },
    content = function(con) {
      
      tempdata <- data
      
      names(tempdata)[names(tempdata) == 'tumour_count'] <- 'Number of tumours treated'
      names(tempdata)[names(tempdata) == 'ratio'] <- 'Proportion of tumours'
      names(tempdata)[names(tempdata) == 'rt_flag'] <- 'Radiotherapy (Y/N)'
      names(tempdata)[names(tempdata) == 'ct_flag'] <- 'Chemotherapy (Y/N)'
      names(tempdata)[names(tempdata) == 'sg_flag'] <- 'Tumour Resection (Y/N)'
      names(tempdata)[names(tempdata) == 'factor'] <- 'Factor of interest'
      names(tempdata)[names(tempdata) == 'cancergroup'] <- 'Cancer type'
      names(tempdata)[names(tempdata) == 'alliance'] <- 'Cancer Alliance'  
      names(tempdata)[names(tempdata) == 'year_range'] <- 'Year of diagnosis'  
      
      tempdata <- tempdata[!tempdata$'Cancer type'=="All defined malignant neoplasms",]
      
      tempdata <- tempdata[, c(1,7,2,3,4,5,6)]
      
      write.csv(tempdata, con, row.names=FALSE)
    }
  )  
  
  output$downloadGraph1 <- downloadHandler(
    filename = function() {
      if (input$cancer=="All malignant neoplasms with surgery defined" | 
          input$cancer == "All non-malignant neoplasms with surgery defined" | 
          input$cancer == "All malignant neoplasms with surgery defined, excluding BCC, cSSC and rare skin cancers" | 
          input$cancer == "All defined non-malignant neoplasms" |
          input$cancer == "Other: Malignant neoplasms" | 
          input$cancer == "Other: Non-malignant neoplasms" | 
          input$cancer == "Lung: Non-small cell lung cancer" | 
          input$cancer == "Lung: Small cell lung cancer" | 
          input$cancer == "Other: Malignant neoplasms" | 
          input$cancer == "Other: Non-malignant neoplasms" | 
          input$cancer == "Skin: BCC (experimental)" | 
          input$cancer == "Skin: cSCC (experimental)" | 
          input$cancer == "Skin: Rare"| 
          input$cancer == "Haematological malignancy: Acute Lymphoblastic Leukaemia (ALL) (experimental)" 
          | input$cancer == "Haematological malignancy: Acute Myeloid Leukaemia (AML) (experimental)" 
          | input$cancer == "Haematological malignancy: Chronic Lymphocytic Leukaemia (CLL) or Small Lymphocytic Lymphoma (SLL) (experimental)" 
          | input$cancer == "Haematological malignancy: Chronic Myeloid Leukaemia (CML) (experimental)" 
          | input$cancer == "Haematological malignancy: Diffuse Large B-Cell Lymphoma (DLBCL) and other high grade mature B-cell neoplasms (experimental)" 
          | input$cancer == "Haematological malignancy: Follicular Lymphoma (experimental)"
          | input$cancer == "Haematological malignancy: Hodgkin Lymphoma (experimental)" 
          | input$cancer == "Haematological malignancy: Mantle Cell Lymphoma (MCL) (experimental)"
          | input$cancer == "Haematological malignancy: Myeloma (experimental)" 
          | input$cancer == "Haematological malignancy: Other haematological malignancies (experimental)"
          | input$cancer == "Cholangiocarcinoma (experimental)") {
        paste('NCRAS-CRUK - Independent treatment graph for ', sapply(gsub("Haematological malignancy: ", "",input$cancer),tolower), ' ',input$alliance,' ',input$year,' ', Sys.Date(), '.tiff', sep='') %>%
          str_remove(., ":")
        
      }
      else {
        paste('NCRAS-CRUK - Independent treatment graph for ', sapply(input$cancer,tolower), ' cancer ',input$alliance,' ',input$year,' ', Sys.Date(), '.tiff', sep='') %>%
          str_remove(., ":")
      }
    },
    content = function(file) {
      
      
      ggsave(file,shovegraphintofunction1(input), dpi=300,width = 45, height = 25, units = "cm")
    }
  )
  
  output$downloadGraph2 <- downloadHandler(
    filename = function() {
      if (input$cancer=="All malignant neoplasms with surgery defined" | 
          input$cancer == "All non-malignant neoplasms with surgery defined" | 
          input$cancer == "All defined non-malignant neoplasms" | 
          input$cancer == "All malignant neoplasms with surgery defined, excluding BCC, cSSC and rare skin cancers" | 
          input$cancer == "Lung: Non-small cell lung cancer" | 
          input$cancer == "Lung: Small cell lung cancer" | 
          input$cancer == "Skin: BCC (experimental)" | 
          input$cancer == "Skin: cSCC (experimental)" | 
          input$cancer == "Skin: Rare"| 
          input$cancer == "Haematological malignancy: Acute Lymphoblastic Leukaemia (ALL) (experimental)" 
          | input$cancer == "Haematological malignancy: Acute Myeloid Leukaemia (AML) (experimental)" 
          | input$cancer == "Haematological malignancy: Chronic Lymphocytic Leukaemia (CLL) or Small Lymphocytic Lymphoma (SLL) (experimental)" 
          | input$cancer == "Haematological malignancy: Chronic Myeloid Leukaemia (CML) (experimental)" 
          | input$cancer == "Haematological malignancy: Diffuse Large B-Cell Lymphoma (DLBCL) and other high grade mature B-cell neoplasms (experimental)" 
          | input$cancer == "Haematological malignancy: Follicular Lymphoma (experimental)"
          | input$cancer == "Haematological malignancy: Hodgkin Lymphoma (experimental)" 
          | input$cancer == "Haematological malignancy: Mantle Cell Lymphoma (MCL) (experimental)"
          | input$cancer == "Haematological malignancy: Myeloma (experimental)" 
          | input$cancer == "Haematological malignancy: Other haematological malignancies (experimental)"
          | input$cancer == "Cholangiocarcinoma (experimental)") {
        paste('NCRAS-CRUK - Treatment combination graph for ', sapply(gsub("Haematological malignancy: ", "",input$cancer),tolower),' ',input$alliance,' ',input$year,' ', Sys.Date(), '.tiff', sep='') %>%
          str_remove(., ":")
        
      }
      else {
        paste('NCRAS-CRUK - Treatment combination graph for ', sapply(input$cancer,tolower), ' cancer ',input$alliance,' ',input$year,' ', Sys.Date(), '.tiff', sep='') %>%
          str_remove(., ":")
      }
    },
    content = function(file) {
      
      
      ggsave(file,shovegraphintofunction2(input), dpi=300,width = 45, height = 25, units = "cm")
    }
  )
  
  ##Create explanatory text for explanation tab
  
  output$text <- renderUI({
    str0 <- strong("Introduction")
    str1 <- p("This work aims to provide basic information on the percentage of tumours receiving chemotherapy, radiotherapy and / or a surgical tumour resection as part of the primary course of treatment following diagnosis.")
    str2 <- p("These statistics on cancer treatments aim to support the understanding of how patients are treated for their cancer diagnosis and how this varies between cancer sites. This understanding is vital to assess variation and make improvements to treatment pathways.")
    str3 <- strong("Methodology")
    str4 <- p("The methodology is described in the standard operating procedure ", a("CAS-SOP v4.9 Linking treatment tables: chemotherapy, tumour resections, and radiotherapy.", href="https://digital.nhs.uk/ndrs/data/data-outputs/cancer-data-hub/cancer-treatments"), em("(Please note this link may not load in all browsers)."))
    str5 <- strong("Datasets:")
    str6 <- p(paste0("Patients diagnosed with cancer in England in ",min(years),"-",max(years),", excluding males with gynaecological cancer and females with prostate cancer, were selected from the National Cancer Registration and Analysis dataset. Death certificate only registrations are included (1% of the cohort)."))
    str7 <- p("Datasets used to capture treatment information include cancer registration data, the Systemic Anti-Cancer Therapy dataset (SACT), RadioTherapy DataSet (RTDS), and inpatient and outpatient Hospital Episode Statistics (HES).")
    str8 <- p("Where a patient has been diagnosed with one or with multiple cancers within 18 months, then all of the above treatment datasets are used to identify treatment data. However, if the patient has been diagnosed with multiple cancers within 18 months then only the tumour linked cancer registration treatment data has been used. This is because SACT, RTDS and HES can only be linked at patient level so the precise tumour that a treatment relates to is not identified, only the person.")
    str9 <- strong("Treatment definitions:")
    str10 <- p("A tumour resection is an attempt to surgically remove the whole of the primary tumour. These have been identified using OPCS-4 codes through consultation with site-specific clinicians. For the following cancer sites, some procedures (for example, endoscopic resections) have been identified as tumour resections in early-stage disease only: colon, rectum, cervical, bladder, stomach, oesophagus and liver excluding intrahepatic cholangiocarcinoma.")
    #        str13 <- p("Tumour resections have been defined for 48 malignant sites and 4 benign sites.")
    str11 <- p("Surgical tumour resections are not defined for all cancer sites. Only sites which have a defined list of tumour resection procedures will be grouped. Surgical tumour resections have not been defined for any of the cancer sites included in the 'Other: Malignant neoplasms', 'Other: Non-malignant neoplasms' or any of the 'Haematological malignancy' groups. However, some of these tumours could have been treated with surgery.")
    str12 <- p("Radiotherapy includes both curative and palliative teletherapy procedures, and brachytherapy, and excludes contact radiotherapy")
    str13 <- p("Chemotherapy includes both curative and palliative chemotherapy, and excludes hormonal therapy, and other supportive drugs such as zoledronic acid, pamidronate, and denosumab. Chemotherapy treatment data is extracted from HES inpatient and outpatient data, in addition to NCRD and SACT.")
    str14 <- p("Other care represents the group of patients who had no record of chemotherapy, tumour resection, or radiotherapy in the time frame assessed. This may include patients who received other treatments (such as hormonal therapy or management of symptoms), treatment outside of the time frame assessed, treatment in a private setting, or there may be data missing from the datasets used. Other care for haematological subsites may also include patients who have undergone tumour resection treatment as surgery has not yet been defined for these cancers.")
    str15 <- strong("Time period:")
    str16 <- p(paste("Treatments occurring in the period from 1 month before diagnosis to either 6, 9, 12, 15 or 18 months after diagnosis are displayed for tumours diagnosed in ",min(years),"-",max(years),". The time period within which the majority of first course of treatments occurred varies by cancer site and treatment type. Therefore, an appropriate time period for each cancer site has been chosen using a data-driven approach in consultation with clinicians."))
    str16a <- p("For more information, and a sensitivity analysis showing the effect of varying the time periods, see", a("CAS-SOP v4.9 Linking treatment tables: chemotherapy, tumour resections, and radiotherapy.", href="https://digital.nhs.uk/ndrs/data/data-outputs/cancer-data-hub/cancer-treatments"))
    str17 <- strong("Known issues:")
    str18 <- p("For pancreatic tumours, a much lower proportion of early-stage tumours are recorded to have been resected than expected. Feedback from clinical experts highlighted that this does not fit with clinical experience, so further investigation is needed to understand whether all resections are being captured by the data and methodology.")
    str19 <- p("Figures for non-melanoma skin cancers (NMSC) are currently experimental and likely to be undercounting surgical tumour resections.")
    str20 <- strong("How to use the app:")
    str21 <- p("The unadjusted proportion of tumours receiving each treatment, and combination of treatments, is presented.")
    str22 <- p("Independent treatments: The proportion of tumours receiving radiotherapy, chemotherapy and / or tumour resection presented independently.")
    str23 <- p("Treatment combinations: The proportion of tumours receiving radiotherapy, chemotherapy and / or tumour resection presented as the following combinations:")
    str24 <- p(tags$ol(
      tags$li("Tumour resection, radiotherapy and chemotherapy"), 
      tags$li("Tumour resection and radiotherapy"), 
      tags$li("Tumour resection and chemotherapy"),
      tags$li("Chemotherapy and radiotherapy"),
      tags$li("Radiotherapy only"),
      tags$li("Tumour resection only"),
      tags$li("Chemotherapy only"),
      tags$li("Other care")
    ))       
    str25 <- p("The data may be viewed broken down by cancer site, year of diagnosis and cancer alliance.")
    str26 <- p("Please select from the drop-down lists in the sidebar on the left-hand side of this screen to view graphs for a specific cancer site, year of diagnosis, and cancer alliance. Select ‘year of cancer diagnosis’ as the factor of interest if you would do not wish to see the data presented by any of the breakdowns.")
    str27 <- p("Select the ‘Overall’ tab to view the proportion of tumours receiving each of the treatments, presented independently and combined, for each cancer site and year of diagnosis. The data on these tabs is not displayed by the factor of interest.")
    str28 <- p("Select the ‘Independent treatment’ tab to view the proportion of tumours receiving radiotherapy, chemotherapy and / or tumour resection, presented by the selected cancer site and year of diagnosis and factor of interest.")
    str29 <- p("Select the ‘Stacked Treatment Combinations’ tab to view the proportion of tumours receiving radiotherapy, chemotherapy and / or tumour resection combinations, presented by the selected cancer site and year of diagnosis and factor of interest as a stacked bar chart, or the ‘Treatment Combinations’ tab to view this as an unstacked bar chart.  Note that treatment combinations for haematological malignancies are only based on radiotherapy and chemotherapy, and do not include tumour resection.")
    str30 <- p("Download the data for each of the graphs, and a TIFF image of each of the graphs using the links in the sidebar on the left hand side of this screen.")
    str31 <- strong("Feedback and support")
    str32 <- p("This tool is produced by the National Disease Registration Service (NDRS), as part of the Cancer Research UK - NHS England Partnership.")
    str33 <- p(" Please send any feedback or queries to ndrsenquiries@nhs.net. Please do not include sensitive or patient identifiable information.")
    
    HTML(paste(str0, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, 
               str11, str12, str13, str14, str15, str16, str16a, str17, str18, str19, str20, 
               str21, str22, str23, str24, str25, str26, str27, str28, str29,str30, 
               str31, str32, str33, sep = ' '))
  })
  
  
  
}


#############################################################################
# Run the application 
#############################################################################
shinyApp(ui = ui, server = server)


