## Extracting data per cluster from the household recode.
############################################################################################################

### Functions

get_data <- function(cc = "AO", year = 2010){
  # Selects desired surveys
  survs <- dhs_surveys(countryIds = c(cc), surveyYearStart = year, surveyCharacteristicIds =c(89, 8))

  # Selects the desired datasets
  pr_dataset <- dhs_datasets(surveyIds = survs$SurveyId, 
                             fileFormat = "FL", 
                             fileType = "PR")

  # Downloads the chosen datasets
  downloads_pr <- get_datasets(pr_dataset$FileName)

  # Selects the variables that want to investigate
  pr_questions <- search_variables(pr_dataset$FileName, 
                                   variables = c("hv002", "hv005", "hv103", "hml12", "hml20",
                                                 "hv253", "hv253a", "hv253b", "hv253c", 
                                                 "shmlweight", "hv103", "hml16a", 
                                                 "hml35", "hml32", "hc17", "hc18", "hc19", "hc1"))

  # Extracts the data
  extract_pr <- extract_dhs(pr_questions, add_geo = TRUE)
  
  return (extract_pr)
}


extract_info <- function(data){

  # Percentage people who slept under a LLIN
  all_net <- data[which(data$hml20 == 1),]
  num_nets <- sum(all_net$hv005/1e6)
  num_all <- sum(data$hv005/1e6)
  llin_usage <- num_nets / num_all
  
  #---------------------------------------------------------------------------------------
  # Prevalence
  # Select kids 6-59 months
  admin_kids <- data[which(data$hml16a >= 6 & data$hml16a <= 59),]
  
  # Split kids on if slept under a net
  kids_net <- admin_kids[which(admin_kids$hml20 == 1),]
  kids_no_net <- admin_kids[which(admin_kids$hml20 != 1),]
  
  # RDT data - all
  # Selects only positive results
  RDT_positive <- admin_kids[which(admin_kids$hml35 == 1),]
  # Selects those tested
  RDT_all <- admin_kids[which(admin_kids$hml35 >= 0 & admin_kids$hml35 <= 1),]
  
  num_rdt_positive <- sum(RDT_positive$hv005/1e6) 
  num_rdt <- sum(RDT_all$hv005/1e6)
  rdt_prev <- num_rdt_positive / num_rdt
  
  return (c("usage" = llin_usage, "prev" = rdt_prev, 
            "num_users"  = length(kids_net$hv002), 
            "num_non_users"  = length(kids_no_net$hv002)))
}


link_data <- function(country_data, admin_data, cluster_data, row){

  country_info <- country_data[which(country_data$unit == row[1]),]
  admin_info <- admin_data[which(admin_data$unit == row[2]),]
  cluster_info <- cluster_data[which(cluster_data$unit == as.integer(row[3])),]
  
  row_data <- data.frame("country_usage" = country_info$usage, 
                         "country_prev" = country_info$prev,
                         "admin1_usage" = admin_info$usage,
                         "admin1_prev" = admin_info$prev,
                         "cluster_usage" = cluster_info$usage,
                         "cluster_prev" = cluster_info$prev, 
                         "num_users" = cluster_info$num_users, 
                         "num_non_users" = cluster_info$num_non_users, 
                         "num_people" = cluster_info$num_people)

  return (row_data)
}


############################################################################################################

# Imports the libraries needed 
library(rdhs)
library(ggplot2)
library(plyr)
library(cowplot)
library(readr)
library(lubridate)

cc <- "UG"
year <- 2010

extract_pr <- get_data(cc = cc, year = year)

for (c in 1:length(extract_pr)){

  # Label the data for the current year
  extract_pr_bound <- rbind_labelled(extract_pr[c])
  
  # Get survey year
  survey_year <- year(as.Date(substr((extract_pr_bound$SurveyId[1]), 3, 6), format = "%Y"))
  print(survey_year)
  
  ## Compute country level info
  country_info <- extract_info(extract_pr_bound)
  
  country_data <- data.frame("unit" = cc, "year" = year, "usage" = country_info[1],
                             "prev" = country_info[2])
  
  #-------------------------------------------------------------------------------
  ## Compute admin level info
  admins <- unique(extract_pr_bound$ADM1NAME)
  
  # Initialises empty vectors
  admin_usage <- vector(length = length(admins))
  admin_prev <- vector(length = length(admins))
  
  for (idx in 1:length(admins)){
    # Selects people from each admin region who slept there last night
    admin_data <- extract_pr_bound[which(extract_pr_bound$ADM1NAME == admins[idx] 
                                         & extract_pr_bound$hv103 == 1),]
    admin_info <- extract_info(admin_data)
    
    admin_usage[idx] <- admin_info[1]
    admin_prev[idx] <- admin_info[2]
  }
  
  admin_data <- data.frame("unit" = admins, "year" = year, "usage" = admin_usage, 
                           "prev" = admin_prev)
  
  #-------------------------------------------------------------------------------
  ## Compute cluster level info
  clusters <- unique(extract_pr_bound$CLUSTER)
  
  # Initialises empty vectors
  cluster_usage <- vector(length = length(clusters))
  cluster_prev <- vector(length = length(clusters))
  num_users <- vector(length = length(clusters))
  num_non_users <- vector(length = length(clusters))
  num_people <- vector(length = length(clusters))
  
  for (idx in 1:length(clusters)){
    # Selects people from each admin region who slept there last night
    cluster_sub_data <- extract_pr_bound[which(extract_pr_bound$CLUSTER == clusters[idx] 
                                         & extract_pr_bound$hv103 == 1),]
    cluster_info <- extract_info(cluster_sub_data)
    
    cluster_usage[idx] <- cluster_info[1]
    cluster_prev[idx] <- cluster_info[2]
    
    num_users[idx] <- cluster_info[3]
    num_non_users[idx] <- cluster_info[4]
    
    num_people[idx] <- length(cluster_sub_data$CLUSTER)
    
  }
  
  cluster_data <- data.frame("unit" = clusters, "year" = year, "usage" = cluster_usage, 
                             "prev" = cluster_prev, "num_users" = num_users,
                             "num_non_users" = num_non_users, 
                             "num_people" = num_people)

  
  #-------------------------------------------------------------------------------
  ## Individual level data
  admin_kids <- extract_pr_bound[which(extract_pr_bound$hml16a >= 6 & extract_pr_bound$hml16a <= 59),]  
  rdt_tested <- admin_kids[which(admin_kids$hml35 >= 0 & admin_kids$hml35 <= 1),]


  individual_data <- data.frame("country" = rep(cc, length(rdt_tested$hv002)),
                                "admin1" = rdt_tested$ADM1NAME,
                                "cluster" = rdt_tested$CLUSTER,
                                "year" = rep(survey_year, length(rdt_tested$hv002)),
                                "age" = rdt_tested$hml16a,
                                "used_net" = rdt_tested$hml20,
                                "rdt" = rdt_tested$hml35, 
                                "w" = rdt_tested$hv005/1e6)
  
   output <- do.call(rbind, apply(individual_data, 1, link_data, country_data = country_data, 
                     admin_data = admin_data, cluster_data = cluster_data))
   
   indiv <- cbind(individual_data, output)
   
   
   write_csv(indiv, paste0("outputs/dhs_data/individual_data_", cc, "_", survey_year, ".csv", sep=""))
}



