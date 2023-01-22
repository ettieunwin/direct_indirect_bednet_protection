library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)

######################################################################################
usage_factor <- function(row){
  
  cluster_usage <- row[3]
  
  if (cluster_usage <= 0.1 | is.nan(cluster_usage) == TRUE | is.na(cluster_usage) == TRUE){
    return (1)
  } else if (cluster_usage > 0.1 & cluster_usage <= 0.2){
    return (2)
  } else if (cluster_usage > 0.2 & cluster_usage <= 0.3){
    return (3)
  } else if (cluster_usage > 0.3 & cluster_usage <= 0.4){
    return (4)
  } else if (cluster_usage > 0.4 & cluster_usage <= 0.5){
    return (5)
  } else if (cluster_usage > 0.5 & cluster_usage <= 0.6){
    return (6)
  } else if (cluster_usage > 0.6 & cluster_usage <= 0.7){
    return (7)
  } else if (cluster_usage > 0.7 & cluster_usage <= 0.8){
    return (8)
  } else if (cluster_usage > 0.8 & cluster_usage <= 0.9){
    return (9)
  } else if (cluster_usage > 0.9 & cluster_usage <= 1.0){
    return (10)
  }
}

prev_factor <- function(row){
  
  cluster_prev <- row[4]
  
  if (cluster_prev <= 0.1 | is.nan(cluster_prev) == TRUE | is.na(cluster_prev) == TRUE){
    return (1)
  } else if (cluster_prev > 0.1 & cluster_prev <= 0.2){
    return (2)
  } else if (cluster_prev > 0.2 & cluster_prev <= 0.3){
    return (3)
  } else if (cluster_prev > 0.3 & cluster_prev <= 0.4){
    return (4)
  } else if (cluster_prev > 0.4 & cluster_prev <= 0.5){
    return (5)
  } else if (cluster_prev > 0.5 & cluster_prev <= 0.6){
    return (6)
  } else if (cluster_prev > 0.6 & cluster_prev <= 0.7){
    return (7)
  } else if (cluster_prev > 0.7 & cluster_prev <= 0.8){
    return (8)
  } else if (cluster_prev > 0.8 & cluster_prev <= 0.9){
    return (9)
  } else if (cluster_prev > 0.9 & cluster_prev <= 1.0){
    return (10)
  }
}

######################################################################################
path <- "outputs/dhs_data/"
files <- list.files(path = path, pattern = ".csv")

data_list <- lapply(paste0(path, files), read_csv )
data <- do.call(rbind, data_list)
data <- as.data.frame(data)

# Remove any clusters that don't have more than 5 users and non users.
# This means cluster usage computation is more accurate
# Change to 5 users AND 5 non-users
#data <- data[which(data$num_users > 4 & data$num_non_users > 4), ]


data$num_children = data$num_users + data$num_non_users

data <- data[which(data$num_children >= 20),]
print(sprintf("%d number of children", length(data$age)))

data_ <- select(data, country, cluster, year, num_people, num_children)
data_un <- unique(data_)
#print(sprintf("%f prop clusters with between 10 and 15 children", length(data_un$num_children[data_un$num_children < 15])/length(data_un$num_children)))

prev_users = prev_non_users = cluster_usage = cluster_prev = country = year = num_children <- numeric(length((unique(data[c("country", "cluster", "year")]))$country))

count <- 1
for(i in 1:length(unique(data$country))){
  data_sub <- data[which(data$country == unique(data$country)[i]),]
  
  for (j in 1:length(unique(data_sub$year))){
    
    data_year <- data_sub[which(data_sub$year == unique(data_sub$year)[j]),]
    
    for (k in 1:length(unique(data_year$cluster))){
      data_cluster <- data_year[which(data_year$cluster == unique(data_year$cluster)[k]),]
      
      prev_users[count] <- sum(data_cluster$rdt[which(data_cluster$used_net == 1)]) /  
        length(data_cluster$rdt[which(data_cluster$used_net == 1)])
      prev_non_users[count] <- sum(data_cluster$rdt[which(data_cluster$used_net == 0)]) /  
        length(data_cluster$rdt[which(data_cluster$used_net == 0)])
      
      country[count] <- data_cluster$country[1]
      year[count] <- data_cluster$year[1]
      
      cluster_usage[count] <- data_cluster$cluster_usage[1]
      cluster_prev[count] <- data_cluster$admin1_prev[1]
      num_children[count] <- data_cluster$num_children[1]
      count <- count + 1
    }
  }
}

cluster_data <- data.frame("users" = prev_users, "non_users" = prev_non_users,
                           "cluster_usage" = cluster_usage, "cluster_prev" = cluster_prev,
                           "num_children" = num_children)
cluster_data$users[is.nan(cluster_data$users)] <- 0
cluster_data$non_users[is.nan(cluster_data$non_users)] <- 0
cluster_data$diff <- cluster_data$non_users - cluster_data$users

usage_factor_ <- factor(apply(cluster_data, 1, usage_factor))
prev_factor_ <- factor(apply(cluster_data, 1, prev_factor))

cluster_data$usage_factor <- usage_factor_
cluster_data$prev_factor <- prev_factor_ 

print(sprintf("%d number of clusters", length(cluster_data$prev_factor)))

mean_matrix <- matrix(ncol = 10, nrow = 10)
num_cluster_matrix <- matrix(ncol = 10, nrow = 10)

for (f in 1:10){
  for (g in 1:10){
    print(c(f, g))
    cd <- cluster_data[which(cluster_data$usage_factor == g & cluster_data$prev_factor == f),]
    num_cluster_matrix[f,g] <- length(cd$usage_factor)
    mean_matrix[f,g] <- mean(cd$non_users-cd$users)
  }
  
}

mean_data <- as.data.frame(mean_matrix)


colnames(mean_data) <- 1:10/10 #- 0.05
rownames(mean_data) <- 1:10/10 #- 0.05

saveRDS(mean_data, "outputs/cluster_prev_usage_mean.RDS")
saveRDS(num_cluster_matrix, "outputs/num_cluster_prev_usage.RDS")
saveRDS(cluster_data, "outputs/cluster_data.RDS")


cluster_data_country = cluster_data
cluster_data_country$country = country
cluster_data_country$year = year


m <- lmer(diff ~ (1|country) + cluster_usage*cluster_prev + year, 
          data = cluster_data_country)
print(summary(m))
#print(anova(m))
#plot_model(m)
#plot_model(m, "re")

cluster_data_country$group_prev = factor(ifelse(cluster_data_country$cluster_prev < 0.333, 1,
                                                ifelse(cluster_data_country$cluster_prev >= 0.666, 3, 2)))
cluster_data_country$group_usage = factor(ifelse(cluster_data_country$cluster_usage < 0.5, 1, 2))

data2 = cluster_data_country[cluster_data_country$cluster_usage > 0.2 &
                               cluster_data_country$cluster_usage <= 0.8,]

m <- lmer(diff ~ (1|country) + group_prev*group_usage, data = data2)
print(summary(m))

samples = bootMer(m, predict, nsim = 1000, seed = 10)
dat_predict = data.frame(t(samples$t))
dat_predict$group_prev = data2$group_prev
dat_predict$group_usage = data2$group_usage

saveRDS(data2, "outputs/cluster_categories.RDS")
saveRDS(dat_predict, "outputs/cluster_categories_predict_boot.RDS")

                      