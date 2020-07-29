
### Lead analyst: Adnane Ez-Zizi

###################
# Preliminary steps
###################

### Load libraries
library(data.table)
library(dplyr) 

options(show.signif.stars=FALSE)

# Load the data
load('exp_data.rda')

###################
# Useful functions
###################

# Function that produces confidence intervals for binomial proportions
CI_binomial_proportion = function(alpha = 0.05, n = 50, p = 0.5) {
  Cl = qbinom(alpha/2, n, p)/n
  Cu = qbinom(1-alpha/2, n, p)/n
  return(c(Cl, Cu))
} 

# Function that extracts the last n letters
extract_last_letters = function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

### Function that finds the n most associated endings for each cluster based on the proportion of 
# appearence of each ending in a cluster 
FindStrongestEnding <- function(P = prop_table2_CI, n_strong_ends = 10, n_total_filter = 5){
  
  # Number of clusters (number of columns of P - 1)
  n_clust = ncol(P) - 1
  
  # Filter out endings that occurs less than n_total_filter
  P = P[P$Total>=n_total_filter, ]
  
  # Initialise the dataframe containing the strongest positive cues
  Strong_ends = matrix("z", nrow = n_strong_ends, ncol = n_clust)
  Strong_ends = as.data.frame(Strong_ends)
  rownames(Strong_ends) = 1:n_strong_ends
  colnames(Strong_ends) = paste0("Cl", 1:n_clust)
  Strong_ends[] = lapply(Strong_ends, as.character)
  
  # Fill in the data
  for (cl in 1:n_clust){
    
    # filter out endings that have proportions lower than 1/3
    P_filtered = P[P[, cl]>1/3, ] 
    
    # Extract the n_strong_ends endings
    Strong_ends[, cl] = rownames(P_filtered[order(-P_filtered[, cl]), cl, drop = FALSE])[1:n_strong_ends] # Strongest endings
  }
  
  return(Strong_ends)
  
}

###########################################
# Data analyses
###########################################

############ To compute the binomial confidence intervals for participants  ##############

### First let's extract the number of participants
Part_vec = unique(exp_data$ParticipantNo)
N_part = length(Part_vec) # 216

### Then, let's construct the dataframe that will contain the counts
# Initialise the dataframe
data_part_counts = data.frame(ParticipantNo = Part_vec,
                              Count_u = 99,
                              Count_a = 99,
                              Count_all = 9,
                              Prop_u = 99
                              )
# Fill in the dataframe
for (j in 1:N_part){
  ParticipantNo_j = data_part_counts$ParticipantNo[j]
  data_part_counts$Count_all[j] = nrow(exp_data[exp_data$ParticipantNo==ParticipantNo_j,])
  data_part_counts$Count_a[j] = sum(as.numeric(as.character(exp_data$Response[exp_data$ParticipantNo==ParticipantNo_j])))
  data_part_counts$Count_u[j] = data_part_counts$Count_all[j] - data_part_counts$Count_a[j]
  data_part_counts$Prop_u[j] = data_part_counts$Count_u[j]/data_part_counts$Count_all[j]
}

# Add the variables that encode the confidence bounds and detect whether the proportion is significantly 
# different from chance
data_part_counts$Prop_u_cl = 99 
data_part_counts$Prop_u_cu = 99 
data_part_counts$Is_evidence_against_chance = 99 
data_part_counts$Cluster_CI = 99 # 3-way clustering based on the evidence for being biased towards an ending
for (j in 1:N_part){
    # Compute CI bounds
    data_part_counts$Prop_u_cl[j] = CI_binomial_proportion(0.05, data_part_counts$Count_all[j], 0.5)[1] 
    data_part_counts$Prop_u_cu[j] = CI_binomial_proportion(0.05, data_part_counts$Count_all[j], 0.5)[2]
    # Check whether their is evidence that the proportion is not due to chance 
    test1_j = ( data_part_counts$Prop_u[j] < data_part_counts$Prop_u_cu[j] ) 
    test2_j = ( data_part_counts$Prop_u[j] > data_part_counts$Prop_u_cl[j] )
    test_j = (test1_j & test2_j)
    data_part_counts$Is_evidence_against_chance[j] = ifelse(test = test_j, yes = 0, no = 1)
    # Cluster assignment
    if (data_part_counts$Is_evidence_against_chance[j] == 0){ # Cluster 3 contains unbiased participants
        data_part_counts$Cluster_CI[j] = 3
    }
    else if (data_part_counts$Is_evidence_against_chance[j] == 1 && 
             data_part_counts$Prop_u[j] < 0.5){ # Cluster 2 contains participants who are biased towards the -a ending
        data_part_counts$Cluster_CI[j] = 2
    }
    else if (data_part_counts$Is_evidence_against_chance[j] == 1 && 
             data_part_counts$Prop_u[j] > 0.5){ # Cluster 1 contains participants who are biased towards the -u ending
        data_part_counts$Cluster_CI[j] = 1
    }
    
}
data_part_counts$Cluster_CI = as.factor(as.character(data_part_counts$Cluster_CI))


############ To compute the binomial confidence intervals for words (similar to the above) ##############

### First let's extract the number of participants
Word_vec = unique(exp_data$nonce)
N_word = length(Word_vec)

### Then, let's construct the dataframe that will contain the counts
# Initialise the dataframe
data_word_counts = data.frame(Word = Word_vec,
                              Count_u = 99,
                              Count_a = 99,
                              Count_all = 99,
                              Prop_u = 99
                              )
# Fill in the dataframe
for (j in 1:N_word){
  Word_j = data_word_counts$Word[j]
  data_word_counts$Count_all[j] = nrow(exp_data[exp_data$nonce==Word_j,])
  data_word_counts$Count_a[j] = sum(as.numeric(as.character(exp_data$Response[exp_data$nonce==Word_j])))
  data_word_counts$Count_u[j] = data_word_counts$Count_all[j] - data_word_counts$Count_a[j]
  data_word_counts$Prop_u[j] = data_word_counts$Count_u[j]/data_word_counts$Count_all[j]
}
# Sort the dataframe by the proportion of -u choices
data_word_counts = data_word_counts[order(data_word_counts$Prop_u, decreasing = TRUE), ]

# Add the variables that encode the confidence bounds and detect whether the proportion is significantly 
# different from chance
data_word_counts$Prop_u_cl = 99 
data_word_counts$Prop_u_cu = 99 
data_word_counts$Is_evidence_against_chance = 99 
data_word_counts$Cluster_CI = 99 # 3-way clustering based on the evidence for being biased towards an ending
for (j in 1:N_word){
    # Compute CI bounds
    data_word_counts$Prop_u_cl[j] = CI_binomial_proportion(0.05, data_word_counts$Count_all[j], 0.5)[1] 
    data_word_counts$Prop_u_cu[j] = CI_binomial_proportion(0.05, data_word_counts$Count_all[j], 0.5)[2]
    # Check whether their is evidence that the proportion is not due to chance 
    test1_j = ( data_word_counts$Prop_u[j] < data_word_counts$Prop_u_cu[j] ) 
    test2_j = ( data_word_counts$Prop_u[j] > data_word_counts$Prop_u_cl[j] )
    test_j = (test1_j & test2_j)
    data_word_counts$Is_evidence_against_chance[j] = ifelse(test = test_j, yes = 0, no = 1) 
    # Cluster assignment
    if (data_word_counts$Is_evidence_against_chance[j] == 0){ # Cluster 3 contains unbiased participants
        data_word_counts$Cluster_CI[j] = 3
    }
    else if (data_word_counts$Is_evidence_against_chance[j] == 1 && 
             data_word_counts$Prop_u[j] < 0.5){ # Cluster 2 contains participants who are biased towards the -a ending
        data_word_counts$Cluster_CI[j] = 2
    }
    else if (data_word_counts$Is_evidence_against_chance[j] == 1 && 
             data_word_counts$Prop_u[j] > 0.5){ # Cluster 1 contains participants who are biased towards the -u ending
        data_word_counts$Cluster_CI[j] = 1
    }
}
data_word_counts$Cluster_CI = as.factor(as.character(data_word_counts$Cluster_CI))


########################## Shapiro test ###############################

# Shapiro test
shapiro.test(data_part_counts$Prop_u)
#     Shapiro-Wilk normality test
# 
# data:  data_part_counts$Prop_u
# W = 0.99125, p-value = 0.2207

########################## Extract top predictive bigrams (Table 5)) ###############################

### Add the two-letter and three-letter endings to the data_resp_counts
data_word_counts$lemma_ending2 = sapply(as.character(data_word_counts$Word), function(y) extract_last_letters(y,2))

# For the two-letter endings
freq_table2_CI = xtabs(~ lemma_ending2 + Cluster_CI, data = data_word_counts)
Total = apply(freq_table2_CI, 1, sum)
prop_table2_CI = cbind(prop.table(freq_table2_CI, 1), Total)
prop_table2_CI = as.data.frame(prop_table2_CI)
colnames(prop_table2_CI)[1:3] = c("Cl1", "Cl2", "Cl3")

# Here is the list for two-letter endings
strongest_ending2_CI = FindStrongestEnding(prop_table2_CI, n_strong_ends = 10, n_total_filter = 5)


