# organize the data into format suitable for JASP
# input: csv files in Step1
# output: csv files in step 2 for JASP & for trial-by-trial analysis

# Author: Raude Killian
# date: 16.07.2025
# Description: Organize participant data into format suitable for JASP

library(tidyverse)

# Define base path
path_A <- "L:/Common/Users/Qiaoyue/MEG_project/"
# path_A <- "C:/Users/raude/Desktop/IB_Memory_Ren/"

# Read participant list
participants_file <- paste0(path_A, "Data/participants.csv")
participants <- read.csv(participants_file, stringsAsFactors = FALSE)

# Initialize list to store all participant summaries
all_data <- list()

# Loop through participants
for (subID in participants$subID) {
  
  # Define input paths
  step1_path <- paste0(path_A, "Results/bhv/", subID, "/Step1/")
  encoding_file <- list.files(path = step1_path, pattern = "^main.*\\.csv$", full.names = TRUE)
  recognition_file <- list.files(path = step1_path, pattern = "^recognition_.*\\.csv$", full.names = TRUE)
  rating_file <- list.files(path = step1_path, pattern = "^ratings_.*\\.csv$", full.names = TRUE)
  
  # Skip participant if any required file is missing
  if (length(encoding_file) == 0 || length(recognition_file) == 0) next
  
  # Read files
  file_encod <- read.csv(encoding_file[1], stringsAsFactors = FALSE)
  file_recall <- read.csv(recognition_file[1], stringsAsFactors = FALSE)
  file_rating <- if (length(rating_file) > 0) read.csv(rating_file[1], stringsAsFactors = FALSE) else NULL
  
  # Step 1: Merge encoding and recall by Picture
  file_recall$order_index <- seq_len(nrow(file_recall))
  merged_data <- merge(file_recall, file_encod, by = "Picture", all.x = TRUE, suffixes = c("_recall", "_encod"))
  merged_data <- merged_data[order(merged_data$order_index), ]
  merged_data$order_index <- NULL
  
  # Add trialtype
  merged_data  <- merged_data %>%
    mutate(trialtype = case_when(
      choice_rt > 3 ~ "too-long",
      
      Actual_Location == "left" & answer == "oldleft" ~ "correctold-correctlocation",
      Actual_Location == "left" & answer == "oldright" ~ "correctold-incorrectlocation",
      Actual_Location == "left" & answer == "oldunknow" ~ "correctold-unknowlocation",
      
      Actual_Location == "right" & answer == "oldright" ~ "correctold-correctlocation",
      Actual_Location == "right" & answer == "oldleft" ~ "correctold-incorrectlocation",
      Actual_Location == "right" & answer == "oldunknow" ~ "correctold-unknowlocation",
      
      Type == "new" & answer == "new" ~ "correctnew-cr",
      Type == "new" & answer != "new" ~ "incorrectnew-fa",
      Type == "old" & answer == "new" ~ "incorrectold-miss",
      
      TRUE ~ NA_character_
    )) %>%
    mutate(trialtypeMEG = case_when(
      is.na(Congruency_encod) | trialtype == "too-long" ~ trialtype,
      TRUE ~ paste0(trialtype, "_", Congruency_encod)
    ))
  
  # Output directory for merged trial-level data
  step2_dir <- paste0(path_A, "Results/bhv/", subID, "/Step2/")
  if (!dir.exists(step2_dir)) dir.create(step2_dir, recursive = TRUE)
  
  write.csv(merged_data, paste0(step2_dir, subID, "_merged_data.csv"), row.names = FALSE)
  
  # Step 2: Summary statistics for JASP
  
  rejected_trials_count <- sum(merged_data$trialtype == "too-long")
  
  merged_data <- merged_data[merged_data$trialtype != "too-long", ]
  file_encod <- file_encod[file_encod$choice_rt < 3 , ]
  
  this_participant_data <- data.frame(
    
    # Participant specific informations
    sub_ID = file_encod$subID[1],       
    age = file_encod$age[1],     
    sex = file_encod$gender[1],
    handedness = file_encod$handedness[1],
    rating_for_oldnew = file_rating$rating_foroldnew[1],
    rating_for_left_right = file_rating$rating_forleftright[1],
    
    rejected_trials_count = rejected_trials_count,
    unknown_count = sum(merged_data$answer == "oldunknow"),
    
    # choice_rt in encoding session
    mean_choice_rt = mean(file_encod$choice_rt),
    median_choice_rt = median(file_encod$choice_rt),
    mean_choice_rt_congruent  = mean(file_encod$choice_rt[file_encod$Congruency == "congruent"]),
    median_choice_rt_congruent = median(file_encod$choice_rt[file_encod$Congruency == "congruent"]),
    mean_choice_rt_incongruent = mean(file_encod$choice_rt[file_encod$Congruency == "incongruent"]),
    median_choice_rt_incongruent = median(file_encod$choice_rt[file_encod$Congruency == "incongruent"]),
    
    # rt in recall session
    mean_recall_rt = mean(file_recall$rt),
    median_recall_rt = median(file_recall$rt),
    
    mean_recall_rt_congruent = mean(file_recall$rt[file_recall$Congruency == "congruent"]),
    median_recall_rt_congruent = median(file_recall$rt[file_recall$Congruency == "congruent"]),
    mean_recall_rt_incongruent = mean(file_recall$rt[file_recall$Congruency == "incongruent"]),
    median_recall_rt_incongruent = median(file_recall$rt[file_recall$Congruency == "incongruent"]),
    
    mean_recall_rt_new = mean(file_recall$rt[file_recall$answer == "new"]),
    median_recall_rt_new = median(file_recall$rt[file_recall$answer == "new"]),
    mean_recall_rt_old = mean(file_recall$rt[file_recall$answer != "new"]),
    median_recall_rt_old = median(file_recall$rt[file_recall$answer != "new"]),
    
    mean_recall_rt_old_left = mean(file_recall$rt[file_recall$answer == "oldleft"]),
    median_recall_rt_old_left = median(file_recall$rt[file_recall$answer == "oldleft"]),
    mean_recall_rt_old_right = mean(file_recall$rt[file_recall$answer == "oldright"]),
    median_recall_rt_old_right = median(file_recall$rt[file_recall$answer == "oldright"]),
    
    mean_recall_rt_correctold_congruent = mean(merged_data$rt[merged_data$answer != "new" & merged_data$Type == "old" & merged_data$Congruency_recall == "congruent"]),
    median_recall_rt_correctold_congruent = median(merged_data$rt[merged_data$answer != "new" & merged_data$Type == "old" & merged_data$Congruency_recall == "congruent"]),
    mean_recall_rt_correctold_incongruent = mean(merged_data$rt[merged_data$answer != "new" & merged_data$Type == "old" & merged_data$Congruency_recall == "incongruent"]),
    median_recall_rt_correctold_incongruent = median(merged_data$rt[merged_data$answer != "new" & merged_data$Type == "old" & merged_data$Congruency_recall == "incongruent"]),
    
    mean_recall_rt_incorrectold_congruent = mean(merged_data$rt[merged_data$answer == "new" & merged_data$Type == "old" & merged_data$Congruency_recall == "congruent"]),
    median_recall_rt_incorrectold_congruent = median(merged_data$rt[merged_data$answer == "new" & merged_data$Type == "old" & merged_data$Congruency_recall == "congruent"]),
    mean_recall_rt_incorrectold_incongruent = mean(merged_data$rt[merged_data$answer == "new" & merged_data$Type == "old" & merged_data$Congruency_recall == "incongruent"]),
    median_recall_rt_incorrectold_incongruent = median(merged_data$rt[merged_data$answer == "new" & merged_data$Type == "old" & merged_data$Congruency_recall == "incongruent"]),
    
    mean_recall_rt_correctold_correctloc_congruent = mean(merged_data$rt[merged_data$trialtype == "correctold-correctlocation" & merged_data$Type == "old" & merged_data$Congruency_recall == "congruent"]),
    median_recall_rt_correctold_correctloc_congruent = median(merged_data$rt[merged_data$trialtype == "correctold-correctlocation" & merged_data$Type == "old" & merged_data$Congruency_recall == "congruent"]),
    mean_recall_rt_correctold_incorrectloc_congruent = mean(merged_data$rt[merged_data$trialtype == "correctold-incorrectlocation" & merged_data$Type == "old" & merged_data$Congruency_recall == "congruent"]),
    median_recall_rt_correctold_incorrectloc_congruent = median(merged_data$rt[merged_data$trialtype == "correctold-incorrectlocation" & merged_data$Type == "old" & merged_data$Congruency_recall == "congruent"]),
    mean_recall_rt_correctold_unknownloc_congruent = mean(merged_data$rt[merged_data$answer == "oldunknow" & merged_data$Type == "old" & merged_data$Congruency_recall == "congruent"]),
    median_recall_rt_correctold_unknownloc_congruent = median(merged_data$rt[merged_data$answer == "oldunknow" & merged_data$Type == "old" & merged_data$Congruency_recall == "congruent"]),
    
    mean_recall_rt_correctold_correctloc_incongruent = mean(merged_data$rt[merged_data$trialtype == "correctold-correctlocation" & merged_data$Type == "old" & merged_data$Congruency_recall == "incongruent"]),
    median_recall_rt_correctold_correctloc_incongruent = median(merged_data$rt[merged_data$trialtype == "correctold-correctlocation" & merged_data$Type == "old" & merged_data$Congruency_recall == "incongruent"]),
    mean_recall_rt_correctold_incorrectloc_incongruent = mean(merged_data$rt[merged_data$trialtype == "correctold-incorrectlocation" & merged_data$Type == "old" & merged_data$Congruency_recall == "incongruent"]),
    median_recall_rt_correctold_incorrectloc_incongruent = median(merged_data$rt[merged_data$trialtype == "correctold-incorrectlocation" & merged_data$Type == "old" & merged_data$Congruency_recall == "incongruent"]),
    mean_recall_rt_correctold_unknownloc_incongruent = mean(merged_data$rt[merged_data$answer == "oldunknow" & merged_data$Type == "old" & merged_data$Congruency_recall == "incongruent"]),
    median_recall_rt_correctold_unknownloc_incongruent = median(merged_data$rt[merged_data$answer == "oldunknow" & merged_data$Type == "old" & merged_data$Congruency_recall == "incongruent"]),
    
    
    
    # a_rating in encoding session
    mean_a_rating_congruent = mean(file_encod$Arating[file_encod$Congruency == "congruent"]),
    mean_a_rating_incongruent = mean(file_encod$Arating[file_encod$Congruency == "incongruent"]),
    
    # left/right proportion
    right_choice_proportion = (sum(file_encod$choice == "right" | file_encod$choice == "8" ) / length(file_encod$choice)),
    left_choice_proportion = (sum(file_encod$choice == "left" | file_encod$choice == "7") / length(file_encod$choice)),
    right_location_proportion = (sum(file_encod$Actual_Location == "right") / length(file_encod$Actual_Location)),
    left_proportion_proportion = (sum(file_encod$Actual_Location == "left") / length(file_encod$Actual_Location)),
    
    # accuracy, hit, miss, fa, cr
    new_correctold_count_CR = sum(file_recall$Type == "new" & file_recall$answer == "new"),
    new_incorrectold_count_FA = sum(file_recall$Type == "new" & file_recall$answer != "new"),
    new_correctold_rate_CR = sum(file_recall$Type == "new" & file_recall$answer == "new") / sum(file_recall$Type == "new"),
    new_incorrectold_rate_FA = sum(file_recall$Type == "new" & file_recall$answer != "new") / sum(file_recall$Type == "new"),
    
    correctold_count_HIT = sum(merged_data$answer != "new" & merged_data$Type == "old"),
    correctold_rate_HIT = sum(merged_data$answer != "new" & merged_data$Type == "old") / sum(merged_data$Type == "old"),
    incorrectold_count_MISS = sum(merged_data$answer == "new" & merged_data$Type == "old"),
    incorrectold_rate_MISS = sum(merged_data$answer == "new" & merged_data$Type == "old") / sum(merged_data$Type == "old"),
    
    correctold_congruent_rate = sum(merged_data$answer != "new" & merged_data$Type == "old" & merged_data$Congruency_recall == "congruent") / sum(merged_data$Congruency_recall == "congruent"),
    correctold_incongruent_rate = sum(merged_data$answer != "new" & merged_data$Type == "old" & merged_data$Congruency_recall == "incongruent") / sum(merged_data$Congruency_recall == "incongruent"),
    incorrectold_congruent_rate = sum(merged_data$answer == "new" & merged_data$Type == "old" & merged_data$Congruency_recall == "congruent") / sum(merged_data$Congruency_recall == "congruent"),
    incorrectold_incongruent_rate = sum(merged_data$answer == "new" & merged_data$Type == "old" & merged_data$Congruency_recall == "incongruent") / sum(merged_data$Congruency_recall == "incongruent"),
    
    correctold_correctloc_congruent_rate = sum(merged_data$trialtype == "correctold-correctlocation" & merged_data$Type == "old" & merged_data$Congruency_recall == "congruent") / sum(merged_data$answer != "new" & merged_data$Type == "old" & merged_data$Congruency_recall == "congruent"),
    correctold_incorrectloc_congruent_rate = sum(merged_data$trialtype == "correctold-incorrectlocation" & merged_data$Type == "old" & merged_data$Congruency_recall == "congruent") / sum(merged_data$answer != "new" & merged_data$Type == "old" & merged_data$Congruency_recall == "incongruent"),
    correctold_unknownloc_congruent_rate = sum(merged_data$answer == "oldunknow" & merged_data$Type == "old" & merged_data$Congruency_recall == "congruent") / sum(merged_data$Congruency_recall == "congruent"),
    
    correctold_correctloc_incongruent_rate = sum(merged_data$trialtype == "correctold-correctlocation" & merged_data$Type == "old" & merged_data$Congruency_recall == "incongruent") / sum(merged_data$Congruency_recall == "incongruent"),
    correctold_incorrectloc_incongruent_rate = sum(merged_data$trialtype == "correctold-incorrectlocation" & merged_data$Type == "old" & merged_data$Congruency_recall == "incongruent") / sum(merged_data$Congruency_recall == "incongruent"),
    correctold_unknownloc_incongruent_rate = sum(merged_data$answer == "oldunknow" & merged_data$Type == "old" & merged_data$Congruency_recall == "incongruent") / sum(merged_data$Congruency_recall == "incongruent")
    
  )
  
  all_data <- append(all_data, list(this_participant_data))
}

# Combine and save all participant summaries
combined_data <- do.call(rbind, all_data)
combined_output_path <- paste0(path_A, "Results/bhv/combined_participant_bhv_data.csv")
write.csv(combined_data, combined_output_path, row.names = FALSE)
