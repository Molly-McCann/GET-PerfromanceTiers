# Cluster Analysis
# Making the segmented dfs based on the clusters from GETReport.Rmd

# Load libraries 
library(dplyr)

# Read in regular/original datasets

test_measure <- read.csv("~/Downloads/Performance/Project/test_measure.csv")
subject_info <- read.csv("~/Downloads/Performance/Project/subject-info.csv")

# Edit the data such that the derived variables are within the datasets

data <- merge(test_measure, subject_info, by = c("ID", "ID_test"))

duplicates <- subject_info |>
  group_by(ID) |>
  filter(n() > 1)

duplicate_counts <- subject_info |>
  group_by(ID) |>
  summarise(frequency = n()) |>
  filter(frequency > 1)  # Keep only duplicated IDs

max_age_test <- subject_info |>
  group_by(ID) |>
  filter(Age == max(Age)) |>
  select(ID, Age, ID_test)  # Select relevant columns

single_attempt_ids <- subject_info |>
  count(ID) |>
  filter(n == 1) |>
  pull(ID)

single_attempt_tests <- subject_info |>
  filter(ID %in% single_attempt_ids) |>
  select(ID, ID_test)

recent_tests <- max_age_test |>
  left_join(single_attempt_tests, by = "ID_test")

subject_info <- subject_info |>
  filter(ID_test %in% recent_tests$ID_test)

test_measure <- test_measure |>
  filter(ID_test %in% recent_tests$ID_test)

same_IDs <- subject_info |>
  group_by(ID) |>
  filter(n() > 1)

subject_info <- subject_info |>
  filter(!ID_test %in% c("388_1", "155_1"))

test_measure <- test_measure |>
  filter(!ID_test %in% c("388_1", "155_1"))

subject_info <- subject_info |>
  mutate(Warmup_start = 0, Warmup_end = NA, Recovery_start = NA, Recovery_end = NA)

for (id in unique(subject_info$ID)) {
  subject_subset <- test_measure |> 
    filter(ID == id)
  warmup_end_time <- subject_subset |>
    filter(lag(Speed, default = 0) == 5.0 & Speed >= 5.1) |>
    slice_head(n = 1) |>
    pull(time)
  recovery_start_time <- subject_subset |>
    filter(lag(Speed, default = first(Speed)) > 5.0 & Speed <= 5.0) |>
    slice_head(n = 1) |>
    pull(time)
  recovery_end_time <- max(subject_subset$time, na.rm = TRUE)
  subject_info <- subject_info |>
    mutate(
      Warmup_end = ifelse(ID == id, warmup_end_time, Warmup_end),
      Recovery_start = ifelse(ID == id, recovery_start_time, Recovery_start),
      Recovery_end = ifelse(ID == id, recovery_end_time, Recovery_end))}

subject_info <- subject_info |>
  mutate(Warmup_start = 0, Warmup_end = NA, Recovery_start = NA, Recovery_end = NA)

results <- list()

for (id in unique(subject_info$ID)) {
  subject_subset <- test_measure |> 
    filter(ID == id)
  warmup_end_time <- subject_subset |>
    filter(lag(Speed, default = 0) <= 5.0 & Speed >= 5.1) |>
    slice_head(n = 1) |>
    pull(time)
  recovery_start_time <- subject_subset |>
    filter(lag(Speed, default = first(Speed)) > 5.0 & Speed <= 5.0) |>
    slice_head(n = 1) |>
    pull(time)
  recovery_end_time <- max(subject_subset$time, na.rm = TRUE)
  results[[id]] <- tibble(ID = id, 
                          Warmup_end = ifelse(length(warmup_end_time) > 0, warmup_end_time, NA),
                          Recovery_start = ifelse(length(recovery_start_time) > 0, recovery_start_time, NA),
                          Recovery_end = recovery_end_time)}

results_df <- bind_rows(results)

subject_info <- left_join(subject_info, results_df, by = "ID") |>
  mutate(Warmup_end = coalesce(Warmup_end.y, Warmup_end.x),
         Recovery_start = coalesce(Recovery_start.y, Recovery_start.x),
         Recovery_end = coalesce(Recovery_end.y, Recovery_end.x)) |>
  select(-ends_with(".x"), -ends_with(".y"))

calc_max <- function(data, id_column, value_column) {
  max_values <- data |>
    group_by(.data[[id_column]]) |>
    summarize(max_value = max(.data[[value_column]], na.rm = TRUE), .groups = "drop")
 return(max_values)}

speed_max <- calc_max(data, id_column = "ID", value_column = "Speed")

data <- data |>
  left_join(speed_max, by = "ID") |>
  rename(Max_Speed = max_value)

peak_hr <- calc_max(data, id_column = "ID", value_column = "HR")

subject_info <- subject_info |>
  left_join(peak_hr, by = "ID") |>
  rename(Peak_HR = max_value)

data <- data |>
  left_join(peak_hr, by = "ID") |>
  rename(Peak_HR = max_value)

peak_hr_times <- data |>
  filter(HR == Peak_HR) |> 
  select(ID, time)

peak_hr_time <- data |>
  filter(HR == Peak_HR) |> 
  group_by(ID) |> 
  summarize(Peak_HR_Time = min(time), .groups = "drop")

subject_info <- subject_info |>
  left_join(peak_hr_time, by = "ID")

peak_hr_speeds <- data |>
  filter(HR == Peak_HR) |> 
  select(ID, Speed)

peak_hr_speed <- data |>
  filter(HR == Peak_HR) |> 
  group_by(ID) |> 
  slice_min(time) |>   
  ungroup() |> 
  select(ID, Peak_HR_Speed = Speed)  

subject_info <- subject_info |>
  left_join(peak_hr_speed |> distinct(ID, .keep_all = TRUE), by = "ID")

test_measure <- test_measure |>
  mutate(time = as.numeric(time)) 

initial_hr <- test_measure |>
  group_by(ID) |>
  filter(!is.na(HR)) |>  
  filter(time == min(time)) |>  
  summarize(Initial_HR = first(HR), .groups = "drop")

last_hr <- test_measure |>
  group_by(ID) |>
  slice_max(time, n = 1, with_ties = FALSE) |> 
  summarize(Last_HR = HR, .groups = "drop")

subject_info <- subject_info |>
  left_join(initial_hr %>% distinct(ID, .keep_all = TRUE), by = "ID") |>
  left_join(last_hr %>% distinct(ID, .keep_all = TRUE), by = "ID")

hr_recovery <- subject_info |>
  group_by(ID) |>
  summarize(HR_recovery_rate = ((Peak_HR - Last_HR) / (Recovery_end - Recovery_start)), .groups = "drop") |>
  distinct(ID, .keep_all = TRUE)  

subject_info <- subject_info |>
  left_join(hr_recovery, by = "ID")

speed_max <- speed_max |> 
  distinct(ID, .keep_all = TRUE)

subject_info <- subject_info |>
  left_join(speed_max, by = "ID") |>
  rename(Max_Speed = max_value)

speed_hr_ratio <- subject_info |>
  group_by(ID) |>
  summarize(Speed_HR_ratio = (Max_Speed/Peak_HR), .groups = "drop")

speed_hr_ratio <- speed_hr_ratio |> 
  distinct(ID, .keep_all = TRUE)

subject_info <- subject_info |>
  left_join(speed_hr_ratio, by = "ID")

vo2_max <- calc_max(data, id_column = "ID", value_column = "VO2")

vo2_max <- vo2_max |> 
  distinct(ID, .keep_all = TRUE)

subject_info <- subject_info |>
  left_join(vo2_max, by = "ID") |>
  rename(VO2_Max = max_value)

vo2_speed_ratio <- subject_info |>
  group_by(ID) |>
  summarize(VO2_Speed_ratio = (VO2_Max/Max_Speed), .groups = "drop")

vo2_speed_ratio <- vo2_speed_ratio |> 
  distinct(ID, .keep_all = TRUE)

subject_info <- subject_info |>
  left_join(vo2_speed_ratio, by = "ID")

peak_hr_summary <- peak_hr_times |>
  group_by(ID) |>
  summarize(num_peak_hr_times = n(), .groups = "drop")

combined_time <- subject_info |>
  left_join(peak_hr_summary, by = "ID")

combined_time <- combined_time |>
  mutate(Percent_time_peak_HR = (num_peak_hr_times / Recovery_end)) 

combined_time <- combined_time |> 
  distinct(ID, .keep_all = TRUE)

subject_info$Percent_time_peak_HR <- combined_time$Percent_time_peak_HR

HR_sd <- test_measure |>
  group_by(ID) |>
  summarize(HR_variability = sd(HR, na.rm = TRUE))

HR_sd <- HR_sd |> 
  distinct(ID, .keep_all = TRUE)

subject_info <- subject_info |>
  left_join(HR_sd, by = "ID")

recovery_HR <- subject_info |>
  group_by(ID) |>
  summarize(HR_recovery_drop_percent = ((Peak_HR - Last_HR)/Peak_HR))

recovery_HR <- recovery_HR |>
  distinct(ID, .keep_all = TRUE)

subject_info <- subject_info |>
  left_join(recovery_HR, by = "ID")

vent_eff <- test_measure |>
  group_by(ID) |>
  summarize(Vent_efficiency = VE / VO2, .groups = "drop")

vent_eff <- vent_eff |>
  distinct(ID, .keep_all = TRUE)

test_measure <- test_measure |>
  left_join(vent_eff, by = "ID")

avg_vent_eff <- test_measure |>
  group_by(ID) |>
  summarize(Avg_ventilatory_efficiency = mean(Vent_efficiency))

avg_vent_eff <- avg_vent_eff |>
  distinct(ID, .keep_all = TRUE)

subject_info <- subject_info |>
  left_join(avg_vent_eff, by = "ID")

resp_exch <- test_measure |>
  group_by(ID) |>
  summarize(Resp_exchange = VCO2 / VO2, .groups = "drop")

resp_exch <- resp_exch |>
  distinct(ID, .keep_all = TRUE)

test_measure <- test_measure |>
  left_join(resp_exch, by = "ID")

avg_resp_exch_ratio <- test_measure |>
  group_by(ID) |>
  summarize(Avg_respiratory_exchange_ratio = mean(Resp_exchange))

avg_resp_exch_ratio <- avg_resp_exch_ratio |>
  distinct(ID, .keep_all = TRUE)

subject_info <- subject_info |>
  left_join(avg_resp_exch_ratio, by = "ID")

avg_hr_segments <- test_measure |>
  left_join(subject_info, by = "ID") |>
  mutate(
    in_warmup = time >= Warmup_start & time <= Warmup_end,
    in_effort = time > Warmup_end & time <= Recovery_start,
    in_recovery = time > Recovery_start & time <= Recovery_end) |>
  group_by(ID) |>
  summarize(
    Avg_HR_warmup = mean(HR[in_warmup], na.rm = TRUE),
    Avg_HR_effort = mean(HR[in_effort], na.rm = TRUE),
    Avg_HR_recovery = mean(HR[in_recovery], na.rm = TRUE),
    .groups = "drop")

subject_info <- subject_info |>
  left_join(avg_hr_segments, by = "ID")

avg_rr_segments <- test_measure |>
  left_join(subject_info, by = "ID") |>
  mutate(
    in_warmup = time >= Warmup_start & time <= Warmup_end,
    in_effort = time > Warmup_end & time <= Recovery_start,
    in_recovery = time > Recovery_start & time <= Recovery_end) |>
  group_by(ID) |>
  summarize(
    Avg_RR_warmup = mean(RR[in_warmup], na.rm = TRUE),
    Avg_RR_effort = mean(RR[in_effort], na.rm = TRUE),
    Avg_RR_recovery = mean(RR[in_recovery], na.rm = TRUE),
    .groups = "drop")

subject_info <- subject_info |>
  left_join(avg_rr_segments, by = "ID")

# Part 1 - Clustering 1

c1p1g1 <- read.csv("~/Downloads/Performance/Project/Cluster1Part1/cluster_1_df_p1.csv")
part1cluster1group1 <- subject_info[subject_info$ID %in% c1p1g1$ID, ]
write.csv(part1cluster1group1, "part1cluster1group1.csv", row.names = FALSE)

c1p2g1 <- read.csv("~/Downloads/Performance/Project/Cluster1Part1/cluster_2_df_p1.csv")
part1cluster2group1 <- subject_info[subject_info$ID %in% c1p2g1$ID, ]
write.csv(part1cluster2group1, "part1cluster2group1.csv", row.names = FALSE)

c1p3g1 <- read.csv("~/Downloads/Performance/Project/Cluster1Part1/cluster_3_df_p1.csv")
part1cluster3group1 <- subject_info[subject_info$ID %in% c1p3g1$ID, ]
write.csv(part1cluster3group1, "part1cluster3group1.csv", row.names = FALSE)

c1p4g1 <- read.csv("~/Downloads/Performance/Project/Cluster1Part1/cluster_4_df_p1.csv")
part1cluster4group1 <- subject_info[subject_info$ID %in% c1p4g1$ID, ]
write.csv(part1cluster4group1, "part1cluster4group1.csv", row.names = FALSE)

c1p5g1 <- read.csv("~/Downloads/Performance/Project/Cluster1Part1/cluster_5_df_p1.csv")
part1cluster5group1 <- subject_info[subject_info$ID %in% c1p5g1$ID, ]
write.csv(part1cluster5group1, "part1cluster5group1.csv", row.names = FALSE)

c1p6g1 <- read.csv("~/Downloads/Performance/Project/Cluster1Part1/cluster_6_df_p1.csv")
part1cluster6group1 <- subject_info[subject_info$ID %in% c1p6g1$ID, ]
write.csv(part1cluster6group1, "part1cluster6group1.csv", row.names = FALSE)

c1p7g1 <- read.csv("~/Downloads/Performance/Project/Cluster1Part1/cluster_7_df_p1.csv")
part1cluster7group1 <- subject_info[subject_info$ID %in% c1p7g1$ID, ]
write.csv(part1cluster7group1, "part1cluster7group1.csv", row.names = FALSE)

c1p8g1 <- read.csv("~/Downloads/Performance/Project/Cluster1Part1/cluster_8_df_p1.csv")
part1cluster8group1 <- subject_info[subject_info$ID %in% c1p8g1$ID, ]
write.csv(part1cluster8group1, "part1cluster8group1.csv", row.names = FALSE)

c1p9g1 <- read.csv("~/Downloads/Performance/Project/Cluster1Part1/cluster_9_df_p1.csv")
part1cluster9group1 <- subject_info[subject_info$ID %in% c1p9g1$ID, ]
write.csv(part1cluster9group1, "part1cluster9group1.csv", row.names = FALSE)

# Part 1 - Clustering 2 

c1p1g2 <- read.csv("~/Downloads/Performance/Project/Cluster1Part2/cluster_1_df_p2.csv")
part1cluster1group2 <- subject_info[subject_info$ID %in% c1p1g2$ID, ]
write.csv(part1cluster1group2, "part1cluster1group2.csv", row.names = FALSE)

c1p2g2 <- read.csv("~/Downloads/Performance/Project/Cluster1Part2/cluster_2_df_p2.csv")
part1cluster2group2 <- subject_info[subject_info$ID %in% c1p2g2$ID, ]
write.csv(part1cluster2group2, "part1cluster2group2.csv", row.names = FALSE)

c1p3g2 <- read.csv("~/Downloads/Performance/Project/Cluster1Part2/cluster_3_df_p2.csv")
part1cluster3group2 <- subject_info[subject_info$ID %in% c1p3g2$ID, ]
write.csv(part1cluster3group2, "part1cluster3group2.csv", row.names = FALSE)

c1p4g2 <- read.csv("~/Downloads/Performance/Project/Cluster1Part2/cluster_4_df_p2.csv")
part1cluster4group2 <- subject_info[subject_info$ID %in% c1p4g2$ID, ]
write.csv(part1cluster4group2, "part1cluster4group2.csv", row.names = FALSE)

c1p5g2 <- read.csv("~/Downloads/Performance/Project/Cluster1Part2/cluster_5_df_p2.csv")
part1cluster5group2 <- subject_info[subject_info$ID %in% c1p5g2$ID, ]
write.csv(part1cluster5group2, "part1cluster5group2.csv", row.names = FALSE)

c1p6g2 <- read.csv("~/Downloads/Performance/Project/Cluster1Part2/cluster_6_df_p2.csv")
part1cluster6group2 <- subject_info[subject_info$ID %in% c1p6g2$ID, ]
write.csv(part1cluster6group2, "part1cluster6group2.csv", row.names = FALSE)

c1p7g2 <- read.csv("~/Downloads/Performance/Project/Cluster1Part2/cluster_7_df_p2.csv")
part1cluster7group2 <- subject_info[subject_info$ID %in% c1p7g2$ID, ]
write.csv(part1cluster7group2, "part1cluster7group2.csv", row.names = FALSE)

c1p8g2 <- read.csv("~/Downloads/Performance/Project/Cluster1Part2/cluster_8_df_p2.csv")
part1cluster8group2 <- subject_info[subject_info$ID %in% c1p8g2$ID, ]
write.csv(part1cluster8group2, "part1cluster8group2.csv", row.names = FALSE)

c1p9g2 <- read.csv("~/Downloads/Performance/Project/Cluster1Part2/cluster_9_df_p2.csv")
part1cluster9group2 <- subject_info[subject_info$ID %in% c1p9g2$ID, ]
write.csv(part1cluster9group2, "part1cluster9group2.csv", row.names = FALSE)

c1p10g2 <- read.csv("~/Downloads/Performance/Project/Cluster1Part2/cluster_10_df_p2.csv")
part1cluster10group2 <- subject_info[subject_info$ID %in% c1p10g2$ID, ]
write.csv(part1cluster10group2, "part1cluster10group2.csv", row.names = FALSE)

c1p11g2 <- read.csv("~/Downloads/Performance/Project/Cluster1Part2/cluster_11_df_p2.csv")
part1cluster11group2 <- subject_info[subject_info$ID %in% c1p11g2$ID, ]
write.csv(part1cluster11group2, "part1cluster11group2.csv", row.names = FALSE)

c1p12g2 <- read.csv("~/Downloads/Performance/Project/Cluster1Part2/cluster_12_df_p2.csv")
part1cluster12group2 <- subject_info[subject_info$ID %in% c1p12g2$ID, ]
write.csv(part1cluster12group2, "part1cluster12group2.csv", row.names = FALSE)

c1p13g2 <- read.csv("~/Downloads/Performance/Project/Cluster1Part2/cluster_13_df_p2.csv")
part1cluster13group2 <- subject_info[subject_info$ID %in% c1p13g2$ID, ]
write.csv(part1cluster13group2, "part1cluster13group2.csv", row.names = FALSE)

c1p14g2 <- read.csv("~/Downloads/Performance/Project/Cluster1Part2/cluster_14_df_p2.csv")
part1cluster14group2 <- subject_info[subject_info$ID %in% c1p14g2$ID, ]
write.csv(part1cluster14group2, "part1cluster14group2.csv", row.names = FALSE)

# Part 2 - Clustering 1

c2p1g1 <- read.csv("~/Downloads/Performance/Project/Cluster2Part1/cluster_1_df_p3.csv")
part2cluster1group1 <- subject_info[subject_info$ID %in% c2p1g1$ID, ]
write.csv(part2cluster1group1, "part2cluster1group1.csv", row.names = FALSE)

c2p2g1 <- read.csv("~/Downloads/Performance/Project/Cluster2Part1/cluster_2_df_p3.csv")
part2cluster2group1 <- subject_info[subject_info$ID %in% c2p2g1$ID, ]
write.csv(part2cluster2group1, "part2cluster2group1.csv", row.names = FALSE)

c2p3g1 <- read.csv("~/Downloads/Performance/Project/Cluster2Part1/cluster_3_df_p3.csv")
part2cluster3group1 <- subject_info[subject_info$ID %in% c2p3g1$ID, ]
write.csv(part2cluster3group1, "part2cluster3group1.csv", row.names = FALSE)

c2p4g1 <- read.csv("~/Downloads/Performance/Project/Cluster2Part1/cluster_4_df_p3.csv")
part2cluster4group1 <- subject_info[subject_info$ID %in% c2p4g1$ID, ]
write.csv(part2cluster4group1, "part2cluster4group1.csv", row.names = FALSE)

c2p5g1 <- read.csv("~/Downloads/Performance/Project/Cluster2Part1/cluster_5_df_p3.csv")
part2cluster5group1 <- subject_info[subject_info$ID %in% c2p5g1$ID, ]
write.csv(part2cluster5group1, "part2cluster5group1.csv", row.names = FALSE)

# Part 2 - Clustering 2

c2p1g2 <- read.csv("~/Downloads/Performance/Project/Cluster2Part2/cluster_1_df_p4.csv")
part2cluster1group2 <- subject_info[subject_info$ID %in% c2p1g2$ID, ]
write.csv(part2cluster1group2, "part2cluster1group2.csv", row.names = FALSE)

c2p2g2 <- read.csv("~/Downloads/Performance/Project/Cluster2Part2/cluster_2_df_p4.csv")
part2cluster2group2 <- subject_info[subject_info$ID %in% c2p2g2$ID, ]
write.csv(part2cluster2group2, "part2cluster2group2.csv", row.names = FALSE)

c2p3g2 <- read.csv("~/Downloads/Performance/Project/Cluster2Part2/cluster_3_df_p4.csv")
part2cluster3group2 <- subject_info[subject_info$ID %in% c2p3g2$ID, ]
write.csv(part2cluster3group2, "part2cluster3group2.csv", row.names = FALSE)

c2p4g2 <- read.csv("~/Downloads/Performance/Project/Cluster2Part2/cluster_4_df_p4.csv")
part2cluster4group2 <- subject_info[subject_info$ID %in% c2p4g2$ID, ]
write.csv(part2cluster4group2, "part2cluster4group2.csv", row.names = FALSE)

c2p5g2 <- read.csv("~/Downloads/Performance/Project/Cluster2Part2/cluster_5_df_p4.csv")
part2cluster5group2 <- subject_info[subject_info$ID %in% c2p5g2$ID, ]
write.csv(part2cluster5group2, "part2cluster5group2.csv", row.names = FALSE)

c2p6g2 <- read.csv("~/Downloads/Performance/Project/Cluster2Part2/cluster_6_df_p4.csv")
part2cluster6group2 <- subject_info[subject_info$ID %in% c2p6g2$ID, ]
write.csv(part2cluster6group2, "part2cluster6group2.csv", row.names = FALSE)

c2p7g2 <- read.csv("~/Downloads/Performance/Project/Cluster2Part2/cluster_7_df_p4.csv")
part2cluster7group2 <- subject_info[subject_info$ID %in% c2p7g2$ID, ]
write.csv(part2cluster7group2, "part2cluster7group2.csv", row.names = FALSE)


