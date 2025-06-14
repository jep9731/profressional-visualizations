## Amyloid Discrepant Visual SUVr reads

# Import libraries --------------------------------------------------------
library(tidyverse)
library(readr)
library(writexl)

# Import data -------------------------------------------------------------
PET_metrics <- read.csv('AlzheimersDiseaseRes-AmyloidPETmetricsADC_DATA_LABELS_2024-10-28_1324.csv')
nrow(PET_metrics)

ripple <- read.csv('2024-12-09T17_58_12.809Z_clinical_core_export.csv')

SCAN_aPET_GAAIN <- read.csv('SCAN Amyloid PET (MRI-free) GAAIN analysis data.csv')
SCAN_aPET_GAAIN
nrow(SCAN_aPET_GAAIN)

SCAN_aPET_NPDKA <- read.csv('SCAN Amyloid PET (MRI-free) NPDKA analysis data.csv')
SCAN_aPET_NPDKA
nrow(SCAN_aPET_NPDKA)

# Clean data --------------------------------------------------
# PET metrics data
PET_metrics_clean <- PET_metrics %>%
  select("Ripple.Global.ID", "Amyloid.PET.scan.date.of.occurence", # Select important columns to keep
         "Amyloid.PET.Clinician.read..Rater..1.", "Amyloid.PET.Clinician.read.source..Rater..1.",
         "Amyloid.PET.Clinician.read..Rater..2.", "Amyloid.PET.Clinician.read.source..Rater..2.",
         "Amyloid.PET.Clinician.read.CONSENSUS", "Amyloid.PET.Mean.SUVr...Susan.Landau.method") %>%
  rename(., global_id = Ripple.Global.ID, scan_date = Amyloid.PET.scan.date.of.occurence, # Rename columns for easier readability
         clinician_read_1 = Amyloid.PET.Clinician.read..Rater..1., clinician_rater_1 = Amyloid.PET.Clinician.read.source..Rater..1.,
         clinician_read_2 = Amyloid.PET.Clinician.read..Rater..2., clinician_rater_2 = Amyloid.PET.Clinician.read.source..Rater..2.,
         consensus_read = Amyloid.PET.Clinician.read.CONSENSUS, mean_SUVr = Amyloid.PET.Mean.SUVr...Susan.Landau.method) %>%
  mutate(
    method = rep("IBC", nrow(PET_metrics)) # Creates new column with method type
  ) %>%
  filter(scan_date > "2020-12-31") %>% # Filter by date after 2020 (SCAN started in 2021)
  na.omit(mean_SUVr) # Removes rows with missing SUVr values
PET_metrics_clean

# Create new column with final read
PET_metrics_final <- PET_metrics_clean %>%
  mutate(final_read = case_when(clinician_read_2 == "" ~ clinician_read_1, # if clinician_read_2 is blank, put the clinician read 1 value 
                                clinician_read_1 != PET_metrics_clean$clinician_read_2 ~ consensus_read, # if discrepant read, put consensus read
                                TRUE ~ PET_metrics_clean$clinician_read_1), # everything else, put clinician read 1 value
         .after = consensus_read) # place the new column after the consensus read column
PET_metrics_final

# Clean ripple data
ripple_clean <- ripple %>%
  rename(global_id = globalId, PTID = cv.ptid) # Changing name of ripple output to match other datasets outputs

# Clean SCAN GAAIN method
SCAN_aPET_GAAIN_clean <- SCAN_aPET_GAAIN %>%
  select(AMYLOID_STATUS, SCANDATE, CENTILOIDS, GAAIN_SUMMARY_SUVR, PTID) %>% # Selects necessary columns
  rename(mean_SUVr = GAAIN_SUMMARY_SUVR, scan_date = SCANDATE) %>% # Renames SUVr and scan date columns
  mutate(
    method = rep("GAAIN", nrow(SCAN_aPET_GAAIN)), # Creates new column with method type
    final_read = ifelse(AMYLOID_STATUS == 0, "Negative", "Positive") # Creates new column with read
    )
SCAN_aPET_GAAIN_clean

# Clean SCAN NPDKA method
SCAN_aPET_NPDKA_clean <- SCAN_aPET_NPDKA %>%
  select(SCANDATE, NPDKA_SUMMARY_SUVR, PTID) %>% # Selects necessary columns
  rename(mean_SUVr = NPDKA_SUMMARY_SUVR, scan_date = SCANDATE) %>% # Renames SUVr and scan date columns
  mutate(
    method = rep("NPDKA", nrow(SCAN_aPET_NPDKA)), # Creates new column with method type
    final_read = ifelse(mean_SUVr >= 1.12, "Positive", "Negative") # Creates final read for NPDKA method
  )
SCAN_aPET_NPDKA_clean

# Merge datasets
IC_merged <- inner_join(PET_metrics_final, ripple_clean)
merged <- inner_join(ripple_clean, full_join(SCAN_aPET_NPDKA_clean, SCAN_aPET_GAAIN_clean))

merged_final <- merge(IC_merged, merged, by = intersect(names(IC_merged), names(merged)), all = T)
merged_final

# Determine number of reads that only have 1 read -------------------------
one_clinical_read <- PET_metrics_final[which(PET_metrics_final$clinician_read_2 == ""), ]
one_clinical_read

table(number_of_reads_by_one_clinician = nrow(one_clinical_read))

# Determine visual and SUVr discrepancies ---------------------------------
discrepant_reads_neg <- PET_metrics_final[PET_metrics_final$final_read == "Negative" & 
                                            PET_metrics_final$mean_SUVr >= 1.2, ] # Negative discrepancies
discrepant_reads_neg

discrepant_reads_pos <- PET_metrics_final[PET_metrics_final$final_read == "Positive" & 
                                            PET_metrics_final$mean_SUVr <= 1.2, ] # Positive discrepancies
discrepant_reads_pos

discrepant_reads_final <- rbind(discrepant_reads_neg, discrepant_reads_pos) # Combine into a dataframe
discrepant_reads_final
table(number_of_visual_suvr_discrepancies = nrow(discrepant_reads_final))

# Create plots -------------------------------------------------------------
ggplot(data = PET_metrics_final, aes(x = global_id, y = mean_SUVr)) + # Add data and mapping; x = global_id, y = SUVr
  geom_point(size = 2, aes(shape = final_read, color = final_read)) + # Create plot and distinguishing shape and color by clinical read
  geom_hline(yintercept = 1.2, color = "black", linetype = "dashed", linewidth = .5) + # Add horizontal line at SUVr cutoff of 1.2
  geom_point(data = PET_metrics_final[PET_metrics_final$final_read == "Negative" & PET_metrics_final$mean_SUVr >= 1.2, ], 
             aes(shape = final_read, color = final_read), pch=0, fill=NA, size=4, color="red", stroke=1) + # Creates circle around all negative reads that are greater than or equal 1.2
  geom_point(data = PET_metrics_final[PET_metrics_final$final_read == "Positive" & PET_metrics_final$mean_SUVr <= 1.2, ],
             aes(shape = final_read, color = final_read), pch=0, fill=NA, size=4, color="red", stroke=1) + # Creates circle around all positive reads that are less than or equal 1.2
  geom_text(data = PET_metrics_final[PET_metrics_final$final_read == "Negative" & PET_metrics_final$mean_SUVr >= 1.2, ],
            aes(label = mean_SUVr, x = global_id, y = mean_SUVr), position = position_dodge(0.9), vjust = -.95, size = 3.5) + # add data labels for neg/SUVr >= 1.2 discrepancies
  geom_text(data = PET_metrics_final[PET_metrics_final$final_read == "Positive" & PET_metrics_final$mean_SUVr <= 1.2, ],
            aes(label = mean_SUVr, x = global_id, y = mean_SUVr), position = position_dodge(0.9), vjust = -.95, size = 3.5) + # add data labels for pos/SUVr <= 1.2 discrepancies
  labs(title = "IBC Mean SUVr by Clinical Visual Read For Amyloid PET", # Change title of plot
       x = "", # Remove x axis label
       y = "Mean SUVr", # Change y axis label
       color = "Clinical Read", # Change legend color title
       shape = "Clinical Read",
       point = "Visual/Quantitative Discrepancy") + # Change legend shape title
  scale_y_continuous(breaks = seq(0, 2.2, by = .2)) + # Change y axis tick values
  scale_color_manual(values = c("darkred", "darkblue"), # Change color of points
                     breaks = c("Negative", "Positive")) + # Add labels
  scale_shape_manual(values = c(19, 17), # Change shape of points
                     breaks = c("Negative", "Positive")) + # Add labels
  theme(legend.box.background = element_rect(), # Add legend box
        legend.background = element_blank(), # Remove legend background
        legend.key = element_blank(), # Remove box around colors
        legend.position = "inside", # Move legend inside graph
        legend.position.inside = c(.9, .92), # Change position of legend
        panel.background = element_blank(), # Remove plot panel background
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.border = element_blank(), # Remove panel border
        axis.ticks.x = element_blank(), # Remove x axis
        axis.line.x = element_blank(), # Remove x line
        axis.text.x = element_blank(), # Remove x values
        axis.line.y = element_line(linewidth = .5, linetype = "solid"), # Change y axis line size
        plot.title = element_text(size = 14, hjust = .5, face = "bold") # Change title size and position
  )

ggplot(data = merged_final, aes(x = global_id, y = mean_SUVr, group = method)) + # Add data and mapping; x = global_id, y = SUVr
  geom_point(size = 2, aes(color = final_read)) + # Create plot and distinguishing shape and color by clinical read
  geom_hline(aes(yintercept = 1.2, linetype = "IBC cutoff"), linewidth = .5, color = "black") + # Add horizontal line for IBC SUVr cutoff of 1.2
  geom_hline(aes(yintercept = 1.12, linetype = "SCAN cutoff"), linewidth = .5, color = "red") + # Add horizontal line at SCAN SUVr cuttoff of 1.12
  facet_wrap(method ~ .) +
  labs(title = "Amyloid PET Clinical Visual Reads Between IBC & SCAN Methods", # Change title of plot
       x = "", # Remove x axis label
       y = "Mean SUVr", # Change y axis label
       color = "Clinical Read:") + # Change legend color title
  scale_y_continuous(breaks = seq(0, 2.2, by = .2)) + # Change y axis tick values
  scale_color_manual(values = c("darkred", "darkblue"), # Change color of points
                     breaks = c("Negative", "Positive")) + # Add labels
  scale_linetype_manual(name = "SUVr Cutoff:", # Change legend title
                        values = c(2, 2), # Add linetype values
                        guide = guide_legend(override.aes = list(color = c("black", "red")))) + # Override color
  theme(legend.box.background = element_rect(), # Add legend box
        legend.background = element_blank(), # Remove legend background
        legend.key = element_blank(), # Remove box around colors
        legend.position = "top", # Move under title
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.line.x = element_blank(), # Remove x line
        axis.text.x = element_blank(), # Remove x values
        axis.ticks.x = element_blank(), # Remove x tick lines
        axis.line.y = element_line(linewidth = .5, linetype = "solid"), # Change y axis line size
        plot.title = element_text(size = 14, hjust = .5, face = "bold") # Change title size and position
  )

ggplot(data = merged_final, aes(x = mean_SUVr, group = method)) + # Add data and mapping; x = mean_SUVr, group = method
  geom_histogram(bins = 30, color = "black", position = "dodge", # Change number of bins, create black boarder, make bars dodge
                 alpha = .75, aes(fill = method)) + # Change transparency and set fill to method
  facet_wrap(~factor(method, c("IBC", "GAAIN", "NPDKA"))) + # Change order in facet wrap
  labs(title = "Amyloid PET Clinical Visual Reads Between IBC & SCAN Methods", # Change title of plot
       x = "Mean SUVr Value", # Change x axis title
       y = "Count", # Change y axis title
       fill = "Method:") + # Change legend fill title
  scale_fill_manual(values = c("darkred", "darkblue", "darkgreen"), # Change colors of bars
                    breaks = c("IBC", "GAAIN", "NPDKA")) + # Change order in legend
  scale_x_continuous(breaks = seq(0, 2.2, by = .2)) + # Change x axis tick values
  theme(legend.box.background = element_rect(), # Add legend box
        legend.background = element_blank(), # Remove legend background
        legend.key = element_blank(), # Remove box around colors
        legend.position = "top", # Move under title
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.line.x = element_line(linewidth = .5, linetype = "solid"), # Change x axis line size and boldness
        axis.line.y = element_line(linewidth = .5, linetype = "solid"), # Change y axis line size & boldness
        plot.title = element_text(size = 14, hjust = .5, face = "bold") # Change title size and position
  )

# Export tables -----------------------------------------------------------
writexl::write_xlsx(one_clinical_read, "one_clinical_read_all.xlsx")
writexl::write_xlsx(discrepant_reads_final, "all visual_quantitative discrepancies.xlsx")
