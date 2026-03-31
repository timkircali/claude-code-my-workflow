# =============================================================================
# Tax Enforcement Survey Experiment — Pilot Analysis
# =============================================================================
# Author:  Tim Kircali
# Date:    2026-03-30
# PAP:     Project/Overleaf/Original_Files/01_Survey_PAP.tex
#
# Pipeline:
#   1. Load & merge Qualtrics + Prolific (inner join on Prolific ID)
#   2. Apply exclusions (attention checks, bot flags)
#   3. Construct PAP-defined variables
#   4. Save data_clean.rds and data_time.rds
#   5. Run all PAP regressions
#   6. Write 04_Pilot_Results.tex
# =============================================================================

set.seed(20260330)

library(tidyverse)
library(broom)

# -----------------------------------------------------------------------------
# Paths (relative to repo root — run via: Rscript Project/Analysis/01_analysis.R)
# -----------------------------------------------------------------------------
path_qualtrics  <- "Project/Data/Data_Raw/Qualtrics/Tax Enforcement_March 30, 2026_10.12.csv"
path_prolific   <- "Project/Data/Data_Raw/Prolific/prolific_demographic_export_69b7d04312972f3940c8a2d8.csv"
path_processed  <- "Project/Data/Data_Processed"
path_output_tex <- "Project/Overleaf/Update_Files/04_Pilot_Results.tex"

dir.create(path_processed, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# SECTION 1: LOAD & MERGE
# =============================================================================

# Qualtrics: row 1 = headers, row 2 = question labels, row 3 = import IDs, row 4+ = data
q_raw <- read_csv(path_qualtrics, skip = 2, col_names = FALSE, show_col_types = FALSE)
q_headers <- read_csv(path_qualtrics, n_max = 1, col_names = TRUE, show_col_types = FALSE) %>% names()

# Handle duplicate column names (Qualtrics has duplicate "Q11", "Q42", "Time tracker_*" etc.)
# Make unique by appending position index
q_headers_unique <- make.unique(q_headers, sep = "_dup")
colnames(q_raw) <- q_headers_unique

# Drop the import-ID row (row 1 of q_raw after skip=2 is the import ID row)
q_raw <- q_raw[-1, ]

# Prolific
p_raw <- read_csv(path_prolific, show_col_types = FALSE)
# Clean Prolific column names
p_raw <- p_raw %>% rename(
  prolific_id       = `Participant id`,
  status            = Status,
  income_prolific   = `Household income (usd) [us participants only]`,
  age_prolific      = Age,
  sex_prolific      = Sex,
  ethnicity         = `Ethnicity simplified`,
  student_prolific  = `Student status`,
  employment_prolific = `Employment status`,
  bot_check         = `Authenticity check: Bots`
)

# Merge: inner join drops test responses (no Prolific ID) and unmatched
df_merged <- q_raw %>%
  rename(prolific_id = PROLIFIC_PID) %>%
  filter(nchar(prolific_id) > 20) %>%           # drop test obs (fake/empty IDs)
  filter(Finished == "1") %>%                    # drop incomplete responses
  inner_join(p_raw %>% select(prolific_id, income_prolific, age_prolific,
                               sex_prolific, ethnicity, student_prolific,
                               employment_prolific, bot_check),
             by = "prolific_id")

cat("Rows after merge:", nrow(df_merged), "\n")

# =============================================================================
# SECTION 2: EXCLUSIONS
# =============================================================================

# Helper: parse numeric safely
as_num <- function(x) suppressWarnings(as.numeric(x))

# Attention check columns (correct answer = 2 for all text-based checks)
# Attention 1: correct = 2 ("Not very often")
# Attention 2: video instruction check — non-empty means they saw it
# Attention 3: correct = 1
# Attention 4: correct = 4861 (sequential number video)
# Attention 5: correct = 1

df_merged <- df_merged %>%
  mutate(
    att1_fail = as_num(`Attention 1`) != 2,
    att2_fail = as_num(`Attention 2`) != 2,
    att3_fail = as_num(`Attention 3`) != 1,
    att4_fail = as_num(`Attention 4`) != 4861,
    att5_fail = as_num(`Attention 5`) != 1,
    att_fails = rowSums(cbind(att1_fail, att3_fail, att4_fail, att5_fail), na.rm = TRUE),
    exclude_attention = att_fails >= 2,
    exclude_bot = bot_check == "low",   # "high" = authentic, "low" = likely bot
    exclude = exclude_attention | exclude_bot
  )

cat("Excluded (attention):", sum(df_merged$exclude_attention, na.rm = TRUE), "\n")
cat("Excluded (bot):", sum(df_merged$exclude_bot, na.rm = TRUE), "\n")

df_clean_full <- df_merged  # keep full for attrition checks
df <- df_merged %>% filter(!exclude)

cat("Final analysis sample:", nrow(df), "\n")

# =============================================================================
# SECTION 3: VARIABLE CONSTRUCTION
# =============================================================================

# --- Treatment assignment ---
# Treatment arm detected by which Video Attention column set has a response:
# Cols (by original position): Evasion=161/162, Evasion&Ineq=175/176,
# Filing=189/190, Filing&Lobby=203/204, Full Info=251/252, Control=Q134/Q135
# After deduplication, find columns by checking which video-attention positions
# contain responses. We use the fact that only one arm's VA columns are filled.

# Get column indices for each arm's video attention (using unique names post-dedup)
# Original indices: 161=VA1_arm1, 162=VA2_arm1, 175=VA1_arm2, 176=VA2_arm2,
#                   189=VA1_arm3, 190=VA2_arm3, 203=VA1_arm4, 204=VA2_arm4,
#                   251=VA1_arm5, 252=VA2_arm5, 229=Q134(control), 230=Q135(control)
# After skip=2 and drop of import row, columns are 1-indexed same as original

# Use positional column references (1-based after make.unique)
# Position 162 = col index 162 in 0-based = column 163 in R 1-based
# R is 1-based, original Python was 0-based, so add 1

c_va_evasion_1   <- q_headers_unique[162]   # col 161 (0-based) = 162 (1-based)
c_va_evasion_2   <- q_headers_unique[163]
c_va_evineq_1    <- q_headers_unique[176]
c_va_evineq_2    <- q_headers_unique[177]
c_va_filing_1    <- q_headers_unique[190]
c_va_filing_2    <- q_headers_unique[191]
c_va_fillobby_1  <- q_headers_unique[204]
c_va_fillobby_2  <- q_headers_unique[205]
c_va_fullinfo_1_v1  <- q_headers_unique[230]
c_va_fullinfo_2_v1  <- q_headers_unique[231]
c_va_fullinfo_1_v2  <- q_headers_unique[252]
c_va_fullinfo_2_v2  <- q_headers_unique[253]
c_q134           <- q_headers_unique[210]   # Control arm video check


df <- df %>%
  mutate(
    treat_evasion      = !is.na(as_num(.data[[c_va_evasion_1]]))  > 0,
    treat_evineq       = !is.na(as_num(.data[[c_va_evineq_1]]))  > 0,
    treat_filing       = !is.na(as_num(.data[[c_va_filing_1]]))  > 0,
    treat_fillobby     = !is.na(as_num(.data[[c_va_fillobby_1]])) > 0,
    treat_fullinfo_1   = !is.na(as_num(.data[[c_va_fullinfo_1_v1]])) >0,
    treat_fullinfo_2   = !is.na(as_num(.data[[c_va_fullinfo_1_v2]])) >0,
    treat_control      = !treat_evasion & !treat_evineq & !treat_filing & !treat_fillobby & !treat_fullinfo_1 & !treat_fullinfo_2,
    treatment = case_when(
      treat_evasion  ~ "Evasion",
      treat_evineq   ~ "Evasion & Inequality",
      treat_filing   ~ "Filing",
      treat_fillobby ~ "Filing & Lobbying",
      treat_fullinfo_1 ~ "Full Info 1",
      treat_fullinfo_2 ~ "Full Info 2",
      treat_control  ~ "Control",
      TRUE           ~ NA_character_
    ),
    treatment = factor(treatment, levels = c("Control","Evasion","Evasion & Inequality",
                                              "Filing","Filing & Lobbying","Full Info 1","Full Info 2")),
    t_control = ifelse(treatment == "Control",1,0),
    t_evasion = ifelse(treatment == "Evasion",1,0),
    t_evasion_inequality = ifelse(treatment == "Evasion & Inequality",1,0),
    t_filing = ifelse(treatment == "Filing",1,0),
    t_filing_lobbying = ifelse(treatment == "Filing & Lobbying",1,0),
    t_full_info_1 = ifelse(treatment == "Full Info 1",1,0),
    t_full_info_2 = ifelse(treatment == "Full Info 2",1,0),
    t_full_info = ifelse(treatment == "Full Info 1",1,ifelse(treatment == "Full Info 2",1,0))
  )

cat("\nTreatment distribution:\n")
print(table(df$treatment, useNA = "always"))

# --- Helper: convert Qualtrics column to numeric ---
qn <- function(df, col) as_num(df[[col]])

# --- Outcome variables (PAP Section mapping) ---
df <- df %>% mutate(
  # Knowledge (PAP: Perception outcomes)
  Perception_Tax_Gap              = qn(df, "Q24"),
  Perception_Filing_Cost_Time     = qn(df, "Q25"),
  Perception_Filing_Cost_Cost     = qn(df, "Q26"),

  # Government views
  Government_Trust                = qn(df, "Q27"),
  Government_Public_Interest      = qn(df, "Q28"),
  Government_Reduce_Resources     = qn(df, "Q29"),
  Government_More_Involved        = qn(df, "Q30"),
  Government_Quality              = qn(df, "Q31"),

  # Considerations on economic problems
  Tax_Evasion_Serious_Problem     = qn(df, "Q32"),
  Tax_Evasion_Unequal             = qn(df, "Q33"),
  Tax_Evasion_Importance_Reduce   = qn(df, "Q34"),
  Filing_Cost_High                = qn(df, "Q35"),
  Filing_Cost_Importance_Reduce   = qn(df, "Q36"),
  Filing_Fair                     = qn(df, "Q37"),
  Data_Privacy_Concerned          = qn(df, "Q38"),
  Data_Privacy_Importance_Protect = qn(df, "Q39"),

  # TPR considerations
  TPR_Reduces_Tax_Evasion         = qn(df, "Q40"),
  TPR_Reduces_Admin_Costs         = qn(df, "Q41"),
  TPR_Increases_Tax_Revenue       = qn(df, q_headers_unique[339]),   # Q42 (first)
  TPR_Increases_Fairness          = qn(df, q_headers_unique[340]),   # Q42_dup1
  TPR_Reduces_Inequality          = qn(df, "Q43"),
  TPR_Reduces_Filing_Costs        = qn(df, "Q44"),
  TPR_Data_Privacy_Concerned      = qn(df, "Q45"),

  # Detailed data privacy (9 conditions) — Q46_1 through Q46_8 + Q45 as baseline
  TPR_Privacy_Employment_General  = qn(df, "Q46_1"),
  TPR_Privacy_Employment_Limited  = qn(df, "Q46_2"),
  TPR_Privacy_Health_General      = qn(df, "Q46_3"),
  TPR_Privacy_Health_Limited      = qn(df, "Q46_4"),
  TPR_Privacy_Bank_General        = qn(df, "Q46_5"),
  TPR_Privacy_Bank_Limited        = qn(df, "Q46_6"),
  TPR_Privacy_Payment             = qn(df, "Q46_7"),
  TPR_Privacy_Payment_Limited     = qn(df, "Q46_8"),

  # TPR support (primary + conditional)
  TPR_Support_General             = qn(df, "Q47"),

  # Conditional TPR support (Q48_1 to Q48_5 = conditions 2-6)
  TPR_Support_Equal               = qn(df, "Q48_1"),
  TPR_Support_Businesses          = qn(df, "Q48_2"),
  TPR_Support_HighIncome          = qn(df, "Q48_3"),
  TPR_Support_LargeEnterprises    = qn(df, "Q48_4"),
  TPR_Support_TaxOnly             = qn(df, "Q48_5"),

  # TPR petition
  TPR_Petition                    = qn(df, "Q49"),

  # IEI considerations
  IEI_Reduces_Tax_Evasion         = qn(df, "Q50"),
  IEI_Reduces_Admin_Costs         = qn(df, "Q51"),
  IEI_Increases_Tax_Revenue       = qn(df, "Q52"),
  IEI_Increases_Fairness          = qn(df, "Q53"),
  IEI_Reduces_Inequality          = qn(df, "Q54"),
  IEI_Reduces_Filing_Costs        = qn(df, "Q55"),
  IEI_Data_Privacy_Concerned      = qn(df, "Q56"),

  # IEI support
  IEI_Support                     = qn(df, "Q57"),

  # Tax software considerations
  TS_Reduces_Filing_Costs         = qn(df, "Q58"),
  TS_Tax_Authority_General        = qn(df, "Q59"),
  TS_Costly                       = qn(df, "Q60"),
  TS_Share_Data_Problematic       = qn(df, "Q61"),
  TS_Tax_Auth_Privacy             = qn(df, "Q62"),
  TS_Tax_Auth_Fairness            = qn(df, "Q63"),
  TS_Tax_Auth_Overall             = qn(df, "Q64"),

  # TS support
  TS_Support                      = qn(df, "Q65"),

  # PPR considerations
  PPR_Reduces_Filing_Costs        = qn(df, "Q66"),
  PPR_Costly                      = qn(df, "Q67"),
  PPR_Share_Data_Problematic      = qn(df, "Q68"),

  # PPR support
  PPR_Support                     = qn(df, "Q69"),

  # Conditional TPR support (post-PPR info) — Q70_4, Q70_5, Q70_6
  TPR_Support_PrepopData          = qn(df, "Q70_4"),
  TPR_Support_PrepopOnly          = qn(df, "Q70_5"),
  TPR_Support_VoluntaryOptIn      = qn(df, "Q70_6"),

  # Voluntary disclosure
  Voluntary_Disclosure_TaxOnly    = qn(df, "Q71_1"),
  Voluntary_Disclosure_TaxPrepop  = qn(df, "Q71_2"),
  Voluntary_Disclosure_PrepopOnly = qn(df, "Q71_3"),

  # TS/PPR petition
  TS_PPR_Petition                 = qn(df, "Q72"),
  
  #Channel indices
  TPR_Tax_Revenue_Plus = TPR_Increases_Tax_Revenue + TPR_Reduces_Admin_Costs + TPR_Reduces_Tax_Evasion,
  TPR_Inequality_Minus = TPR_Increases_Fairness + TPR_Reduces_Inequality, 
  IEI_Tax_Revenue_Plus = IEI_Increases_Tax_Revenue + IEI_Reduces_Admin_Costs + IEI_Reduces_Tax_Evasion,
  IEI_Inequality_Minus = IEI_Increases_Fairness + IEI_Reduces_Inequality
)

# --- Demographic controls (X_i^D) ---
df <- df %>% mutate(
  # From Qualtrics
  Age           = qn(df, q_headers_unique[37]),   # Q11 (age, first occurrence col 36 0-based)
  Male          = as.integer(qn(df, "Q2") == 1), #Female is coded as 2
  Female        = as.integer(qn(df, "Q2") == 2), #Female is coded as 2
  Gender_Other  = as.integer(qn(df, "Q2") == 3),
  Region_raw      = qn(df, "Q3"), #Has to be recoded
  Zipcode_raw     = qn(df, "Q4"), #Has to be recoded
  Marital_raw     = qn(df, "Q5"),
  Married       = as.integer(qn(df, "Q5") == 2),
  Widowed       = as.integer(qn(df, "Q5") == 3),
  Divorced      = as.integer(qn(df, "Q5") == 4),
  Education_raw   = qn(df, "Q6"),
  Less_Than_High_School = as.integer(qn(df, "Q6") %in% 1:2),
  Less_Than_4yr_College = as.integer(qn(df, "Q6") %in% 3:5),
  College_4yr_Plus      = as.integer(qn(df, "Q6") %in% 6:9),
  Partner      = as.integer(qn(df, "Q7") == 1),
  
  Homeowner     = as.integer(qn(df, "Q10") %in% c(1,3)),
  Employment_raw  = qn(df, q_headers_unique[88]),   # Q11 (employment, second occurrence)
  Employed      = as.integer(qn(df, q_headers_unique[88]) %in% c(1,2,4)), 
  Unemployed    = as.integer(qn(df, q_headers_unique[88]) == 5),
  Not_in_LF     = as.integer(qn(df, q_headers_unique[88]) %in% c(6,7)),
  Self_Employed = as.integer(qn(df, q_headers_unique[88]) %in% c(3,4)),
  Student       = as.integer(qn(df, "Q12") == 1),
  Income_raw      = qn(df, "Q13"),
  Income_0_20   = as.integer(qn(df, "Q13") %in% 1:3),
  Income_20_40  = as.integer(qn(df, "Q13") %in% 4:5),
  Income_40_60  = as.integer(qn(df, "Q13") %in% 6:7),
  Income_60_100 = as.integer(qn(df, "Q13") %in% 8:11),
  Income_100_150= as.integer(qn(df, "Q13") %in% 12:13),
  Income_150_200= as.integer(qn(df, "Q13") %in% 14),
  Income_Middle = as.integer(qn(df, "Q13") %in% 7:12),
  Income_High   = as.integer(qn(df, "Q13") %in% 13:14),
  Wealth_raw      = qn(df, "Q15"),
  Wealth_0_25   = as.integer(qn(df, "Q15") %in% 1:3),
  Wealth_25_50  = as.integer(qn(df, "Q15") %in% 4:6),
  Wealth_50_75  = as.integer(qn(df, "Q15") %in% 7:9),
  Wealth_75_100 = as.integer(qn(df, "Q15") %in% 10:12),
  Wealth_Middle = as.integer(qn(df, "Q15") %in% 3:5),
  Wealth_High   = as.integer(qn(df, "Q15") %in% 6:7),
  Politic_raw     = qn(df, "Q16"),
  Democrat      = as.integer(qn(df, "Q16") == 1),
  Republican    = as.integer(qn(df, "Q16") == 2),
  Independent   = as.integer(qn(df, "Q16") %in% c(3,5)),
  Other_Affil   = as.integer(qn(df, "Q16") == 4),

  # Household size and kids
  Household_Size = qn(df, "Q8")-4, #-4 to match recoding of qualtrics
  Household_Kids = qn(df, "Q9")-4, #-4 to match recoding of qualtrics

  # Income sources (Q14 is multi-select, comma-separated)
  #Income_TPR_Covered       = as.integer(grepl("1""4""6""9",  df$Q14)),
  #Income_Abroad_i          = as.integer(grepl("5","10",  df$Q14)),
  #Income_Self_Employ_i     = as.integer(grepl("2",  df$Q14)),
  #Income_Property_i        = as.integer(grepl("3",  df$Q14)),
  #Income_Privat_Business_i = as.integer(grepl("7",  df$Q14)),
  #Income_Digital_Assets_i  = as.integer(grepl("6",  df$Q14)),
  #Income_Cash_i            = as.integer(grepl("11",  df$Q14)),
  Income_TPR_Covered = as.integer(sapply(strsplit(Q14, ","), function(x) any(trimws(x) %in% c("1", "4", "6", "9")))),
  Income_Abroad = as.integer(sapply(strsplit(Q14, ","), function(x) any(trimws(x) %in% c("5", "10")))),
  Income_Self_Employ = as.integer(sapply(strsplit(Q14, ","), function(x) any(trimws(x) %in% c("2")))),
  Income_Property = as.integer(sapply(strsplit(Q14, ","), function(x) any(trimws(x) %in% c("3")))),
  Income_Privat_Business = as.integer(sapply(strsplit(Q14, ","), function(x) any(trimws(x) %in% c("7")))),
  Income_Digital_Assets = as.integer(sapply(strsplit(Q14, ","), function(x) any(trimws(x) %in% c("6")))),
  Income_Cash = as.integer(sapply(strsplit(Q14, ","), function(x) any(trimws(x) %in% c("11")))),
  
  #Qualtrics gender question has to be implemented first
  #White_i tbd 
  #Black_i tbd
  #Hispanic_i tbd
  #Asian_i tbd
  #Other_Race_i = as.integer(!ethnicity %in% c("White","Black","Hispanic","Asian")),
  
  # From Prolific
  White    = as.integer(ethnicity == "White"),
  Black    = as.integer(ethnicity == "Black"),
  Asian    = as.integer(ethnicity == "Asian"),
  Other_Race = as.integer(!ethnicity %in% c("White","Black","Asian")),

  # Exposure controls (X_i^R)
  Exposure_Tax_Filing   = as.integer(qn(df, "Q20") %in% c(1, 2, 4)),
  Exposure_Tax_Software = as.integer(qn(df, "Q21") %in% c(2, 3)), 
  Exposure_Filing_Mistakes = ifelse(qn(df, "Q22") == 6, NA, qn(df, "Q22")),
  Exposure_Filing_Mistakes_Prefernottosay = ifelse(qn(df, "Q22") == 6, 1, 0),
  Exposure_Tax_Evasion = ifelse(qn(df, "Q23") == 6, NA, qn(df, "Q23")),
  Exposure_Tax_Evasion_Prefernottosay = ifelse(qn(df, "Q23") == 6, 1, 0)
)

# =============================================================================
# SECTION 4: SAVE DATASETS
# =============================================================================

# PAP variables only
pap_vars <- c(
  "ResponseId", "prolific_id", "treatment",
  "t_control","t_evasion","t_evasion_inequality","t_filing","t_filing_lobbying","t_full_info_1","t_full_info_2","t_full_info",
  # Knowledge
  "Perception_Tax_Gap","Perception_Filing_Cost_Time","Perception_Filing_Cost_Cost",
  # Government views
  "Government_Trust","Government_Public_Interest","Government_Reduce_Resources",
  "Government_More_Involved","Government_Quality",
  # Considerations on economic problems
  "Tax_Evasion_Serious_Problem","Tax_Evasion_Unequal","Tax_Evasion_Importance_Reduce",
  "Filing_Cost_High","Filing_Cost_Importance_Reduce","Filing_Fair",
  "Data_Privacy_Concerned","Data_Privacy_Importance_Protect",
  # TPR considerations
  "TPR_Reduces_Tax_Evasion","TPR_Reduces_Admin_Costs","TPR_Increases_Tax_Revenue",
  "TPR_Increases_Fairness","TPR_Reduces_Inequality","TPR_Reduces_Filing_Costs",
  "TPR_Data_Privacy_Concerned",
  # Detailed privacy
  "TPR_Privacy_Employment_General","TPR_Privacy_Employment_Limited",
  "TPR_Privacy_Health_General","TPR_Privacy_Health_Limited",
  "TPR_Privacy_Bank_General","TPR_Privacy_Bank_Limited",
  "TPR_Privacy_Payment","TPR_Privacy_Payment_Limited",
  # TPR support
  "TPR_Support_General","TPR_Support_Equal","TPR_Support_Businesses",
  "TPR_Support_HighIncome","TPR_Support_LargeEnterprises","TPR_Support_TaxOnly",
  "TPR_Support_PrepopData","TPR_Support_PrepopOnly","TPR_Support_VoluntaryOptIn",
  "TPR_Petition",
  # IEI
  "IEI_Reduces_Tax_Evasion","IEI_Reduces_Admin_Costs","IEI_Increases_Tax_Revenue",
  "IEI_Increases_Fairness","IEI_Reduces_Inequality","IEI_Reduces_Filing_Costs",
  "IEI_Data_Privacy_Concerned","IEI_Support",
  # TS
  "TS_Reduces_Filing_Costs","TS_Tax_Authority_General","TS_Costly",
  "TS_Share_Data_Problematic","TS_Tax_Auth_Privacy","TS_Tax_Auth_Fairness",
  "TS_Tax_Auth_Overall","TS_Support","TS_PPR_Petition",
  # PPR
  "PPR_Reduces_Filing_Costs","PPR_Costly","PPR_Share_Data_Problematic","PPR_Support",
  # Voluntary disclosure
  "Voluntary_Disclosure_TaxOnly","Voluntary_Disclosure_TaxPrepop","Voluntary_Disclosure_PrepopOnly",
  # Demographics
  "Age","Male","Female","Gender_Other","Region_raw","Zipcode_raw","Married","Widowed","Divorced",
  "Less_Than_High_School","Less_Than_4yr_College","College_4yr_Plus","Partner","Homeowner",
  "Employed","Unemployed","Not_in_LF","Self_Employed","Student",
  "Income_0_20","Income_20_40","Income_40_60","Income_60_100","Income_100_150","Income_150_200","Income_Middle","Income_High","Wealth_Middle","Wealth_High",
  "Democrat","Republican","Independent","Other_Affil",
  "Household_Size","Household_Kids",
  "White","Black","Asian","Other_Race",
  #"White","Black","Asian","Hispanic","Other_Race", #As soon as i have the qualtrics demographics
  "Income_Abroad","Income_Self_Employ","Income_Property",
  "Income_Digital_Assets","Income_Cash",
  # Exposure
  "Exposure_Tax_Filing","Exposure_Tax_Software",
  "Exposure_Filing_Mistakes","Exposure_Filing_Mistakes_Prefernottosay",
  "Exposure_Tax_Evasion","Exposure_Tax_Evasion_Prefernottosay"
)

data_clean <- df %>% select(any_of(pap_vars))
saveRDS(data_clean, file.path(path_processed, "data_clean.rds"))
cat("\nSaved data_clean.rds:", nrow(data_clean), "rows,", ncol(data_clean), "columns\n")

# Time tracker dataset
time_cols <- names(df)[grepl("Submit", names(df), ignore.case = FALSE)]
data_time <- df %>% select(ResponseId, prolific_id, treatment, all_of(time_cols))
saveRDS(data_time, file.path(path_processed, "data_time.rds"))
cat("Saved data_time.rds:", nrow(data_time), "rows,", ncol(data_time), "columns\n")

# =============================================================================
# SECTION 5: REGRESSIONS
# =============================================================================

# Treatment dummies (control = reference)
treat_vars <- c("t_evasion","t_evasion_inequality","t_filing","t_filing_lobbying","t_full_info")

# Run OLS — Spec 1 (no controls)
run_ols <- function(outcome, data, controls = NULL) {
  rhs <- if (is.null(controls)) treat_vars else c(treat_vars, controls)
  # Drop rows where outcome is NA
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 3 || sum(!is.na(d[[outcome]])) < 3) return(NULL)
  fml <- as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
  tryCatch(lm(fml, data = d), error = function(e) NULL)
}

# Extract coefficient + SE for a treatment variable from a model
coef_se <- function(mod, var) {
  if (is.null(mod)) return(c(NA, NA, NA))
  s <- tryCatch(summary(mod)$coefficients, error = function(e) NULL)
  if (is.null(s) || !var %in% rownames(s)) return(c(NA, NA, NA))
  c(s[var, "Estimate"], s[var, "Std. Error"], s[var, "Pr(>|t|)"])
}

# Format: coefficient with stars, SE in parentheses below
fmt_coef <- function(est, se, pval, digits = 3) {
  if (is.na(est)) return(list(coef = "--", se = ""))
  stars <- if (!is.na(pval) && pval < 0.01) "***" else if (!is.na(pval) && pval < 0.05) "**" else if (!is.na(pval) && pval < 0.1) "*" else ""
  list(coef = paste0(formatC(est, digits = digits, format = "f"), stars),
       se   = paste0("(", formatC(se, digits = digits, format = "f"), ")"))
}

# Format R2 and N
fmt_r2 <- function(mod) if (is.null(mod)) "--" else formatC(summary(mod)$r.squared, digits = 3, format = "f")
fmt_n  <- function(mod) if (is.null(mod)) "--" else as.character(nobs(mod))

# Run all models
d <- data_clean

# ---- Outcomes per table ----

# Table 1: TPR + IEI support
m_tpr <- run_ols("TPR_Support_General", d)
m_iei <- run_ols("IEI_Support", d)

# Table 2: Conditional TPR support (panel — stacked)
tpr_conditions <- c("TPR_Support_General","TPR_Support_Equal","TPR_Support_Businesses",
                    "TPR_Support_HighIncome","TPR_Support_LargeEnterprises","TPR_Support_TaxOnly",
                    "TPR_Support_PrepopData","TPR_Support_PrepopOnly","TPR_Support_VoluntaryOptIn")
m_tpr_cond_list <- lapply(tpr_conditions, run_ols, data = d)

# Table 3: TS + PPR support
m_ts  <- run_ols("TS_Support", d)
m_ppr <- run_ols("PPR_Support", d)

# Table 4: Government views
gov_outcomes <- c("Government_Trust","Government_Public_Interest","Government_Reduce_Resources",
                  "Government_More_Involved","Government_Quality")
m_gov <- lapply(gov_outcomes, run_ols, data = d)

# Table 5: Considerations on economic problems
econ_outcomes <- c("Tax_Evasion_Serious_Problem","Tax_Evasion_Unequal","Tax_Evasion_Importance_Reduce",
                   "Filing_Cost_High","Filing_Cost_Importance_Reduce","Filing_Fair",
                   "Data_Privacy_Concerned","Data_Privacy_Importance_Protect")
m_econ <- lapply(econ_outcomes, run_ols, data = d)

# Table 6: Policy considerations TPR + IEI (14 columns)
pol_outcomes <- c("TPR_Reduces_Tax_Evasion","IEI_Reduces_Tax_Evasion",
                  "TPR_Reduces_Admin_Costs","IEI_Reduces_Admin_Costs",
                  "TPR_Increases_Tax_Revenue","IEI_Increases_Tax_Revenue",
                  "TPR_Increases_Fairness","IEI_Increases_Fairness",
                  "TPR_Reduces_Inequality","IEI_Reduces_Inequality",
                  "TPR_Reduces_Filing_Costs","IEI_Reduces_Filing_Costs",
                  "TPR_Data_Privacy_Concerned","IEI_Data_Privacy_Concerned")
m_pol <- lapply(pol_outcomes, run_ols, data = d)

# Table 7: Detailed data privacy (panel — 8 conditions)
priv_conditions <- c("TPR_Privacy_Employment_General","TPR_Privacy_Employment_Limited",
                     "TPR_Privacy_Health_General","TPR_Privacy_Health_Limited",
                     "TPR_Privacy_Bank_General","TPR_Privacy_Bank_Limited",
                     "TPR_Privacy_Payment","TPR_Privacy_Payment_Limited")
m_priv_list <- lapply(priv_conditions, run_ols, data = d)

# Table 8: TS + PPR considerations (6 cols)
tsppr_outcomes <- c("TS_Costly","PPR_Costly","TS_Reduces_Filing_Costs","PPR_Reduces_Filing_Costs",
                    "TS_Share_Data_Problematic","PPR_Share_Data_Problematic")
m_tsppr <- lapply(tsppr_outcomes, run_ols, data = d)

# Table 9: Drivers of support (channels) — no treatment, just controls + considerations
# PAP: no treatment variables in this regression; use considerations as RHS
channel_rhs_TPR <- c("Government_Trust","Government_Public_Interest","Government_Reduce_Resources",
                 "Government_More_Involved","Government_Quality",
                 "Tax_Evasion_Serious_Problem","Tax_Evasion_Unequal","Tax_Evasion_Importance_Reduce",
                 "Filing_Cost_High","Filing_Cost_Importance_Reduce","Filing_Fair",
                 "Data_Privacy_Concerned","Data_Privacy_Importance_Protect",
                 "TPR_Tax_Revenue_Plus","TPR_Inequality_Minus","TPR_Reduces_Filing_Costs","TPR_Data_Privacy_Concerned")
channel_rhs_IEI <- c("Government_Trust","Government_Public_Interest","Government_Reduce_Resources",
                     "Government_More_Involved","Government_Quality",
                     "Tax_Evasion_Serious_Problem","Tax_Evasion_Unequal","Tax_Evasion_Importance_Reduce",
                     "Filing_Cost_High","Filing_Cost_Importance_Reduce","Filing_Fair",
                     "Data_Privacy_Concerned","Data_Privacy_Importance_Protect",
                     "IEI_Tax_Revenue_Plus","IEI_Inequality_Minus","IEI_Reduces_Filing_Costs","IEI_Data_Privacy_Concerned")
channel_rhs_TS <- c("Government_Trust","Government_Public_Interest","Government_Reduce_Resources",
                     "Government_More_Involved","Government_Quality",
                     "Tax_Evasion_Serious_Problem","Tax_Evasion_Unequal","Tax_Evasion_Importance_Reduce",
                     "Filing_Cost_High","Filing_Cost_Importance_Reduce","Filing_Fair",
                     "Data_Privacy_Concerned","Data_Privacy_Importance_Protect",
                    "TS_Costly","TS_Reduces_Filing_Costs","TS_Share_Data_Problematic")
channel_rhs_PPR <- c("Government_Trust","Government_Public_Interest","Government_Reduce_Resources",
                    "Government_More_Involved","Government_Quality",
                    "Tax_Evasion_Serious_Problem","Tax_Evasion_Unequal","Tax_Evasion_Importance_Reduce",
                    "Filing_Cost_High","Filing_Cost_Importance_Reduce","Filing_Fair",
                    "Data_Privacy_Concerned","Data_Privacy_Importance_Protect",
                    "PPR_Costly","PPR_Reduces_Filing_Costs","PPR_Share_Data_Problematic")

run_channels <- function(outcome, data,channel_rhs) {
  d2 <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d2) < 3) return(NULL)
  fml <- as.formula(paste(outcome, "~", paste(channel_rhs, collapse = " + ")))
  tryCatch(lm(fml, data = d2), error = function(e) NULL)
}
m_chan_TPR <- mapply(run_channels, 
                     outcome = c("TPR_Support_General", "IEI_Support", "TS_Support", "PPR_Support"),
                     channel_rhs = list(channel_rhs_TPR, channel_rhs_IEI, channel_rhs_TS, channel_rhs_PPR),
                     MoreArgs = list(data = d),
                     SIMPLIFY = FALSE)

# Table 10 (appendix): Knowledge
m_know <- lapply(c("Perception_Tax_Gap","Perception_Filing_Cost_Time","Perception_Filing_Cost_Cost"),
                 run_ols, data = d)

# Table 11: Tax software provider (5 conditions)
tsprov_conditions <- c("TS_Tax_Authority_General","TS_Reduces_Filing_Costs",
                       "TS_Tax_Auth_Overall","TS_Tax_Auth_Privacy","TS_Tax_Auth_Fairness")
m_tsprov_list <- lapply(tsprov_conditions, run_ols, data = d)

# Table 12: Exposure (no treatment vars — just demographic controls)
exp_outcomes <- c("Exposure_Tax_Filing","Exposure_Tax_Software",
                  "Exposure_Filing_Mistakes","Exposure_Tax_Evasion")
run_exposure <- function(outcome, data) {
  dem_controls <- c("Female","Age","College_4yr_Plus","Income_Middle","Income_High",
                    "Wealth_Middle","Wealth_High","Self_Employed","Republican")
  d2 <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d2) < 3) return(NULL)
  fml <- as.formula(paste(outcome, "~", paste(dem_controls, collapse = " + ")))
  tryCatch(lm(fml, data = d2), error = function(e) NULL)
}
m_exp <- lapply(exp_outcomes, run_exposure, data = d)

# =============================================================================
# SECTION 6: BUILD LaTeX OUTPUT
# =============================================================================

treat_labels <- c("Evasion", "Evasion \\& Inequality", "Filing", "Filing \\& Lobbying", "Full info")
treat_r_vars <- treat_vars  # same order

dem_labels <- c("Female","Age","College degree","Middle income","High income",
                "Middle wealth","High wealth","Self-employed","Republican")
dem_r_vars <- c("Female","Age","College_4yr_Plus","Income_Middle","Income_High",
                "Wealth_Middle","Wealth_High","Self_Employed","Republican")

# Helper: build a row for a coefficient
make_row <- function(label, models, var) {
  cells <- lapply(models, function(m) fmt_coef(coef_se(m, var)[1], coef_se(m, var)[2], coef_se(m, var)[3]))
  coef_row <- paste0(label, " & ", paste(sapply(cells, `[[`, "coef"), collapse = " & "), " \\\\")
  se_row   <- paste0(" & ",        paste(sapply(cells, `[[`, "se"),   collapse = " & "), " \\\\")
  paste(coef_row, se_row, sep = "\n")
}

# Helper: stats rows
make_stats <- function(models) {
  n_row  <- paste("Observations &", paste(sapply(models, fmt_n),  collapse = " & "), "\\\\")
  r2_row <- paste("$R^2$ &",        paste(sapply(models, fmt_r2), collapse = " & "), "\\\\")
  paste(n_row, r2_row, sep = "\n")
}

# Helper: demographic rows block
make_dem_rows <- function(models) {
  paste(mapply(make_row, dem_labels, MoreArgs = list(models = models), var = dem_r_vars), collapse = "\n")
}

# Helper: treatment rows block
make_treat_rows <- function(models) {
  paste(mapply(make_row, treat_labels, MoreArgs = list(models = models), var = treat_r_vars), collapse = "\n")
}

# ------- TABLE 1: TPR + IEI support -------
build_table1 <- function() {
  mods <- list(m_tpr, m_iei)
  dem  <- make_dem_rows(mods)
  trts <- make_treat_rows(mods)
  int  <- make_row("Intercept", mods, "(Intercept)")
  sts  <- make_stats(mods)
  sprintf("\\begin{table}[!htbp]\\centering
\\caption{Support for Third-Party Reporting and International Exchange of Information}
\\label{tab:supportTPRIEI}
\\begin{threeparttable}
\\begin{tabular}{l|cc}
\\toprule
& (1) & (2) \\\\
 & Support & Support  \\\\
 & TPR  & IEI \\\\
\\midrule
%s\\\\[1em]
%s\\\\[0.5em]
%s
\\midrule
%s
Controls & $\\mathbf{X}_{i}^{D}$ & $\\mathbf{X}_{i}^{D}$ \\\\
\\bottomrule
\\end{tabular}
\\begin{tablenotes}[flushleft]
\\footnotesize
\\item \\textit{Notes}: PILOT RESULTS. N$\\approx$%s. Coefficients from OLS. Standard errors in parentheses. * p$<$0.1, ** p$<$0.05, *** p$<$0.01.
\\end{tablenotes}
\\end{threeparttable}
\\end{table}", dem, trts, int, sts, fmt_n(m_tpr))
}

# ------- TABLE 2: Conditional TPR support -------
build_table2 <- function() {
  cond_labels <- c(
    "Support for comprehensive third-party reporting (general)",
    "--it applied equally to all taxpayers.",
    "--it applied primarily to businesses.",
    "--it focuses on high-income individuals.",
    "--it focuses on large enterprises.",
    "--the data could excl. be used for tax administration.",
    "--the data would also be used for prepopulated returns.",
    "--the data could only be used for prepopulated returns.",
    "--taxpayers could voluntarily opt in."
  )
  rows <- mapply(function(lbl, mod) {
    cs <- coef_se(mod, "(Intercept)")  # just using intercept as placeholder for condition mean
    # For panel, show condition mean (intercept of no-control model)
    m_simple <- tryCatch(lm(as.formula(paste(tpr_conditions[which(tpr_conditions == tpr_conditions[1])], "~ 1")), data = d), error=function(e) NULL)
    # Actually show treatment effects pooled across conditions
    cs_ev  <- coef_se(mod, "treat_evasionTRUE")
    if (is.na(cs_ev[1])) cs_ev <- coef_se(mod, "treat_evasion")
    fmt <- fmt_coef(cs_ev[1], cs_ev[2], cs_ev[3])
    paste(lbl, "&", fmt$coef, "\\\\", fmt$se, "& \\\\")
  }, cond_labels, m_tpr_cond_list)

  trts <- make_treat_rows(m_tpr_cond_list[1])  # show treatment effects from general model
  int  <- make_row("Intercept", list(m_tpr_cond_list[[1]]), "(Intercept)")
  sts  <- make_stats(list(m_tpr_cond_list[[1]]))

  sprintf("\\begin{table}[!htbp]\\centering
\\caption{Conditional Support for Third-Party Reporting}
\\label{tab:conditionalsupporttpr}
\\begin{threeparttable}
\\begin{tabular}{l|c}
\\toprule
 & (1)\\\\
 & Support \\\\
 \\midrule
Support for comprehensive third-party reporting if \\\\[0.25em]
--(general) & %s \\\\[0.25em]
--it applied equally to all taxpayers. & %s \\\\[0.25em]
--it applied primarily to businesses. & %s \\\\[0.25em]
--it focuses on high-income individuals. & %s \\\\[0.25em]
--it focuses on large enterprises. & %s \\\\[0.25em]
--data only for tax enforcement. & %s \\\\[0.25em]
--data also for prepopulated returns. & %s \\\\[0.25em]
--data only for prepopulated returns. & %s \\\\[0.25em]
--taxpayers could voluntarily opt in. & %s \\\\[1em]
%s\\\\[0.5em]
%s
\\midrule
%s
Controls & $\\mathbf{X}_{i}^{D}$ \\\\
\\bottomrule
\\end{tabular}
\\begin{tablenotes}[flushleft]
\\footnotesize
\\item \\textit{Notes}: PILOT RESULTS. Condition means from separate OLS regressions. * p$<$0.1, ** p$<$0.05, *** p$<$0.01.
\\end{tablenotes}
\\end{threeparttable}
\\end{table}",
    sapply(m_tpr_cond_list, function(m) {
      cs <- coef_se(m, "(Intercept)")
      fmt_coef(cs[1], cs[2], cs[3])$coef
    }) %>% paste(collapse = ", ") %>% {strsplit(., ", ")[[1]]} %>% {.},
    make_treat_rows(list(m_tpr_cond_list[[1]])),
    make_row("Intercept", list(m_tpr_cond_list[[1]]), "(Intercept)"),
    make_stats(list(m_tpr_cond_list[[1]]))
  )
}

# Build a generic multi-column table
build_generic_table <- function(caption, label, col_headers, models, row_labels = NULL,
                                 row_vars = NULL, show_dem = TRUE, show_treat = TRUE,
                                 notes = NULL) {
  ncols <- length(models)
  col_spec <- paste0("l|", paste(rep("c", ncols), collapse = ""))
  nums <- paste(seq_len(ncols), collapse = ") & (")
  header_str <- if (!is.null(col_headers)) paste(col_headers, collapse = " & ") else ""

  rows_out <- ""
  if (show_dem) rows_out <- paste0(rows_out, make_dem_rows(models), "\n\\\\[0.5em]\n")
  if (!is.null(row_labels) && !is.null(row_vars)) {
    extra <- paste(mapply(make_row, row_labels, MoreArgs=list(models=models), var=row_vars), collapse="\n")
    rows_out <- paste0(rows_out, extra, "\n\\\\[0.5em]\n")
  }
  if (show_treat) rows_out <- paste0(rows_out, make_treat_rows(models), "\n\\\\[0.5em]\n")
  rows_out <- paste0(rows_out, make_row("Intercept", models, "(Intercept)"), "\n")

  ctrl_row <- paste("Controls &", paste(rep("$\\mathbf{X}_{i}^{D}$", ncols), collapse = " & "), "\\\\")
  note_str <- if (is.null(notes)) "PILOT RESULTS. OLS. SE in parentheses. * p$<$0.1, ** p$<$0.05, *** p$<$0.01." else notes

  paste0(
    "\\begin{table}[!htbp]\\centering\n",
    sprintf("\\caption{%s}\n\\label{%s}\n", caption, label),
    "\\begin{threeparttable}\n",
    sprintf("\\begin{tabular}{%s}\n\\toprule\n", col_spec),
    sprintf("& (%s) \\\\\n", nums),
    if (header_str != "") paste0(header_str, " \\\\\n") else "",
    "\\midrule\n",
    rows_out,
    "\\midrule\n",
    make_stats(models), "\n",
    ctrl_row, "\n",
    "\\bottomrule\n\\end{tabular}\n",
    "\\begin{tablenotes}[flushleft]\n\\footnotesize\n",
    sprintf("\\item \\textit{Notes}: %s\n", note_str),
    "\\end{tablenotes}\n\\end{threeparttable}\n\\end{table}\n"
  )
}

# Build a stacked panel regression table (Tables 2, 7, 11)
# panel_model : single lm() on stacked long data with condition dummies cond_2...cond_k
# cond_labels : row labels for condition dummy rows
# cond_vars   : coef names in model ("cond_2", ...; use "(Intercept)" to show intercept as a condition row)
# show_intercept : TRUE = separate Intercept row after treatments (Tables 2, 7)
#                  FALSE = intercept already included via cond_labels/cond_vars (Table 11)
build_panel_regression_table <- function(
  caption, label, col_label = "Support",
  panel_model,
  cond_section_header = NULL,
  cond_labels,
  cond_vars,
  show_intercept = TRUE,
  notes = NULL
) {
  mod <- list(panel_model)
  header_str <- if (!is.null(cond_section_header))
    paste0(cond_section_header, " \\\\[0.25em]\n") else ""
  cond_rows <- paste(mapply(function(lbl, var) {
    paste0(make_row(lbl, mod, var), "\\\\[0.25em]")
  }, cond_labels, cond_vars), collapse = "\n")
  int_block <- if (show_intercept)
    paste0(make_row("Intercept", mod, "(Intercept)"), "\n") else ""
  note_str <- if (is.null(notes))
    "PILOT RESULTS. Stacked panel OLS; condition dummies relative to c=1 (reference). SE in parentheses. * p$<$0.1, ** p$<$0.05, *** p$<$0.01."
  else notes
  paste0(
    "\\begin{table}[!htbp]\\centering\n",
    sprintf("\\caption{%s}\n\\label{%s}\n", caption, label),
    "\\begin{threeparttable}\n",
    "\\begin{tabular}{l|c}\n\\toprule\n",
    sprintf(" & (1)\\\\\n & %s \\\\\n\\midrule\n", col_label),
    "\\textit{Conditions} \\\\[0.25em]\n",
    header_str,
    cond_rows, "\n\\\\[1em]\n",
    "\\textit{Treatments} \\\\[0.25em]\n",
    make_treat_rows(mod), "\n\\\\[0.5em]\n",
    int_block,
    "\\midrule\n",
    make_stats(mod), "\n",
    "Controls & $\\mathbf{X}_{i}^{D}$ \\\\\n",
    "\\bottomrule\n\\end{tabular}\n",
    "\\begin{tablenotes}[flushleft]\n\\footnotesize\n",
    sprintf("\\item \\textit{Notes}: %s\n", note_str),
    "\\end{tablenotes}\n\\end{threeparttable}\n\\end{table}\n"
  )
}

# --- Panel regression helper ---
run_panel_ols <- function(outcome_vars, data) {
  long <- do.call(rbind, lapply(seq_along(outcome_vars), function(i) {
    d2 <- data[, c(treat_vars, outcome_vars[i]), drop = FALSE]
    d2$outcome_value <- as.numeric(d2[[outcome_vars[i]]])
    d2[[outcome_vars[i]]] <- NULL  # drop named column so rbind sees identical columns
    d2$condition <- i
    d2
  }))
  for (i in 2:length(outcome_vars)) {
    long[[paste0("cond_", i)]] <- as.integer(long$condition == i)
  }
  d2 <- long[!is.na(long$outcome_value), ]
  if (nrow(d2) < 3) return(NULL)
  rhs <- c(treat_vars, paste0("cond_", 2:length(outcome_vars)))
  fml <- as.formula(paste("outcome_value ~", paste(rhs, collapse = " + ")))
  tryCatch(lm(fml, data = d2), error = function(e) NULL)
}

# Table 2: 9 TPR conditions; c1 = general support (reference → intercept)
tpr_panel_vars <- c("TPR_Support_General","TPR_Support_Equal","TPR_Support_Businesses",
                    "TPR_Support_HighIncome","TPR_Support_LargeEnterprises","TPR_Support_TaxOnly",
                    "TPR_Support_PrepopData","TPR_Support_PrepopOnly","TPR_Support_VoluntaryOptIn")
m_tpr_panel <- run_panel_ols(tpr_panel_vars, d)

# Table 7: c1 = general data privacy concern; c2–c9 = specific data-type conditions
priv_panel_vars <- c("TPR_Data_Privacy_Concerned",
                     "TPR_Privacy_Employment_General","TPR_Privacy_Employment_Limited",
                     "TPR_Privacy_Health_General","TPR_Privacy_Health_Limited",
                     "TPR_Privacy_Bank_General","TPR_Privacy_Bank_Limited",
                     "TPR_Privacy_Payment","TPR_Privacy_Payment_Limited")
m_priv_panel <- run_panel_ols(priv_panel_vars, d)

# Table 11: 5 TS provider conditions; c1 = In general (reference, shown as intercept row)
tsprov_panel_vars <- c("TS_Tax_Authority_General","TS_Reduces_Filing_Costs",
                       "TS_Tax_Auth_Overall","TS_Tax_Auth_Privacy","TS_Tax_Auth_Fairness")
m_tsprov_panel <- run_panel_ols(tsprov_panel_vars, d)

# =============================================================================
# Build all tables
# =============================================================================

t1 <- build_generic_table(
  "Support for Third-Party Reporting and International Exchange of Information",
  "tab:supportTPRIEI",
  c("& Support TPR", "Support IEI"),
  list(m_tpr, m_iei)
)

t2 <- build_panel_regression_table(
  "Conditional Support for Third-Party Reporting",
  "tab:conditionalsupporttpr",
  col_label = "Support",
  panel_model = m_tpr_panel,
  cond_section_header = "Support for comprehensive third-party reporting if",
  cond_labels = c(
    "--it applied equally to all taxpayers.",
    "--it applied primarily to businesses.",
    "--it focuses on high-income individuals.",
    "--it focuses on large enterprises.",
    "--the data collected could only be used for purposes related to tax enforcement and tax administration.",
    "--the data collected would also be used to offer prepopulated tax returns.",
    "--the data collected could only be used to offer prepopulated tax returns.",
    "--taxpayers could voluntarily choose to have their own data covered."
  ),
  cond_vars = paste0("cond_", 2:9),
  show_intercept = TRUE
)

t3 <- build_generic_table(
  "Support for free tax software and prepopulated tax returns",
  "tab:supporttaxsoftware",
  c("& Support TS", "Support PPR"),
  list(m_ts, m_ppr)
)

t4 <- build_generic_table(
  "Government Views",
  "tab:government_views",
  c("& Trust in Gov.", "Gov. Actions Align", "Decrease Resources", "More Involved", "Quality"),
  m_gov
)

t5 <- build_generic_table(
  "Considerations on Economic Problems",
  "tab:considerationseconomicproblems",
  c("& Evasion Serious","Evasion Unequal","Reduce Evasion","Filing Costs High",
    "Reduce Filing Costs","Filing Fair","Data Privacy Concern","Data Privacy Important"),
  m_econ
)

t6 <- build_generic_table(
  "Considerations on Third-Party Reporting and International Exchange of Information",
  "tab:policyconsiderationstpr",
  NULL,
  m_pol
)

t7 <- build_panel_regression_table(
  "Detailed Data Privacy Concerns",
  "tab:detaileddataprivacy",
  col_label = "Concerned about data privacy",
  panel_model = m_priv_panel,
  cond_section_header = "if third-party reporting includes..",
  cond_labels = c(
    "--employment data in general",
    "--employment data limited to tax relevant information",
    "--general health insurance data",
    "--health insurance data limited to tax-relevant financial information",
    "--general bank account data",
    "--bank account data limited to tax-relevant information",
    "--payment data",
    "--payment data limited to tax-relevant information"
  ),
  cond_vars = paste0("cond_", 2:9),
  show_intercept = TRUE
)

t8 <- build_generic_table(
  "Considerations on Tax Software and Prepopulated Tax Returns",
  "tab:considerationsts",
  c("& Costly TS","Costly PPR","Reduces Costs TS","Reduces Costs PPR",
    "Data Problematic TS","Data Problematic PPR"),
  m_tsppr
)

# Table 9: Channels (no treatment vars)
build_channels_table <- function() {
  mods <- m_chan_TPR  # list of 4 models: TPR, IEI, TS, PPR

  # Common channel variables — appear in all 4 models
  common_labels <- c(
    "Trust in government", "Gov. aligns w. public interest",
    "Gov. decrease resources", "Gov. more involved", "Quality of gov. services",
    "Evasion is serious", "Evasion is unequal", "Reduce evasion important",
    "Filing costs are high", "Reduce filing costs important", "Filing process is fair",
    "Data collection concern", "Data privacy important"
  )
  common_vars <- c(
    "Government_Trust","Government_Public_Interest","Government_Reduce_Resources",
    "Government_More_Involved","Government_Quality",
    "Tax_Evasion_Serious_Problem","Tax_Evasion_Unequal","Tax_Evasion_Importance_Reduce",
    "Filing_Cost_High","Filing_Cost_Importance_Reduce","Filing_Fair",
    "Data_Privacy_Concerned","Data_Privacy_Importance_Protect"
  )

  # Policy-specific channels: each row has a different var per column
  # (NA means that variable is not in that model → shows "--")
  pol_labels <- c("Revenue index", "Inequality index", "Reduces filing costs", "Data privacy concern")
  pol_vars <- list(
    c("TPR_Tax_Revenue_Plus",    "IEI_Tax_Revenue_Plus",    "TS_Costly",                 "PPR_Costly"),
    c("TPR_Inequality_Minus",    "IEI_Inequality_Minus",    "TS_Reduces_Filing_Costs",   "PPR_Reduces_Filing_Costs"),
    c("TPR_Reduces_Filing_Costs","IEI_Reduces_Filing_Costs","TS_Share_Data_Problematic", "PPR_Share_Data_Problematic"),
    c("TPR_Data_Privacy_Concerned","IEI_Data_Privacy_Concerned", NA, NA)
  )

  make_pol_row <- function(label, vars_per_col) {
    cells <- vector("list", length(mods))
    for (i in seq_along(mods)) {
      v <- vars_per_col[i]
      if (is.na(v)) {
        cells[[i]] <- list(coef = "--", se = "")
      } else {
        cs <- coef_se(mods[[i]], v)
        cells[[i]] <- fmt_coef(cs[1], cs[2], cs[3])
      }
    }
    coef_row <- paste0(label, " & ", paste(sapply(cells, `[[`, "coef"), collapse = " & "), " \\\\")
    se_row   <- paste0(" & ",        paste(sapply(cells, `[[`, "se"),   collapse = " & "), " \\\\")
    paste(coef_row, se_row, sep = "\n")
  }

  common_rows <- paste(
    mapply(make_row, common_labels, MoreArgs = list(models = mods), var = common_vars),
    collapse = "\n"
  )
  pol_rows <- paste(mapply(make_pol_row, pol_labels, pol_vars), collapse = "\n")
  int_row  <- make_row("Intercept", mods, "(Intercept)")

  paste0(
    "\\begin{table}[!htbp]\\centering\n",
    "\\caption{Drivers of Policy Support}\n\\label{tab:PolicySupportDrivers}\n",
    "\\begin{threeparttable}\n",
    "\\begin{tabular}{l|cccc}\n\\toprule\n",
    "& (1) & (2) & (3) & (4) \\\\\n",
    "& Support TPR & Support IEI & Support TS & Support PPR \\\\\n",
    "\\midrule\n",
    common_rows, "\n\\\\[0.5em]\n",
    pol_rows, "\n\\\\[0.5em]\n",
    int_row, "\n",
    "\\midrule\n",
    make_stats(mods), "\n",
    "Controls & $\\mathbf{X}_{i}^{D}$ & $\\mathbf{X}_{i}^{D}$ & $\\mathbf{X}_{i}^{D}$ & $\\mathbf{X}_{i}^{D}$ \\\\\n",
    "\\bottomrule\n\\end{tabular}\n",
    "\\begin{tablenotes}[flushleft]\n\\footnotesize\n",
    "\\item \\textit{Notes}: PILOT RESULTS. No treatment variables --- channels regression only. OLS. SE in parentheses. * p$<$0.1, ** p$<$0.05, *** p$<$0.01.\n",
    "\\end{tablenotes}\n\\end{threeparttable}\n\\end{table}\n"
  )
}
t9 <- build_channels_table()

t10 <- build_generic_table(
  "Knowledge of Economic Problems",
  "tab:perception",
  c("& Perception Tax Gap","Perception Filing Time","Perception Filing Cost"),
  m_know
)

t11 <- build_panel_regression_table(
  "Private or Government Tax Software Provider",
  "tab:taxsoftwareprovider",
  col_label = "Tax Authority Should Provide Tax Software",
  panel_model = m_tsprov_panel,
  cond_section_header = NULL,
  cond_labels = c(
    "--In general",
    "--In terms of reducing time and monetary costs for taxpayers",
    "--In terms of overall costs to the society",
    "--In terms of data privacy",
    "--In terms of fairness in how tax filing is organized"
  ),
  cond_vars = c("(Intercept)", paste0("cond_", 2:5)),
  show_intercept = FALSE
)

# Table 12: Exposure (no treatments)
build_exposure_table <- function() {
  dem_labels2 <- c("Female","Age","College degree","Middle income","High income",
                   "Middle wealth","High wealth","Self-employed","Republican")
  dem_vars2   <- c("Female","Age","College_4yr_Plus","Income_Middle","Income_High",
                   "Wealth_Middle","Wealth_High","Self_Employed","Republican")
  rows_out <- paste(mapply(make_row, dem_labels2, MoreArgs = list(models = m_exp), var = dem_vars2), collapse = "\n")
  rows_out <- paste0(rows_out, "\n", make_row("Intercept", m_exp, "(Intercept)"))
  paste0(
    "\\begin{table}[!htbp]\\centering\n",
    "\\caption{Exposure to Tax Filing and Evasion}\n\\label{tab:exposure}\n",
    "\\begin{threeparttable}\n",
    "\\begin{tabular}{lcccc}\n\\toprule\n",
    "& (1) & (2) & (3) & (4) \\\\\n",
    "& Exposure: Tax Filing & Exposure: Tax Software & Exposure: Filing Mistakes & Exposure: Evasion \\\\\n",
    "\\midrule\n",
    rows_out, "\n",
    "\\midrule\n",
    make_stats(m_exp), "\n",
    "Controls & $\\mathbf{X}_{i}^{D}$ & $\\mathbf{X}_{i}^{D}$ & $\\mathbf{X}_{i}^{D}$ & $\\mathbf{X}_{i}^{D}$ \\\\\n",
    "\\bottomrule\n\\end{tabular}\n",
    "\\begin{tablenotes}[flushleft]\n\\footnotesize\n",
    "\\item \\textit{Notes}: PILOT RESULTS. No treatment variables. OLS on demographic controls. SE in parentheses.\n",
    "\\end{tablenotes}\n\\end{threeparttable}\n\\end{table}\n"
  )
}
t12 <- build_exposure_table()

# =============================================================================
# WRITE TEX FILE
# =============================================================================

tex_doc <- paste0(
"\\documentclass[11pt]{article}
\\usepackage[top=1in, bottom=1in, left=1in, right=1in]{geometry}
\\usepackage{booktabs}
\\usepackage{threeparttable}
\\usepackage{pdflscape}
\\usepackage{placeins}
\\usepackage{amsmath}
\\usepackage{xcolor}
\\usepackage{longtable}
\\usepackage{threeparttablex}

\\title{Tax Enforcement Survey Experiment\\\\
\\large \\textcolor{red}{PILOT RESULTS --- N$\\approx$", nrow(data_clean), " --- FOR ILLUSTRATION ONLY}\\\\
\\large \\textcolor{red}{Standard errors unreliable at this sample size. Do not interpret substantively.}}
\\author{Tim Kircali, Stefanie Stantcheva, Matthias Weber}
\\date{", format(Sys.Date(), "%B %d, %Y"), "}

\\begin{document}
\\maketitle
\\tableofcontents
\\newpage

\\section{Main Results}

", t1, "\n\\FloatBarrier\n",
t2, "\n\\FloatBarrier\n",
t3, "\n\\FloatBarrier\n",
t4, "\n\\FloatBarrier\n",
t5, "\n\\FloatBarrier\n",
t6, "\n\\FloatBarrier\n",
t7, "\n\\FloatBarrier\n",
t8, "\n\\FloatBarrier\n",
t9, "\n\\FloatBarrier\n",

"\\section{Appendix}

", t10, "\n\\FloatBarrier\n",
t11, "\n\\FloatBarrier\n",
t12, "\n\\FloatBarrier\n",

"\\end{document}\n"
)

writeLines(tex_doc, path_output_tex)
cat("\nWrote:", path_output_tex, "\n")
cat("Done.\n")
