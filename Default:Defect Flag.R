# Group Project 4

setwd('/Users/gavin-kunish/Downloads')

library(readr)
library(dplyr)

df_orig <- read.delim('sample_orig_2007.txt', sep = "|", header = FALSE, stringsAsFactors = FALSE, na.strings = "")
df_mp <- read.delim("sample_svcg_2007.txt", sep = "|", header = FALSE, stringsAsFactors = FALSE, na.strings = "")

origination_colnames <- c(
  "credit_score",
  "first_payment_date",
  "first_time_homebuyer_flag",
  "maturity_date",
  "msa",
  "mi_percent",
  "num_units",
  "occupancy_status",
  "original_cltv",
  "original_dti",
  "original_upb",
  "original_ltv",
  "original_interest_rate",
  "channel",
  "ppm_flag",
  "amortization_type",
  "property_state",
  "property_type",
  "zip_code",
  "loan_sequence_number",
  "loan_purpose",
  "loan_term",
  "num_borrowers",
  "seller_name",
  "servicer_name",
  "super_conforming_flag",
  "pre_relief_refi_loan_seq_num",
  "program_indicator",
  "relief_refi_indicator",
  "property_valuation_method",
  "interest_only_indicator",
  "mi_cancel_indicator"
)

colnames(df_orig) <- origination_colnames

performance_colnames <- c(
  "loan_sequence_number",
  "reporting_period",
  "current_actual_upb",
  "current_loan_delinquency_status",
  "loan_age",
  "remaining_months_to_maturity",
  "defect_settlement_date",
  "modification_flag",
  "zero_balance_code",
  "zero_balance_effective_date",
  "current_interest_rate",
  "non_interest_bearing_upb",
  "ddlpi",  # Due Date of Last Paid Installment
  "mi_recoveries",
  "net_sale_proceeds",
  "non_mi_recoveries",
  "total_expenses",
  "legal_costs",
  "maintenance_preservation_costs",
  "taxes_insurance",
  "misc_expenses",
  "actual_loss",
  "cumulative_modification_cost",
  "step_mod_flag",
  "payment_deferral_flag",
  "estimated_ltv",
  "zero_balance_removal_upb",
  "delinquent_accrued_interest",
  "disaster_flag",
  "borrower_assistance_status",
  "current_month_modification_cost",
  "interest_bearing_upb"
)

colnames(df_mp) <- performance_colnames

str(df_orig)
summary(df_orig)

colSums(is.na(df_orig))              
sapply(df_orig, function(x) sum(x %in% c(9, 99, 999, 9999)))  

df_mp_trimmed <- df_mp %>%
  select(1,4,5,7,22) %>%
  setNames(c("loan_sequence_number", "status", "loan_age", "defect_settlement_date", "actual_loss"))

n_distinct(df_mp_trimmed$loan_sequence_number) #all id's are unique

summary(df_mp_trimmed$status) #character, but it looks like all int outside of RA values
summary(is.na(df_mp_trimmed$status)) # no missing values
table(df_mp_trimmed$status, useNA = "ifany") #at least 1 RA          
unique(df_mp_trimmed$status)    #at least 1 RA                       
sum(!grepl("^[0-9]+$", df_mp_trimmed$status)) # ~21k RA values

summary(df_mp_trimmed$loan_age) #character
summary(is.na(df_mp_trimmed$loan_age)) # no missing values
sum(!grepl("^[0-9]+$", df_mp_trimmed$loan_age)) # no non numeric values


table(nchar(df_mp_trimmed$defect_settlement_date), useNA = "ifany") # majority of loans did not defect
head(sort(unique(df_mp_trimmed$defect_settlement_date)))
sum(df_mp_trimmed$defect_settlement_date == "", na.rm = TRUE)


summary(df_mp_trimmed$actual_loss)
summary(is.na(df_mp_trimmed$actual_loss)) #~\ ~4500 non missing values\
sum(!grepl("^-?[0-9.]+$", df_mp_trimmed$actual_loss))  # no non numerics

df_mp_f36 <- df_mp_trimmed %>%
  filter(as.numeric(loan_age) <= 36)

n_distinct(df_mp_f36$loan_sequence_number) # 26 unique id's removed


default_flag <- df_mp_f36 %>%
  mutate(
    status_num = suppressWarnings(as.numeric(status)),
    loss_num = suppressWarnings(as.numeric(actual_loss)),
    trigger = ((!is.na(status_num) & status_num >= 3) | status == "RA") | 
      (!is.na(loss_num) & loss_num < 0)
    ) %>%
  group_by(loan_sequence_number) %>%
  summarise(default_flag = as.integer(any(trigger)), .groups = "drop") %>%
  pull(default_flag)

table(default_flag)

defect_flag <- df_mp_f36 %>%
  group_by(loan_sequence_number) %>%
  summarise(defect_flag = as.integer(any(!is.na(defect_settlement_date) & defect_settlement_date != "")), .groups = "drop") %>%
  pull(defect_flag)  

table(defect_flag)

loan_ids <- unique(df_mp_f36$loan_sequence_number)

loan_flags <- data.frame(
  loan_sequence_number = loan_ids,
  default_flag = default_flag,
  defect_flag = defect_flag,
  stringsAsFactors = FALSE
)

df_orig_flagged <- df_orig %>%
  inner_join(loan_flags, by = "loan_sequence_number")

table(df_orig_flagged$credit_score, useNA = "ifany")


default_df <- df_orig_flagged %>%
  mutate(
    credit_score = ifelse(credit_score == 9999, NA, credit_score),
    original_cltv = ifelse(original_cltv == 999, NA, original_cltv),
    original_dti = ifelse(original_dti == 999, NA, original_dti)
  )

default.logit <- glm(default_flag ~
                       credit_score + original_cltv + original_dti + original_upb + original_interest_rate,
                     family = binomial(link=logit),
                     data = default_df
)
summary(default.logit)

default.probit <- glm(default_flag ~
                        credit_score + original_cltv + original_dti + original_upb + original_interest_rate,
                      family = binomial(link=probit),
                      data = default_df
)
summary(default.probit)

confusion <- function(glm) table(list(Actual=glm$y,Preds=(glm$fitted.values>0.5)))
cf.logit <- confusion(default.logit)
cf.probit <- confusion(default.probit)
cf.logit
cf.probit

cf.l <- as.data.frame.matrix(cf.logit)
cf.p <- as.data.frame.matrix(cf.probit)

accuracy <- c((cf.l[2,2] + cf.l[1,1]) / sum(cf.l),
              (cf.p[2,2] + cf.p[1,1]) / sum(cf.p))
precision <- c(cf.l[2,2] / (cf.l[2,2] + cf.l[1,2]),
               cf.p[2,2] / (cf.p[2,2] + cf.p[1,2]))
recall <- c(cf.l[2,2] / (cf.l[2,1] + cf.l[2,2]),
            cf.p[2,2] / (cf.p[2,1] + cf.p[2,2]))
balanced_acc <- c(0.5 * (cf.l[2,2] / (cf.l[2,1] + cf.l[2,2]) + cf.l[1,1] / (cf.l[1,1] + cf.l[1,2])),
                  0.5 * (cf.p[2,2] / (cf.p[2,1] + cf.p[2,2]) + cf.p[1,1] / (cf.p[1,1] + cf.p[1,2])))

rnd.pct <- function(x){paste0(round(x, digits = 4)*100, '%')}

model_metrics <- data.frame(
  model = c("Logit", "Probit"),
  accuracy = rnd.pct(accuracy),
  precision = rnd.pct(precision),
  recall = rnd.pct(recall),
  balanced_acc = rnd.pct(balanced_acc),
  stringsAsFactors = FALSE
)
model_metrics

# Expected Loss = .25 * Loan Balance * Predicted Prob.
# I think Loan balance is UPB (Unpaid Principal Balance)

# Find predicted probabilities

default_df$predicted_prob <- predict(default.logit, newdata = default_df, type = "response")


expected_loss <- default_df %>%
  filter(defect_flag == 1) %>%
  mutate(expected_loss = 0.25 * as.numeric(original_upb) * predicted_prob) %>%
  summarise(total_expected_loss = sum(expected_loss, na.rm = TRUE)) %>%
  pull(total_expected_loss)

expected_loss


observed_loss <- default_df %>%
  filter(defect_flag == 1, default_flag == 1) %>%
  summarise(total_observed_loss = sum(0.25 * as.numeric(original_upb), na.rm = TRUE)) %>%
  pull(total_observed_loss)

observed_loss

scale_1M <- 1000000 / nrow(default_df)

scaled_EL <- scale_1M * expected_loss
scaled_OL <- scale_1M * observed_loss

scaled_EL
scaled_OL

scaled_OL - scaled_EL

refined_df <- df_orig_flagged %>%
  mutate(
    credit_score = ifelse(credit_score == 9999, NA, credit_score),
    original_cltv = ifelse(original_cltv == 999, NA, original_cltv),
    original_dti = ifelse(original_dti == 999, NA, original_dti),
    num_borrowers = ifelse(num_borrowers == 99, NA, original_dti),
    loan_purpose = factor(loan_purpose),
    occupancy_status = factor(occupancy_status),
    first_time_homebuyer_flag = factor(first_time_homebuyer_flag),
    interest_only_indicator = factor(interest_only_indicator),
    channel = factor(channel),
    ppm_flag = factor(ppm_flag),
    amortization_type = factor(amortization_type),
    property_state = factor(property_state),
    property_type = factor(property_type),
    zip_code = factor(zip_code),
    # loan_term = factor(loan_term),
    defect_flag = factor(defect_flag)
  ) %>%
  subset(select = -c(amortization_type, seller_name,
                     servicer_name, super_conforming_flag, pre_relief_refi_loan_seq_num,
                     program_indicator, relief_refi_indicator, property_valuation_method,
                     interest_only_indicator, mi_cancel_indicator))

loan_age_latest <- df_mp_f36 %>%
  group_by(loan_sequence_number) %>%
  slice_max(order_by = loan_age, n = 1, with_ties = FALSE) %>%
  select(loan_sequence_number, loan_age)

refined_df <- refined_df %>%
  left_join(loan_age_latest, by = "loan_sequence_number")

test.logit <- glm(default_flag ~
                    credit_score + original_cltv + original_dti + original_upb + original_interest_rate + defect_flag+ original_upb/original_cltv + loan_term + loan_age,
                  family = binomial(link=logit),
                  data = refined_df
)
summary(test.logit)

colnames(df_mp)

latest_rates <- df_mp %>%
  group_by(loan_sequence_number) %>%
  slice_max(order_by = loan_age, n = 1, with_ties = FALSE) %>%
  mutate(current_interest_rate = as.numeric(current_interest_rate)) %>%
  select(loan_sequence_number, current_interest_rate)

refined_df <- refined_df %>%
  left_join(latest_rates, by = "loan_sequence_number") %>%
  mutate(interest_rate_ratio = original_interest_rate / current_interest_rate)

max(refined_df$interest_rate_ratio)

refined_df <- refined_df %>%
  mutate(log_rate_ratio = log(interest_rate_ratio))

ggplot(refined_df, aes(x = log_rate_ratio)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Log of Interest Rate Ratio",
       x = "log(Original / Current Interest Rate)",
       y = "Count") +
  theme_minimal()

table(refined_df$interest_rate_ratio)

refined_df <- refined_df %>%
  mutate(
    rate_change_group = case_when(
      interest_rate_ratio < 0.98 ~ "Refinanced Lower",
      interest_rate_ratio > 1.02 ~ "Refinanced Higher",
      TRUE ~ "No Rate Change"
    )
  )

table(refined_df$rate_change_group)

refined_df$loan_term_decade <- factor(as.numeric(as.character(refined_df$loan_term))%/%120)
refined_df$original_cltv_lo <- ifelse(refined_df$original_cltv >90, 1, 0)

refined.logit <- glm(default_flag ~
                       credit_score + original_cltv + original_dti + original_upb + original_interest_rate + defect_flag + original_cltv*original_upb +loan_term + loan_age + rate_change_group,
                     family = binomial(link=logit),
                     data = refined_df
)
summary(refined.logit)

cf.r <- confusion(refined.logit)
cf.t <- confusion(test.logit)
cf.r
cf.t

accuracy <- c((cf.l[2,2] + cf.l[1,1]) / sum(cf.l),
              (cf.p[2,2] + cf.p[1,1]) / sum(cf.p),
              (cf.r[2,2] + cf.r[1,1]) / sum(cf.r),
              (cf.t[2,2] + cf.t[1,1]) / sum(cf.t))
precision <- c(cf.l[2,2] / (cf.l[2,2] + cf.l[1,2]),
               cf.p[2,2] / (cf.p[2,2] + cf.p[1,2]),
               cf.r[2,2] / (cf.r[2,2] + cf.r[1,2]),
               cf.t[2,2] / (cf.t[2,2] + cf.t[1,2]))
recall <- c(cf.l[2,2] / (cf.l[2,1] + cf.l[2,2]),
            cf.p[2,2] / (cf.p[2,1] + cf.p[2,2]),
            cf.r[2,2] / (cf.r[2,1] + cf.r[2,2]),
            cf.t[2,2] / (cf.t[2,1] + cf.t[2,2]))
balanced_acc <- c(0.5 * (cf.l[2,2] / (cf.l[2,1] + cf.l[2,2]) + cf.l[1,1] / (cf.l[1,1] + cf.l[1,2])),
                  0.5 * (cf.p[2,2] / (cf.p[2,1] + cf.p[2,2]) + cf.p[1,1] / (cf.p[1,1] + cf.p[1,2])),
                  0.5 * (cf.r[2,2] / (cf.r[2,1] + cf.r[2,2]) + cf.r[1,1] / (cf.r[1,1] + cf.r[1,2])),
                  0.5 * (cf.t[2,2] / (cf.t[2,1] + cf.t[2,2]) + cf.t[1,1] / (cf.t[1,1] + cf.t[1,2])))
f1 <- 2/(1/precision + 1/recall)

rnd.pct <- function(x){paste0(round(x, digits = 4)*100, '%')}

model_metrics <- data.frame(
  model = c("Logit", "Probit", "Refined", "Test"),
  accuracy = rnd.pct(accuracy),
  precision = rnd.pct(precision),
  recall = rnd.pct(recall),
  balanced_acc = rnd.pct(balanced_acc),
  f1 = rnd.pct(f1),
  stringsAsFactors = FALSE
)
model_metrics
