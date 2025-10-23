# scr02_summarize_transactions_by_type.R

## Overview of the three main types--Transfer, Income, and Expense

library(tidyverse)

# Load the cleaned summary of transactions
df_select2 <- readRDS("df_transactions_cleaned_categorized.Rds")

# We will want to cut off at the end of the third quarter.
# Therefore create variables for month and quarter
df_select2 <- df_select2 %>% 
  mutate(month_name = month(date, label = TRUE, abbr = TRUE),
         qrtr = quarter(date))

df_select3 <- df_select2 %>% 
  filter(qrtr <= 3)

tail(df_select3)

# Get type as ordered factor
df_select3$type <- factor(df_select3$type, levels = c("Transfer","Income","Expense"))
levels(df_select3$type)

# Look at summary totals for the first three quarters
df_select3 %>% 
  group_by(type) %>% 
  summarise(nObs = n(),
            total = sum(amount))
# # A tibble: 3 × 3
#   type      nObs   total
#   <fct>    <int>   <dbl>
# 1 Transfer    79   6630.
# 2 Income      40  80721.
# 3 Expense   1274 -89843.

# Believable on the face. Need to delve into transfers, determine how much income
# is non-paycheck, then parse expenses

unique(df_select3$group)
# [1] "Living"         "Discretionary"  "Bills"          "Transfer Types" "Charlotte"     
# [6] "Primary Income"

#----------------------------------------------------------------------------#
#-- Save first data set for the first three quarters w months & quarters ----

saveRDS(df_select3,"df_q1_to_q1_indexed_qrtr_month.Rds")
