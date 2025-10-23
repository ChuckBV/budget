# scr03_review_transfers.R

## x

library(tidyverse)

# Load transactions
df_select3 <- readRDS("df_q1_to_q1_indexed_qrtr_month.Rds")

# Narrow to transfers
df_transfers <- df_select3 %>% 
  filter(type == "Transfer")      # 79 observations total

# How many by month?
df_transfers %>% 
  group_by(month_name) %>% 
  summarise(nObs = n())
# # A tibble: 9 × 2
#   month_name  nObs
#   <ord>      <int>
# 1 Jan            5
# 2 Feb            8
# 3 Mar            6
# 4 Apr           11
# 5 May           17
# 6 Jun           14
# 7 Jul            6
# 8 Aug            7
# 9 Sep            5

## Fluries in the spring (Apr, May, Jun). Go through month-by-month

# Jan
df_transfers %>% 
  filter(month_name == "Jan") 
# # A tibble: 5 × 10
#   rec_id date       description            amount account category group type  month_name  qrtr
#    <int> <date>     <chr>                   <dbl> <chr>   <chr>    <chr> <fct> <ord>      <int>
# 1      4 2025-01-01 "Online Ach Payment, …  3083. VISA S… Transfer Tran… Tran… Jan            1
# 2      6 2025-01-02 "Transfer / Withdrawa…  -100  Checki… Transfer Tran… Tran… Jan            1
# 3     16 2025-01-03 "Webwells Fargo Card … -3083. Checki… Transfer Tran… Tran… Jan            1
# 4     53 2025-01-10 "Payment Thank You - …  2974. CREDIT… Transfer Tran… Tran… Jan            1
# 5     70 2025-01-13 "Webchase Credit Crd … -2974. Checki… Transfer Tran… Tran… Jan

## Two offsetting pairs of credit card payments and credits. The $100 payment 
## was to the HELOC (loan 92). Note that I scheduled the $100 payment shown to
## this loan, and there was an additional $194 debited automatically. All of the
## $100 and $26.53 of the $194 was charged as interest. The $194 is shown as an 
## expense of the category "loan payment". The variable rate went from 7.5 to 
## 7% on 1/1. In this case the attempted practice to this point has been for 
## the HELOC has been to show repayment of interest as "Loan Payment" and 
## repayment of interest as "Transfer". For other amortized loans it is all 
## counted as "Loan Payment". That is probably the better practice here as well.



