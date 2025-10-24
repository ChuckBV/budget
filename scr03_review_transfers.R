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

df_select3 %>% 
  filter(rec_id == 7)
# # A tibble: 1 × 10
#   rec_id date       description                                      amount account category group type  month_name  qrtr
#   <int> <date>     <chr>                                             <dbl> <chr>   <chr>    <chr> <fct> <ord>      <int>
#   1      7 2025-01-02 Withdrawal / Withdrawal-transfer-tsdl (eff. dat…   -194 Checki… Loan Pa… Bills Expe… Jan            1

## Two offsetting pairs of credit card payments and credits. The $100 payment 
## was to the HELOC (loan 92). Note that I scheduled the $100 payment shown to
## this loan, and there was an additional $194 debited automatically. All of the
## $100 and $26.53 of the $194 was charged as interest. The $194 is shown as an 
## expense of the category "loan payment". The variable rate went from 7.5 to 
## 7% on 1/1. In this case the attempted practice to this point has been for 
## the HELOC has been to show repayment of interest as "Loan Payment" and 
## repayment of interest as "Transfer". For other amortized loans it is all 
## counted as "Loan Payment". That is probably the better practice here as well.

x <- df_select3 %>% 
  filter(str_detect(description,"Withdrawal-transfer-tsdl"))
x
# # A tibble: 8 × 10
#   rec_id date       description                                      amount account category group type  month_name  qrtr
#    <int> <date>     <chr>                                             <dbl> <chr>   <chr>    <chr> <fct> <ord>      <int>
# 1      7 2025-01-02 Withdrawal / Withdrawal-transfer-tsdl (eff. dat…   -194 Checki… Loan Pa… Bills Expe… Jan            1
# 2    174 2025-02-03 Withdrawal / Withdrawal-transfer-tsdl (eff. dat…   -226 Checki… LoanInt… Livi… Expe… Feb            1
# 3    473 2025-04-01 Withdrawal Withdrawal-transfer-tsdl (eff. date …   -260 Checki… LoanInt… Livi… Expe… Apr            2
# 4    610 2025-05-01 Withdrawal Withdrawal-transfer-tsdl (eff. date …   -260 Checki… Loan Pa… Bills Expe… May            2
# 5    790 2025-06-01 Withdrawal Withdrawal-transfer-tsdl (eff. date …   -405 Checki… Loan Pa… Bills Expe… Jun            2
# 6    951 2025-07-01 Withdrawal Withdrawal-transfer-tsdl (eff. date …   -405 Checki… Loan Pa… Bills Expe… Jul            3
# 7   1081 2025-08-01 Withdrawal Withdrawal-transfer-tsdl (eff. date …   -405 Checki… Loan Pa… Bills Expe… Aug            3
# 8   1255 2025-09-01 Withdrawal Withdrawal-transfer-tsdl (eff. date …   -410 Checki… Loan Pa… Bills Expe… Sep            3

df_select3[df_select3$rec_id == 7, 6]
# # A tibble: 1 × 1
#   category     
#   <chr>        
# 1 Loan Payments

df_select3[df_select3$rec_id == 7, 7]
# # A tibble: 1 × 1
#   group
#   <chr>
# 1 Bills

# ## Reasign categories for transactions 174
df_select3[df_select3$rec_id == 174, 6] <- "Loan Payments"
df_select3[df_select3$rec_id == 174, 7] <- "Bill"
# 
# ## Reasign categories for transactions 473
df_select3[df_select3$rec_id == 473, 6] <- "Loan Payments"
df_select3[df_select3$rec_id == 473, 7] <- "Bill"

## After the adjustments above, there is a transaction for each month except
## March. The March EECU statement shows a payment to the HELOC Account 92
## on March 1 for $260. In df_select3 this shows transaction is rec_id 310
## and shows as simply "withdrawal. 
# 
df_select3[df_select3$rec_id == 310, 6] <- "Loan Payments"
df_select3[df_select3$rec_id == 310, 7] <- "Bill"

# Now tag the HELOC payments more unambiguously. 
# First get list of rec_id values
heloc_rec_id_vector <- as.vector(x$rec_id)
heloc_rec_id_vector <- c(heloc_rec_id_vector,310)
heloc_rec_id_vector <- sort(heloc_rec_id_vector)
heloc_rec_id_vector

df_select3 <- df_select3 %>% 
  mutate(description = ifelse(rec_id %in% heloc_rec_id_vector,
                              paste(description," (HELOC)"),
                              description))

# Verify
x3 <- df_select3 %>% 
  mutate(last_25 = str_sub(description, -25)) %>% 
  select(last_25) 
  # %>% 
  # filter(str_detect(last_25,"HELOC"))
x3

# Save trimmed three quarter transactions again
saveRDS(df_select3,"df_q1_to_q1_indexed_qrtr_month.Rds")

# Continue through the months
# Jan
df_transfers %>% 
  filter(month_name == "Feb") 

# Two transfers are loan payments, rec_id 214 and 272
df_select3 %>% 
  filter(rec_id %in% c(214,272)) %>% 
  pull(amount) %>% 
  dput()
# c(-320.25, -203.38)

# Can these loan payments be recognized by the amount?
df_select3 %>% 
  filter(amount == -320.25)


