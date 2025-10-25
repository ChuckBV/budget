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

## Now tag the HELOC payments more unambiguously. 
## First get list of rec_id values
heloc_rec_id_vector <- as.vector(x$rec_id)
heloc_rec_id_vector <- c(heloc_rec_id_vector,310)
heloc_rec_id_vector <- sort(heloc_rec_id_vector)
heloc_rec_id_vector

df_select3 <- df_select3 %>% 
  mutate(description = ifelse(rec_id %in% heloc_rec_id_vector,
                              paste(description," (HELOC)"),
                              description))

## Verify
x3 <- df_select3 %>% 
  mutate(last_25 = str_sub(description, -25)) %>% 
  select(last_25) 
  # %>% 
  # filter(str_detect(last_25,"HELOC"))
x3

# Save trimmed three quarter transactions again
saveRDS(df_select3,"df_q1_to_q1_indexed_qrtr_month.Rds")

# Continue through the months
# Feb
df_transfers %>% 
  filter(month_name == "Feb") 
# # A tibble: 8 × 10
#   rec_id date       description            amount account category group type  month_name  qrtr
#    <int> <date>     <chr>                   <dbl> <chr>   <chr>    <chr> <fct> <ord>      <int>
# 1    172 2025-02-03 "Transfer / Withdrawa…  -100  Checki… Transfer Tran… Tran… Feb            1
# 2    176 2025-02-03 "Transfer / Deposit @…  4064. Checki… Transfer Tran… Tran… Feb            1
# 3    197 2025-02-06 "Online Ach Payment, …  4718. VISA S… Transfer Tran… Tran… Feb            1
# 4    203 2025-02-07 "\"webwells Fargo Car… -4718. Checki… Transfer Tran… Tran… Feb            1
# 5    214 2025-02-09 "\"transfer\""          -320. Checki… Transfer Tran… Tran… Feb            1
# 6    223 2025-02-11 "Payment Thank You-mo…  2572. CREDIT… Transfer Tran… Tran… Feb            1
# 7    230 2025-02-12 "\"webchase Credit Cr… -2572. Checki… Transfer Tran… Tran… Feb            1
# 8    272 2025-02-20 "\"transfer\""          -203. Checki… Transfer Tran… Tran… Feb            1

## rec_id 172 is to the HELOC, and should also be a loan payment
df_select3[df_select3$rec_id == 172, 6] <- "Loan Payments"
df_select3[df_select3$rec_id == 172, 7] <- "Bill"
df_select3 <- df_select3 %>% 
  mutate(description = ifelse(rec_id == 172,
                              paste(description," (HELOC)"),
                              description))


## Two transfers are loan payments, rec_id 214 and 272
df_select3 %>% 
  filter(rec_id %in% c(214,272)) %>% 
  pull(amount) %>% 
  dput()
# c(-320.25, -203.38)

# Can these loan payments be recognized by the amount?
df_select3 %>% 
  filter(amount == -320.25)

# Missing a transaction on July 9
missing_01 <- data.frame(rec_id = as.integer(10001),
                         date = as.Date("2025-07-09"),
                         description = as.character("CAR PAYMENT"),
                         amount = as.double(-320.25),
                         account = as.character("Checking main"),
                         category = as.character("Car Payment"),
                         group = as.character("Bills"),
                         type = as.factor("Expense"),
                         month_name = "Jul",
                         qrtr = 3
                         )

# Merge onto current data set
df_select4 <- rbind(df_select3,missing_01)

# Look for Car Payment
df_select4 %>% 
  filter(category == "Car Payment")

## February missing. rec_id 214 mischaracterized as a transfer
df_select4 %>% 
  filter(rec_id == 214) 
# A tibble: 1 × 10
#    rec_id date       description    amount account       category group   type  month_name  qrtr
#     <int> <date>     <chr>           <dbl> <chr>         <chr>    <chr>   <fct> <ord>      <dbl>
#   1    214 2025-02-09 "\"transfer\""  -320. Checking main Transfer Transf… Tran… Feb            1

df_select4[df_select4$rec_id == 214, "category"]  <- "Car Payment"
df_select4[df_select4$rec_id == 214,  "group"] <- "Bills"
df_select4[df_select4$rec_id == 214,  "type"] <- "Expense"

## Now one car payment for each month, with consistent category, group, and type
df_select4 %>% 
  filter(amount == -203.38)

## 9 Payments, February is the only one that was miscategorized
## Get correct variables
df_select4[df_select4$rec_id == 109, "category"]
# 1 HVAC Payment
df_select4[df_select4$rec_id == 109,  "group"] 
# 1 Bills
df_select4[df_select4$rec_id == 109,  "type"]
# 1 Expense

## Apply to rec_id 272
df_select4[df_select4$rec_id == 272, "category"] <- "HVAC Payment"
df_select4[df_select4$rec_id == 272,  "group"] <- "Bills"
df_select4[df_select4$rec_id == 272,  "type"] <-  "Expense"

## Now each categorized correctly

# Mar
df_select4 %>% 
  filter(type == "Transfer" & month_name == "Mar")
# # A tibble: 6 × 10
#   rec_id date       description            amount account category group type  month_name  qrtr
#    <int> <date>     <chr>                   <dbl> <chr>   <chr>    <chr> <fct> <ord>      <dbl>
# 1    351 2025-03-08 "Online Ach Payment, …  1315. VISA S… Transfer Tran… Tran… Mar            1
# 2    367 2025-03-11 "\"webwells Fargo Car… -1315. Checki… Transfer Tran… Tran… Mar            1
# 3    377 2025-03-13 "Payment Thank You-mo…  1318. CREDIT… Transfer Tran… Tran… Mar            1
# 4    382 2025-03-14 "\"webchase Credit Cr… -1318. Checki… Transfer Tran… Tran… Mar            1
# 5    430 2025-03-23 "Transfer Withdrawal … -2000  Checki… Transfer Tran… Tran… Mar            1
# 6    459 2025-03-30 "Transfer Deposit @ O…   825  Checki… Transfer Tran… Tran… Mar            1

## The transcations 430 and 459 are a bit of a mystery. The transaction 430 was
## passed to savings. Apparently I was trying to re-enforce the emergency reserve.
## Transaction 459 is the amount of a life insurance premium and was taken from
## savings (emergency reserve). This was accidentally paid out of Wells Fargo
## checking, which is currently Pancha's money. That was replaced with a transfer
## to Wells Fargo on Apr 1. For 2026, it might be simpler to include 
## savings/emergency reserve in the accounts examined with Tiller. That should 
## result in Transfer netting to $0. 

## NB. Also need to flag when money is taken from HELOC

df_select4 %>% 
  filter(abs(amount) == 825)
# # A tibble: 2 × 10
#   rec_id date       description            amount account category group type  month_name  qrtr
#    <int> <date>     <chr>                   <dbl> <chr>   <chr>    <chr> <fct> <ord>      <dbl>
# 1    459 2025-03-30 "Transfer Deposit @ O…    825 Checki… Transfer Tran… Tran… Mar            1
# 2    472 2025-04-01 "Webwells Fargo Ifi (…   -825 Checki… Transfer Tran… Tran… Apr            2

## Note also that the April 1 transaction should be an insurance expense.

df_select4[df_select4$rec_id == 472, "category"] <- "Life insurance"
df_select4[df_select4$rec_id == 472,  "group"] <- "Bills"
df_select4[df_select4$rec_id == 472,  "type"] <-  "Expense"

df_select4 %>% 
  filter(category == "Auto Insurance")
## Monthly payments captured for 9 months

df_select4 %>% 
  filter(category == "Life insurance")
## Monthly payments captured for 9 months, along with the additional annual payment
## Note that case inconsistency complicates these categories in R

# # A tibble: 10 × 10
#    rec_id date       description           amount account category group type  month_name  qrtr
#     <int> <date>     <chr>                  <dbl> <chr>   <chr>    <chr> <fct> <ord>      <dbl>
#  1    125 2025-01-24 "Telnylife of Arizon…  -223. Checki… Life in… Bills Expe… Jan            1
#  2    282 2025-02-24 "\"telnylife of Ariz…  -223. Checki… Life in… Bills Expe… Feb            1
#  3    436 2025-03-24 "Telnylife of Arizon…  -223. Checki… Life in… Bills Expe… Mar            1
#  4    472 2025-04-01 "Webwells Fargo Ifi …  -825  Checki… Life in… Bills Expe… Apr            2
#  5    586 2025-04-24 "Telnylife of Arizon…  -223. Checki… Life in… Bills Expe… Apr            2
#  6    765 2025-05-27 "Webnylife of Arizon…  -223. Checki… Life in… Bills Expe… May            2
#  7    927 2025-06-24 "Telnylife of Arizon…  -223. Checki… Life in… Bills Expe… Jun            2
#  8   1047 2025-07-24 "Telnylife of Arizon…  -223. Checki… Life in… Bills Expe… Jul            3
#  9   1221 2025-08-25 "Telnylife of Arizon…  -223. Checki… Life in… Bills Expe… Aug            3
# 10   1367 2025-09-24 "Webnylife of Arizon…  -223. Checki… Life in… Bills Expe… Sep            3          3

# Apr
df_select4 %>% 
  filter(type == "Transfer" & month_name == "Apr")
# # A tibble: 10 × 10
#    rec_id date       description           amount account category group type  month_name  qrtr
#     <int> <date>     <chr>                  <dbl> <chr>   <chr>    <chr> <fct> <ord>      <dbl>
#  1    490 2025-04-06 "Online Ach Payment,…  1842. VISA S… Transfer Tran… Tran… Apr            2
#  2    504 2025-04-08 "Webwells Fargo Card… -1842. Checki… Transfer Tran… Tran… Apr            2
#  3    506 2025-04-08 "Transfer Deposit @ …  2200  Checki… Transfer Tran… Tran… Apr            2
#  4    510 2025-04-09 "Payment Thank You -…  2199. CREDIT… Transfer Tran… Tran… Apr            2
#  5    522 2025-04-10 "Webchase Credit Crd… -2199. Checki… Transfer Tran… Tran… Apr            2
#  6    542 2025-04-14 "Online Ach Payment,…    93  VISA S… Transfer Tran… Tran… Apr            2
#  7    545 2025-04-15 "Webwells Fargo Card…   -93  Checki… Transfer Tran… Tran… Apr            2
#  8    547 2025-04-16 "Payment Thank You-m…    40  CREDIT… Transfer Tran… Tran… Apr            2
#  9    552 2025-04-17 "Webchase Credit Crd…   -40  Checki… Transfer Tran… Tran… Apr            2
# 10    592 2025-04-26 "Branch Payment - Ca…   180  VISA S… Transfer Tran… Tran… Apr            2

## rec_id 506 is money bulled out of savings to cover expenses in checking
## rec_id 592 appears to be money paid at the Wells Fargo Branch to re-imburse
## expenses. It should be MiscIncome

df_select4 %>% 
  filter(type == "Income") %>% 
  group_by(group,category) %>% 
  summarise(nObs = n())
# # A tibble: 2 × 3
# # Groups:   group [1]
#   group          category    nObs
#   <chr>          <chr>      <int>
# 1 Primary Income MiscIncome    21
# 2 Primary Income Paycheck      19

## Need to look at income in next script. For now...

df_select4[df_select4$rec_id == 592, "category"] <- "MiscIncome"
df_select4[df_select4$rec_id == 592,  "group"] <- "Primary Income"
df_select4[df_select4$rec_id == 592,  "type"] <-  "Income"

rm(x1)
x1 <- df_select4 %>% 
  filter(type == "Income")
View(x1)

# Apr
df_select4 %>% 
  filter(type == "Transfer" & month_name == "May")
# # A tibble: 17 × 10
#    rec_id date       description           amount account category group type  month_name  qrtr
#     <int> <date>     <chr>                  <dbl> <chr>   <chr>    <chr> <fct> <ord>      <dbl>
#  1    615 2025-05-03 "Online Ach Payment…   2184. VISA S… Transfer Tran… Tran… May            2
#  2    636 2025-05-06 "Webwells Fargo Car…  -2184. Checki… Transfer Tran… Tran… May            2
#  3    638 2025-05-06 "Transfer Deposit @…   4000  Checki… Transfer Tran… Tran… May            2
#  4    649 2025-05-08 "Transfer Withdrawa…  -4000  Checki… Transfer Tran… Tran… May            2
#  5    658 2025-05-10 "Transfer Deposit @…   4000  Checki… Transfer Tran… Tran… May            2
#  6    659 2025-05-10 "Transfer Deposit @…  13000  Checki… Transfer Tran… Tran… May            2
#  7    667 2025-05-12 "Payment Thank You-…   2200  CREDIT… Transfer Tran… Tran… May            2
#  8    670 2025-05-12 "Withdrawal Withdra… -17285. Checki… Transfer Tran… Tran… May            2
#  9    677 2025-05-13 "Venmo (cashout) De…   4000  Checki… Transfer Tran… Tran… May            2
# 10    678 2025-05-13 "Webchase Credit Cr…  -2200  Checki… Transfer Tran… Tran… May            2
# 11    682 2025-05-13 "Online Ach Payment…     70  VISA S… Transfer Tran… Tran… May            2
# 12    684 2025-05-14 "Webwells Fargo Car…    -70  Checki… Transfer Tran… Tran… May            2
# 13    688 2025-05-15 "Transfer Withdrawa…  -4000  Checki… Transfer Tran… Tran… May            2
# 14    706 2025-05-18 "Transfer Deposit @…   2000  Checki… Transfer Tran… Tran… May            2
# 15    707 2025-05-18 "Online Ach Payment…   2000  VISA S… Transfer Tran… Tran… May            2
# 16    719 2025-05-20 "Webwells Fargo Car…  -2000  Checki… Transfer Tran… Tran… May            2
# 17    786 2025-05-31 "Online Ach Payment…   1700  VISA S… Transfer Tran… Tran… May            2

## This month got complicated because of the Jeep purchase

# Save updated data frame
saveRDS(df_select4,"df_q1_to_q1_indexed_qrtr_month_v02.Rds")
