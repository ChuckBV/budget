# scr01_transactions_data.R

library(googlesheets4)
library(tidyverse)
library(janitor)

#-----------------------------------------------------------------------------#
#-- 1. Get necessary data sets ------------------------------------------------

gs4_auth()  # Will prompt you to sign in via browser

# URL for Transactions data
sheet_url <- "https://docs.google.com/spreadsheets/d/1hihf6vZzfT24wJ4e7R5Dk_iAVYPzKBb3ckVM_-mdJLs/edit?gid=1256593101#gid=1256593101"

# Specify the range (e.g., Sheet1!A1:B20)
df <- googlesheets4::read_sheet(sheet_url, 
                   range = "Transactions!A1:P")

df <- df %>% 
  dplyr::rename(first_col = `...1`)

unique(df$first_col)
# [1] NA

df <- janitor::clean_names(df)

colnames(df)
# [1] "first_col"        "date"             "description"      "category"        
# [5] "amount"           "account"          "account_number"   "institution"     
# [9] "month"            "week"             "transaction_id"   "account_id"      
# [13] "check_number"     "full_description" "date_added"       "categorized_date"

# Also get Categories data

df_categories <-  googlesheets4::read_sheet(sheet_url, 
                                            range = "Categories!A1:C")

df_categories <- janitor::clean_names(df_categories)
df_categories
# # A tibble: 39 × 3
#   category       group type   
#   <chr>          <chr> <chr>  
# 1 Auto Insurance Bills Expense
# 2 Car Payment    Bills Expense
# 3 Education      Bills Expense
# 4 Hearing Aid    Bills Expense
# 5 HVAC Payment   Bills Expense
# 6 Life insurance Bills Expense
# 7 Mortgage       Bills Expense
# 8 Phone          Bills Expense
# 9 Security       Bills Expense

df_categories %>% 
  group_by(type) %>% 
  summarise(n_types = n_distinct(category))
# # A tibble: 3 × 2
#   type     n_types
#   <chr>      <int>
# 1 Expense       34
# 2 Income         4
# 3 Transfer       1

#-----------------------------------------------------------------------------#
#-- 2. Examine, trim, and merge data sets -------------------------------------

# Clarifiy selected 

unique(df$account)
# [1] "Checking main"               "CREDIT CARD (-7022)"         "VISA SIGNATURE CARD ...0285"
# [4] "CREDIT CARD (-9561)" 

unique(df$account_number)
# [1] "xxxxxxx8" "xxxx7022" "xxxx0285" "xxxx9561"

unique(df$institution)
# [1] "Educational Employees Credit Union" "Chase"                             
# [3] "Wells Fargo"    

df %>% 
  group_by(institution,account) %>% 
  summarise(nObs = n())
# # A tibble: 4 × 3
# # Groups:   institution [3]
#   institution                        account                      nObs
#   <chr>                              <chr>                       <int>
# 1 Chase                              CREDIT CARD (-7022)           445
# 2 Chase                              CREDIT CARD (-9561)             2
# 3 Educational Employees Credit Union Checking main                 545
# 4 Wells Fargo                        VISA SIGNATURE CARD ...0285   484

# Get a chronological rec_id
df <- df %>% 
  arrange(date) %>% 
  mutate(date = as.Date(date),
         first_col = row_number()) %>% 
  rename(rec_id = first_col)


head(df)


df_select <- df %>% 
  select(rec_id,date,description,category,amount,account)
head(df_select)

# Before trying to merge, check for NA in category
x <- df_select %>% 
  filter(is.na(category))            # 74 obs
# 0 observations

# # Get transfers
# transfer_re_ids <- x %>% 
#   filter(str_detect(description, "Transfer|transfer"))
# transfer_rec_ids <- as.vector(transfer_re_ids$rec_id)
# 
# # Get transactions involving Amazon
# amazon_rec_ids <- x %>% 
#     filter(str_detect(description, "Amazon")) # 11
# amazon_rec_id <- as.vector(amazon_rec_ids$rec_id)

# Look for "loan 92" as an indication of further borrowing from the HELOC

# Example: Withdrawal Withdrawal-transfer-tsdl (eff. date 08/01/2025)
# This is a minimum payment on the HELOC, and is a combination of interest
# and principal

# Note that the 7/14 deposit of $2000 is for 

# Merge categories onto select data frame
df_select2 <- left_join(df_categories,df_select)

# Share 2 is the Jeep fund, and should be one of the Charlotte-associated expenses


# Reorder
df_select2 <- df_select2 %>% 
  select(rec_id,date,description,amount,account,category,group,type)

df_select2 <- df_select2 %>% 
  arrange(rec_id)

head(df_select2)


tail(df_select2, 10)
# # A tibble: 10 × 8
#   rec_id date       description                            amount account category group type 
#    <int> <date>     <chr>                                   <dbl> <chr>   <chr>    <chr> <chr>
# 1   1474 2025-10-15 Hobbylobby 1425 Shaw Avenue Withdrawa…  -3.26 Checki… Misc     Livi… Expe…
# 2   1475 2025-10-15 Apple.com/bill x-x-7753 CA Withdrawal…  -9.99 Checki… Subscri… Disc… Expe…
# 3   1476 2025-10-16 Alert 360 (ach) Withdrawal-ach-a-guar… -58.8  Checki… Security Bills Expe…
# 4     NA NA         NA                                      NA    NA      Educati… Char… Expe…
# 5     NA NA         NA                                      NA    NA      Charity  Livi… Expe…
# 6     NA NA         NA                                      NA    NA      Repairs  Livi… Expe…
# 7     NA NA         NA                                      NA    NA      Freelan… Prim… Inco…
# 8     NA NA         NA                                      NA    NA      WorkTra… Prim… Inco…
# 9     NA NA         NA                                      NA    NA      Parking  Work  Expe…
# 10    NA NA         NA                                      NA    NA      Reimbur… Work  Expe

# Drop records for 6 categories not used so far in 2025
df_select2 <- df_select2 %>% 
  filter(!is.na(rec_id))
