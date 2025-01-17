library(tidyverse)
library(readr)
library(psych) 
library(DescTools)
library(zoo)


# Load data
load('./Data/dca_long_format.rdata')
load("./Data/dca.rdata")
LSTMM_df <- read_csv("./Data/Leading_Percent_STDDEV_shock.csv") ### CHANGE THIS
Varname <- read_csv("./Data/Variable list.csv")
raw_data <- read_csv("./Input/HKIMR_2023-10-21.csv")

lf_results$Target = str_remove(lf_results$TO,".return| EQUITY")
lf_results$Source = str_remove(lf_results$FROM,".return| EQUITY")
lf_results$Date = lf_results$Date
lf_results$Dates = lf_results$Date


LSTMM_df <- LSTMM_df %>%
  left_join(Varname %>% transmute(Index, Source = Name), 
            by=(c("Variable" = "Index"))) %>%
  left_join(Varname %>% transmute(Index, Target = Name), 
            by=(c("Target_Variables" = "Index"))) %>%
  transmute(Dates, Source, Target, Value)

LSTMM_df$Target = str_remove(LSTMM_df$Target,".return| EQUITY")
LSTMM_df$Source = str_remove(LSTMM_df$Source,".return| EQUITY")

LSTMM_df <- LSTMM_df %>%
  group_by(Source, Target) %>%
  arrange(Dates) %>%
  mutate(Value = Winsorize(Value)) %>%
  mutate(MAValue = rollmean(Value, 120, na.pad= T, align = "right")) %>%
  na.omit()

# Build insertRow function

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

AutoAddSpace <- function(data,list = c(3,9,15,21,27,32)){
  for (i in list){
    data = insertRow(data, append(list("\\addlinespace"),as.list(rep(NA,ncol(data) - 1))), i)
  }
  return(data)
}


#### Table 2: Log returns, cryptocurrencies and GSIBs: summary statistic ####

var = str_remove(colnames(raw_data[3:33]),".return| EQUITY")

pos1 = which(var == "HSBA LN")

var_list <- c(var[(1:pos1)], "WFC US", var[(pos1+1):(length(var)-1)])

tomap <- data.frame(var_list, rank = c(1:31)) 

Consolidated <- raw_data[3:33] %>%
  na.omit() %>%
  pivot_longer(cols = c(1:31),
               names_to = "name",
               values_to = "Value") %>%
  mutate(name = str_remove(name,".return| EQUITY")) %>%
  left_join(tomap, by = c("name" = "var_list")) %>%
  group_by(rank, name) %>%
  summarize(
    mean = mean(Value),
    min = min(Value),
    max = max(Value),
    sdev = sd(Value)
  ) %>% ungroup() %>%
  select(-rank)

Consolidated <- AutoAddSpace(Consolidated,c(3,9,15,21,27,33))

WP_code <-Consolidated %>%
  mutate(RCode = case_when(
    str_detect(name,"Panel A: ") ~ paste0("\\multicolumn{10}{l}{",name,"}\\\\"),
    is.na(mean) ~ name,
    
    TRUE ~ paste0(name," & ",formatC(mean, format = "f", digits = 7, flag = "0"),
                  " & ",formatC(min, format = "f", digits = 7, flag = "0"),
                  " & ",formatC(max, format = "f", digits = 7, flag = "0"),
                  " & ",formatC(sdev, format = "f", digits = 7, flag = "0"),"\\\\")
  ))

write.csv(WP_code$RCode,"./Table code/Table 2 Log returns, cryptocurrencies and GSIBs summary statistics.csv")

#### Table 3 new: TO Connectedness Measures: Summary Statistics, Full Sample (in percent). ####

BTC2GSIB = lf_results %>%
  filter(Source == "BTC",
         !Target %in% c("BTC","ETH"),
         Source != Target) %>%
  na.omit() %>%
  group_by(Dates) %>%
  summarise(
    bmean = mean(values, na.rm = TRUE),
    cmax = max(values, na.rm = TRUE),
    amin = min(values, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c("bmean","cmax","amin"),
    values_to = "values"
  ) %>%
  group_by(name) %>%
  summarise(
    mean = mean(values, na.rm = TRUE),
    min = min(values, na.rm = TRUE),
    max = max(values, na.rm = TRUE),
    sdev = sd(values, na.rm = TRUE)
  )

ETH2GSIB = lf_results %>%
  filter(Source == "ETH",
         !Target %in% c("BTC","ETH"),
         Source != Target) %>%
  na.omit() %>%
  group_by(Dates) %>%
  summarise(
    bmean = mean(values, na.rm = TRUE),
    cmax = max(values, na.rm = TRUE),
    amin = min(values, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c("bmean","cmax","amin"),
    values_to = "values"
  ) %>%
  group_by(name) %>%
  summarise(
    mean = mean(values, na.rm = TRUE),
    min = min(values, na.rm = TRUE),
    max = max(values, na.rm = TRUE),
    sdev = sd(values, na.rm = TRUE)
  )

Coin2Coin = lf_results %>%
  filter(Source %in% c("BTC","ETH"),
         Target %in% c("BTC","ETH"),
         Source != Target) %>%
  group_by(Dates,
           name = paste0(Source," to ",Target)) %>%
  summarise(values) %>%
  group_by(name) %>%
  summarise(
    mean = mean(values, na.rm = TRUE),
    min = min(values, na.rm = TRUE),
    max = max(values, na.rm = TRUE),
    sdev = sd(values, na.rm = TRUE)
  ) %>%
  mutate(
    name = ifelse(name == "BTC to ETH",
                  "Bitcoin to Ethereum",
                  "Ethereum to Bitcoin") 
  )

GSIB2BTC = lf_results %>%
  filter(Target == "BTC",
         !Source %in% c("BTC","ETH"),
         Source != Target) %>%
  na.omit() %>%
  group_by(Dates) %>%
  summarise(
    bmean = mean(values, na.rm = TRUE),
    cmax = max(values, na.rm = TRUE),
    amin = min(values, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c("bmean","cmax","amin"),
    values_to = "values"
  ) %>%
  group_by(name) %>%
  summarise(
    mean = mean(values, na.rm = TRUE),
    min = min(values, na.rm = TRUE),
    max = max(values, na.rm = TRUE),
    sdev = sd(values, na.rm = TRUE)
  )

GSIB2ETH = lf_results %>%
  filter(Target == "ETH",
         !Source %in% c("BTC","ETH"),
         Source != Target) %>%
  na.omit() %>%
  group_by(Dates) %>%
  summarise(
    bmean = mean(values, na.rm = TRUE),
    cmax = max(values, na.rm = TRUE),
    amin = min(values, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c("bmean","cmax","amin"),
    values_to = "values"
  ) %>%
  group_by(name) %>%
  summarise(
    mean = mean(values, na.rm = TRUE),
    min = min(values, na.rm = TRUE),
    max = max(values, na.rm = TRUE),
    sdev = sd(values, na.rm = TRUE)
  )

GSIB2GSIB = lf_results %>%
  filter(!Source %in% c("BTC","ETH"),
         !Target %in% c("BTC","ETH"),
         Source != Target) %>%
  na.omit() %>%
  group_by(Dates) %>%
  summarise(
    bmean = mean(values, na.rm = TRUE),
    cmax = max(values, na.rm = TRUE),
    amin = min(values, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c("bmean","cmax","amin"),
    values_to = "values"
  ) %>%
  group_by(name) %>%
  summarise(
    mean = mean(values, na.rm = TRUE),
    min = min(values, na.rm = TRUE),
    max = max(values, na.rm = TRUE),
    sdev = sd(values, na.rm = TRUE)
  )


Consolidated = rbind(BTC2GSIB, ETH2GSIB,  GSIB2BTC, GSIB2ETH, GSIB2GSIB, Coin2Coin)

### TO ADD FORMAT

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

Consolidated = insertRow(Consolidated, list("A: Bitcoin to GSIBs\\\\",NA,NA,NA,NA), 1)

Consolidated = insertRow(Consolidated, list("\\addlinespace",NA,NA,NA,NA), 5)
Consolidated = insertRow(Consolidated, list("B: Ethereum to GSIBs\\\\",NA,NA,NA,NA), 6)

Consolidated = insertRow(Consolidated, list("\\addlinespace",NA,NA,NA,NA), 10)
Consolidated = insertRow(Consolidated, list("C: GSIBs to Bitcoin\\\\",NA,NA,NA,NA), 11)

Consolidated = insertRow(Consolidated, list("\\addlinespace",NA,NA,NA,NA), 15)
Consolidated = insertRow(Consolidated, list("D: GSIBs to Ethereum\\\\",NA,NA,NA,NA), 16)

Consolidated = insertRow(Consolidated, list("\\addlinespace",NA,NA,NA,NA), 20)
Consolidated = insertRow(Consolidated, list("E: GSIBs to other GSIBs\\\\",NA,NA,NA,NA), 21)

Consolidated = insertRow(Consolidated, list("\\addlinespace",NA,NA,NA,NA), 25)
Consolidated = insertRow(Consolidated, list("F: Between cryptocurrencies\\\\",NA,NA,NA,NA), 26)

WP_code <-Consolidated %>%
  mutate(
    mean = mean * 100,
    min = min * 100,
    max = max * 100,
    sdev = sdev * 100,
    name = case_when(
      name == "amin" ~ "Minimum",
      name == "bmean" ~ "Mean",
      name == "cmax" ~ "Maximum",
      TRUE ~ name
    )
  )%>%
  mutate(RCode = case_when(
    # str_detect(name,"Panel A: ") ~ paste0("\\multicolumn{10}{l}{",name,"}\\\\"),
    is.na(mean) ~ name,
    
    TRUE ~ paste0("\\hspace{12pt}",str_to_title(name)," & ",formatC(mean, format = "f", digits = 2, flag = "0"),
                  " & ",formatC(min, format = "f", digits = 2, flag = "0"),
                  " & ",formatC(max, format = "f", digits = 2, flag = "0"),
                  " & ",formatC(sdev, format = "f", digits = 2, flag = "0"),"\\\\")
  )) 



write.csv(WP_code$RCode,"./Table code/Table 3 New LSTM multiplier, one percent shock summary statistics.csv")


#### Table 3: TVP-VAR Connectedness measures: summary statistics TO FROM ####
library(psych)

t2_TO_stat <- describe(dca$TO, fast = TRUE)[c('mean','min','max','sd')]
t2_FROM_stat<- describe(dca$FROM, fast = TRUE)[c('mean','min','max','sd')]
t2_NET_stat<- describe(dca$NET, fast = TRUE)[c('mean','min','max','sd')]
t2_NPT_stat<- describe(dca$NPT, fast = TRUE)[c('mean','min','max','sd')]

pos_row = which(str_detecta(rownames(t2_TO_stat),"HSBA"))

table_3.1 = cbind(t2_TO_stat, t2_FROM_stat)
table_3.1$name = row.names(table_3.1)
table_3.1 <- table_3.1[, c(ncol(table_3.1), 1:(ncol(table_3.1) - 1))]

table_3.1 = rbind(
  table_3.1[1:pos_row,],
  table_3.1[31,],
  table_3.1[(pos_row+1):30,]
)


table_3.1_spaced <- AutoAddSpace(table_3.1,c(3,9,15,21,27,33))

table_3.1_spaced <- table_3.1_spaced %>%
  mutate_all(as.character) %>% mutate_all(~ifelse(is.na(.), NA, .)) %>% 
  mutate_at(vars(-name), ~round(as.numeric(.), digits = 2)) %>% 
  mutate_at(vars(-name), ~ifelse(is.na(.), NA, as.character(.))) 


WP_code <- table_3.1_spaced %>%
  rowwise() %>%
  mutate(
    name = str_remove(name,".return| EQUITY"),
    RCode = case_when(
      is.na(mean) ~ name,
      TRUE ~ paste(c_across(everything()), collapse = " & ")),
    RCode = case_when(
      is.na(mean) ~ name,
      TRUE ~ paste0(RCode, "\\\\"))
    )

write.csv(WP_code$RCode,"./Table code/Table 3.1 TVP-VAR Connectedness measures.csv")


table_3.2 = cbind(t2_NET_stat, t2_NPT_stat)
table_3.2$name = row.names(table_3.2)
table_3.2 <- table_3.2[, c(ncol(table_3.2), 1:(ncol(table_3.2) - 1))]


table_3.2 = rbind(
  table_3.2[1:pos_row,],
  table_3.2[31,],
  table_3.2[(pos_row+1):30,]
)


table_3.2_spaced <- AutoAddSpace(table_3.2,c(3,9,15,21,27,33))

table_3.2_spaced <- table_3.2_spaced %>%
  mutate_all(as.character) %>% mutate_all(~ifelse(is.na(.), NA, .)) %>% 
  mutate_at(vars(-name), ~round(as.numeric(.), digits = 2)) %>% 
  mutate_at(vars(-name), ~ifelse(is.na(.), NA, as.character(.))) 

WP_code <- table_3.2_spaced %>%
  rowwise() %>%
  mutate(
    name = str_remove(name,".return| EQUITY"),
    RCode = case_when(
      is.na(mean) ~ name,
      TRUE ~ paste(c_across(everything()), collapse = " & ")),
    RCode = case_when(
      is.na(mean) ~ name,
      TRUE ~ paste0(RCode, "\\\\"))
  )

write.csv(WP_code$RCode,"./Table code/Table 3.2 TVP-VAR Connectedness measures.csv")



#### Table 4: : Cryptocurrencies and GSIBs TVP-VAR connectedness, Sep 2023 ####

x <- data.frame(dca$TABLE)

pos_row = which(str_detect(rownames(x),"HSBA"))
pos_col = which(str_detect(colnames(x),"HSBA"))

x = rbind(
  x[1:pos_row,],
  x[31,],
  x[(pos_row+1):30,],
  x[32:nrow(x),]
)

x = cbind(
  x[,1:pos_col],
  x[,31, drop = FALSE],
  x[,(pos_col+1):30],
  x[,32:ncol(x), drop = FALSE]
)


clean_col <- unlist(lapply(colnames(x), function(w) gsub('.EQUITY','', w)))
clean_col <- unlist(lapply(clean_col, function(w) gsub('.Equity','', w)))
clean_col <- unlist(lapply(clean_col, function(w) gsub('.return','', w)))
clean_col <- unlist(lapply(clean_col, function(w) gsub(regex('\\.'),' ', w)))

clean_row <- unlist(lapply(rownames(x), function(w) gsub('.EQUITY','', w)))
clean_row <- unlist(lapply(clean_row, function(w) gsub('.Equity','', w)))
clean_row <- unlist(lapply(clean_row, function(w) gsub('.return','', w)))


colnames(x) <- clean_col
rownames(x) <- NULL

x_h1 <- x[,1:8]
x_h2 <- x[,9:16]
x_h3 <- x[,17:24]
x_h4 <- x[,25:32]

tbl_1 <- cbind(clean_row, x_h1)
tbl_2 <- cbind(clean_row, x_h2)
tbl_3 <- cbind(clean_row, x_h3)
tbl_4 <- cbind(clean_row, x_h4)

colnames(tbl_1)[1] = 'name'
colnames(tbl_2)[1] = 'name'
colnames(tbl_3)[1] = 'name'
colnames(tbl_4)[1] = 'name'

tbl_1 <- AutoAddSpace(tbl_1,c(3,9,15,21,27,33,38))
tbl_2 <- AutoAddSpace(tbl_2,c(3,9,15,21,27,33,38))
tbl_3 <- AutoAddSpace(tbl_3,c(3,9,15,21,27,33,38))
tbl_4 <- AutoAddSpace(tbl_4,c(3,9,15,21,27,33,38))

listdf = list(tbl_1,tbl_2,tbl_3,tbl_4)

for (i in listdf){
  
  i <- i %>%
    mutate_all(as.character) %>% 
    mutate_all(~ifelse(is.na(.), "", .)) %>% 
    mutate_at(vars(-name), ~round(as.numeric(.), digits = 2)) %>% 
    mutate_at(vars(-name), ~ifelse(is.na(.), "", as.character(.))) 
}


WP_code <- tbl_1 %>%
  rowwise() %>%
  mutate(
    name = str_remove(name,".return| EQUITY"),
    RCode = case_when(
      is.na(BTC) ~ name,
      TRUE ~ paste0(c_across(everything()), collapse = " & ")),
    RCode = case_when(
      is.na(BTC) ~ name,
      TRUE ~ paste0(RCode, "\\\\"))
  )


write.csv(WP_code$RCode,"./Table code/Table 4 connectedness_1.csv")


write.csv(tbl_2 %>%
            rowwise() %>%
            mutate(
              name = str_remove(name,".return| EQUITY"),
              RCode = case_when(
                is.na(`BARC LN`) ~ name,
                TRUE ~ paste0(c_across(everything()), collapse = " &")),
              RCode = case_when(
                is.na(`BARC LN`) ~ name,
                TRUE ~ paste0(RCode, "\\\\"))
            ) %>%
            select(RCode),"./Table code/Table 4 connectedness_2.csv")
write.csv(tbl_3%>%
            rowwise() %>%
            mutate(
              name = str_remove(name,".return| EQUITY"),
              RCode = case_when(
                is.na(`CSGN SW`) ~ name,
                TRUE ~ paste0(c_across(everything()), collapse = " &")),
              RCode = case_when(
                is.na(`CSGN SW`) ~ name,
                TRUE ~ paste0(RCode, "\\\\"))
            ) %>%
            select(RCode),"./Table code/Table 4 connectedness_3.csv")
write.csv(tbl_4%>%
            rowwise() %>%
            mutate(
              name = str_remove(name,".return| EQUITY"),
              RCode = case_when(
                is.na(`GLE FP`) ~ name,
                TRUE ~ paste0(c_across(everything()), collapse = " &")),
              RCode = case_when(
                is.na(`GLE FP`) ~ name,
                TRUE ~ paste0(RCode, "\\\\"))
            ) %>%
            select(RCode),"./Table code/Table 4 connectedness_4.csv")

#### Table 5: LSTMm: on percent shock: summary ####

BTC2GSIB = LSTMM_df %>%
  filter(Source == "BTC",
         !Target %in% c("BTC","ETH"),
         Source != Target) %>%
  na.omit() %>%
  group_by(Dates) %>%
  summarise(
    bmean = mean(MAValue, na.rm = TRUE),
    cmax = max(MAValue, na.rm = TRUE),
    amin = min(MAValue, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c("bmean","cmax","amin"),
    values_to = "MAValue"
  ) %>%
  group_by(name) %>%
  summarise(
    mean = mean(MAValue, na.rm = TRUE),
    max = max(MAValue, na.rm = TRUE),
    min = min(MAValue, na.rm = TRUE),
    sdev = sd(MAValue, na.rm = TRUE)
  )

ETH2GSIB = LSTMM_df %>%
  filter(Source == "ETH",
         !Target %in% c("BTC","ETH"),
         Source != Target) %>%
  na.omit() %>%
  group_by(Dates) %>%
  summarise(
    bmean = mean(MAValue, na.rm = TRUE),
    cmax = max(MAValue, na.rm = TRUE),
    amin = min(MAValue, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c("bmean","cmax","amin"),
    values_to = "MAValue"
  ) %>%
  group_by(name) %>%
  summarise(
    mean = mean(MAValue, na.rm = TRUE),
    max = max(MAValue, na.rm = TRUE),
    min = min(MAValue, na.rm = TRUE),
    sdev = sd(MAValue, na.rm = TRUE)
  )

Coin2Coin = LSTMM_df %>%
  filter(Source %in% c("BTC","ETH"),
         Target %in% c("BTC","ETH"),
         Source != Target) %>%
  group_by(Dates,
           name = paste0(Source," to ",Target)) %>%
  summarise(MAValue) %>%
  group_by(name) %>%
  summarise(
    mean = mean(MAValue, na.rm = TRUE),
    max = max(MAValue, na.rm = TRUE),
    min = min(MAValue, na.rm = TRUE),
    sdev = sd(MAValue, na.rm = TRUE)
  ) %>%
  mutate(
    name = ifelse(name == "BTC to ETH",
                  "Bitcoin to Ethereum",
                  "Ethereum to Bitcoin") 
  )


GSIB2BTC = LSTMM_df %>%
  filter(Target == "BTC",
         !Source %in% c("BTC","ETH"),
         Source != Target) %>%
  na.omit() %>%
  group_by(Dates) %>%
  summarise(
    bmean = mean(MAValue, na.rm = TRUE),
    cmax = max(MAValue, na.rm = TRUE),
    amin = min(MAValue, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c("bmean","cmax","amin"),
    values_to = "MAValue"
  ) %>%
  group_by(name) %>%
  summarise(
    mean = mean(MAValue, na.rm = TRUE),
    max = max(MAValue, na.rm = TRUE),
    min = min(MAValue, na.rm = TRUE),
    sdev = sd(MAValue, na.rm = TRUE)
  )

GSIB2ETH = LSTMM_df %>%
  filter(Target == "ETH",
         !Source %in% c("BTC","ETH"),
         Source != Target) %>%
  na.omit() %>%
  group_by(Dates) %>%
  summarise(
    bmean = mean(MAValue, na.rm = TRUE),
    cmax = max(MAValue, na.rm = TRUE),
    amin = min(MAValue, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c("bmean","cmax","amin"),
    values_to = "MAValue"
  ) %>%
  group_by(name) %>%
  summarise(
    mean = mean(MAValue, na.rm = TRUE),
    max = max(MAValue, na.rm = TRUE),
    min = min(MAValue, na.rm = TRUE),
    sdev = sd(MAValue, na.rm = TRUE)
  )

GSIB2GSIB = LSTMM_df %>%
  filter(!Source %in% c("BTC","ETH"),
         !Target %in% c("BTC","ETH"),
         Source != Target) %>%
  na.omit() %>%
  group_by(Dates) %>%
  summarise(
    bmean = mean(MAValue, na.rm = TRUE),
    cmax = max(MAValue, na.rm = TRUE),
    amin = min(MAValue, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c("bmean","cmax","amin"),
    values_to = "MAValue"
  ) %>%
  group_by(name) %>%
  summarise(
    mean = mean(MAValue, na.rm = TRUE),
    max = max(MAValue, na.rm = TRUE),
    min = min(MAValue, na.rm = TRUE),
    sdev = sd(MAValue, na.rm = TRUE)
  )


Consolidated = rbind(BTC2GSIB, ETH2GSIB, GSIB2BTC, GSIB2ETH, GSIB2GSIB, Coin2Coin)

### TO ADD FORMAT

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

Consolidated = insertRow(Consolidated, list("A: Bitcoin to GSIBs\\\\",NA,NA,NA,NA), 1)

Consolidated = insertRow(Consolidated, list("\\addlinespace",NA,NA,NA,NA), 5)
Consolidated = insertRow(Consolidated, list("B: Ethereum to GSIBs\\\\",NA,NA,NA,NA), 6)

Consolidated = insertRow(Consolidated, list("\\addlinespace",NA,NA,NA,NA), 10)
Consolidated = insertRow(Consolidated, list("C: GSIBs to Bitcoin\\\\",NA,NA,NA,NA), 11)

Consolidated = insertRow(Consolidated, list("\\addlinespace",NA,NA,NA,NA), 15)
Consolidated = insertRow(Consolidated, list("D: GSIBs to Ethereum\\\\",NA,NA,NA,NA), 16)

Consolidated = insertRow(Consolidated, list("\\addlinespace",NA,NA,NA,NA), 20)
Consolidated = insertRow(Consolidated, list("E: GSIBs to other GSIBs\\\\",NA,NA,NA,NA), 21)

Consolidated = insertRow(Consolidated, list("\\addlinespace",NA,NA,NA,NA), 25)
Consolidated = insertRow(Consolidated, list("F: Between cryptocurrencies\\\\",NA,NA,NA,NA), 26)

WP_code <-Consolidated %>%
  mutate(
    # mean = mean * 100,
    # min = min * 100,
    # max = max * 100,
    # sdev = sdev * 100,
    name = case_when(
      name == "amin" ~ "Minimum",
      name == "bmean" ~ "Mean",
      name == "cmax" ~ "Maximum",
      TRUE ~ name
    )
  )%>%
  mutate(RCode = case_when(
    str_detect(name,"Panel A: ") ~ paste0("\\multicolumn{10}{l}{",name,"}\\\\"),
    is.na(mean) ~ name,
    
    TRUE ~ paste0("\\hspace{12pt}",str_to_title(name)," & ",formatC(mean, format = "f", digits = 2, flag = "0"),
                  " & ",formatC(min, format = "f", digits = 2, flag = "0"),
                  " & ",formatC(max, format = "f", digits = 2, flag = "0"),
                  " & ",formatC(sdev, format = "f", digits = 2, flag = "0"),"\\\\")
  ))

write.csv(WP_code$RCode,"./Table code/Table 5 LSTM multiplier, one percent shock summary statistics.csv")


#### Table 6: LSTM multiplier TO GSIBs : summary statistics ####

Name_list <- data.frame(
  Name = c("BTC","ETH","JPM US", "BAC US", "C US",
           "HSBA LN", "WFC US", "3988 HK", "BARC LN", 
           "BNP FP", "DBK GR", "GS US", "601398 CH",
           "8306 JP", "1288 HK", "BK US", "939 HK", 
           "CSGN SW", "ACA FP", "INGA NA", "8411 JP", "MS US", "RY CN", "SAN SM", "GLE FP",  
           "STAN LN", "STT US", "8316 JP", "TD CN", "UBS US", "UCG IM" ),
  Rank = c(1:31))


LSTMM_TO_GSIB = LSTMM_df %>%
  filter(!Target %in% c("BTC","ETH"),
         Source != Target) %>%
  na.omit() %>%
  left_join(Name_list, by= c("Source" = "Name")) %>%
  group_by(Rank, Source) %>%
  summarise(
    mean = mean(MAValue, na.rm = TRUE),
    max = max(MAValue, na.rm = TRUE),
    min = min(MAValue, na.rm = TRUE),
    sdev = sd(MAValue, na.rm = TRUE)
  ) %>% ungroup %>% select(-Rank) 


LSTMM_TO_GSIB = insertRow(LSTMM_TO_GSIB, list("\\addlinespace",NA,NA,NA,NA), 3)
LSTMM_TO_GSIB = insertRow(LSTMM_TO_GSIB, list("\\addlinespace",NA,NA,NA,NA), 9)
LSTMM_TO_GSIB = insertRow(LSTMM_TO_GSIB, list("\\addlinespace",NA,NA,NA,NA), 15)
LSTMM_TO_GSIB = insertRow(LSTMM_TO_GSIB, list("\\addlinespace",NA,NA,NA,NA), 21)
LSTMM_TO_GSIB = insertRow(LSTMM_TO_GSIB, list("\\addlinespace",NA,NA,NA,NA), 27)
LSTMM_TO_GSIB = insertRow(LSTMM_TO_GSIB, list("\\addlinespace",NA,NA,NA,NA), 33)


LSTMM_TO_GSIB <-LSTMM_TO_GSIB %>%
  mutate(RCode = case_when(
    is.na(mean) ~ Source,
    TRUE ~ paste0(Source," & ",formatC(mean, format = "f", digits = 2, flag = "0"),
                  " & ",formatC(min, format = "f", digits = 2, flag = "0"),
                  " & ",formatC(max, format = "f", digits = 2, flag = "0"),
                  " & ",formatC(sdev, format = "f", digits = 2, flag = "0"),"\\\\")))

write.csv(LSTMM_TO_GSIB$RCode,"./Table code/Table 6 LSTMM2GSIB.csv")


LSTMM_TO_BTC = LSTMM_df %>%
  filter(Target %in% c("BTC"),
         Source != Target) %>%
  na.omit() %>%
  left_join(Name_list, by= c("Source" = "Name")) %>%
  group_by(Rank, Source) %>%
  summarise(
    mean = mean(MAValue, na.rm = TRUE),
    max = max(MAValue, na.rm = TRUE),
    min = min(MAValue, na.rm = TRUE),
    sdev = sd(MAValue, na.rm = TRUE)
  ) %>% ungroup %>% select(-Rank)

LSTMM_TO_BTC = insertRow(LSTMM_TO_BTC, list("\\addlinespace",NA,NA,NA,NA), 2)
LSTMM_TO_BTC = insertRow(LSTMM_TO_BTC, list("\\addlinespace",NA,NA,NA,NA), 8)
LSTMM_TO_BTC = insertRow(LSTMM_TO_BTC, list("\\addlinespace",NA,NA,NA,NA), 14)
LSTMM_TO_BTC = insertRow(LSTMM_TO_BTC, list("\\addlinespace",NA,NA,NA,NA), 20)
LSTMM_TO_BTC = insertRow(LSTMM_TO_BTC, list("\\addlinespace",NA,NA,NA,NA), 26)
LSTMM_TO_BTC = insertRow(LSTMM_TO_BTC, list("\\addlinespace",NA,NA,NA,NA), 32)


LSTMM_TO_BTC <-LSTMM_TO_BTC %>%
  mutate(RCode = case_when(
    is.na(mean) ~ Source,
    TRUE ~ paste0(Source," & ",formatC(mean, format = "f", digits = 2, flag = "0"),
                  " & ",formatC(min, format = "f", digits = 2, flag = "0"),
                  " & ",formatC(max, format = "f", digits = 2, flag = "0"),
                  " & ",formatC(sdev, format = "f", digits = 2, flag = "0"),"\\\\")))

write.csv(LSTMM_TO_BTC$RCode,"./Table code/Table 7 LSTMM2BTC.csv")


LSTMM_TO_ETH = LSTMM_df %>%
  filter(Target %in% c("ETH"),
         Source != Target) %>%
  na.omit() %>%
  left_join(Name_list, by= c("Source" = "Name")) %>%
  group_by(Rank, Source) %>%
  summarise(
    mean = mean(MAValue, na.rm = TRUE),
    max = max(MAValue, na.rm = TRUE),
    min = min(MAValue, na.rm = TRUE),
    sdev = sd(MAValue, na.rm = TRUE)
  ) %>% ungroup %>% select(-Rank)

LSTMM_TO_ETH = insertRow(LSTMM_TO_ETH, list("\\addlinespace",NA,NA,NA,NA), 2)
LSTMM_TO_ETH = insertRow(LSTMM_TO_ETH, list("\\addlinespace",NA,NA,NA,NA), 8)
LSTMM_TO_ETH = insertRow(LSTMM_TO_ETH, list("\\addlinespace",NA,NA,NA,NA), 14)
LSTMM_TO_ETH = insertRow(LSTMM_TO_ETH, list("\\addlinespace",NA,NA,NA,NA), 20)
LSTMM_TO_ETH = insertRow(LSTMM_TO_ETH, list("\\addlinespace",NA,NA,NA,NA), 26)
LSTMM_TO_ETH = insertRow(LSTMM_TO_ETH, list("\\addlinespace",NA,NA,NA,NA), 32)


LSTMM_TO_ETH <-LSTMM_TO_ETH %>%
  mutate(RCode = case_when(
    is.na(mean) ~ Source,
    TRUE ~ paste0(Source," & ",formatC(mean, format = "f", digits = 2, flag = "0"),
                  " & ",formatC(min, format = "f", digits = 2, flag = "0"),
                  " & ",formatC(max, format = "f", digits = 2, flag = "0"),
                  " & ",formatC(sdev, format = "f", digits = 2, flag = "0"),"\\\\")))

write.csv(LSTMM_TO_ETH$RCode,"./Table code/Table 8 LSTMM2ETH.csv")





