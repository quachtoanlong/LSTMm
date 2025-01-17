dir = dirname(rstudioapi::getSourceEditorContext()$path)

setwd(dir)

source(file = "./R scripts/0 - Load data.R")
source(file = "./R scripts/WP generating dca Rdata file.R")
source(file = "./R scripts/WP generating Long Relationship file.R")


lf_results$Target = str_remove(lf_results$TO,".return| EQUITY")
lf_results$Source = str_remove(lf_results$FROM,".return| EQUITY")
lf_results$Date = lf_results$Date
lf_results$values = lf_results$values * 100 #Transform to percent

AMRO_Markdown <-   theme_bw() + 
  theme(
    text = element_text(family="sans"),
    legend.position= 'bottom',
    legend.text = element_text(size = 12,
                               colour = 'black',
                               family="sans"),
    axis.text = element_text(size = 12,
                             colour = 'black',
                             family="sans"),
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size= 12, family="sans"),
    axis.line = element_line(colour = "black"),
    plot.caption = element_text(hjust = 0, 
                                family="sans",
                                vjust = -1, size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) 

coin2gsib <- lf_results %>%
  filter(Source %in% c("BTC","ETH"),
         !Target %in% c("BTC", "ETH")) %>%
  group_by(Date = as.Date(Date), Source) %>%
  summarize(value = mean(values))

ggplot(coin2gsib,
       aes(x=Date, y=value, group=Source)) + 
  geom_line(aes(col=Source)) +
  ggtitle("TO GSIBs") + 
  labs(y='', color = NULL) + 
  #caption = "\n Sources: Bloomberg Finance L.P.,  CoinGecko, and authors' calculations.") +
  ylim(0, 1.10) +
  scale_color_manual(values = c("#c00000", "#333333"))


# Evaluate TO and FROM cryptocurrencies and banks

bank2coin <- lf_results %>%
  filter(!Source %in% c("BTC","ETH"),
         Target %in% c("BTC", "ETH")) %>%
  group_by(Date = as.Date(Date), Target) %>%
  summarize(value = sum(values))

ggplot(bank2coin) + 
  geom_line(aes(x=Date, y=value, group=Target, col=Target)) +
  theme_bw() + 
  ggtitle("FROM GSIBs") + 
  #theme(legend.position=c(0.9, 0.9)) +
  labs(y='', color = NULL) +
  # ylim(0, 110)+
  scale_color_manual(values = c("#c00000", "#333333"))

coin2coin <- lf_results %>%
  filter(Target %in% c("BTC", "ETH"),
         Source %in% c("BTC","ETH"),
         Target != Source) %>%
  mutate(Coin = case_when(
    Source == "ETH" &  Target == "BTC" ~ "BTC to ETH",
    Source == "BTC" &  Target == "ETH" ~ "ETH to BTC"
  )) %>% 
  na.omit() %>%
  group_by(Date = as.Date(Date), Coin) %>%
  summarize(value = sum(values))

ggplot(coin2coin, aes(x=Date, y=value, group=Coin)) + geom_line(aes(col=Coin)) +
  theme_bw() + 
  theme(legend.position=c(0.8, 0.85)) +
  labs(y='', x='', color=NULL) +
  ylim(0, 50)+
  scale_color_manual(values = c("#c00000",
                                "#333333"))+
  
  AMRO_Markdown

write.csv(coin2gsib,"./Data/coin2gsib.csv")
write.csv(bank2coin,"./Data/bank2coin.csv")
