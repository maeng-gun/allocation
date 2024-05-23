library(tidyverse)
library(tidyquant)
library(scales)

link <- "https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/fin_data.csv"

dt <- read_csv(link)

dt <- dt %>% 
  group_by(ticker) %>% 
  mutate(idx_price = price/price[1]) %>% 
  tq_mutate(price, dailyReturn, "ret")

tab <- dt %>% 
  filter(!is.na(ret)) %>% 
  group_by(ticker) %>% 
  summarise(
    er = round(mean(ret),4),
    sd = round(sd(ret),4),
  )
  
tab %>% 
  ggplot(aes(x=sd, y=er, color=ticker)) +
  geom_point(size=5) +
  theme_bw() +
  ggtitle("Risk-Return Tradeoff") +
  xlab("Volatility") +
  ylab("Expected Returns") +
  scale_y_continuous(label=percent, limit=c(0, 0.03)) +
  scale_x_continuous(label=percent, limit=c(0, 0.1))

dt %>% 
  ggplot(aes(x=date, y=idx_price, color=ticker)) +
  geom_line() +
  theme_bw() +
  ggtitle("Price Development") +
  xlab("Date") +
  ylab("Price\n(Indexed 2000 = 1)") +
  scale_color_discrete(name="company")
