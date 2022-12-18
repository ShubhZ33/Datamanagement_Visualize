#setup
install.packages("hot.deck")
library(readxl)
library(tidyverse)
library(hot.deck)
install.packages("mice")
library(mice)
# read data
df <-  read_excel('~/Documents/TCD/7141 visaliaztion/group report/new.data.xlsx')

df[df=='NA']=NA

data=df[,-2]
# convert into numeric
sapply(data,class)
data <- data %>% 
  mutate_all(as.numeric) %>% 
  data.frame(.)

#multiple imputation
imp <- mice(data,m=5)
a=complete(imp,action=1)
b=complete(imp,action=2)
c=complete(imp,action=3)
d=complete(imp,action=4)
e=complete(imp,action=5)
new.df <- (a+b+c+d+e)/5

new.df$Company=df$Company

#write csv
write.csv(new.df,file='~/Desktop/twitte+facebook.csv')

#Correlation_______________________________________________________________

library(tidyverse)
library(ggplot2)
library(knitr)
library(gt)
library(tidyverse,warn.conflicts = F)
library(Hmisc)
library(car)
library(caret)
# read new dataset
mydata <-  read_excel('~/Desktop/Twitter_fb_combined_final.xlsx')
# as.factor
mydata$Year= as.factor(mydata$Year)


final.data <- mydata[, -c(1,2,20)]

#corelation

df_cor <- Hmisc::rcorr(as.matrix(final.data))
data.frame(df_cor$r) %>% head %>% gt()

cors <- function(y){
  M <- Hmisc::rcorr(as.matrix(y))
  Mdf <- map(M,~data.frame(.x))
  return(Mdf)}

cors(final.data) %>% map(~rownames_to_column(.x,var='measure1')) %>% 
  first() %>% head() %>% gt()
#________________________________________________________
cors(final.data) %>% 
  map(~rownames_to_column(.x, var="measure1")) %>%
  map(~pivot_longer(.x, -measure1, "measure2")) %>%
  first() %>%
  head() %>%
  gt()
#____________________________________________________________

cors(final.data) %>%
  map(~rownames_to_column(.x, var="measure1")) %>%
  map(~pivot_longer(.x, -measure1, "measure2")) %>%
  bind_rows(.id = "id") %>%
  head() %>%
  gt()
#_____________________________________________________________
cors(final.data) %>%
  map(~rownames_to_column(.x, var="measure1")) %>%
  map(~pivot_longer(.x, -measure1, "measure2")) %>%
  bind_rows(.id = "id") %>%
  pivot_wider(names_from = id, values_from = value) %>%
  head() %>%
  gt()
#_______________________________________________________________
cors(final.data) %>%
  map(~rownames_to_column(.x, var="measure1")) %>%
  map(~pivot_longer(.x, -measure1, "measure2")) %>%
  bind_rows(.id = "id") %>%
  pivot_wider(names_from = id, values_from = value) %>%
  rename(p = P) %>%
  mutate(sig_p = ifelse(p < .05, T, F),
         p_if_sig = ifelse(sig_p, p, NA),
         r_if_sig = ifelse(sig_p, r, NA)) %>%
  head() %>% 
  gt()
#____________________________________________________________
formatted_cors <- function(final.data){
  cors(final.data) %>%
    map(~rownames_to_column(.x, var="measure1")) %>%
    map(~pivot_longer(.x, -measure1, "measure2")) %>%
    bind_rows(.id = "id") %>%
    pivot_wider(names_from = id, values_from = value) %>%
    rename(p = P) %>%
    mutate(sig_p = ifelse(p < .05, T, F),
           p_if_sig = ifelse(sig_p, p, NA),
           r_if_sig = ifelse(sig_p, r, NA)) 
}
formatted_cors(final.data) %>% head() %>% gt()
#______________________________________________________________
formatted_cors(final.data) %>%
  ggplot(aes(x = measure1, y = measure2, fill = r)) +
  geom_tile()
#_____________________________________________________________
formatted_cors(final.data) %>%
  ggplot(aes(measure1, measure2, fill=r, label=round(r_if_sig,2))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill ="" , title="",
       subtitle="") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  geom_text(size=1.5) +
  theme_classic() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme(axis.text=element_text(size=5,face = "bold"),axis.text.x=element_text(size=5,angle=90,hjust=1))










