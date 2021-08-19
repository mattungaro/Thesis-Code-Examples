# Near the end of my work on my thesis, I received a FOIA dataset of requested permits from 2020. Only one was added into my 
# analysis. My time period goes from 2007 to Sept. 2019, and that one permit, while coming from the 2020 dataset, actually had
# a federally complete date from Sept. 2019. Therefore, we added it and its 30 linear feet of stream impacts, into my dataset.
# This is my reassessment of linear feet of stream impacts after adding it in. Note: my dataset was already aggregated by month.

# Generalized linear model ------------
x <-  read_csv("C:\\Users\\Owner\\Documents\Career Resources\\Portfolio\\Thesis\\Code\\all_4_4.16.csv")

x$orm_riv_LF <- as.numeric(x$orm_riv_LF)
summary(glm(orm_riv_LF ~ month_as_num * after_SMM, data = x, family = Gamma(link = log)))
# 30 additional linear feet didn't really make much of a difference.

# Summary statistics ---------------
mean <- sapply(x, mean, na.rm=TRUE)
sd <- sapply(x, sd, na.rm=TRUE)
median <- sapply(x, median, na.rm=TRUE)


after <- x %>% filter(after_SMM_T ==T)
before <- x %>% filter(after_SMM_T ==F)

before_mean <- sapply(before, mean, na.rm=TRUE)
before_sd <- sapply(before, sd, na.rm=TRUE)
before_median <- sapply(before, median, na.rm=TRUE)

after_mean <- sapply(after, mean, na.rm=TRUE)
after_sd <- sapply(after, sd, na.rm=TRUE)
after_median <- sapply(after, median, na.rm=TRUE)

table1 <- tibble(variable = colnames(all2), mean, sd, median, before_mean, before_median, before_sd, after_mean, after_sd, after_median)

# Plotting ----------------
all <- x %>% mutate(dateX = as.Date(Date_test, "%m/%d/%Y")) %>% mutate(after_SMM_true = ifelse(after_SMM == 1, TRUE, FALSE))
dateX = as.Date(all$Date_test, "%m/%d/%Y")
all2 <- all[94:242, ]

all2 %>% ggplot(aes(x = dateX,
                    y = orm_riv_LF,
                    family = after_SMM_true))+
  geom_point()+
  geom_smooth(method = "glm", se = T, 
              method.args = list(family = Gamma(link = "log")), color = "black") + scale_x_date(date_labels = "%m-%Y")+
  geom_vline(xintercept = as.numeric(all$dateX[166]), color = "black", linetype = "dashed", size = 1)+
  xlab("Month")+ylab("Stream Impacts (Linear Feet)")+ ggthemes::theme_clean()+
  theme(plot.title = element_text(size=20, face="bold.italic"),
        axis.title.x = element_text(size=20, face="bold"),
        axis.title.y = element_text(size=20, face="bold"), axis.text=element_text(size=18))

