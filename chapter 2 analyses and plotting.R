library(survival)
library(coxme)
library(tidyverse)
library(ggthemes)
library(ggfortify)
library(lme4)
library(lmerTest)
library(cowplot)
library(wesanderson)


theme_set(theme_tufte())

is.extrafont.installed <- function(){
  if(is.element("extrafont", installed.packages()[,1])){
    library(extrafont)
    # probably need something here to run font_import()
    return(T)
  }else{
    warning("Library extrafont installed; using system sans/serif libraries as fallback fonts. 
    To enable full font support, run: 
      install.packages('extrafont') 
      font_import()")
    return(F)
  }
}

base_font_family_tufte <- function(){
  if(is.extrafont.installed()){
    library(extrafont)
    tuftefont <- choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "Verdana", "serif"), quiet = FALSE)  
  }else{
    tuftefont <- "serif"
  }
  return(tuftefont)
}

theme_tufte_revised <- function(base_size = 11, base_family = base_font_family_tufte(), ticks = TRUE) {
  
  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(
      axis.line = element_line(color = 'black'),
      axis.title.x = element_text(vjust = -0.3), 
      axis.title.y = element_text(vjust = 0.8),
      legend.background = element_blank(), 
      legend.key = element_blank(), 
      legend.title = element_text(face="plain"),
      panel.background = element_blank(), 
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      strip.background = element_blank()
    )
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  
  ret
} 

require(extrafont)

load("chapter2.RData")
temp = data

land = data.frame(time = c(rep("Start of Game", 3), rep("End of Game",7)),
                  use = c("Bare Land","Primary Forest","Secondary Forest",
                          "Paddy","Immature Palm Oil Plantation",
                          "Mature Palm Oil Plantation","Bare Land","Intercropping",
                          "Primary Forest","Secondary Forest"),
                  perc = c(32,36,32,9,3,41,7,1,33,5))

land$time = factor(land$time, levels = c("Start of Game","End of Game"))
land$use = factor(land$use, levels = c("Primary Forest","Secondary Forest","Bare Land",
                                       "Paddy","Intercropping","Immature Palm Oil Plantation",
                                       "Mature Palm Oil Plantation"))


ggp = ggplot(land, aes(x=time, y=perc, fill=use)) + 
  geom_bar(stat = 'identity') +
  xlab("") +
  ylab("percentage land use")+
  theme_tufte_revised()

ggp1 = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20, vjust = 1.5), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT"))+
  scale_fill_manual(values = c("#31954E","#869B27","#E49B36","#78CAE0","#B69AC9","#EA5599","#A13E2B"))
  
png('Fig. 3.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()




usecalc =  function(dt, c, t)
{
  res = summary(survfit(Surv(dt, c, type = "right")~1),1:t)
  idx =  length(res$surv)
  res =  cbind(surv=1-res$surv, se=1.96*res$std.err, 
               lcl48=1-res$surv-1.96*res$std.err,
               ucl48=1-res$surv+1.96*res$std.err)[idx,]
  return(res)
}

##########################################################

temp$tok = 0
temp$strategy = ""
temp$others = temp$clear + temp$idle + temp$plasma
temp$sum = temp$paddy + temp$fish + temp$log + temp$hunt + temp$palmoil + temp$company + temp$others
t1 = t2 = t3 = t4 = t5 = t6 = t7 = temp


t1$tok = t1$paddy
t2$tok = t2$fish
t3$tok = t3$log
t4$tok = t4$hunt
t5$tok = t5$palmoil
t6$tok = t6$company
t7$tok = t7$others

t1$strategy = "Farming"
t2$strategy = "Fishing"
t3$strategy = "Logging"
t4$strategy = "Hunting"
t5$strategy = "Oilpalm"
t6$strategy = "Company Labour"
t7$strategy = "Others"

t = rbind(t1,t2,t3,t4,t5,t6,t7)
t$tok = (t$tok/t$sum)*100

bstrap = function(samp)
{
  l = length(samp)
  a = numeric(1000)
  for (i in 1:1000)
  {
    a[i] = mean(sample(samp,l,replace = T))
  }
  b1 = quantile(a,0.025)
  b2 = quantile(a,0.975)
  b = list(cil = b1, cir = b2)
  return(b)
}

stratdiv = t %>%
  group_by(strategy) %>% summarize(mean = mean(tok),cil = as.numeric(bstrap(tok)[1]),
                                   cir = as.numeric(bstrap(tok)[2]))

stratdiv$strategy = factor(stratdiv$strategy, levels = c("Farming","Fishing","Hunting","Logging",
                                                         "Oilpalm","Company Labour",
                                                         "Others"))

# Fig. 4a

ggp = ggplot(stratdiv, aes(x=strategy, y=mean)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin=cil, ymax=cir), size = 1, width = 0.1) +
  xlab("strategy") +
  ylab("percentage allocation of tokens")+
  theme_tufte_revised()

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 20, vjust = 1.5), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20, vjust = 1.5), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  #scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1), 
  #                   limits = c(0,1)) +
  scale_x_discrete(breaks = c("Farming","Fishing","Hunting","Logging",
                              "Oilpalm","Company Labour",
                              "Others"),
                   labels = c("Paddy\nFarming","Fishing","Hunting","Logging",
                              "Oilpalm","Company\nLabour",
                              "Others")) 

png('Fig. 4.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


##########################################################

temp[temp$hunt != 0,]$hunt = 1
temp[temp$plasma != 0,]$plasma = 1
temp[temp$company != 0,]$company = 1
temp$paddyx = scale(temp$paddy, center = F)
temp$fishx = scale(temp$fish, center = F)
temp$logx = scale(temp$log, center = F)
temp$huntx = as.factor(temp$hunt)
temp$palmoilx = scale(temp$palmoil, center = F)
temp$plasmax = as.factor(temp$plasma)
temp$companyx = as.factor(temp$company)
temp$oiltoktimx = scale(temp$oiltoktim, center = F)
temp$cletoktimx = scale(temp$cletoktim, center = F)

fit1 = lmer(endmon ~ paddyx + fishx + logx + palmoilx + huntx + companyx +
               (1|game), data = temp)

#Table 4
summary(fit1)

comp = temp %>%
  group_by(companyx) %>% summarize(mean = mean(endmon),cil = as.numeric(bstrap(endmon)[1]),
                                   cir = as.numeric(bstrap(endmon)[2]),n = n())

comp$mean = round(comp$mean*90.9262,0)
comp$cil = round(comp$cil*90.9262,0)
comp$cir = round(comp$cir*90.9262,0)
comp$companyx = factor(c("Others","Company Labour"), levels = c("Others","Company Labour"))



## fig. 4b

ggp = ggplot(comp, aes(x=companyx, y=mean)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin=cil, ymax=cir), size = 1, width = 0.1) +
  xlab("strategy") +
  ylab(paste("monetary earnings"))+
  theme_tufte_revised()

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 20, vjust = 1.5), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20, vjust = 1.5), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  annotate("text", x = 0.7, y = 160, label = paste("N =",comp$n[1]), size = 4) +
  annotate("text", x = 1.7, y = 160, label = paste("N =",comp$n[2]," "), size = 4)

png('extra.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


###################################################################



## 

# Table
#fit2 = glmer(paddy ~ ethnicity + sex + occupation + (1|game), data = temp, family = poisson)

# Table
#fit3 = glmer(fish ~ ethnicity + sex + occupation + (1|game), data = temp, family = poisson)


# Table
#fit4 = glmer(palmoil ~ occupation + (1|game), data = temp, family = poisson)


###################################################################

## who is likely to first plant oil palm? (same as previous)
## Although both communities have similar Palm Oil holdings, Kutai is a primarily fishing community 
## whereas Dayak is primarily farming. This may explain their prefernce for Palm Oil. 

fit.oillan = coxme(Surv(staoiltim, staoil, type = "right") ~ ethnicity + sex + occupation + education +
                     (1|game), data = temp)

#Table 1
summary(fit.oillan)

timepoint = 3

finaluse = temp %>% filter(!is.na(ethnicity)) %>% group_by(ethnicity) %>%
  summarise(utility = mean(staoil), n=sum(!is.na(staoil)),
            estuse = usecalc(dt=staoiltim, c=staoil, t=timepoint)["surv"],
            lcluse = usecalc(dt=staoiltim, c=staoil, t=timepoint)["lcl48"],
            ucluse = usecalc(dt=staoiltim, c=staoil, t=timepoint)["ucl48"])

finaluse = finaluse %>%
  mutate(lcluse=replace(lcluse, lcluse < 0, 0)) %>%
  mutate(ucluse=replace(ucluse, ucluse > 1, 1))


ggp = ggplot(finaluse, aes(x=ethnicity, y=estuse)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin=lcluse, ymax=ucluse), size = 1, width = 0.2) +
  xlab("ethnicity") +
  ylab(paste("likelihood of planting oil palm after",timepoint,"rounds"))+
  theme_tufte_revised()

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 20, vjust = 1.5), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20, vjust = 1.5), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  annotate("text", x = 0.7, y = 1, label = paste("N =",finaluse$n[1]), size = 4) +
  annotate("text", x = 1.7, y = 1, label = paste("N =",finaluse$n[2]," "), size = 4)

# education

finaluse = temp %>% group_by(education) %>%
  summarise(utility = mean(staoil), n=sum(!is.na(staoil)),
            estuse = usecalc(dt=staoiltim, c=staoil, t=timepoint)["surv"],
            lcluse = usecalc(dt=staoiltim, c=staoil, t=timepoint)["lcl48"],
            ucluse = usecalc(dt=staoiltim, c=staoil, t=timepoint)["ucl48"])

finaluse = finaluse %>%
  mutate(lcluse=replace(lcluse, lcluse < 0, 0)) %>%
  mutate(ucluse=replace(ucluse, ucluse > 1, 1))


ggp = ggplot(finaluse, aes(x=education, y=estuse)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin=lcluse, ymax=ucluse), size = 1, width = 0.2) +
  xlab("education") +
  ylab("") +
  #ylab(paste("likelihood of planting oil palm after",timepoint,"rounds"))+
  theme_tufte_revised()

ggp2 = ggp +
  theme(axis.title.x = element_text(size = 20, vjust = 1.5), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20, vjust = 1.5), axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  annotate("text", x = 0.7, y = 1, label = paste("N =",finaluse$n[1]), size = 4) +
  annotate("text", x = 1.7, y = 1, label = paste("N =",finaluse$n[2]," "), size = 4) +
  annotate("text", x = 2.7, y = 1, label = paste("N =",finaluse$n[3]," "), size = 4)  


g = plot_grid(ggp1,ggp2,nrow=1,ncol=2,rel_widths = c(2/5, 3/5))

png("Fig. 6.png", units="in", width=10, height=7, res=1000)
grid::grid.draw(g)
dev.off()



###################################################################

## who is likely to first stop growing paddy?
## 

fit.stoppad = coxme(Surv(stoppadtim, stoppad, type = "right") ~ ethnicity + sex + occupation + education +
                      (1|game), data = temp)
# Table 2
summary(fit.stoppad)

timepoint = 3

finaluse = temp %>% group_by(occupation) %>%
  summarise(utility = mean(stoppad), n=sum(!is.na(stoppad)),
            estuse = usecalc(dt=stoppadtim, c=stoppad, t=timepoint)["surv"],
            lcluse = usecalc(dt=stoppadtim, c=stoppad, t=timepoint)["lcl48"],
            ucluse = usecalc(dt=stoppadtim, c=stoppad, t=timepoint)["ucl48"])

finaluse = finaluse %>%
  mutate(lcluse=replace(lcluse, lcluse < 0, 0)) %>%
  mutate(ucluse=replace(ucluse, ucluse > 1, 1))


ggp = ggplot(finaluse, aes(x=occupation, y=estuse)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin=lcluse, ymax=ucluse), size = 1, width = 0.1) +
  xlab("livelihood") +
  ylab(paste("likelihood of not investing in paddy after",timepoint,"rounds"))+
  theme_tufte_revised()

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 20, vjust = 1.5), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20, vjust = 1.5), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  annotate("text", x = 0.7, y = 1, label = paste("N =",finaluse$n[1]), size = 4) +
  annotate("text", x = 1.7, y = 1, label = paste("N =",finaluse$n[2]," "), size = 4) +
  annotate("text", x = 2.7, y = 1, label = paste("N =",finaluse$n[3]), size = 4) +
  annotate("text", x = 3.7, y = 1, label = paste("N =",finaluse$n[4]," "), size = 4) +
  scale_x_discrete(breaks = c("Farming","Fishing",
                              "Oilpalm Smallholding","Others"),
                   labels = c("Paddy\nFarming","Fishing",
                              "Oilpalm\nSmallholding","Others"))

png('Fig. 5.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()



###################################################################



#### who is most likely to clear land?

fit.cletok = coxme(Surv(cletoktim, cletok, type = "right") ~ ethnicity + sex + current + education +
                     (1|game), data = temp)

## Table 3
summary(fit.cletok)

timepoint = 10

finaluse = temp %>% group_by(current, sex) %>%
  summarise(utility = mean(cletok), n=sum(!is.na(cletok)),
            estuse = usecalc(dt=cletoktim, c=cletok, t=timepoint)["surv"],
            lcluse = usecalc(dt=cletoktim, c=cletok, t=timepoint)["lcl48"],
            ucluse = usecalc(dt=cletoktim, c=cletok, t=timepoint)["ucl48"])

finaluse = finaluse %>%
  mutate(lcluse=replace(lcluse, lcluse < 0, 0)) %>%
  mutate(ucluse=replace(ucluse, ucluse > 1, 1))


pd = position_dodge(0.3)
ggp = ggplot(finaluse, aes(x=current, y=estuse, col=sex)) + 
  geom_point(size = 4, position = pd) +
  geom_errorbar(aes(ymin=lcluse, ymax=ucluse), size = 1, width = 0.1, position = pd) +
  xlab("crop") +
  ylab(paste("likelihood of clearing land after",timepoint,"rounds"))+
  theme_tufte_revised()

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 20, vjust = 1.5), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20, vjust = 1.5), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  annotate("text", x = 0.7, y = 1, label = paste("Male =",finaluse$n[1]), size = 4) +
  annotate("text", x = 0.7, y = 0.95, label = paste("Female =",finaluse$n[2]," "), size = 4) +
  annotate("text", x = 1.7, y = 1, label = paste("Male =",finaluse$n[3]), size = 4) +
  annotate("text", x = 1.7, y = 0.95, label = paste("Female =",finaluse$n[4]," "), size = 4) +
  scale_colour_manual(breaks = c("Others","Palm Oil"), 
                      labels = c("Others","Palm Oil"),
                      values = wes_palette("Royal1", n = 2))

png('Fig. 7.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


###########################################################################




## establist lack of correlations, table

sexoccupation = temp %>%
  group_by(sex,occupation) %>% summarize(n = n())

sexcurrent = temp %>%
  group_by(sex,current) %>% summarize(n = n())

sexethnicity = temp %>%
  filter(!is.na(ethnicity)) %>%
  group_by(sex,ethnicity) %>% summarize(n = n())

ethnicityeducation = temp %>%
  filter(!is.na(ethnicity)) %>%
  group_by(ethnicity,education) %>% summarize(n = n())

ethnicitycurrent = temp %>%
  filter(!is.na(ethnicity)) %>%
  group_by(ethnicity,current) %>% summarize(n = n())

ethnicityoccupation = temp %>%
  filter(!is.na(ethnicity)) %>%
  group_by(ethnicity,occupation) %>% summarize(n = n())

sexeducation = temp %>%
  group_by(sex,education) %>% summarize(n = n())

sexagerank = temp %>%
  group_by(sex,agerank) %>% summarize(n = n())

educationagerank = temp %>%
  group_by(education,agerank) %>% summarize(n = n())

