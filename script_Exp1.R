library(tidyverse)
library(stringr)
library(lme4)
library(emmeans)
library(gridExtra)

### EXP 1
### Sound symbolism in Brazilian Portuguese Pokémon names: Evidence for cross-linguistic similarities and differences 
## Godoy, Gomes, Kumagai and Kawahara (to appear): Journal of Portuguese Linguistics


# loading data file

data <- read.csv("Exp1.csv")

### column names

#participante = participant id

#Já.realizou.alguma.pesquisa.sobre.simbolismo.sonoro. = have you ever taken part in a research on sound symbolism (sim/não = yes/no)

#Já.participou.de.algum.experimento.relacionado.a.dar.nomes.a.Pokémons., nome = have you ever taken part in a Pokémon naming experiment (sim/não = yes/no) 

# nome = pokémon id

# tipo = pokémon type (voador/evil = flying/evil)

# pokemon = name created by participants 

# transcript = phonological transcription using SILAC (Oushiro's (2018) )

# mantem = should we keep the data based on the exclusion criteria previously defined (see Section 5.3)

## processing data (cf Section 5.3)
data <- data%>% 
  select(-X) %>% 
  filter(Já.realizou.alguma.pesquisa.sobre.simbolismo.sonoro. == "Não" & Já.participou.de.algum.experimento.relacionado.a.dar.nomes.a.Pokémons. == "Não" & mantem == "sim")%>%
  droplevels()

## 918obs

## number of observations for each participant

participantes.n = data%>%
  group_by(participante)%>%
  summarise(n())

## dataset with participants with n > 10
participantes.n10 <- participantes.n %>% 
  filter(`n()` >= 10) %>%
  select(participante) %>%
  unlist()

## selecting only participants whose n > 10

data <- data %>% filter(participante %in% participantes.n10) %>% droplevels()

unique(data$participante) ## 48 participants

str(data)

##49 participants, 738obs


#### creating response variables

# number of obstruents and sibilants
data$number.obstruents = str_count(data$transcript, "v|b|d|z|Z|g")
data$number.sibilants = str_count(data$transcript, "s|S")

## binomial 
data$outcome.obs        = ifelse(data$number.obstruents > 0, 1, 0)
data$outcome.sibilants = ifelse(data$number.sibilants > 0, 1, 0)


### descriptive 


##### ANALISES DESCRITIVAS

data%>%
  group_by(tipo)%>%
  summarise(qtdd = n())

data%>%
  group_by(tipo, outcome.obs)%>%
  summarise(n = n())%>%
  mutate(freq = n/sum(n))

data%>%
  group_by(tipo, outcome.sibilants)%>%
  summarise(n = n())%>%
  mutate(freq = n/sum(n))

### analysis

## voiced.obs ~ tipo

system.time(binom.voiced <- glmer(outcome.obs ~ tipo + (1|participante) + (1|nome), data, family=binomial(link="logit"), control = glmerControl(optimizer = "bobyqa")))

system.time(binom.voiced.null <- glmer(outcome.obs ~ 1 + (1|participante) + (1|nome), data, family=binomial(link="logit"), control = glmerControl(optimizer = "bobyqa")))

anova(binom.voiced, binom.voiced.null)

summary(binom.voiced)


## sibilant ~ tipo

system.time(binom.sib <- glmer(outcome.sibilants ~ tipo + (1|participante), data, family=binomial(link="logit"), control = glmerControl(optimizer = "bobyqa")))

system.time(binom.sib.null <- glmer(outcome.sibilants ~ 1 + (1|participante), data, family=binomial(link="logit"), control = glmerControl(optimizer = "bobyqa")))

anova(binom.sib, binom.sib.null)  

### plots

#### plot vcd

means.vcd <- emmeans(binom.voiced, ~ tipo, type = "response")

summary(means.vcd)

datplot=data.frame(Response=c("Evil", "Flying"), Prob=c(summary(means.vcd)$prob[1], summary(means.vcd)$prob[2]), lower=c(summary(means.vcd)$prob[1] - summary(means.vcd)$SE[1], summary(means.vcd)$prob[2] - summary(means.vcd)$SE[2]), upper=c(summary(means.vcd)$prob[1]+summary(means.vcd)$SE[1], summary(means.vcd)$prob[2]+summary(means.vcd)$SE[2]))


vcd.plot = (ggplot(datplot, aes(x = Response, y = Prob)) +
              geom_bar(position = "dodge", stat = "identity", fill = "lightgrey")+
              geom_errorbar(data=datplot, mapping=aes(x=Response, ymin=upper, ymax=lower), width=0.2, size=1, color="black") +
              scale_x_discrete(name="")+
              ylab("% Expected responses - (least-square means)")+
              labs(title = "(A)", x = "Voiced Obstruents")+
              scale_y_continuous(labels=scales::percent, breaks = c(0,0.25, 0.50,0.75))+
              #geom_hline(yintercept=0.5,linetype="dashed")+
              coord_cartesian(ylim = c(0, 0.75))+
              theme_bw())


### plot sib

means.sib <- emmeans(binom.sib, ~ tipo, type = "response")

summary(means.sib)

datplot.sib=data.frame(Response=c("Evil", "Flying"), Prob=c(summary(means.sib)$prob[1], summary(means.sib)$prob[2]), lower=c(summary(means.sib)$prob[1] - summary(means.sib)$SE[1], summary(means.sib)$prob[2] - summary(means.sib)$SE[2]), upper=c(summary(means.sib)$prob[1]+summary(means.sib)$SE[1], summary(means.sib)$prob[2]+summary(means.sib)$SE[2]))


sib.plot = (ggplot(datplot.sib, aes(x = Response, y = Prob)) +
              geom_bar(position = "dodge", stat = "identity", fill = "lightgrey")+
              geom_errorbar(data=datplot.sib, mapping=aes(x=Response, ymin=upper, ymax=lower), width=0.2, size=1, color="black") +
              scale_x_discrete(name="")+
              labs(title = "(B)", x = "Sibilants")+
              ylab("% Expected responses - (least-square means)")+
              scale_y_continuous(labels=scales::percent, breaks = c(0,0.25, 0.50,0.75))+
              #geom_hline(yintercept=0.5,linetype="dashed")+
              coord_cartesian(ylim = c(0, 0.75))+
              theme_bw())

grid.arrange(vcd.plot, sib.plot, ncol = 2)
