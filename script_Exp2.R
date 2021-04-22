library(tidyverse)
library(lme4)
library(emmeans)
library(gridExtra)

# loading data file

data <- read.csv("Exp2.csv")

### EXP 2
### Sound symbolism in Brazilian Portuguese Pokémon names: Evidence for cross-linguistic similarities and differences 
## Godoy, Gomes, Kumagai and Kawahara (to appear): Journal of Portuguese Linguistics

# column names

# participant: participant id

# previous_knowledge_ss: question about previous knowledge of sound simbolism (1 = yes; 2 = no)

# native_speaker: native speaker of brazilian portuguese (1 = yes, 2 = no)

# bat1, bat2, bat3, beak1, beak2, black_mosquito, blue_bird, caterpillar, putera, red_eyes, white_blue2, white_blue3, white_orange_bird, white_purple_monster, white_red_monster and yellow_bird: all these columns are the item's id. 1 codes and answer that were in favor of our hipothesis; 2 codes an answer that contradicts our initial hypothesis

# previous_experiments: have you ever taken part in pokemon naming experiments before (1 = yes, 2 = no)


# long format
## new columns

#pokemon: item id
#answer: whether participant's answers were in line with our hypothesis (1 = yes, 2 = no)

data <- data %>% gather(pokemon, answer, bat1, 
                        bat2,
                        bat3,
                        beak1, 
                        beak2, 
                        black_mosquito, 
                        blue_bird, 
                        caterpillar, 
                        putera, 
                        red_eyes, 
                        white_blue2, 
                        white_blue3, 
                        white_orange_bird, 
                        white_purple_monster, 
                        white_red_monster, 
                        yellow_bird) %>% select(-X)



data$participant           <- as.factor(data$participant)
data$native_speaker        <- as.factor(data$native_speaker) ## all native
data$previous_experiments  <- as.factor(data$previous_experiments)
data$previous_knowledge_ss <- as.factor(data$previous_knowledge_ss)
data$pokemon               <- as.factor(data$pokemon)
data$answer                <- as.factor(data$answer)

### excluding pp with previous knowledge of ss or pokemonastics exp

data %>% group_by(previous_knowledge_ss) %>% summarise(n = n()/16) #drop 11

data %>% group_by(previous_experiments) %>% summarise(n = n()/16) #drop 7

data <- data %>% filter(previous_knowledge_ss == "2") %>% filter(previous_experiments == "2") %>% droplevels() ## drop 14 pp; 131 remain

data %>% group_by(answer) %>% summarise(n())


######## ANALYSIS
### model

data$answer <- relevel(data$answer, ref = "2")

mdl <- glmer(answer ~ 1 + (1|participant) + (1|pokemon), data=data, family=binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))

summary(mdl)

##plots

means <- emmeans(mdl, ~ 1, type = "response")

datplot=data.frame(Response=c("Response"), Prob=c(summary(means)$prob[1]), lower=c(summary(means)$prob[1] - summary(means)$SE[1]), upper=c(summary(means)$prob[1]+summary(means)$SE[1]))


bar = (ggplot(datplot, aes(x = Response, y = Prob)) +
         geom_bar(position = "dodge", stat = "identity", fill = "lightgrey")+
         geom_errorbar(data=datplot, mapping=aes(x=Response, ymin=upper, ymax=lower), width=0.2, size=1, color="black") +
         scale_x_discrete(name="")+
         ylab("% Expected responses - (least-square means)")+
         xlab(" ")+
         labs(title = "(4A)")+
         scale_y_continuous(labels=scales::percent, breaks = c(0,0.25, 0.50,0.75))+
         geom_hline(yintercept=0.5,linetype="dashed")+
         coord_cartesian(ylim = c(0, 0.75))+
         theme_bw())

jpeg("Figure4A.jpeg")

bar 

dev.off()

      ## Hist participants

data <- mutate(data, 
               code.answer = case_when(answer == 2 ~ 0, answer == 1 ~ 1))

pp.forced.table <- data %>% 
  group_by(participant) %>%
  summarise(SUM = sum(code.answer), n = n())

length(unique(pp.forced.table$participant))

pp.forced <- data %>% 
  group_by(participant) %>%
  summarise(SUM = sum(code.answer), n = n(), prop = (SUM/n)*100)

length(unique(pp.forced$participant))

hist = ggplot(pp.forced, aes(x = as.factor(SUM), y = ..count..))+
  geom_bar()+
  labs(y = "Number of participants", x = "Expected responses across trials \nper participant", title = "(4B)")+
  theme_bw()

hist = hist + theme(axis.title=element_text(size = 10))

jpeg("Figure4B.jpeg")

hist 

dev.off()

jpeg("Figure4.jpeg")

grid.arrange(bar, hist, ncol = 2)

dev.off()
