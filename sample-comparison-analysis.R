################################
# Elissa Redmiles & Sean Kross #
# Last Updated July 2018       #
################################

###############
# Setup Stuff #
###############
#libraries
library(survey)
library(dplyr)
library(magrittr)
library(tidyr)
library(xtable)
library(purrr)
library(broom)
library(anesrake)
#data cleaning functions
#function for turning NA to 0
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

#function for formatting p values
make_pv <- function(p){
  if(identical(p, 0)){
    "-"
  } else if(p < 0.001){
    "< 0.001**"
  } else if(p <= 0.01){
    paste0(round(p, 3), "**")
  } else if (p <= 0.05){
    paste0(round(p, 3), "*")
  } else {
    as.character(round(p, 3))
  }
}

#function for making percentages
make_pct <- function(frac){
  paste0(round(frac * 100, 1), "%") 
}

#function for coloring tables
# base is compared to comp
# if base < comp and p contains a star then return base with orange
# if base > comp and p contains a star then return base with blue
make_colors <- function(base, comp, p){
  base_ <- as.numeric(gsub("%", "", base))
  comp_ <- as.numeric(gsub("%", "", comp))
  if (grepl("\\*", p)) {
    if (base_ < comp_) {
      paste0("\\colorbox{orange!30}{", base, "}")
    } else if(base_ > comp_) {
      paste0("\\colorbox{blue!30}{", base, "}")
    }
  } else {
    base
  }
}

############
# DATA ETL #
############
#Loading the data#
panel_raw <-read.csv("data/ssi-new.csv", check.names="false")
prob_raw <- read.csv("data/prob.csv", check.names="false")
mturk_raw <- read.csv("data/mturk.csv", check.names="false")
panel <- list()
prob <- list()
mturk <- list()

#Adding demographic information to analysis dataframes
prob$age<-prob_raw$age
prob$education<-prob_raw$educ2
mturk$age<-mturk_raw$age
mturk$education<-mturk_raw$education
panel$age<-as.numeric(panel_raw$age)
panel$education<-as.numeric(panel_raw$education)

#adding the survey weights to the analysis dataframe
prob$weight<-prob_raw$weight
prob$sstrata<-prob_raw$sstrata

####Data cleaning for each survey question.####
#Converting from numeric responses to boolean for all of the samples

##Negative Experiences
prob$experience.stolen.info.bool <- as.numeric(prob_raw$Experience.inaccurate.info) <2
prob$experience.inaccurate.info.bool <- as.numeric(prob_raw$Experience.inaccurate.info) <2
prob$experience.email.compromised.bool <- as.numeric(prob_raw$Experience.email.compromised) <2
prob$experience.victim.scam.bool <- as.numeric(prob_raw$Experience.victim.scam) <2
prob$experience.unwanted.contact.bool <- as.numeric(prob_raw$Experience.unwanted.contact) <2
prob$experience.lost.job.bool <- as.numeric(prob_raw$Experience.lost.job) <2
prob$experience.trouble.relationship.bool <- as.numeric(prob_raw$Experience.trouble.relationship) <2
prob$experience.post.bool <- as.numeric(prob_raw$Experience.post) <2

prob <- as.data.frame(prob)

mturk$experience.stolen.info.bool <- as.numeric(mturk_raw$Experience.stolen.info) <2
mturk$experience.inaccurate.info.bool <- as.numeric(mturk_raw$Experience.inaccurate.info) <2
mturk$experience.email.compromised.bool <- as.numeric(mturk_raw$Experience.email.compromised) <2
mturk$experience.victim.scam.bool <- as.numeric(mturk_raw$Experience.victim.scam) <2
mturk$experience.unwanted.contact.bool <- as.numeric(mturk_raw$Experience.unwanted.contact) <2
mturk$experience.lost.job.bool <- as.numeric(mturk_raw$Experience.lost.job) <2
mturk$experience.trouble.relationship.bool <- as.numeric(mturk_raw$Experience.trouble.relationship) <2
mturk$experience.post.bool <- as.numeric(mturk_raw$Experience.post) <2

mturk <- as.data.frame(mturk)

panel$experience.stolen.info.bool <- as.numeric(panel_raw$Experience.stolen.info) <2
panel$experience.inaccurate.info.bool <- as.numeric(panel_raw$Experience.inaccurate.info) <2
panel$experience.email.compromised.bool <- as.numeric(panel_raw$Experience.email.compromised) <2
panel$experience.victim.scam.bool <- as.numeric(panel_raw$Experience.victim.scam) <2
panel$experience.unwanted.contact.bool <- as.numeric(panel_raw$Experience.unwanted.contact) <2
panel$experience.lost.job.bool <- as.numeric(panel_raw$Experience.lost.job) <2
panel$experience.trouble.relationship.bool <- as.numeric(panel_raw$Experience.trouble.relationship) <2
panel$experience.post.bool <- as.numeric(panel_raw$Experience.post) <2

panel <- as.data.frame(panel)

##Advice
prob$advice.coworker.bool <- as.numeric(prob_raw$Advice.coworker) < 2
prob$advice.teacher.bool <- as.numeric(prob_raw$Advice.teacher) < 2
prob$advice.friend.bool <- as.numeric(prob_raw$Advice.friend) < 2 
prob$advice.librarian.bool <- as.numeric(prob_raw$Advice.librarian) < 2
prob$advice.teacher.bool <- as.numeric(prob_raw$Advice.teacher) < 2
prob$advice.website.bool <- (as.numeric(prob_raw$Advice.website) < 2
| as.numeric(prob_raw$Advice.gov) < 2)

mturk$advice.coworker.bool <- (as.numeric(na.zero(mturk_raw)$Advice.coworker) ==1)
mturk$advice.teacher.bool <- as.numeric(na.zero(mturk_raw)$Advice.teacher) ==1
mturk$advice.friend.bool <- as.numeric(na.zero(mturk_raw)$Advice.friend) ==1
mturk$advice.librarian.bool <- as.numeric(na.zero(mturk_raw)$Advice.librarian) ==1
mturk$advice.teacher.bool <- as.numeric(na.zero(mturk_raw)$Advice.teacher) ==1
mturk$advice.website.bool <- (as.numeric(na.zero(mturk_raw)$Advice.website) ==1 
| as.numeric(na.zero(mturk_raw)$Advice.gov) ==1)

panel$advice.coworker.bool <- (as.numeric(na.zero(panel_raw)$Advice.coworker) ==1)
panel$advice.teacher.bool <- as.numeric(na.zero(panel_raw)$Advice.teacher) ==1
panel$advice.friend.bool <- as.numeric(na.zero(panel_raw)$Advice.friend) ==1
panel$advice.librarian.bool <- as.numeric(na.zero(panel_raw)$Advice.librarian) ==1
panel$advice.teacher.bool <- as.numeric(na.zero(panel_raw)$Advice.teacher) ==1
panel$advice.website.bool <- (as.numeric(na.zero(panel_raw)$Advice.website) ==1 
| as.numeric(na.zero(panel_raw)$Advice.gov) ==1)

##Knowledge
prob$knowledge.passwords.bool<-(as.numeric(prob_raw$Knowledge.passwords)==1)
prob$knowledge.privacy.settings.bool<-(as.numeric(prob_raw$Knowledge.privacy.settings)==1)
prob$knowledge.privacy.policies.bool<-(as.numeric(prob_raw$Knowledge.privacy.policies)==1)
prob$knowledge.wifi.protection.bool<-(as.numeric(prob_raw$Knowledge.wifi.protection)==1)
prob$knowledge.protect.comp.bool<-(as.numeric(prob_raw$Knowledge.protect.comp)==1)
prob$knowledge.online.protect.bool<-(as.numeric(prob_raw$Knowledge.online.protect)==1)
prob$knowledge.online.scam.bool<-(as.numeric(prob_raw$Knowledge.online.scam)==1)

mturk$knowledge.passwords.bool<-(as.numeric(mturk_raw$Knowledge.passwords)==1)
mturk$knowledge.privacy.settings.bool<-(as.numeric(mturk_raw$Knowledge.privacy.settings)==1)
mturk$knowledge.privacy.policies.bool<-(as.numeric(mturk_raw$Knowledge.privacy.policies)==1)
mturk$knowledge.wifi.protection.bool<-(as.numeric(mturk_raw$Knowledge.wifi.protection)==1)
mturk$knowledge.protect.comp.bool<-(as.numeric(mturk_raw$Knowledge.protect.comp)==1)
mturk$knowledge.online.protect.bool<-(as.numeric(mturk_raw$Knowledge.online.protect)==1)
mturk$knowledge.online.scam.bool<-(as.numeric(mturk_raw$Knowledge.online.scam)==1)

panel$knowledge.passwords.bool<-(as.numeric(panel_raw$Knowledge.passwords)==1)
panel$knowledge.privacy.settings.bool<-(as.numeric(panel_raw$Knowledge.privacy.settings)==1)
panel$knowledge.privacy.policies.bool<-(as.numeric(panel_raw$Knowledge.privacy.policies)==1)
panel$knowledge.wifi.protection.bool<-(as.numeric(panel_raw$Knowledge.wifi.protection)==1)
panel$knowledge.protect.comp.bool<-(as.numeric(panel_raw$Knowledge.protect.comp)==1)
panel$knowledge.online.protect.bool<-(as.numeric(panel_raw$Knowledge.online.protect)==1)
panel$knowledge.online.scam.bool<-(as.numeric(panel_raw$Knowledge.online.scam)==1)

##Behavior
prob$internet.socialmedia <- (as.numeric(prob_raw$Interent.socalmedia)==1)
prob$internet.job <- (as.numeric(prob_raw$Internet.job)==1)
prob$internet.govbenefits <- (as.numeric(prob_raw$Internet.gov.benefits)==1)
prob$internet.loan <- (as.numeric(prob_raw$Internet.loans)==1)
prob$internet.health <- (as.numeric(prob_raw$Internet.health.Info)==1)
prob$internet.product <- (as.numeric(prob_raw$Internet.products)==1)

mturk$internet.socialmedia <- (as.numeric(mturk_raw$Interent.socalmedia)==1)
mturk$internet.job <- (as.numeric(mturk_raw$Internet.job)==1)
mturk$internet.govbenefits <- (as.numeric(mturk_raw$Internet.gov.benefits)==1)
mturk$internet.loan <- (as.numeric(mturk_raw$Internet.loans)==1)
mturk$internet.health <- (as.numeric(mturk_raw$Internet.health.Info)==1)
mturk$internet.product <- (as.numeric(mturk_raw$Internet.products)==1)

panel$internet.socialmedia <- (as.numeric(panel_raw$Interent.socalmedia)==1)
panel$internet.job <- (as.numeric(panel_raw$Internet.job)==1)
panel$internet.govbenefits <- (as.numeric(panel_raw$Internet.gov.benefits)==1)
panel$internet.loan <- (as.numeric(panel_raw$Internet.loans)==1)
panel$internet.health <- (as.numeric(panel_raw$Internet.health.Info)==1)
panel$internet.product <- (as.numeric(panel_raw$Internet.products)==1)

#weighing the probabilistic data after adding all relevant variables
probdesign<-svydesign(id = ~1, weights = ~weight,
                      data = prob, strata = ~sstrata)

###Subsetting data on age and education
#Ages 18-29
prob.1829<-subset(prob, age>=18 & age<30)
prob.1829design<-svydesign(id = ~1, weights = ~weight, 
                           data = prob.1829, strata = ~sstrata)
mturk.1829<-subset(mturk, age<2)
panel.1829<-subset(panel, age<2)

#Ages 30-49
prob.3049<-subset(prob, age>=30 & age<50)
prob.3049design<-svydesign(id = ~1, weights = ~weight, 
                           data = prob.3049, strata = ~sstrata)
mturk.3049<-subset(mturk, age<3 & age>1)
panel.3049<-subset(panel, age<3 & age>1)

#Ages 50+
prob.50plus<-subset(prob, age>=50)
prob.50plusdesign<-svydesign(id = ~1, weights = ~weight, 
                             data = prob.50plus, strata = ~sstrata)
mturk.50plus<-subset(mturk, age>2)
panel.50plus<-subset(panel, age>2)

#Education
#Less than High School
prob.lths<-subset(prob, education  %in% 1:2)
mturk.lths<-subset(mturk, education %in% 1)
panel.lths<-subset(panel, education %in% 1)

#High School and below
prob.hs <-subset(prob, education %in% 1:3)
#weighting the probabilistic data for this subset
prob.hsdesign<-svydesign(id = ~1, weights = ~weight, data = prob.hs,
                         strata = ~sstrata)
mturk.hs <- subset(mturk,education %in% 1:2)
panel.hs <- subset(panel,education %in% 1:2)

#Some college and below
prob.ltbs<-subset(prob, education %in% 1:5)
prob.ltbsdesign<-svydesign(id = ~1, weights = ~weight, data = prob.ltbs,
                           strata = ~sstrata)
mturk.ltbs<-subset(mturk, education %in% 1:5)
panel.ltbs<-subset(panel, education %in% 1:5)

#B.S. and above
prob.bs <-subset(prob, education %in% 6:8)
prob.bsdesign<-svydesign(id = ~1, weights = ~weight, data = prob.bs, 
                         strata = ~sstrata)
mturk.bs <- subset(mturk,education %in% 6:9)
panel.bs <- subset(panel,education %in% 6:9)

#########################
## Statistical Analysis #
#########################
#Omnibus tests for the overall sample and each subset
prob_totals <- map(prob, function(x){sum(!is.na(x))})
mturk_totals <- map(mturk, function(x){sum(!is.na(x))})
panel_totals <- map(panel, function(x){sum(!is.na(x))})

#getting the weighted proportions from the probabilistic dataset
prob_tbl <- setdiff(colnames(prob), c("age", "education", "weight",
                                      "sstrata")) %>%
  map(function(x){
    result <- svytable(as.formula(paste0("~", x)), 
                       design=probdesign,
                       Ntotal = prob_totals[[x]])["TRUE"]
    names(result) <- x
    result
  }) %>%
  unlist() %>%
  data_frame(pop = "prob", variable = names(.), n_true = .)

mturk_tbl <- mturk %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "mturk") %>%
  gather(variable, n_true, -pop)

panel_tbl <- panel %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "panel") %>%
  gather(variable, n_true, -pop)


prop_omni_tbl <- bind_rows(prob_tbl, mturk_tbl, panel_tbl) %>%
  spread(pop, n_true) %>%
  #doing proportion tests and adding test statistic information to tables
  mutate(stat = pmap(list(.$variable, .$mturk, .$prob, .$panel),
                     function(e, m, p, o){
    tidy(prop.test(c(m, p, o), c(mturk_totals[[e]], 
                                 prob_totals[[e]],
                                 panel_totals[[e]])))$statistic
  }) %>% unlist()) %>%
  #doing proportion tests and adding p-values to tables
  mutate(p_value = pmap(list(.$variable, .$mturk, .$prob, 
                             .$panel), function(e, m, p, o){
    tidy(prop.test(c(m, p, o), c(mturk_totals[[e]], 
                                 prob_totals[[e]],
                                 panel_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  # Now some cosmetics
  mutate(mturk = map(.$variable, function(e){
    round(mean(mturk[[e]], na.rm = TRUE), 3)
  }) %>% unlist()) %>%
  mutate(prob = map(.$variable, function(e){
    round(mean(prob[[e]], na.rm = TRUE), 3)
  }) %>% unlist()) %>%
  mutate(panel = map(.$variable, function(e){
    round(mean(panel[[e]], na.rm = TRUE), 3)
  }) %>% unlist()) %>%
  mutate(variable = map(.$variable, function(e){
    gsub(".bool", "", e)
  }) %>% unlist())

prop_omni_table <- prop_omni_tbl %>%
  select(variable, prob, mturk, panel, stat, p_value)

xtable(prop_omni_table)

#AGE
prob.1829_totals <- map(prob.1829, function(x){sum(!is.na(x))})
mturk.1829_totals <- map(mturk.1829, function(x){sum(!is.na(x))})
panel.1829_totals <- map(panel.1829, function(x){sum(!is.na(x))})
prob.1829_tbl <- setdiff(colnames(prob.1829), c("age", "education", 
                                                "weight", "sstrata")) %>%
  map(function(x){
    result <- svytable(as.formula(paste0("~", x)),
                       design=prob.1829design, 
                       Ntotal = prob.1829_totals[[x]])["TRUE"]
    names(result) <- x
    result
  }) %>%
  unlist() %>%
  data_frame(pop = "prob.1829", variable = names(.), n_true = .)

mturk.1829_tbl <- mturk.1829 %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "mturk.1829") %>%
  gather(variable, n_true, -pop)

panel.1829_tbl <- panel.1829 %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "panel.1829") %>%
  gather(variable, n_true, -pop)


prop_omni_tbl <- bind_rows(prob.1829_tbl, mturk.1829_tbl,
                           panel.1829_tbl) %>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$mturk.1829, .$prob.1829,
                          .$panel.1829), function(e, m, p, o){
    tidy(prop.test(c(m, p, o), c(mturk.1829_totals[[e]],
                                 prob.1829_totals[[e]],panel.1829_totals[[e]])))$statistic
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$mturk.1829, .$prob.1829,
                             .$panel.1829), function(e, m, p, o){
    tidy(prop.test(c(m, p, o), c(mturk.1829_totals[[e]], 
                                 prob.1829_totals[[e]],panel.1829_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  # Now some cosmetics
  mutate(mturk.1829 = map(.$variable, function(e){
    round(mean(mturk.1829[[e]], na.rm = TRUE), 3)
  }) %>% unlist()) %>%
  mutate(prob.1829 = map(.$variable, function(e){
    round(mean(prob.1829[[e]], na.rm = TRUE), 3)
  }) %>% unlist()) %>%
  mutate(panel.1829 = map(.$variable, function(e){
    round(mean(panel.1829[[e]], na.rm = TRUE), 3)
  }) %>% unlist()) %>%
  mutate(variable = map(.$variable, function(e){
    gsub(".bool", "", e)
  }) %>% unlist())

prop.1829_omni_table <- prop_omni_tbl %>%
  select(variable, prob.1829, mturk.1829, panel.1829, stat, p_value)

xtable(prop.1829_omni_table)

prob.3049_totals <- map(prob.3049, function(x){sum(!is.na(x))})
mturk.3049_totals <- map(mturk.3049, function(x){sum(!is.na(x))})
panel.3049_totals <- map(panel.3049, function(x){sum(!is.na(x))})
prob.3049_tbl <- setdiff(colnames(prob.3049), c("age", "education",
                                                "weight", "sstrata")) %>%
  map(function(x){
    result <- svytable(as.formula(paste0("~", x)), 
                       design=prob.3049design, Ntotal = prob.3049_totals[[x]])["TRUE"]
    names(result) <- x
    result
  }) %>%
  unlist() %>%
  data_frame(pop = "prob.3049", variable = names(.), n_true = .)

mturk.3049_tbl <- mturk.3049 %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "mturk.3049") %>%
  gather(variable, n_true, -pop)

panel.3049_tbl <- panel.3049 %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "panel.3049") %>%
  gather(variable, n_true, -pop)


prop_omni_tbl <- bind_rows(prob.3049_tbl, mturk.3049_tbl, 
                           panel.3049_tbl) %>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$mturk.3049, .$prob.3049,
                          .$panel.3049), function(e, m, p, o){
    tidy(prop.test(c(m, p, o), c(mturk.3049_totals[[e]],
                                 prob.3049_totals[[e]],panel.3049_totals[[e]])))$statistic
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$mturk.3049, .$prob.3049, 
                             .$panel.3049), function(e, m, p, o){
    tidy(prop.test(c(m, p, o), c(mturk.3049_totals[[e]],
                                 prob.3049_totals[[e]],panel.3049_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  # Now some cosmetics
  mutate(mturk.3049 = map(.$variable, function(e){
    round(mean(mturk.3049[[e]], na.rm = TRUE), 3)
  }) %>% unlist()) %>%
  mutate(prob.3049 = map(.$variable, function(e){
    round(mean(prob.3049[[e]], na.rm = TRUE), 3)
  }) %>% unlist()) %>%
  mutate(panel.3049 = map(.$variable, function(e){
    round(mean(panel.3049[[e]], na.rm = TRUE), 3)
  }) %>% unlist()) %>%
  mutate(variable = map(.$variable, function(e){
    gsub(".bool", "", e)
  }) %>% unlist())

prop.3049_omni_table <- prop_omni_tbl %>%
  select(variable, prob.3049, mturk.3049, panel.3049, stat, p_value)

xtable(prop.3049_omni_table)

prob.50plus_totals <- map(prob.50plus, function(x){sum(!is.na(x))})
mturk.50plus_totals <- map(mturk.50plus, function(x){sum(!is.na(x))})
panel.50plus_totals <- map(panel.50plus, function(x){sum(!is.na(x))})
prob.50plus_tbl <- setdiff(colnames(prob.50plus), c("age", "education",
                                                    "weight", "sstrata")) %>%
  map(function(x){
    result <- svytable(as.formula(paste0("~", x)),
                       design=prob.50plusdesign,
                       Ntotal = prob.50plus_totals[[x]])["TRUE"]
    names(result) <- x
    result
  }) %>%
  unlist() %>%
  data_frame(pop = "prob.50plus", variable = names(.), n_true = .)

mturk.50plus_tbl <- mturk.50plus %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "mturk.50plus") %>%
  gather(variable, n_true, -pop)

panel.50plus_tbl <- panel.50plus %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "panel.50plus") %>%
  gather(variable, n_true, -pop)


prop_omni_tbl <- bind_rows(prob.50plus_tbl, mturk.50plus_tbl,
                           panel.50plus_tbl) %>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$mturk.50plus, .$prob.50plus,
                          .$panel.50plus), function(e, m, p, o){
    tidy(prop.test(c(m, p, o), c(mturk.50plus_totals[[e]],
                                 prob.50plus_totals[[e]],panel.50plus_totals[[e]])))$statistic
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$mturk.50plus, .$prob.50plus, .$panel.50plus), function(e, m, p, o){
    tidy(prop.test(c(m, p, o), c(mturk.50plus_totals[[e]], 
                                 prob.50plus_totals[[e]],panel.50plus_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  # Now some cosmetics
  mutate(mturk.50plus = map(.$variable, function(e){
    round(mean(mturk.50plus[[e]], na.rm = TRUE), 3)
  }) %>% unlist()) %>%
  mutate(prob.50plus = map(.$variable, function(e){
    round(mean(prob.50plus[[e]], na.rm = TRUE), 3)
  }) %>% unlist()) %>%
  mutate(panel.50plus = map(.$variable, function(e){
    round(mean(panel.50plus[[e]], na.rm = TRUE), 3)
  }) %>% unlist()) %>%
  mutate(variable = map(.$variable, function(e){
    gsub(".bool", "", e)
  }) %>% unlist())

prop.50plus_omni_table <- prop_omni_tbl %>%
  select(variable, prob.50plus, mturk.50plus, panel.50plus, stat,
         p_value)

xtable(prop.50plus_omni_table)

#EDUCATION
prob.ltbs_totals <- map(prob.ltbs, function(x){sum(!is.na(x))})
mturk.ltbs_totals <- map(mturk.ltbs, function(x){sum(!is.na(x))})
panel.ltbs_totals <- map(panel.ltbs, function(x){sum(!is.na(x))})
prob.ltbs_tbl <- setdiff(colnames(prob.ltbs), c("age", "education",
                                                "weight", "sstrata")) %>%
  map(function(x){
    result <- svytable(as.formula(paste0("~", x)), 
                       design=prob.ltbsdesign, Ntotal = prob.ltbs_totals[[x]])["TRUE"]
    names(result) <- x
    result
  }) %>%
  unlist() %>%
  data_frame(pop = "prob.ltbs", variable = names(.), n_true = .)

mturk.ltbs_tbl <- mturk.ltbs %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "mturk.ltbs") %>%
  gather(variable, n_true, -pop)

panel.ltbs_tbl <- panel.ltbs %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "panel.ltbs") %>%
  gather(variable, n_true, -pop)


prop_omni_tbl <- bind_rows(prob.ltbs_tbl, mturk.ltbs_tbl,
                           panel.ltbs_tbl) %>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$mturk.ltbs, .$prob.ltbs,
                          .$panel.ltbs), function(e, m, p, o){
    tidy(prop.test(c(m, p, o), c(mturk.ltbs_totals[[e]],
                                 prob.ltbs_totals[[e]],panel.ltbs_totals[[e]])))$statistic
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$mturk.ltbs, .$prob.ltbs,
                             .$panel.ltbs), function(e, m, p, o){
    tidy(prop.test(c(m, p, o), c(mturk.ltbs_totals[[e]],
                                 prob.ltbs_totals[[e]],panel.ltbs_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  # Now some cosmetics
  mutate(mturk.ltbs = map(.$variable, function(e){
    round(mean(mturk.ltbs[[e]], na.rm = TRUE), 3)
  }) %>% unlist()) %>%
  mutate(prob.ltbs = map(.$variable, function(e){
    round(mean(prob.ltbs[[e]], na.rm = TRUE), 3)
  }) %>% unlist()) %>%
  mutate(panel.ltbs = map(.$variable, function(e){
    round(mean(panel.ltbs[[e]], na.rm = TRUE), 3)
  }) %>% unlist()) %>%
  mutate(variable = map(.$variable, function(e){
    gsub(".bool", "", e)
  }) %>% unlist())

prop.ltbs_omni_table <- prop_omni_tbl %>%
  select(variable, prob.ltbs, mturk.ltbs, panel.ltbs, stat, p_value)

xtable(prop.ltbs_omni_table)

prob.bs_totals <- map(prob.bs, function(x){sum(!is.na(x))})
mturk.bs_totals <- map(mturk.bs, function(x){sum(!is.na(x))})
panel.bs_totals <- map(panel.bs, function(x){sum(!is.na(x))})
prob.bs_tbl <- setdiff(colnames(prob.bs), c("age", "education",
                                            "weight", "sstrata")) %>%
  map(function(x){
    result <- svytable(as.formula(paste0("~", x)), 
                       design=prob.bsdesign, Ntotal = prob.bs_totals[[x]])["TRUE"]
    names(result) <- x
    result
  }) %>%
  unlist() %>%
  data_frame(pop = "prob.bs", variable = names(.), n_true = .)

mturk.bs_tbl <- mturk.bs %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "mturk.bs") %>%
  gather(variable, n_true, -pop)

panel.bs_tbl <- panel.bs %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "panel.bs") %>%
  gather(variable, n_true, -pop)


prop_omni_tbl <- bind_rows(prob.bs_tbl, mturk.bs_tbl, 
                           panel.bs_tbl) %>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$mturk.bs, .$prob.bs,
                          .$panel.bs), function(e, m, p, o){
    tidy(prop.test(c(m, p, o), c(mturk.bs_totals[[e]],
                                 prob.bs_totals[[e]],panel.bs_totals[[e]])))$statistic
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$mturk.bs, .$prob.bs,
                             .$panel.bs), function(e, m, p, o){
    tidy(prop.test(c(m, p, o), c(mturk.bs_totals[[e]], 
                                 prob.bs_totals[[e]],panel.bs_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  # Now some cosmetics
  mutate(mturk.bs = map(.$variable, function(e){
    round(mean(mturk.bs[[e]], na.rm = TRUE), 3)
  }) %>% unlist()) %>%
  mutate(prob.bs = map(.$variable, function(e){
    round(mean(prob.bs[[e]], na.rm = TRUE), 3)
  }) %>% unlist()) %>%
  mutate(panel.bs = map(.$variable, function(e){
    round(mean(panel.bs[[e]], na.rm = TRUE), 3)
  }) %>% unlist()) %>%
  mutate(variable = map(.$variable, function(e){
    gsub(".bool", "", e)
  }) %>% unlist())

prop.bs_omni_table <- prop_omni_tbl %>%
  select(variable, prob.bs, mturk.bs, panel.bs, stat, p_value)

xtable(prop.bs_omni_table)

#Pairwise proportion tests for the overall sample and each subset
#LaTeX tables created as part of analysis
prob_totals <- map(prob, function(x){sum(!is.na(x))})
mturk_totals <- map(mturk, function(x){sum(!is.na(x))})
panel_totals <- map(panel, function(x){sum(!is.na(x))})

prob_totals_tbl <- prob_totals %>%
  as.data.frame() %>%
  gather(variable, prob_total)
mturk_totals_tbl <- mturk_totals %>%
  as.data.frame() %>%
  gather(variable, mturk_total)
panel_totals_tbl <- panel_totals %>%
  as.data.frame() %>%
  gather(variable, panel_total)

prob_tbl <- setdiff(colnames(prob), c("age", "education",
                                      "weight", "sstrata")) %>%
  map(function(x){
    result <- svytable(as.formula(paste0("~", x)),
                       design=probdesign, Ntotal = prob_totals[[x]])["TRUE"]
    names(result) <- x
    result
  }) %>%
  unlist() %>%
  data_frame(pop = "prob", variable = names(.), n_true = .)

mturk_tbl <- mturk %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "mturk") %>%
  gather(variable, n_true, -pop)

panel_tbl <- panel %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "panel") %>%
  gather(variable, n_true, -pop)


prop_mturk_tbl <- bind_rows(prob_tbl, mturk_tbl) %>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$mturk, .$prob),
                     function(e, m, p){
    tidy(prop.test(c(m, p), c(mturk_totals[[e]],
                              prob_totals[[e]])))$statistic
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$mturk, .$prob),
                        function(e, m, p){
    tidy(prop.test(c(m, p), c(mturk_totals[[e]], 
                              prob_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  left_join(mturk_totals_tbl) %>%
  left_join(prob_totals_tbl) %>%
  mutate(mturk = mturk / mturk_total) %>%
  mutate(mturk = map_chr(.$mturk, make_pct)) %>%
  mutate(prob = prob / prob_total) %>%
  mutate(prob = map_chr(.$prob, make_pct)) %>%
  mutate(variable = map_chr(.$variable, function(e){
    gsub(".bool", "", e)
  })) %>%
  select(-mturk_total, -prob_total)

prop_panel_tbl <- bind_rows(prob_tbl, panel_tbl) %>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$panel, .$prob),
                     function(e, o, p){
    tryCatch(tidy(prop.test(c(o, p), c(panel_totals[[e]],
                                       prob_totals[[e]])))$statistic,
             error = function(xxx){
               message(e)
             })
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$panel, .$prob),
                        function(e, o, p){
    tidy(prop.test(c(o, p), c(panel_totals[[e]],
                              prob_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  left_join(panel_totals_tbl) %>%
  left_join(prob_totals_tbl) %>%
  mutate(panel = panel / panel_total) %>%
  mutate(panel = map_chr(.$panel, make_pct)) %>%
  mutate(prob = prob / prob_total) %>%
  mutate(prob = map_chr(.$prob, make_pct)) %>%
  mutate(variable = map_chr(.$variable, function(e){
    gsub(".bool", "", e)
  })) %>%
  select(-panel_total, -prob_total)

prop_panel_tbl %<>%
  rename(stat_op = stat) %>%
  rename(p_value_op = p_value)

prop_mturk_tbl %<>%
  rename(stat_mp = stat) %>%
  rename(p_value_mp = p_value)

prop_table <- prop_panel_tbl %>%
  select(-prob) %>%
  left_join(prop_mturk_tbl, by = "variable") %>%
  select(variable, mturk, panel, prob, p_value_mp,p_value_op) %>%
  # color
  mutate(mturk = pmap(list(.$mturk, .$prob, .$p_value_mp),
                      make_colors) %>% unlist()) %>%
  mutate(panel = pmap(list(.$panel, .$prob, .$p_value_op),
                      make_colors) %>% unlist())


print(xtable(prop_table), include.rownames = F)

# 
# Subsets

#age
prob.1829_totals <- map(prob.1829, function(x){sum(!is.na(x))})
mturk.1829_totals <- map(mturk.1829, function(x){sum(!is.na(x))})
panel.1829_totals <- map(panel.1829, function(x){sum(!is.na(x))})

prob.1829_totals_tbl <- prob.1829_totals %>%
  as.data.frame() %>%
  gather(variable, prob.1829_total)
mturk.1829_totals_tbl <- mturk.1829_totals %>%
  as.data.frame() %>%
  gather(variable, mturk.1829_total)
panel.1829_totals_tbl <- panel.1829_totals %>%
  as.data.frame() %>%
  gather(variable, panel.1829_total)

prob.1829_tbl <- setdiff(colnames(prob.1829), c("age", "education",
                                                "weight", "sstrata")) %>%
  map(function(x){
    result <- svytable(as.formula(paste0("~", x)),
                       design=prob.1829design,
                       Ntotal = prob.1829_totals[[x]])["TRUE"]
    names(result) <- x
    result
  }) %>%
  unlist() %>%
  data_frame(pop = "prob.1829", variable = names(.), n_true = .)

mturk.1829_tbl <- mturk.1829 %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "mturk.1829") %>%
  gather(variable, n_true, -pop)

panel.1829_tbl <- panel.1829 %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "panel.1829") %>%
  gather(variable, n_true, -pop)


prop.1829_mturk.1829_tbl <- bind_rows(prob.1829_tbl, mturk.1829_tbl) %>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$mturk.1829, .$prob.1829),
                     function(e, m, p){
    tidy(prop.test(c(m, p), c(mturk.1829_totals[[e]],
                              prob.1829_totals[[e]])))$statistic
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$mturk.1829, .$prob.1829),
                        function(e, m, p){
    tidy(prop.test(c(m, p), c(mturk.1829_totals[[e]],
                              prob.1829_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  left_join(mturk.1829_totals_tbl) %>%
  left_join(prob.1829_totals_tbl) %>%
  mutate(mturk.1829 = mturk.1829 / mturk.1829_total) %>%
  mutate(mturk.1829 = map_chr(.$mturk.1829, make_pct)) %>%
  mutate(prob.1829 = prob.1829 / prob.1829_total) %>%
  mutate(prob.1829 = map_chr(.$prob.1829, make_pct)) %>%
  mutate(variable = map_chr(.$variable, function(e){
    gsub(".bool", "", e)
  })) %>%
  select(-mturk.1829_total, -prob.1829_total)

prop.1829_panel.1829_tbl <- bind_rows(prob.1829_tbl, panel.1829_tbl) %>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$panel.1829, .$prob.1829),
                     function(e, o, p){
    tryCatch(tidy(prop.test(c(o, p), c(panel.1829_totals[[e]],
                                       prob.1829_totals[[e]])))$statistic,
             error = function(xxx){
               message(e)
             })
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$panel.1829, .$prob.1829),
                        function(e, o, p){
    tidy(prop.test(c(o, p), c(panel.1829_totals[[e]],
                              prob.1829_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  left_join(panel.1829_totals_tbl) %>%
  left_join(prob.1829_totals_tbl) %>%
  mutate(panel.1829 = panel.1829 / panel.1829_total) %>%
  mutate(panel.1829 = map_chr(.$panel.1829, make_pct)) %>%
  mutate(prob.1829 = prob.1829 / prob.1829_total) %>%
  mutate(prob.1829 = map_chr(.$prob.1829, make_pct)) %>%
  mutate(variable = map_chr(.$variable, function(e){
    gsub(".bool", "", e)
  })) %>%
  select(-panel.1829_total, -prob.1829_total)

prop.1829_panel.1829_tbl %<>%
  rename(stat_op = stat) %>%
  rename(p_value_op = p_value)

prop.1829_mturk.1829_tbl %<>%
  rename(stat_mp = stat) %>%
  rename(p_value_mp = p_value)

prop.1829_table <- prop.1829_panel.1829_tbl %>%
  select(-prob.1829) %>%
  left_join(prop.1829_mturk.1829_tbl, by = "variable") %>%
  select(variable, mturk.1829, panel.1829, prob.1829, p_value_mp,
         p_value_op) %>%
  # color
  mutate(mturk.1829 = pmap(list(.$mturk.1829, .$prob.1829, .$p_value_mp), make_colors) %>% unlist()) %>%
  mutate(panel.1829 = pmap(list(.$panel.1829, .$prob.1829, .$p_value_op), make_colors) %>% unlist())


print(xtable(prop.1829_table), include.rownames = F)

prob.3049_totals <- map(prob.3049, function(x){sum(!is.na(x))})
mturk.3049_totals <- map(mturk.3049, function(x){sum(!is.na(x))})
panel.3049_totals <- map(panel.3049, function(x){sum(!is.na(x))})

prob.3049_totals_tbl <- prob.3049_totals %>%
  as.data.frame() %>%
  gather(variable, prob.3049_total)
mturk.3049_totals_tbl <- mturk.3049_totals %>%
  as.data.frame() %>%
  gather(variable, mturk.3049_total)
panel.3049_totals_tbl <- panel.3049_totals %>%
  as.data.frame() %>%
  gather(variable, panel.3049_total)

prob.3049_tbl <- setdiff(colnames(prob.3049), c("age", "education",
                                                "weight", "sstrata")) %>%
  map(function(x){
    result <- svytable(as.formula(paste0("~", x)),
                       design=prob.3049design,
                       Ntotal = prob.3049_totals[[x]])["TRUE"]
    names(result) <- x
    result
  }) %>%
  unlist() %>%
  data_frame(pop = "prob.3049", variable = names(.), n_true = .)

mturk.3049_tbl <- mturk.3049 %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "mturk.3049") %>%
  gather(variable, n_true, -pop)

panel.3049_tbl <- panel.3049 %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "panel.3049") %>%
  gather(variable, n_true, -pop)


prop.3049_mturk.3049_tbl <- bind_rows(prob.3049_tbl, mturk.3049_tbl) %>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$mturk.3049, .$prob.3049),
                     function(e, m, p){
    tidy(prop.test(c(m, p), c(mturk.3049_totals[[e]],
                              prob.3049_totals[[e]])))$statistic
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$mturk.3049, .$prob.3049),
                        function(e, m, p){
    tidy(prop.test(c(m, p), c(mturk.3049_totals[[e]],
                              prob.3049_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  left_join(mturk.3049_totals_tbl) %>%
  left_join(prob.3049_totals_tbl) %>%
  mutate(mturk.3049 = mturk.3049 / mturk.3049_total) %>%
  mutate(mturk.3049 = map_chr(.$mturk.3049, make_pct)) %>%
  mutate(prob.3049 = prob.3049 / prob.3049_total) %>%
  mutate(prob.3049 = map_chr(.$prob.3049, make_pct)) %>%
  mutate(variable = map_chr(.$variable, function(e){
    gsub(".bool", "", e)
  })) %>%
  select(-mturk.3049_total, -prob.3049_total)

prop.3049_panel.3049_tbl <- bind_rows(prob.3049_tbl, panel.3049_tbl)%>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$panel.3049, .$prob.3049),
                     function(e, o, p){
    tryCatch(tidy(prop.test(c(o, p), c(panel.3049_totals[[e]]
                                      , prob.3049_totals[[e]])))$statistic,
             error = function(xxx){
               message(e)
             })
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$panel.3049, 
                             .$prob.3049), function(e, o, p){
    tidy(prop.test(c(o, p), c(panel.3049_totals[[e]], 
                              prob.3049_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  left_join(panel.3049_totals_tbl) %>%
  left_join(prob.3049_totals_tbl) %>%
  mutate(panel.3049 = panel.3049 / panel.3049_total) %>%
  mutate(panel.3049 = map_chr(.$panel.3049, make_pct)) %>%
  mutate(prob.3049 = prob.3049 / prob.3049_total) %>%
  mutate(prob.3049 = map_chr(.$prob.3049, make_pct)) %>%
  mutate(variable = map_chr(.$variable, function(e){
    gsub(".bool", "", e)
  })) %>%
  select(-panel.3049_total, -prob.3049_total)

prop.3049_panel.3049_tbl %<>%
  rename(stat_op = stat) %>%
  rename(p_value_op = p_value)

prop.3049_mturk.3049_tbl %<>%
  rename(stat_mp = stat) %>%
  rename(p_value_mp = p_value)

prop.3049_table <- prop.3049_panel.3049_tbl %>%
  select(-prob.3049) %>%
  left_join(prop.3049_mturk.3049_tbl, by = "variable") %>%
  select(variable, mturk.3049, panel.3049, prob.3049, p_value_mp,
         p_value_op) %>%
  # color
  mutate(mturk.3049 = pmap(list(.$mturk.3049, .$prob.3049, .$p_value_mp), make_colors) %>% unlist()) %>%
  mutate(panel.3049 = pmap(list(.$panel.3049, .$prob.3049, .$p_value_op), make_colors) %>% unlist())


print(xtable(prop.3049_table), include.rownames = F)

prob.50plus_totals <- map(prob.50plus, function(x){sum(!is.na(x))})
mturk.50plus_totals <- map(mturk.50plus, function(x){sum(!is.na(x))})
panel.50plus_totals <- map(panel.50plus, function(x){sum(!is.na(x))})

prob.50plus_totals_tbl <- prob.50plus_totals %>%
  as.data.frame() %>%
  gather(variable, prob.50plus_total)
mturk.50plus_totals_tbl <- mturk.50plus_totals %>%
  as.data.frame() %>%
  gather(variable, mturk.50plus_total)
panel.50plus_totals_tbl <- panel.50plus_totals %>%
  as.data.frame() %>%
  gather(variable, panel.50plus_total)

prob.50plus_tbl <- setdiff(colnames(prob.50plus), c("age", "education",
                                                    "weight", "sstrata")) %>%
  map(function(x){
    result <- svytable(as.formula(paste0("~", x)),
                       design=prob.50plusdesign,
                       Ntotal = prob.50plus_totals[[x]])["TRUE"]
    names(result) <- x
    result
  }) %>%
  unlist() %>%
  data_frame(pop = "prob.50plus", variable = names(.), n_true = .)

mturk.50plus_tbl <- mturk.50plus %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "mturk.50plus") %>%
  gather(variable, n_true, -pop)

panel.50plus_tbl <- panel.50plus %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "panel.50plus") %>%
  gather(variable, n_true, -pop)


prop.50plus_mturk.50plus_tbl <- bind_rows(prob.50plus_tbl,
                                          mturk.50plus_tbl) %>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$mturk.50plus, .$prob.50plus),
                     function(e, m, p){
    tidy(prop.test(c(m, p), c(mturk.50plus_totals[[e]],
                              prob.50plus_totals[[e]])))$statistic
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$mturk.50plus,
                             .$prob.50plus), function(e, m, p){
    tidy(prop.test(c(m, p), c(mturk.50plus_totals[[e]],
                              prob.50plus_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  left_join(mturk.50plus_totals_tbl) %>%
  left_join(prob.50plus_totals_tbl) %>%
  mutate(mturk.50plus = mturk.50plus / mturk.50plus_total) %>%
  mutate(mturk.50plus = map_chr(.$mturk.50plus, make_pct)) %>%
  mutate(prob.50plus = prob.50plus / prob.50plus_total) %>%
  mutate(prob.50plus = map_chr(.$prob.50plus, make_pct)) %>%
  mutate(variable = map_chr(.$variable, function(e){
    gsub(".bool", "", e)
  })) %>%
  select(-mturk.50plus_total, -prob.50plus_total)

prop.50plus_panel.50plus_tbl <- bind_rows(prob.50plus_tbl,
                                          panel.50plus_tbl) %>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$panel.50plus, .$prob.50plus),
                     function(e, o, p){
    tryCatch(tidy(prop.test(c(o, p), c(panel.50plus_totals[[e]],
                                       prob.50plus_totals[[e]])))$statistic,
             error = function(xxx){
               message(e)
             })
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$panel.50plus,
                             .$prob.50plus), function(e, o, p){
    tidy(prop.test(c(o, p), c(panel.50plus_totals[[e]], 
                              prob.50plus_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  left_join(panel.50plus_totals_tbl) %>%
  left_join(prob.50plus_totals_tbl) %>%
  mutate(panel.50plus = panel.50plus / panel.50plus_total) %>%
  mutate(panel.50plus = map_chr(.$panel.50plus, make_pct)) %>%
  mutate(prob.50plus = prob.50plus / prob.50plus_total) %>%
  mutate(prob.50plus = map_chr(.$prob.50plus, make_pct)) %>%
  mutate(variable = map_chr(.$variable, function(e){
    gsub(".bool", "", e)
  })) %>%
  select(-panel.50plus_total, -prob.50plus_total)

prop.50plus_panel.50plus_tbl %<>%
  rename(stat_op = stat) %>%
  rename(p_value_op = p_value)

prop.50plus_mturk.50plus_tbl %<>%
  rename(stat_mp = stat) %>%
  rename(p_value_mp = p_value)

prop.50plus_table <- prop.50plus_panel.50plus_tbl %>%
  select(-prob.50plus) %>%
  left_join(prop.50plus_mturk.50plus_tbl, by = "variable") %>%
  select(variable, mturk.50plus, panel.50plus, prob.50plus, 
         p_value_mp,p_value_op) %>%
  # color
  mutate(mturk.50plus = pmap(list(.$mturk.50plus, .$prob.50plus,
                                  .$p_value_mp), make_colors) %>% unlist()) %>%
  mutate(panel.50plus = pmap(list(.$panel.50plus, .$prob.50plus,
                                  .$p_value_op), make_colors) %>% unlist())


print(xtable(prop.50plus_table), include.rownames = F)

#Education

prob.ltbs_totals <- map(prob.ltbs, function(x){sum(!is.na(x))})
mturk.ltbs_totals <- map(mturk.ltbs, function(x){sum(!is.na(x))})
panel.ltbs_totals <- map(panel.ltbs, function(x){sum(!is.na(x))})

prob.ltbs_totals_tbl <- prob.ltbs_totals %>%
  as.data.frame() %>%
  gather(variable, prob.ltbs_total)
mturk.ltbs_totals_tbl <- mturk.ltbs_totals %>%
  as.data.frame() %>%
  gather(variable, mturk.ltbs_total)
panel.ltbs_totals_tbl <- panel.ltbs_totals %>%
  as.data.frame() %>%
  gather(variable, panel.ltbs_total)

prob.ltbs_tbl <- setdiff(colnames(prob.ltbs), c("age", "education",
                                                "weight", "sstrata")) %>%
  map(function(x){
    result <- svytable(as.formula(paste0("~", x)), 
                       design=prob.ltbsdesign, Ntotal = prob.ltbs_totals[[x]])["TRUE"]
    names(result) <- x
    result
  }) %>%
  unlist() %>%
  data_frame(pop = "prob.ltbs", variable = names(.), n_true = .)

mturk.ltbs_tbl <- mturk.ltbs %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "mturk.ltbs") %>%
  gather(variable, n_true, -pop)

panel.ltbs_tbl <- panel.ltbs %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "panel.ltbs") %>%
  gather(variable, n_true, -pop)


prop.ltbs_mturk.ltbs_tbl <- bind_rows(prob.ltbs_tbl, mturk.ltbs_tbl) %>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$mturk.ltbs, .$prob.ltbs), 
                     function(e, m, p){
    tidy(prop.test(c(m, p), c(mturk.ltbs_totals[[e]], 
                              prob.ltbs_totals[[e]])))$statistic
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$mturk.ltbs,
                             .$prob.ltbs), function(e, m, p){
    tidy(prop.test(c(m, p), c(mturk.ltbs_totals[[e]],
                              prob.ltbs_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  left_join(mturk.ltbs_totals_tbl) %>%
  left_join(prob.ltbs_totals_tbl) %>%
  mutate(mturk.ltbs = mturk.ltbs / mturk.ltbs_total) %>%
  mutate(mturk.ltbs = map_chr(.$mturk.ltbs, make_pct)) %>%
  mutate(prob.ltbs = prob.ltbs / prob.ltbs_total) %>%
  mutate(prob.ltbs = map_chr(.$prob.ltbs, make_pct)) %>%
  mutate(variable = map_chr(.$variable, function(e){
    gsub(".bool", "", e)
  })) %>%
  select(-mturk.ltbs_total, -prob.ltbs_total)

prop.ltbs_panel.ltbs_tbl <- bind_rows(prob.ltbs_tbl, panel.ltbs_tbl) %>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$panel.ltbs, .$prob.ltbs), 
                     function(e, o, p){
    tryCatch(tidy(prop.test(c(o, p), c(panel.ltbs_totals[[e]], 
                                       prob.ltbs_totals[[e]])))$statistic,
             error = function(xxx){
               message(e)
             })
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$panel.ltbs, .$prob.ltbs),
                        function(e, o, p){
    tidy(prop.test(c(o, p), c(panel.ltbs_totals[[e]], 
                              prob.ltbs_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  left_join(panel.ltbs_totals_tbl) %>%
  left_join(prob.ltbs_totals_tbl) %>%
  mutate(panel.ltbs = panel.ltbs / panel.ltbs_total) %>%
  mutate(panel.ltbs = map_chr(.$panel.ltbs, make_pct)) %>%
  mutate(prob.ltbs = prob.ltbs / prob.ltbs_total) %>%
  mutate(prob.ltbs = map_chr(.$prob.ltbs, make_pct)) %>%
  mutate(variable = map_chr(.$variable, function(e){
    gsub(".bool", "", e)
  })) %>%
  select(-panel.ltbs_total, -prob.ltbs_total)

prop.ltbs_panel.ltbs_tbl %<>%
  rename(stat_op = stat) %>%
  rename(p_value_op = p_value)

prop.ltbs_mturk.ltbs_tbl %<>%
  rename(stat_mp = stat) %>%
  rename(p_value_mp = p_value)

prop.ltbs_table <- prop.ltbs_panel.ltbs_tbl %>%
  select(-prob.ltbs) %>%
  left_join(prop.ltbs_mturk.ltbs_tbl, by = "variable") %>%
  select(variable, mturk.ltbs, panel.ltbs, prob.ltbs, p_value_mp,
         p_value_op) %>%
  # color
  mutate(mturk.ltbs = pmap(list(.$mturk.ltbs, .$prob.ltbs, .$p_value_mp), make_colors) %>% unlist()) %>%
  mutate(panel.ltbs = pmap(list(.$panel.ltbs, .$prob.ltbs, .$p_value_op), make_colors) %>% unlist())


print(xtable(prop.ltbs_table), include.rownames = F)

prob.bs_totals <- map(prob.bs, function(x){sum(!is.na(x))})
mturk.bs_totals <- map(mturk.bs, function(x){sum(!is.na(x))})
panel.bs_totals <- map(panel.bs, function(x){sum(!is.na(x))})

prob.bs_totals_tbl <- prob.bs_totals %>%
  as.data.frame() %>%
  gather(variable, prob.bs_total)
mturk.bs_totals_tbl <- mturk.bs_totals %>%
  as.data.frame() %>%
  gather(variable, mturk.bs_total)
panel.bs_totals_tbl <- panel.bs_totals %>%
  as.data.frame() %>%
  gather(variable, panel.bs_total)

prob.bs_tbl <- setdiff(colnames(prob.bs), c("age", "education",
                                            "weight", "sstrata")) %>%
  map(function(x){
    result <- svytable(as.formula(paste0("~", x)), 
                       design=prob.bsdesign, 
                       Ntotal = prob.bs_totals[[x]])["TRUE"]
    names(result) <- x
    result
  }) %>%
  unlist() %>%
  data_frame(pop = "prob.bs", variable = names(.), n_true = .)

mturk.bs_tbl <- mturk.bs %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "mturk.bs") %>%
  gather(variable, n_true, -pop)

panel.bs_tbl <- panel.bs %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "panel.bs") %>%
  gather(variable, n_true, -pop)


prop.bs_mturk.bs_tbl <- bind_rows(prob.bs_tbl, mturk.bs_tbl) %>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$mturk.bs, .$prob.bs),
                     function(e, m, p){
    tidy(prop.test(c(m, p), c(mturk.bs_totals[[e]], prob.bs_totals[[e]])))$statistic
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$mturk.bs, .$prob.bs),
                        function(e, m, p){
    tidy(prop.test(c(m, p), c(mturk.bs_totals[[e]], prob.bs_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  left_join(mturk.bs_totals_tbl) %>%
  left_join(prob.bs_totals_tbl) %>%
  mutate(mturk.bs = mturk.bs / mturk.bs_total) %>%
  mutate(mturk.bs = map_chr(.$mturk.bs, make_pct)) %>%
  mutate(prob.bs = prob.bs / prob.bs_total) %>%
  mutate(prob.bs = map_chr(.$prob.bs, make_pct)) %>%
  mutate(variable = map_chr(.$variable, function(e){
    gsub(".bool", "", e)
  })) %>%
  select(-mturk.bs_total, -prob.bs_total)

prop.bs_panel.bs_tbl <- bind_rows(prob.bs_tbl, panel.bs_tbl) %>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$panel.bs, .$prob.bs),
                     function(e, o, p){
    tryCatch(tidy(prop.test(c(o, p), c(panel.bs_totals[[e]],
                                       prob.bs_totals[[e]])))$statistic,
             error = function(xxx){
               message(e)
             })
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$panel.bs, .$prob.bs),
                        function(e, o, p){
    tidy(prop.test(c(o, p), c(panel.bs_totals[[e]], prob.bs_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  left_join(panel.bs_totals_tbl) %>%
  left_join(prob.bs_totals_tbl) %>%
  mutate(panel.bs = panel.bs / panel.bs_total) %>%
  mutate(panel.bs = map_chr(.$panel.bs, make_pct)) %>%
  mutate(prob.bs = prob.bs / prob.bs_total) %>%
  mutate(prob.bs = map_chr(.$prob.bs, make_pct)) %>%
  mutate(variable = map_chr(.$variable, function(e){
    gsub(".bool", "", e)
  })) %>%
  select(-panel.bs_total, -prob.bs_total)

prop.bs_panel.bs_tbl %<>%
  rename(stat_op = stat) %>%
  rename(p_value_op = p_value)

prop.bs_mturk.bs_tbl %<>%
  rename(stat_mp = stat) %>%
  rename(p_value_mp = p_value)

prop.bs_table <- prop.bs_panel.bs_tbl %>%
  select(-prob.bs) %>%
  left_join(prop.bs_mturk.bs_tbl, by = "variable") %>%
  select(variable, mturk.bs, panel.bs, prob.bs, p_value_mp,p_value_op) %>%
  # color
  mutate(mturk.bs = pmap(list(.$mturk.bs, .$prob.bs, .$p_value_mp), 
                         make_colors) %>% unlist()) %>%
  mutate(panel.bs = pmap(list(.$panel.bs, .$prob.bs, .$p_value_op),
                         make_colors) %>% unlist())

print(xtable(prop.bs_table), include.rownames = F)

###Just HS for panel and prob, not enough samples in mturk (n=57)
prob.hs_totals <- map(prob.hs, function(x){sum(!is.na(x))})
panel.hs_totals <- map(panel.hs, function(x){sum(!is.na(x))})

prob.hs_totals_tbl <- prob.hs_totals %>%
  as.data.frame() %>%
  gather(variable, prob.hs_total)
panel.hs_totals_tbl <- panel.hs_totals %>%
  as.data.frame() %>%
  gather(variable, panel.hs_total)

prob.hs_tbl <- setdiff(colnames(prob.hs), c("age", "education", 
                                            "weight", "sstrata")) %>%
  map(function(x){
    result <- svytable(as.formula(paste0("~", x)), design=prob.hsdesign,
                       Ntotal = prob.hs_totals[[x]])["TRUE"]
    names(result) <- x
    result
  }) %>%
  unlist() %>%
  data_frame(pop = "prob.hs", variable = names(.), n_true = .)

panel.hs_tbl <- panel.hs %>%
  select(-age, -education) %>%
  map(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(pop = "panel.hs") %>%
  gather(variable, n_true, -pop)

prop.hs_panel.hs_tbl <- bind_rows(prob.hs_tbl, panel.hs_tbl) %>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$panel.hs, .$prob.hs), 
                     function(e, o, p){
    tryCatch(tidy(prop.test(c(o, p), c(panel.hs_totals[[e]], 
                                       prob.hs_totals[[e]])))$statistic,
             error = function(xxx){
               message(e)
             })
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$panel.hs, .$prob.hs), 
                        function(e, o, p){
    tidy(prop.test(c(o, p), c(panel.hs_totals[[e]], prob.hs_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  left_join(panel.hs_totals_tbl) %>%
  left_join(prob.hs_totals_tbl) %>%
  mutate(panel.hs = panel.hs / panel.hs_total) %>%
  mutate(panel.hs = map_chr(.$panel.hs, make_pct)) %>%
  mutate(prob.hs = prob.hs / prob.hs_total) %>%
  mutate(prob.hs = map_chr(.$prob.hs, make_pct)) %>%
  mutate(variable = map_chr(.$variable, function(e){
    gsub(".bool", "", e)
  })) %>%
  select(-panel.hs_total, -prob.hs_total)

prop.hs_panel.hs_tbl %<>%
  rename(stat_op = stat) %>%
  rename(p_value_op = p_value)

prop.hs_table <- prop.hs_panel.hs_tbl %>%
  select(variable, panel.hs, prob.hs, p_value_op) %>%
  # color
  mutate(panel.hs = pmap(list(.$panel.hs, .$prob.hs, .$p_value_op), 
                         make_colors) %>% unlist())


print(xtable(prop.hs_table), include.rownames = F)

################################################
## Survey Weighting of the MTurk data          #
## (e.g., demographic balancing or weighting)  #
################################################
#Weights computed based on three age strata (subsets)
# and three educational strata

#Setting target demographic values based on US census figures
age_target <- c(.21, .35, .44)
names(age_target) <- c("18_29", "30_49", "50+")

ed_target <- c(.13 + .28, .31, .28)
names(ed_target) <- c("hs", "gt_hs", "gte_bs")

targets <- list(age = age_target, education = ed_target)
mturk$id <- 1:nrow(mturk)

#Dividing the MTurk data into the three age and three education strata
mturk_demo <- mturk %>%
  select(id, age, education) %>%
  mutate(age = map_chr(.$age, function(x){
    if(is.na(x)){return(NA)}
    
    if(x < 2){
      "18_29"
    } else if(x < 3){
      "30_49"
    } else if(x >= 3){
      "50+"
    } else {
      x
    }
  })) %>%
  mutate(education = map_chr(.$education, function(x){
    if(is.na(x)){return(NA)}
    
    if(x %in% 1:2){
      "hs"
    } else if(x %in% 3:5){
      "gt_hs"
    } else if(x %in% 6:10){
      "gte_bs"
    } else {
      x
    }
  })) %>%
  mutate(age = age %>% as.factor()) %>%
  mutate(education = education %>% as.factor())

#calculating weights for each response in the mturk data 
mturk_rake <- anesrake(targets, mturk_demo, caseid = mturk$id,
                       verbose = TRUE)
mturk_rake$weightvec

#applying weighting to the mturk data
mturk$cases<-1:nrow(mturk)
mturk.weighted<-merge(mturk,data.frame(cases=mturk_rake$caseid,
                                       weights=mturk_rake$weightvec),
                      by="cases")
mturkdesign<-svydesign(id=~1,weights=~weights,data=mturk.weighted)


##Statistical analysis on weighted data
#Proportion tests to compare weighted and unweighted mturk data to prob
#Also making LaTeX tables
mturk.weighted.totals <- map(mturk.weighted, function(x){sum(!is.na(x))})
mturk.weighted.totals_tbl <- mturk.weighted.totals %>%
  as.data.frame() %>%
  gather(variable, mtw_total)

mturk.weighted.tbl <- setdiff(colnames(mturk.weighted), c("age",
                                                          "education", 
                                                          "weights",
                                                          "cases", 
                                                          "id")) %>%
  map(function(x){
    result <- svytable(as.formula(paste0("~", x)), design=mturkdesign,
                       Ntotal = mturk.weighted.totals[[x]])["TRUE"]
    names(result) <- x
    result
  }) %>%
  unlist() %>%
  data_frame(pop = "mturk.weighted", variable = names(.), n_true = .)
prop_weighted_tbl <- bind_rows(prob_tbl, mturk.weighted.tbl) %>%
  spread(pop, n_true) %>%
  mutate(stat = pmap(list(.$variable, .$mturk.weighted, .$prob),
                     function(e, w, p){
    tidy(prop.test(c(w, p), c(mturk.weighted.totals[[e]], 
                              prob_totals[[e]])))$statistic
  }) %>% unlist()) %>%
  mutate(p_value = pmap(list(.$variable, .$mturk.weighted, .$prob), 
                        function(e, w, p){
    tidy(prop.test(c(w, p), c(mturk.weighted.totals[[e]],
                              prob_totals[[e]])))$p.value
  }) %>% unlist()) %>%
  # p-value adjustments and formatting
  mutate(p_value = p.adjust(.$p_value, method = "bonferroni")) %>%
  mutate(p_value = map_chr(.$p_value, make_pv)) %>%
  left_join(mturk.weighted.totals_tbl) %>%
  left_join(prob_totals_tbl) %>%
  mutate(mturk.weighted = mturk.weighted / mtw_total) %>%
  mutate(mturk.weighted = map_chr(.$mturk.weighted, make_pct)) %>%
  mutate(prob = prob / prob_total) %>%
  mutate(prob = map_chr(.$prob, make_pct)) %>%
  mutate(variable = map_chr(.$variable, function(e){
    gsub(".bool", "", e)
  })) %>%
  select(-mtw_total, -prob_total)

prop_weighted_tbl %<>%
  rename(stat_wp = stat) %>%
  rename(p_value_wp = p_value)
prop_w_table <- prop_weighted_tbl %>%
  select(-prob) %>%
  left_join(prop_mturk_tbl, by = "variable") %>%
  select(variable, mturk.weighted, mturk, prob, p_value_wp,
         p_value_mp) %>%
  # color
  mutate(mturk.weighted = pmap(list(.$mturk.weighted, .$prob,
                                    .$p_value_wp), make_colors) %>% unlist()) %>%
  mutate(mturk = pmap(list(.$mturk, .$prob, .$p_value_mp), 
                      make_colors) %>% unlist())
print(xtable(prop_w_table), include.rownames = F)
