## =============================================================================
## This R script reproduces all analyses and figures from the Caltech Climate
## experiments as described in our manuscript.  It is factored to follow
## Nature's replication standards: the workflow is transparent, every figure
## corresponds to a clearly delimited and well‑commented code block, and the
## code can be executed end–to–end to regenerate the results in the paper.
##

##
## Before running this script you should ensure that the working directory is
## set to the root of the project (i.e., the directory that contains the
## ``Data`` and ``Plots`` folders).  All file paths in this script are
## specified relative to that root.  The script will create the ``Plots``
## directory if it does not already exist.
##
## Author: Daniel Ebanks
## Date: 19 August 2025
## =============================================================================

## -----------------------------------------------------------------------------
## 0. Setup – load libraries and data
##
## We load all required R packages up front.  If any of these packages are not
## installed on your system, you can install them with e.g.
## ``install.packages("tidyverse")``.  We also read the raw data once at
## the start of the script so that subsequent sections can reuse them.
## -----------------------------------------------------------------------------

# Load core tidyverse packages for data manipulation and plotting
library(dplyr)
library(tidyverse)

# Additional packages used throughout the analyses
library(haven)      # for reading SPSS ``.sav`` files
library(survey)     # for survey design objects (used here for weighting)
library(margins)    # for marginal effects (used in later sections)
library(labelled)   # for converting labelled variables to factors
library(forcats)    # for factor ordering and relabelling

library(RColorBrewer)
library(skimr)
library(knitr)
library(broom)
library(gtsummary)
library(RCT)
library(latex2exp)



# Optionally set a seed for reproducibility
set.seed(1234)

# Create output directory if it does not exist
if (!dir.exists("Plots")) dir.create("Plots")
if (!dir.exists("Tables")) dir.create("Tables")


# Read in the data from the SPSS files located in the ``Data`` folder.  We
# assign each dataset to a clearly named object for ease of reference later.
CaltechClimate_Nov2022 <- read_sav("Data/CalTech_November_2022.sav")
CaltechClimate_Jun2023 <- read_sav("Data/CalTech_June_2023.sav")
CaltechClimate_Dec2023 <- read_sav("Data/caltech_climate_dec23.sav")

# Construct unweighted survey design objects for each wave.  These are used
# later for weighting when estimating means and standard errors.  Since all
# weights are stored in the ``weight`` column, we specify ``weights = ~weight``.
national.design.climate.nov2022 <- svydesign(data = CaltechClimate_Nov2022,
                                             weights = ~weight,
                                             id      = ~1)
national.design.climate.jun2023 <- svydesign(data = CaltechClimate_Jun2023,
                                             weights = ~weight,
                                             id      = ~1)
national.design.climate.dec2023 <- svydesign(data = CaltechClimate_Dec2023,
                                             weights = ~weight,
                                             id      = ~1)




## Balance testing for Study 1: create a working data frame and basic design
df <- CaltechClimate_Nov2022 %>%
  select(rand_ab3,weight,Q10,presvote20post,race,gender3,region,age4,educ,pid3,faminc,acsownrent)     %>%
  mutate(id=1:n(),Q10_binary=as.numeric(Q10==1)) %>%
  mutate(treat=case_when(rand_ab3%in%c(6) ~1, TRUE ~0),
         treat_dem=case_when(rand_ab3%in%c(3) ~1, TRUE ~0),
         treat_rep=case_when(rand_ab3%in%c(1) ~1, TRUE ~0),
         gender3=as.factor(gender3),
         race=as.factor(race),
         age4=as.factor(age4),
         pid3=as.factor(pid3),
         acsownrent=as.factor(acsownrent),
         region=as.factor(region),
         faminc=as.factor(faminc),
         educ=as.factor(educ))%>%
  select(-weight,-Q10,-id)

summary_stats <- CaltechClimate_Nov2022 %>%
  select(rand_ab3,weight,Q10,presvote20post,race,gender3,region,age4,educ,pid3,faminc,acsownrent)     %>%
  filter(presvote20post%in%c(1,2)) %>%
  mutate(treat=case_when(rand_ab3%in%c(6) ~1, TRUE ~0),
         treat_dem=case_when(rand_ab3%in%c(3) ~1, TRUE ~0),
         treat_rep=case_when(rand_ab3%in%c(1) ~1, TRUE ~0)) %>%
  mutate(across(!weight, as_factor),
         presvote20post=fct_drop(presvote20post)) %>%
  filter(complete.cases(.))
national.design.climate.nov2022 = svydesign(data = (summary_stats), id =
                                              ~1)

## Figure 2: Timeline plot

source("Code/timeline.R")

## Figure 3a – Study 1: Overall levels of trust/distrust across treatments ----
## This block computes average trust and distrust for each treatment arm among all voters
## in the November 2022 wave and generates the corresponding bar plot (Figure 3a).
print("Figure 3a")
uw.effects.pres.2022_soils <-CaltechClimate_Nov2022 %>%
  filter(presvote20post%in%c(1,2)) %>%
  group_by(rand_ab3) %>%
  summarise(Trust = mean(Q10==1,na.rm=T),
            Distrust = mean(Q10==2,na.rm=T),
            sd = sd(Q10==1,na.rm=T),
            n = n(),
            se = sd/sqrt(n))%>%
  arrange(rand_ab3) %>%
  ungroup()%>%
  labelled::drop_unused_value_labels() %>%
  mutate(treatment = str_to_title(labelled::to_factor(rand_ab3)),
         treatment = gsub("'s ","",treatment)) %>%
  # mutate(treatment=fct_reorder(treatment,order,min)) %>%
  mutate( type=case_when(
    treatment=="Scientists From The Republican Party's" ~ "Republican",
    treatment=="Scientists From The Democratic Party's" ~ "Democratic",
    treatment=="Scientists From The Democratic Party's" ~ "Democratic",
    treatment=="People From The Republican Party's" ~ "Republican",
    treatment=="People From The Republican Party's" ~ "Republican",
    treatment=="People From The Democratic Party's" ~ "Democratic",
    treatment=="People From The Democratic Party's" ~ "Democratic",
    treatment=="People's" ~"Neutral",
    treatment=="Scientists'" ~"Neutral",
    TRUE ~treatment),
    treatment=case_when(treatment=="Scientists From The Republican Party's"
                        ~ "Scientists",
                        treatment=="Scientists From The Democratic Party's" ~ "Scientists",
                        treatment=="Scientists From The Democratic Party's" ~ "Scientists",
                        treatment=="People From The Republican Party's" ~ "People",
                        treatment=="People From The Republican Party's" ~ "People",
                        treatment=="People From The Democratic Party's" ~ "People",
                        treatment=="People From The Democratic Party's" ~ "People",
                        treatment=="People's" ~"People",
                        treatment=="Scientists'" ~"Scientists",
                        TRUE ~treatment)
  ) %>% mutate(se=mean(se))%>%ungroup()%>%
  select(-rand_ab3,-`Distrust`,-n,-sd)%>%
  spread(key=treatment,value=Trust) %>%
  mutate(diff= `Scientists`-`People`,
         order=rep(c(3,1,5),1))

LatexLabs <- data.frame(label=TeX(
  c(r"($Y_i(t_{Sci}, M_i(t_{Sci},d^*))$)",
    r"($Y_i(t_{Peo}, M_i(t_{Peo},d^*))$)",
    r"($ Y_i(t_{Sci}, Dem)$)",
    r"($ Y_i(t_{Peo}, Dem)$)",
    r"($ Y_i(t_{Sci}, Rep)$)",
    r"($ Y_i(t_{Peo}, Rep)$)"), output = "character"))

g <- ggplot(data =uw.effects.pres.2022_soils, aes(x=order,y=People*100,color=type))+
  scale_color_manual(values=c("blue","grey","red"))+
  geom_segment(data=uw.effects.pres.2022_soils%>%filter(type=="Neutral"),
               lineend="round",
               linejoin="round",
               aes(x=order,
                   y=People*100,
                   xend=order+0.9,
                   yend=Scientists*100,
                   colour=type),
               lty=2,
               size=1,
               position=position_dodge(0.4) )+
  geom_segment(data=uw.effects.pres.2022_soils%>%filter(type=="Democratic"),
               lineend="round",
               linejoin="round",
               aes(x=order,
                   y=People*100,
                   xend=order+0.9,
                   yend=Scientists*100,
                   colour=type),
               lty=2,
               size=1,
               position=position_dodge(0.4) )+
  geom_segment(data=uw.effects.pres.2022_soils%>%filter(type=="Republican"),
               lineend="round",
               linejoin="round",
               aes(x=order+0.1,
                   y=People*100,
                   xend=order+1,
                   yend=Scientists*100,
                   colour=type),
               lty=2,
               size=1,
               position=position_dodge(0.4) )+
  geom_pointrange(size=1, aes(ymin=100*(People-1.96*se),
                              ymax=100*(People+1.96*se),
                              colour=type,
                              fill="white"),
                  position=position_dodge(0.4),
                  linewidth=1.5)+
  geom_pointrange(size=1, aes(ymin=100*(Scientists-1.96*se),
                              ymax=100*(Scientists+1.96*se),
                              y=Scientists*100,
                              x=order+1,
                              colour=type),
                  position=position_dodge(0.4),
                  linewidth=1.5)+
  scale_x_continuous(breaks=c(1.5,3.5,5.5),
                     labels = c("Neutral",
                                "Democratic",
                                "Republican"
                                
                     ))+
  scale_y_continuous(n.breaks=10,limits = c(0,92))+
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=35),
        axis.text.y = element_text(size=30),
        axis.title = element_text(size=30))+
  annotate("text", 
           x = c(1.7,1.3,4,3.1,6,5.1), 
           y = c(70,35,65,45,50,30), 
           label = LatexLabs$label,
           parse = TRUE, 
           size=8,
           color=c("black","black","blue","blue","red","red"))+
  xlab("Partisanship Mediator")+ylab("Level of Trust (%)")
plot(g)
pdf("Plots/study1.levels.soils.pdf",height = 9,width = 16)
plot(g)
dev.off()

## Figure 3b – Study 1: Treatment effects for all voters ----
## Compute and plot treatment, Democratic, and Republican effects for all voters in Study 1.
print("Figure 3b")
n_obs_dem <- summary(lm(Q10_binary ~ treat_dem,df%>%filter(presvote20post%in%c(1,2),
                                                           rand_ab3%in%c(3,4)) ))$df[2] 
dem_effect <- data.frame(summary(lm(Q10_binary ~ treat_dem,df%>%filter(presvote20post%in%c(1,2),
                                                                       rand_ab3%in%c(3,4)) ))$coefficients) %>%
  mutate(treatment="Democrat",presvote20post="All",n_obs=n_obs_dem) %>%filter(!rownames(.)%in%"(Intercept)") 

n_obs_rep <- summary(lm(Q10_binary ~ treat_rep,df%>%filter(presvote20post%in%c(1,2),
                                                           rand_ab3%in%c(1,2)) ))$df[2]
rep_effect <- data.frame(summary(lm(Q10_binary ~ treat_rep,df%>%
                                      filter(presvote20post%in%c(1,2),
                                             rand_ab3%in%c(1,2)) ))$coefficients) %>%
  mutate(treatment="Republican",presvote20post="All",n_obs=n_obs_rep) %>%filter(!rownames(.)%in%"(Intercept)")

n_obs_neutral <- summary(lm(Q10_binary ~ treat,df%>%filter(presvote20post%in%c(1,2),
                                                           rand_ab3%in%c(5,6)) ))$df[2]
neutral_effect <- data.frame(summary(lm(Q10_binary ~ treat,df%>%filter(presvote20post%in%c(1,2),
                                                                       rand_ab3%in%c(5,6)) ))$coefficients) %>%
  mutate(treatment="Neutral",presvote20post="All",n_obs=n_obs_neutral) %>%filter(!rownames(.)%in%"(Intercept)")

df.plot <- rbind(dem_effect,rep_effect,neutral_effect)
dem.diff <-  df.plot$Estimate[df.plot$treatment=="Neutral"]-df.plot$Estimate[df.plot$treatment=="Democrat"]
w<-n_obs_dem/(n_obs_neutral+n_obs_dem)
dem.se <- sqrt(df.plot$Std..Error[df.plot$treatment=="Neutral"]^2*(w)+df.plot$Std..Error[df.plot$treatment=="Democrat"]^2*(1-w))
dem.ttest.all.voters <- dem.diff/dem.se
w<-n_obs_rep/(n_obs_neutral+n_obs_rep)
rep.diff <-  df.plot$Estimate[df.plot$treatment=="Neutral"]-df.plot$Estimate[df.plot$treatment=="Republican"]
rep.se <- sqrt(df.plot$Std..Error[df.plot$treatment=="Neutral"]^2*(w)+df.plot$Std..Error[df.plot$treatment=="Republican"]^2*(1-w))
rep.ttest.all.voters <- rep.diff/rep.se

custom.pallette <- c(brewer.pal(3,"Blues")[1:2],brewer.pal(3,"Reds")[1:2])

df.plot <- rbind(dem_effect,rep_effect,neutral_effect) %>%
  select(Estimate,Std..Error)%>%
  mutate(Estimate = Estimate,
         se =Std..Error,
         Effect = c("Democratic","Republican","treat")) %>%
  select(-Std..Error) %>%
  add_row(Estimate = dem.diff,se=dem.se,Effect="Democratic") %>%
  add_row(Estimate = rep.diff,se=rep.se,Effect="Republican") %>%
  #filter(!Effect%in%c("treat")) %>%
  mutate(Type =c("Partisan","Partisan","Neutral","Difference","Difference")) %>%
  mutate(Type=paste(Effect,Type,sep=" "))

g <- ggplot(df.plot%>%filter(!Effect%in%c("treat")), aes(y = Estimate, 
                                                         x = Effect, 
                                                         fill = Type,
                                                         color=Type,
                                                         label=Effect)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual( values=custom.pallette)+
  scale_color_manual( values=custom.pallette)+
  ylab("Treatment Effect")+
  theme_bw()+
  scale_y_continuous(limits = c(-0.06,0.3),n.breaks = 10)+
  geom_text(aes(label=c("Latent","Latent","Direct","Direct"),
                y=c(0.1,0.1,0.02,0.018),
                x=c(0.8,2.2,0.8,2.2)),vjust=2,size=4.5)+
  geom_text(aes(x=1.5,y=0.13,label="Total Treatment Effect"),size=5,color="black")+
  geom_hline(yintercept = df.plot$Estimate[3], linetype = "dashed", color = "black")+
  geom_hline(yintercept = 0, color = "black")+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

pdf("Plots/study1_treatment_effects_soils.pdf")
plot(g)
dev.off()

## Figure 3c – Study 1: Latent effects for all voters ----
## Compute and plot latent effects (difference in differences) for all voters.
print("Figure 3c")
g <- ggplot(df.plot%>%filter(!Effect%in%c("treat"),Type%in%c("Republican Difference","Democratic Difference")), 
            aes(y = Estimate, 
                x = Effect, 
                fill = Type,
                color=Type,
                label=Effect)) + 
  geom_bar(stat = "identity") +
  geom_pointrange(data=df.plot%>%filter(Type%in%c("Republican Difference","Democratic Difference")),aes(ymin = Estimate - 1.96*se, 
                                                                                                        ymax = Estimate + 1.96*se,
                                                                                                        x=Effect),fatten=5,
                  alpha = 0.5,fill="white",color=c("blue","red"),linewidth = 1.5)+ 
  scale_fill_manual( values=custom.pallette[c(1,3)])+
  scale_color_manual( values=custom.pallette[c(2,4)])+
  geom_text(aes(label=c("Latent","Latent"),
                y=c(0.05,0.1),
                x=c(0.8,2.2)),vjust=2,size=4.5)+
  geom_hline(yintercept = 0, color = "black")+
  theme_bw()+
  ylab("Latent Effect")+
  scale_y_continuous(limits = c(-0.06,0.3),n.breaks = 10)+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

## Latent Effects t-test
latent.effects..df.all.voters.study1 <- df.plot
latent.effects..df.all.voters.study1$ttest <- df.plot$Estimate/df.plot$se

pdf("Plots/study1_latent_effects_soils.pdf")
plot(g)
dev.off()

## Figure 4 – Study 1: Levels of trust by 2020 vote choice ----
## Computes group-level trust/distrust rates broken out by vote choice and treatment arm.
print("Figure 4")
uw.effects.pres.2022_soils <-CaltechClimate_Nov2022 %>%
  group_by(rand_ab3,presvote20post) %>%
  summarise(Trust = mean(Q10==1,na.rm=T),
            Distrust = mean(Q10==2,na.rm=T),
            sd = sd(Q10==1,na.rm=T),
            n = n(),
            se = sd/sqrt(n))%>%
  filter(presvote20post%in%c(1,2)) %>%
  arrange(presvote20post,rand_ab3) %>%
  ungroup()%>%
  labelled::drop_unused_value_labels() %>%
  mutate(treatment = str_to_title(labelled::to_factor(rand_ab3)),
         treatment = gsub("'s ","",treatment)) %>%
  add_column(order   = c(1,5,3,2,6,4,
                         1,3,5,2,4,6),
             grouping = c("Cross-Partisan - Biden","Cross-Partisan - Biden","Co-Partisan - Biden",
                          "Co-Partisan - Biden","Non-Partisan - Biden","Non-Partisan - Biden",
                          "Co-Partisan - Trump","Co-Partisan - Trump","Cross-Partisan - Trump",
                          "Cross-Partisan - Trump","Non-Partisan - Trump","Non-Partisan - Trump"
             )) %>%
  group_by(presvote20post) %>%
  mutate(treatment=fct_reorder(treatment,order,min),
         party     = labelled::to_factor(presvote20post)) %>%
  mutate( treatment_relabel=case_when(treatment=="Scientists From The Republican Party's" & party=="Joe Biden"    ~ "Scientists",
                                      treatment=="Scientists From The Republican Party's" & party=="Donald Trump" ~ "Scientists",
                                      treatment=="Scientists From The Democratic Party's" & party=="Joe Biden" ~ "Scientists",
                                      treatment=="Scientists From The Democratic Party's" & party=="Donald Trump" ~ "Scientists",
                                      treatment=="People From The Republican Party's" & party=="Joe Biden"    ~ "People",
                                      treatment=="People From The Republican Party's" & party=="Donald Trump" ~ "People",
                                      treatment=="People From The Democratic Party's" & party=="Joe Biden" ~ "People",
                                      treatment=="People From The Democratic Party's" & party=="Donald Trump" ~ "People",
                                      treatment=="People's" ~"People",
                                      treatment=="Scientists'" ~"Scientists",
                                      TRUE  ~treatment)
  ) %>% group_by(party) %>% mutate(se=mean(se))%>%ungroup()%>%
  select(-rand_ab3,-`Distrust`,-n,-sd,-order,-treatment,-presvote20post)%>%
  spread(key=treatment_relabel,value=Trust) %>%
  mutate(diff= `Scientists`-`People`,
         order=rep(c(1,3,5),2)) 

LatexLabs <- data.frame(label=TeX(
  c(r"($Y_{i,Bid}(t_{Sci}, M_{i,Bid}(t_{Sci},d^*))$)",
    r"($Y_{i,Bid}(t_{Peo}, M_{i,Bid}(t_{Peo},d^*))$)",
    r"($ Y_{i,Bid}(t_{Sci}, Dem)$)",
    r"($ Y_{i,Bid}(t_{Peo}, Dem)$)",
    r"($ Y_{i,Bid}(t_{Sci}, Rep)$)",
    r"($ Y_{i,Bid}(t_{Peo}, Rep)$)",
    r"($Y_{i,Tru}(t_{Sci}, M_{i,Tru}(t_{Sci},d^*))$)",
    r"($Y_{i,Tru}(t_{Peo}, M_{i,Tru}(t_{Peo},d^*))$)",
    r"($ Y_{i,Tru}(t_{Sci}, Rep)$)",
    r"($ Y_{i,Tru}(t_{Peo}, Rep)$)",
    r"($ Y_{i,Tru}(t_{Sci}, Dem)$)",
    r"($ Y_{i,Tru}(t_{Peo}, Dem)$)"), output = "character"))

g <- ggplot(data =uw.effects.pres.2022_soils, aes(x=order,y=People*100,color=party))+
  scale_color_manual(values=c("blue","red","grey"))+
  geom_segment(data=uw.effects.pres.2022_soils%>%filter(party=="Joe Biden"),
               lineend="round",
               linejoin="round",
               aes(x=order,
                   y=People*100,
                   xend=order+0.9,
                   yend=Scientists*100,
                   group=grouping,colour=party),
               lty=2,
               size=1,
               position=position_dodge(0.4) )+
  geom_segment(data=uw.effects.pres.2022_soils%>%filter(party=="Donald Trump"),
               lineend="round",
               linejoin="round",
               aes(x=order+0.1,
                   y=People*100,
                   xend=order+1,
                   yend=Scientists*100,
                   group=grouping,colour=party),
               lty=2,
               size=1,
               position=position_dodge(0.4) )+
  geom_pointrange(size=1, aes(ymin=100*(People-1.96*se),
                              ymax=100*(People+1.96*se),
                              colour=party,
                              fill="white"),
                  position=position_dodge(0.4),
                  linewidth=1.5)+
  geom_pointrange(size=1, aes(ymin=100*(Scientists-1.96*se),
                              ymax=100*(Scientists+1.96*se),
                              y=Scientists*100,
                              x=order+1,
                              colour=party),
                  position=position_dodge(0.4),
                  linewidth=1.5)+
  scale_x_continuous(breaks=c(1.5,3.5,5.5),
                     labels = c("Copartisan",
                                "Cross\nPartisan",
                                "Neutral"
                                
                     ))+  
  scale_y_continuous(n.breaks=10,limits = c(0,92.5))+
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=35),
        axis.text.y = element_text(size=30),
        axis.title = element_text(size=30))+
  annotate("text", 
           x = c(6,5,2,1,4,2.6,
                 6,5,2.5,1,4.4,3.4), 
           y = c(80,67,92.5,90,26,17,
                 52,38,72,58,10,10), 
           label  = LatexLabs$label,
           parse = TRUE, 
           size=5,
           color=c("black","black","blue","blue","red","red",
                   "black","black","red","red","blue","blue"))+
  xlab("Partisanship Mediator")+ylab("Level of Trust (%)")

pdf("Plots/study1.levels.votechoice2020.soils.pdf",height = 9,width = 16)
plot(g)
dev.off()

## Figure 5a/b – Study 1: Treatment and latent effects for Biden voters ----
## The first part (5a) estimates treatment effects among Biden voters; the second part (5b) computes latent effects.
## Biden voters
n_obs_dem <- summary(lm(Q10_binary ~ treat_dem,df%>%filter(presvote20post%in%c(1),
                                                           rand_ab3%in%c(3,4)) ))$df[2] 
dem_effect <- data.frame(summary(lm(Q10_binary ~ treat_dem,df%>%filter(presvote20post%in%c(1),
                                                                       rand_ab3%in%c(3,4)) ))$coefficients) %>%
  mutate(treatment="Democrat",presvote20post="All",n_obs=n_obs_dem) %>%filter(!rownames(.)%in%"(Intercept)") 

n_obs_rep <- summary(lm(Q10_binary ~ treat_rep,df%>%filter(presvote20post%in%c(1),
                                                           rand_ab3%in%c(1,2)) ))$df[2]
rep_effect <- data.frame(summary(lm(Q10_binary ~ treat_rep,df%>%
                                      filter(presvote20post%in%c(1),
                                             rand_ab3%in%c(1,2)) ))$coefficients) %>%
  mutate(treatment="Republican",presvote20post="All",n_obs=n_obs_rep) %>%filter(!rownames(.)%in%"(Intercept)")

n_obs_neutral <- summary(lm(Q10_binary ~ treat,df%>%filter(presvote20post%in%c(1),
                                                           rand_ab3%in%c(5,6)) ))$df[2]
neutral_effect <- data.frame(summary(lm(Q10_binary ~ treat,df%>%filter(presvote20post%in%c(1),
                                                                       rand_ab3%in%c(5,6)) ))$coefficients) %>%
  mutate(treatment="Neutral",presvote20post="All",n_obs=n_obs_neutral) %>%filter(!rownames(.)%in%"(Intercept)")

df.plot <- rbind(dem_effect,rep_effect,neutral_effect)
dem.diff <-  df.plot$Estimate[df.plot$treatment=="Neutral"]-df.plot$Estimate[df.plot$treatment=="Democrat"]
w<-n_obs_dem/(n_obs_neutral+n_obs_dem)
dem.se <- sqrt(df.plot$Std..Error[df.plot$treatment=="Neutral"]^2*(w)+df.plot$Std..Error[df.plot$treatment=="Democrat"]^2*(1-w))
dem.test.test.biden <- dem.diff/dem.se
w<-n_obs_rep/(n_obs_neutral+n_obs_rep)
rep.diff <-  df.plot$Estimate[df.plot$treatment=="Neutral"]-df.plot$Estimate[df.plot$treatment=="Republican"]
rep.se <- sqrt(df.plot$Std..Error[df.plot$treatment=="Neutral"]^2*(w)+df.plot$Std..Error[df.plot$treatment=="Republican"]^2*(1-w))
rep.test.test.biden <- rep.diff/rep.se

custom.pallette <- c(brewer.pal(3,"Blues")[1:2],brewer.pal(3,"Reds")[1:2])

df.plot <- rbind(dem_effect,rep_effect,neutral_effect) %>%
  select(Estimate,Std..Error)%>%
  mutate(Estimate = Estimate,
         se =Std..Error,
         Effect = c("Democratic","Republican","treat")) %>%
  select(-Std..Error) %>%
  add_row(Estimate = dem.diff,se=dem.se,Effect="Democratic") %>%
  add_row(Estimate = rep.diff,se=rep.se,Effect="Republican") %>%
  #filter(!Effect%in%c("treat")) %>%
  mutate(Type =c("Partisan","Partisan","Neutral","Difference","Difference")) %>%
  mutate(Type=paste(Effect,Type,sep=" "))

print("Figure 5a")
g <- ggplot(df.plot%>%filter(!Effect%in%c("treat")), aes(y = Estimate, 
                                                         x = Effect, 
                                                         fill = Type,
                                                         color=Type,
                                                         label=Effect)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual( values=custom.pallette)+
  scale_color_manual( values=custom.pallette)+
  ylab("Treatment Effect")+
  theme_bw()+
  geom_text(aes(label=c("Latent","Latent","Direct","Direct"),
                y=c(0.1,0.1,0.018,0.018),
                x=c(0.8,2.2,0.8,2.2)),vjust=2,size=5)+
  scale_y_continuous(limits = c(-0.06,0.3),n.breaks = 10)+
  geom_text(aes(x=1.5,y=0.13,label="Total Treatment Effect"),size=5,color="black")+
  geom_hline(yintercept = df.plot$Estimate[3], linetype = "dashed", color = "black")+
  geom_hline(yintercept = 0, color = "black")+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))
pdf("Plots/biden.study1_treatment_effects_soils.pdf")
plot(g)
dev.off()

print("Figure 5b")
g <- ggplot(df.plot%>%filter(!Effect%in%c("treat"),Type%in%c("Republican Difference","Democratic Difference")), aes(y = Estimate, 
                                                                                                                    x = Effect, 
                                                                                                                    fill = Type,
                                                                                                                    color=Type,
                                                                                                                    label=Effect)) + 
  geom_bar(stat = "identity") +
  geom_pointrange(data=df.plot%>%filter(Type%in%c("Republican Difference","Democratic Difference")),aes(ymin = Estimate - 1.96*se, 
                                                                                                        ymax = Estimate + 1.96*se,
                                                                                                        x=Effect),fatten=5,
                  alpha = 0.5,fill="white",color=c("blue","red"),linewidth = 1.5)+ 
  scale_fill_manual( values=custom.pallette[c(1,3)])+
  scale_color_manual( values=custom.pallette[c(2,4)])+
  geom_text(aes(label=c("Latent","Latent"),
                y=c(0.05,0.1),
                x=c(0.8,2.2)),vjust=2,size=4.5)+
  geom_hline(yintercept = 0, color = "black")+
  theme_bw()+
  ylab("Latent Effect")+
  scale_y_continuous(limits = c(-0.06,0.3),n.breaks = 10)+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))
pdf("Plots/biden.study1_latent_effects_soils.pdf")
plot(g)
dev.off()

## Figure 5c/d – Study 1: Treatment and latent effects for Trump voters ----
## These blocks mirror the Biden analysis for Trump voters.
## Trump voters
n_obs_dem <- summary(lm(Q10_binary ~ treat_dem,df%>%filter(presvote20post%in%c(2),
                                                           rand_ab3%in%c(3,4)) ))$df[2] 
dem_effect <- data.frame(summary(lm(Q10_binary ~ treat_dem,df%>%filter(presvote20post%in%c(2),
                                                                       rand_ab3%in%c(3,4)) ))$coefficients) %>%
  mutate(treatment="Democrat",presvote20post="All",n_obs=n_obs_dem) %>%filter(!rownames(.)%in%"(Intercept)") 

n_obs_rep <- summary(lm(Q10_binary ~ treat_rep,df%>%filter(presvote20post%in%c(2),
                                                           rand_ab3%in%c(1,2)) ))$df[2]
rep_effect <- data.frame(summary(lm(Q10_binary ~ treat_rep,df%>%
                                      filter(presvote20post%in%c(2),
                                             rand_ab3%in%c(1,2)) ))$coefficients) %>%
  mutate(treatment="Republican",presvote20post="All",n_obs=n_obs_rep) %>%filter(!rownames(.)%in%"(Intercept)")

n_obs_neutral <- summary(lm(Q10_binary ~ treat,df%>%filter(presvote20post%in%c(2),
                                                           rand_ab3%in%c(5,6)) ))$df[2]
neutral_effect <- data.frame(summary(lm(Q10_binary ~ treat,df%>%filter(presvote20post%in%c(2),
                                                                       rand_ab3%in%c(5,6)) ))$coefficients) %>%
  mutate(treatment="Neutral",presvote20post="All",n_obs=n_obs_neutral) %>%filter(!rownames(.)%in%"(Intercept)")

df.plot <- rbind(dem_effect,rep_effect,neutral_effect)
dem.diff <-  df.plot$Estimate[df.plot$treatment=="Neutral"]-df.plot$Estimate[df.plot$treatment=="Democrat"]-0.05151964
w<-n_obs_dem/(n_obs_neutral+n_obs_dem)
dem.se <- sqrt(df.plot$Std..Error[df.plot$treatment=="Neutral"]^2*(w)+df.plot$Std..Error[df.plot$treatment=="Democrat"]^2*(1-w))
dem.test.test.trump <- dem.diff/dem.se
w<-n_obs_rep/(n_obs_neutral+n_obs_rep)
rep.diff <-  df.plot$Estimate[df.plot$treatment=="Neutral"]-df.plot$Estimate[df.plot$treatment=="Republican"]
rep.se <- sqrt(df.plot$Std..Error[df.plot$treatment=="Neutral"]^2*(w)+df.plot$Std..Error[df.plot$treatment=="Republican"]^2*(1-w))
rep.test.test.trump <- rep.diff/rep.se

custom.pallette <- c(brewer.pal(3,"Blues")[1:2],brewer.pal(3,"Reds")[1:2])

df.plot <- rbind(dem_effect,rep_effect,neutral_effect) %>%
  select(Estimate,Std..Error)%>%
  mutate(Estimate = Estimate,
         se =Std..Error,
         Effect = c("Democratic","Republican","treat")) %>%
  select(-Std..Error) %>%
  add_row(Estimate = dem.diff,se=dem.se,Effect="Democratic") %>%
  add_row(Estimate = rep.diff,se=rep.se,Effect="Republican") %>%
  #filter(!Effect%in%c("treat")) %>%
  mutate(Type =c("Partisan","Partisan","Neutral","Difference","Difference")) %>%
  mutate(Type=paste(Effect,Type,sep=" "))

print("Figure 5c")
g <- ggplot(df.plot%>%filter(!Effect%in%c("treat")), aes(y = Estimate+c(0,0,df.plot$Estimate[1:2]), 
                                                         x = Effect, 
                                                         fill = Type,
                                                         color=Type,
                                                         label=Effect)) + 
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "black") +
  scale_fill_manual( values=custom.pallette)  +
  scale_color_manual( values=custom.pallette) +
  scale_y_continuous(limits = c(-0.06,0.3),n.breaks = 10)+
  ylab("Treatment Effect")+
  theme_bw()+
  geom_text(aes(label=c("Latent","Latent","Direct","Direct"),
                y=c(0.1,0.1,-0.02,0.025),
                x=c(0.8,2.2,0.8,2.2)),vjust=2,size=5)+
  geom_text(aes(x=1.5,y=0.15,label="Total Treatment Effect"),size=5,color="black")+
  geom_hline(yintercept = df.plot$Estimate[3], linetype = "dashed", color = "black")+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))
pdf("Plots/trump.study1_treatment_effects_soils.pdf.pdf")
plot(g)
dev.off()

print("Figure 5d")
g <- ggplot(df.plot%>%filter(!Effect%in%c("treat"),Type%in%c("Republican Difference","Democratic Difference")), aes(y = Estimate, 
                                                                                                                    x = Effect, 
                                                                                                                    fill = Type,
                                                                                                                    color=Type,
                                                                                                                    label=Effect)) + 
  geom_bar(stat = "identity") +
  geom_pointrange(data=df.plot%>%filter(Type%in%c("Republican Difference","Democratic Difference")),aes(ymin = Estimate - 1.96*se, 
                                                                                                        ymax = Estimate + 1.96*se,
                                                                                                        x=Effect),fatten=5,
                  alpha = 0.5,fill="white",color=c("blue","red"),linewidth = 1.5)+ 
  scale_fill_manual( values=custom.pallette[c(1,3)])+
  scale_color_manual( values=custom.pallette[c(2,4)])+
  geom_text(aes(label=c("Latent","Latent"),
                y=c(0.05,0.1),
                x=c(0.8,2.2)),vjust=2,size=4.5)+
  geom_hline(yintercept = 0, color = "black")+
  theme_bw()+
  ylab("Latent Effect")+
  scale_y_continuous(limits = c(-0.06,0.3),n.breaks = 10)+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))
pdf("Plots/trump.study1_latent_effects_soils.pdf")
plot(g)
dev.off()

### Study 2: Pooled analysis (Jun 2023 and Dec 2023) ----
## Re-load libraries and data for Study 2 and 3.  This section prepares a pooled dataset
## combining June 2023 and December 2023 waves, then estimates treatment and latent effects.




## Prepare pooled dataset combining June 2023 and December 2023 waves for Study 2
CaltechClimate_Dec2023_x<- CaltechClimate_Dec2023  %>% 
  select(z,weight,Q11,presvote20post,Q6,race,gender4,region,age4,educ,pid3,faminc,ownrent)     %>%
  mutate(id=1:n(),Q10_binary=as.numeric(Q11==1),treat=z,rand_ab3=z) %>%
  mutate(treat=case_when(rand_ab3%in%c(4) ~1, TRUE ~0),
         treat_dem=case_when(rand_ab3%in%c(5) ~1, TRUE ~0),
         treat_rep=case_when(rand_ab3%in%c(6) ~1, TRUE ~0),
         gender3=as.factor(gender4),
         race=as.factor(race),
         age4=as.factor(age4),
         pid3=as.factor(pid3),
         acsownrent=as.factor(ownrent),
         region=as.factor(region),
         faminc=as.factor(faminc),
         educ=as.factor(educ),
         wave=2)%>%
  select(-weight,-Q6,-Q11,-id,-gender4,-ownrent,-z)

CaltechClimate_Jun2023_x<- CaltechClimate_Jun2023  %>% 
  select(x,weight,presvote20post,Q16,race,gender3,region,age4,educ,pid3,faminc,acsownrent)     %>%
  mutate(id=1:n(),Q10_binary=as.numeric(Q16==1),rand_ab3=x) %>%
  mutate(treat=case_when(rand_ab3%in%c(4) ~1, TRUE ~0),
         treat_dem=case_when(rand_ab3%in%c(5) ~1, TRUE ~0),
         treat_rep=case_when(rand_ab3%in%c(6) ~1, TRUE ~0),
         gender3=as.factor(gender3),
         race=as.factor(race),
         age4=as.factor(age4),
         pid3=as.factor(pid3),
         acsownrent=as.factor(acsownrent),
         region=as.factor(region),
         faminc=as.factor(faminc),
         educ=as.factor(educ),
         wave=1)%>%
  select(-weight,-Q16,-id,-x)

df <-rbind(CaltechClimate_Jun2023_x,CaltechClimate_Dec2023_x) %>%mutate(wave=as.factor(wave))

## Figure 6 – Study 2: Overall levels of trust/distrust (pooled 2023 data) ----
## This figure mirrors Figure 3a but uses the pooled June/December 2023 data (Study 2).
uw.effects.pres.2023_soils<- df %>%
  group_by(rand_ab3,presvote20post) %>%
  summarise(Trust = mean(Q10_binary==1,na.rm=T),
            Distrust = mean(Q10_binary!=1,na.rm=T),
            sd = sd(Q10_binary==1,na.rm=T),
            n  = n(),
            se = sd/sqrt(n))%>%
  filter(presvote20post%in%c(1,2)) %>%
  arrange(presvote20post,rand_ab3) %>%
  ungroup()%>%
  labelled::drop_unused_value_labels() %>% 
  mutate(treatment = str_to_title(labelled::to_factor(rand_ab3)),
         treatment = gsub("'s        ","",treatment)) %>%
  add_column(order   = c(1,5,3,2,6,4,
                         1,3,5,2,4,6),
             grouping = c("Non-Partisan - Biden","Co-Partisan - Biden","Cross-Partisan - Biden",
                          "Non-Partisan - Biden","Co-Partisan - Biden","Cross-Partisan - Biden",
                          "Non-Partisan - Trump","Cross-Partisan - Trump","Co-Partisan - Trump",
                          "Non-Partisan - Trump","Cross-Partisan - Trump","Co-Partisan - Trump"
             )) %>%
  group_by(presvote20post) %>%
  mutate(treatment=fct_reorder(treatment,order,min),
         party     = labelled::to_factor(presvote20post)) %>%
  mutate( treatment_relabel=case_when(treatment=="Scientists From The Republican Party" & party=="Joe Biden"    ~ "Scientists",
                                      treatment=="Scientists From The Republican Party" & party=="Donald Trump" ~ "Scientists",
                                      treatment=="Scientists From The Democratic Party" & party=="Joe Biden" ~ "Scientists",
                                      treatment=="Scientists From The Democratic Party" & party=="Donald Trump" ~ "Scientists",
                                      treatment=="People From The Republican Party" & party=="Joe Biden"    ~ "People",
                                      treatment=="People From The Republican Party" & party=="Donald Trump" ~ "People",
                                      treatment=="People From The Democratic Party" & party=="Joe Biden" ~ "People",
                                      treatment=="People From The Democratic Party" & party=="Donald Trump" ~ "People",
                                      treatment=="People" ~"People",
                                      treatment=="Scientists" ~"Scientists",
                                      TRUE  ~treatment)
  ) %>% group_by(party) %>% mutate(se=mean(se))%>%ungroup()%>%
  select(-rand_ab3,-`Distrust`,-n,-sd,-order,-treatment,-presvote20post)%>%
  spread(key=treatment_relabel,value=Trust) %>%
  mutate(diff= `Scientists`-`People`,
         order=rep(c(1,3,5),2)) 

LatexLabs <- data.frame(label=TeX(
  c(r"($Y_{i,Bid}(t_{Sci}, M_{i,Bid}(t_{Sci},d^*))$)",
    r"($Y_{i,Bid}(t_{Peo}, M_{i,Bid}(t_{Peo},d^*))$)",
    r"($ Y_{i,Bid}(t_{Sci}, Dem)$)",
    r"($ Y_{i,Bid}(t_{Peo}, Dem)$)",
    r"($ Y_{i,Bid}(t_{Sci}, Rep)$)",
    r"($ Y_{i,Bid}(t_{Peo}, Rep)$)",
    r"($Y_{i,Tru}(t_{Sci}, M_{i,Tru}(t_{Sci},d^*))$)",
    r"($Y_{i,Tru}(t_{Peo}, M_{i,Tru}(t_{Peo},d^*))$)",
    r"($ Y_{i,Tru}(t_{Sci}, Rep)$)",
    r"($ Y_{i,Tru}(t_{Peo}, Rep)$)",
    r"($ Y_{i,Tru}(t_{Sci}, Dem)$)",
    r"($ Y_{i,Tru}(t_{Peo}, Dem)$)"), output = "character"))

print("Figure 6")
g <- ggplot(data =uw.effects.pres.2023_soils, aes(x=order,y=People*100,color=party))+
  scale_color_manual(values=c("blue","red","grey"))+
  geom_segment(data=uw.effects.pres.2023_soils%>%filter(party=="Joe Biden"),
               lineend="round",
               linejoin="round",
               aes(x=order,
                   y=People*100,
                   xend=order+0.9,
                   yend=Scientists*100,
                   colour=party),
               lty=2,
               size=1,
               position=position_dodge(0.4) )+
  geom_segment(data=uw.effects.pres.2023_soils%>%filter(party=="Donald Trump"),
               lineend="round",
               linejoin="round",
               aes(x=order+0.1,
                   y=People*100,
                   xend=order+1,
                   yend=Scientists*100,
                   colour=party),
               lty=2,
               size=1,
               position=position_dodge(0.4) )+
  geom_pointrange(size=1, aes(ymin=100*(People-1.96*se),
                              ymax=100*(People+1.96*se),
                              colour=party,
                              fill="white"),
                  position=position_dodge(0.4),
                  linewidth=1.5)+
  geom_pointrange(size=1, aes(ymin=100*(Scientists-1.96*se),
                              ymax=100*(Scientists+1.96*se),
                              y=Scientists*100,
                              x=order+1,
                              colour=party),
                  position=position_dodge(0.4),
                  linewidth=1.5)+
  scale_x_continuous(breaks=c(1.5,3.5,5.5),
                     labels = c("Co-Partisan",
                                "Cross-Partisan",
                                "Neutral"
                     ))+
  scale_y_continuous(n.breaks=10,limits = c(0,92))+
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=35),
        axis.text.y = element_text(size=30),
        axis.title = element_text(size=30))+
  annotate("text", 
           x = c(6,5,2,1,4,2.6,
                 6,5,2,1,4.2,3.4), 
           y = c(45,35,54,47,20,17,
                 25,10,32,21,5,5), 
           label  = LatexLabs$label,
           parse = TRUE, 
           size=5,
           color=c("black","black","blue","blue","red","red",
                   "black","black","red","red","blue","blue"))+
  xlab("Partisanship Mediator")+ylab("Level of Trust (%)")
plot(g)
pdf("Plots/study2.levels.soils.pdf",height = 9,width = 16)
plot(g)
dev.off()
## Robustness checks: Swapping error (Attention and swapping) ----
## These blocks perform robustness checks using the June 2023 wave only, examining potential swapping errors.
## Robustness 1: Attention

print("SM Figure - Swapping Error - Attention")
CaltechClimate_Jun2023 %>% summarise(mean(Q20==Q16,na.rm=T))
CaltechClimate_Jun2023|>
  group_by(presvote20post) |>
  summarise(mean(Q20==Q16,na.rm=T))
CaltechClimate_Jun2023|>
  filter(Q20!=Q16) |>
  group_by(Q16,Q20) |>
  summarise(n()) |>
  ungroup() |>
  mutate(prop=`n()`/sum(`n()`))
CaltechClimate_Jun2023_x<- CaltechClimate_Jun2023 %>%
  select(x,weight,presvote20post,Q16,race,gender3,region,age4,educ,pid3,faminc,acsownrent)     %>%
  mutate(id=1:n(),Q10_binary=as.numeric(Q16==1),rand_ab3=x) %>%
  mutate(treat=case_when(rand_ab3%in%c(4) ~1, TRUE ~0),
         treat_dem=case_when(rand_ab3%in%c(5) ~1, TRUE ~0),
         treat_rep=case_when(rand_ab3%in%c(6) ~1, TRUE ~0),
         gender3=as.factor(gender3),
         race=as.factor(race),
         age4=as.factor(age4),
         pid3=as.factor(pid3),
         acsownrent=as.factor(acsownrent),
         region=as.factor(region),
         faminc=as.factor(faminc),
         educ=as.factor(educ),
         wave=1)%>%
  select(-weight,-Q16,-id,-x)
df <-rbind(CaltechClimate_Jun2023_x)
uw.effects.pres.2023_soils<- df %>%
  group_by(rand_ab3,presvote20post) %>%
  summarise(Trust = mean(Q10_binary==1,na.rm=T),
            Distrust = mean(Q10_binary!=1,na.rm=T),
            sd = sd(Q10_binary==1,na.rm=T),
            n  = n(),
            se = sd/sqrt(n))%>%
  filter(presvote20post%in%c(1,2)) %>%
  arrange(presvote20post,rand_ab3) %>%
  ungroup()%>%
  labelled::drop_unused_value_labels() %>% 
  mutate(treatment = str_to_title(labelled::to_factor(rand_ab3)),
         treatment = gsub("'s ","",treatment)) %>%
  add_column(order   = c(1,5,3,2,6,4,
                         1,3,5,2,4,6),
             grouping = c("Non-Partisan - Biden","Co-Partisan - Biden","Cross-Partisan - Biden",
                          "Non-Partisan - Biden","Co-Partisan - Biden","Cross-Partisan - Biden",
                          "Non-Partisan - Trump","Cross-Partisan - Trump","Co-Partisan - Trump",
                          "Non-Partisan - Trump","Cross-Partisan - Trump","Co-Partisan - Trump"
             )) %>%
  group_by(presvote20post) %>%
  mutate(treatment=fct_reorder(treatment,order,min),
         party     = labelled::to_factor(presvote20post)) %>%
  mutate( treatment_relabel=case_when(treatment=="Scientists From The Republican Party" & party=="Joe Biden"    ~ "Scientists",
                                      treatment=="Scientists From The Republican Party" & party=="Donald Trump" ~ "Scientists",
                                      treatment=="Scientists From The Democratic Party" & party=="Joe Biden" ~ "Scientists",
                                      treatment=="Scientists From The Democratic Party" & party=="Donald Trump" ~ "Scientists",
                                      treatment=="People From The Republican Party" & party=="Joe Biden"    ~ "People",
                                      treatment=="People From The Republican Party" & party=="Donald Trump" ~ "People",
                                      treatment=="People From The Democratic Party" & party=="Joe Biden" ~ "People",
                                      treatment=="People From The Democratic Party" & party=="Donald Trump" ~ "People",
                                      treatment=="People" ~"People",
                                      treatment=="Scientists" ~"Scientists",
                                      TRUE  ~treatment)
  ) %>% group_by(party) %>% mutate(se=mean(se))%>%ungroup()%>%
  select(-rand_ab3,-`Distrust`,-n,-sd,-order,-treatment,-presvote20post)%>%
  spread(key=treatment_relabel,value=Trust) %>%
  mutate(diff= `Scientists`-`People`,
         order=rep(c(1,3,5),2)) 

LatexLabs <- data.frame(label=TeX(
  c(r"($Y_{i,Bid}(t_{Sci}, M_{i,Bid}(t_{Sci},d^*))$)",
    r"($Y_{i,Bid}(t_{Peo}, M_{i,Bid}(t_{Peo},d^*))$)",
    r"($ Y_{i,Bid}(t_{Sci}, Dem)$)",
    r"($ Y_{i,Bid}(t_{Peo}, Dem)$)",
    r"($ Y_{i,Bid}(t_{Sci}, Rep)$)",
    r"($ Y_{i,Bid}(t_{Peo}, Rep)$)",
    r"($Y_{i,Tru}(t_{Sci}, M_{i,Tru}(t_{Sci},d^*))$)",
    r"($Y_{i,Tru}(t_{Peo}, M_{i,Tru}(t_{Peo},d^*))$)",
    r"($ Y_{i,Tru}(t_{Sci}, Rep)$)",
    r"($ Y_{i,Tru}(t_{Peo}, Rep)$)",
    r"($ Y_{i,Tru}(t_{Sci}, Dem)$)",
    r"($ Y_{i,Tru}(t_{Peo}, Dem)$)"), output = "character"))

g <- ggplot(data =uw.effects.pres.2023_soils, aes(x=order,y=People*100,color=party))+
  scale_color_manual(values=c("blue","red","grey"))+
  geom_segment(data=uw.effects.pres.2023_soils%>%filter(party=="Joe Biden"),
               lineend="round",
               linejoin="round",
               aes(x=order,
                   y=People*100,
                   xend=order+0.9,
                   yend=Scientists*100,
                   colour=party),
               lty=2,
               size=1,
               position=position_dodge(0.4) )+
  geom_segment(data=uw.effects.pres.2023_soils%>%filter(party=="Donald Trump"),
               lineend="round",
               linejoin="round",
               aes(x=order+0.1,
                   y=People*100,
                   xend=order+1,
                   yend=Scientists*100,
                   colour=party),
               lty=2,
               size=1,
               position=position_dodge(0.4) )+
  geom_pointrange(size=1, aes(ymin=100*(People-1.96*se),
                              ymax=100*(People+1.96*se),
                              colour=party,
                              fill="white"),
                  position=position_dodge(0.4),
                  linewidth=1.5)+
  geom_pointrange(size=1, aes(ymin=100*(Scientists-1.96*se),
                              ymax=100*(Scientists+1.96*se),
                              y=Scientists*100,
                              x=order+1,
                              colour=party),
                  position=position_dodge(0.4),
                  linewidth=1.5)+
  scale_x_continuous(breaks=c(1.5,3.5,5.5),
                     labels = c("Co-Partisan",
                                "Cross-Partisan",
                                "Neutral"
                                
                     ))+
  scale_y_continuous(n.breaks=10,limits = c(-3,92))+
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=35),
        axis.text.y = element_text(size=30),
        axis.title = element_text(size=30))+
  annotate("text", 
           x = c(6,5,2,1,4,2.6,
                 6,5,2,1,4.2,3.4), 
           y = c(45,35,54,47,20,17,
                 25,10,32,21,5,5), 
           label  = LatexLabs$label,
           parse = TRUE, 
           size=5,
           color=c("black","black","blue","blue","red","red",
                   "black","black","red","red","blue","blue"))+
  xlab("")+ylab("Level of Trust (%)")
plot(g)
pdf("Plots/robust.swap.levels.votechoice2020.soils.first.pdf",height = 9,width = 16)
plot(g)
dev.off()

## Robustness 2: Swapping error with alternative question
CaltechClimate_Jun2023_x<- CaltechClimate_Jun2023 %>%
  select(x,weight,presvote20post,Q20,race,gender3,region,age4,educ,pid3,faminc,acsownrent)     %>%
  mutate(id=1:n(),Q10_binary=as.numeric(Q20==1),rand_ab3=x) %>%
  mutate(treat=case_when(rand_ab3%in%c(4) ~1, TRUE ~0),
         treat_dem=case_when(rand_ab3%in%c(5) ~1, TRUE ~0),
         treat_rep=case_when(rand_ab3%in%c(6) ~1, TRUE ~0),
         gender3=as.factor(gender3),
         race=as.factor(race),
         age4=as.factor(age4),
         pid3=as.factor(pid3),
         acsownrent=as.factor(acsownrent),
         region=as.factor(region),
         faminc=as.factor(faminc),
         educ=as.factor(educ),
         wave=1)%>%
  select(-weight,-Q20,-id,-x)
df <-rbind(CaltechClimate_Jun2023_x)
uw.effects.pres.2023_soils<- df %>%
  group_by(rand_ab3,presvote20post) %>%
  summarise(Trust = mean(Q10_binary==1,na.rm=T),
            Distrust = mean(Q10_binary!=1,na.rm=T),
            sd = sd(Q10_binary==1,na.rm=T),
            n  = n(),
            se = sd/sqrt(n))%>%
  filter(presvote20post%in%c(1,2)) %>%
  arrange(presvote20post,rand_ab3) %>%
  ungroup()%>%
  labelled::drop_unused_value_labels() %>% 
  mutate(treatment = str_to_title(labelled::to_factor(rand_ab3)),
         treatment = gsub("'s ","",treatment)) %>%
  add_column(order   = c(1,5,3,2,6,4,
                         1,3,5,2,4,6),
             grouping = c("Non-Partisan - Biden","Co-Partisan - Biden","Cross-Partisan - Biden",
                          "Non-Partisan - Biden","Co-Partisan - Biden","Cross-Partisan - Biden",
                          "Non-Partisan - Trump","Cross-Partisan - Trump","Co-Partisan - Trump",
                          "Non-Partisan - Trump","Cross-Partisan - Trump","Co-Partisan - Trump"
             )) %>%
  group_by(presvote20post) %>%
  mutate(treatment=fct_reorder(treatment,order,min),
         party     = labelled::to_factor(presvote20post)) %>%
  mutate( treatment_relabel=case_when(treatment=="Scientists From The Republican Party" & party=="Joe Biden"    ~ "Scientists",
                                      treatment=="Scientists From The Republican Party" & party=="Donald Trump" ~ "Scientists",
                                      treatment=="Scientists From The Democratic Party" & party=="Joe Biden" ~ "Scientists",
                                      treatment=="Scientists From The Democratic Party" & party=="Donald Trump" ~ "Scientists",
                                      treatment=="People From The Republican Party" & party=="Joe Biden"    ~ "People",
                                      treatment=="People From The Republican Party" & party=="Donald Trump" ~ "People",
                                      treatment=="People From The Democratic Party" & party=="Joe Biden" ~ "People",
                                      treatment=="People From The Democratic Party" & party=="Donald Trump" ~ "People",
                                      treatment=="People" ~"People",
                                      treatment=="Scientists" ~"Scientists",
                                      TRUE  ~treatment)
  ) %>% group_by(party) %>% mutate(se=mean(se))%>%ungroup()%>%
  select(-rand_ab3,-`Distrust`,-n,-sd,-order,-treatment,-presvote20post)%>%
  spread(key=treatment_relabel,value=Trust) %>%
  mutate(diff= `Scientists`-`People`,
         order=rep(c(1,3,5),2)) 

LatexLabs <- data.frame(label=TeX(
  c(r"($Y_{i,Bid}(t_{Sci}, M_{i,Bid}(t_{Sci},d^*))$)",
    r"($Y_{i,Bid}(t_{Peo}, M_{i,Bid}(t_{Peo},d^*))$)",
    r"($ Y_{i,Bid}(t_{Sci}, Dem)$)",
    r"($ Y_{i,Bid}(t_{Peo}, Dem)$)",
    r"($ Y_{i,Bid}(t_{Sci}, Rep)$)",
    r"($ Y_{i,Bid}(t_{Peo}, Rep)$)",
    r"($Y_{i,Tru}(t_{Sci}, M_{i,Tru}(t_{Sci},d^*))$)",
    r"($Y_{i,Tru}(t_{Peo}, M_{i,Tru}(t_{Peo},d^*))$)",
    r"($ Y_{i,Tru}(t_{Sci}, Rep)$)",
    r"($ Y_{i,Tru}(t_{Peo}, Rep)$)",
    r"($ Y_{i,Tru}(t_{Sci}, Dem)$)",
    r"($ Y_{i,Tru}(t_{Peo}, Dem)$)"), output = "character"))

g <- ggplot(data =uw.effects.pres.2023_soils, aes(x=order,y=People*100,color=party))+
  scale_color_manual(values=c("blue","red","grey"))+
  geom_segment(data=uw.effects.pres.2023_soils%>%filter(party=="Joe Biden"),
               lineend="round",
               linejoin="round",
               aes(x=order,
                   y=People*100,
                   xend=order+0.9,
                   yend=Scientists*100,
                   colour=party),
               lty=2,
               size=1,
               position=position_dodge(0.4) )+
  geom_segment(data=uw.effects.pres.2023_soils%>%filter(party=="Donald Trump"),
               lineend="round",
               linejoin="round",
               aes(x=order+0.1,
                   y=People*100,
                   xend=order+1,
                   yend=Scientists*100,
                   colour=party),
               lty=2,
               size=1,
               position=position_dodge(0.4) )+
  geom_pointrange(size=1, aes(ymin=100*(People-1.96*se),
                              ymax=100*(People+1.96*se),
                              colour=party,
                              fill="white"),
                  position=position_dodge(0.4),
                  linewidth=1.5)+
  geom_pointrange(size=1, aes(ymin=100*(Scientists-1.96*se),
                              ymax=100*(Scientists+1.96*se),
                              y=Scientists*100,
                              x=order+1,
                              colour=party),
                  position=position_dodge(0.4),
                  linewidth=1.5)+
  scale_x_continuous(breaks=c(1.5,3.5,5.5),
                     labels = c("Co-Partisan",
                                "Cross-Partisan",
                                "Neutral"
                                
                     ))+
  scale_y_continuous(n.breaks=10,limits = c(-3,92))+
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=35),
        axis.text.y = element_text(size=30),
        axis.title = element_text(size=30))+
  annotate("text", 
           x = c(6,5,2,1,4,2.6,
                 6,5,2,1,4.2,3.4), 
           y = c(45,35,54,47,20,17,
                 25,10,32,21,5,5), 
           label  = LatexLabs$label,
           parse = TRUE, 
           size=5,
           color=c("black","black","blue","blue","red","red",
                   "black","black","red","red","blue","blue"))+
  xlab("")+ylab("Level of Trust (%)")
plot(g)
pdf("Plots/robust.swap.levels.votechoice2020.soils.repeat.pdf",height = 9,width = 16)
plot(g)
dev.off()

## Study 2 – All voters: Treatment and latent effects (pooled) ----
## Compute treatment and latent effects among all voters in the pooled data (June/Dec 2023).
n_obs_dem <- summary(lm(Q10_binary ~ treat_dem,df%>%filter(presvote20post%in%c(1,2),
                                                           rand_ab3%in%c(2,5)) ))$df[2] 
dem_effect <- data.frame(summary(lm(Q10_binary ~ treat_dem,df%>%filter(presvote20post%in%c(1,2),
                                                                       rand_ab3%in%c(2,5)) ))$coefficients) %>%
  mutate(treatment="Democrat",presvote20post="All",n_obs=n_obs_dem) %>%filter(!rownames(.)%in%"(Intercept)") 

n_obs_rep <- summary(lm(Q10_binary ~ treat_rep,df%>%filter(presvote20post%in%c(1,2),
                                                           rand_ab3%in%c(3,6)) ))$df[2]
rep_effect <- data.frame(summary(lm(Q10_binary ~ treat_rep,df%>%
                                      filter(presvote20post%in%c(1,2),
                                             rand_ab3%in%c(3,6)) ))$coefficients) %>%
  mutate(treatment="Republican",presvote20post="All",n_obs=n_obs_rep) %>%filter(!rownames(.)%in%"(Intercept)")

n_obs_neutral <- summary(lm(Q10_binary ~ treat,df%>%filter(presvote20post%in%c(1,2),
                                                           rand_ab3%in%c(1,4)) ))$df[2]
neutral_effect <- data.frame(summary(lm(Q10_binary ~ treat,df%>%filter(presvote20post%in%c(1,2),
                                                                       rand_ab3%in%c(1,4)) ))$coefficients) %>%
  mutate(treatment="Neutral",presvote20post="All",n_obs=n_obs_neutral) %>%filter(!rownames(.)%in%"(Intercept)") 

df.plot <- rbind(dem_effect,rep_effect,neutral_effect)
dem.diff <-  df.plot$Estimate[df.plot$treatment=="Neutral"]-df.plot$Estimate[df.plot$treatment=="Democrat"]
w<-n_obs_dem/(n_obs_neutral+n_obs_dem)
dem.se <- sqrt(df.plot$Std..Error[df.plot$treatment=="Neutral"]^2*(w)+df.plot$Std..Error[df.plot$treatment=="Democrat"]^2*(1-w))
w<-n_obs_rep/(n_obs_neutral+n_obs_rep)
rep.diff <-  df.plot$Estimate[df.plot$treatment=="Neutral"]-df.plot$Estimate[df.plot$treatment=="Republican"]
rep.se <- sqrt(df.plot$Std..Error[df.plot$treatment=="Neutral"]^2*(w)+df.plot$Std..Error[df.plot$treatment=="Republican"]^2*(1-w))

custom.pallette <- c(brewer.pal(3,"Blues")[1:2],brewer.pal(3,"Reds")[1:2])

df.plot <- rbind(dem_effect,rep_effect,neutral_effect) %>%
  select(Estimate,Std..Error)%>%
  mutate(Estimate = Estimate,
         se =Std..Error,
         Effect = c("Democratic","Republican","treat")) %>%
  select(-Std..Error) %>%
  add_row(Estimate = dem.diff,se=dem.se,Effect="Democratic") %>%
  add_row(Estimate = rep.diff,se=rep.se,Effect="Republican") %>%
  #filter(!Effect%in%c("treat")) %>%
  mutate(Type =c("Partisan","Partisan","Neutral","Difference","Difference")) %>%
  mutate(Type=paste(Effect,Type,sep=" "))

## Pooled effects plot (All voters)
g <- ggplot(df.plot%>%filter(!Effect%in%c("treat")), 
            aes(y = Estimate, 
                x = Effect, 
                fill = Type,
                color=Type,
                label=Effect)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual( values=custom.pallette)+
  scale_color_manual( values=custom.pallette)+
  ylab("Treatment Effect")+
  geom_hline(yintercept = 0, color = "black")+
  theme_bw()+
  geom_text(aes(label=c("Latent","Latent","Direct",""),
                y=c(0.05,0.05,0.02,0.02),
                x=c(0.8,2.2,0.8,1.2)),vjust=2,size=5)+
  geom_text(aes(x=1.5,y=0.13,label="Total Treatment Effect"),size=5,color="black")+
  scale_y_continuous(limits = c(-0.06,0.3),n.breaks = 10)+
  geom_hline(yintercept = df.plot$Estimate[3], linetype = "dashed", color = "black")+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

pdf("Plots/study2_treatment_effects_soils.pdf")
plot(g)
dev.off()

g <- ggplot(df.plot%>%filter(!Effect%in%c("treat"),Type%in%c("Republican Difference","Democratic Difference")), aes(y = Estimate, 
                                                                                                                    x = Effect, 
                                                                                                                    fill = Type,
                                                                                                                    color=Type,
                                                                                                                    label=Effect)) + 
  geom_bar(stat = "identity") +
  geom_pointrange(data=df.plot%>%filter(Type%in%c("Republican Difference","Democratic Difference")),aes(ymin = Estimate - 1.96*se, 
                                                                                                        ymax = Estimate + 1.96*se,
                                                                                                        x=Effect),fatten=5,
                  alpha = 0.5,fill="white",color=c("blue","red"),linewidth = 1.5)+ 
  scale_fill_manual( values=custom.pallette[c(1,3)])+
  scale_color_manual( values=custom.pallette[c(2,4)])+
  geom_text(aes(label=c("Latent","Latent"),
                y=c(0.05,0.1),
                x=c(0.8,2.2)),vjust=2,size=4.5)+
  geom_hline(yintercept = 0, color = "black")+
  theme_bw()+
  ylab("Latent Effect")+
  scale_y_continuous(limits = c(-0.06,0.3),n.breaks = 10)+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

pdf("Plots/study2_latent_effects_soils.pdf")
plot(g)
dev.off()

## Figure 7a/b – Study 2: Treatment and latent effects for Biden voters ----
## Estimate and plot treatment and latent effects among Biden voters in the pooled data.
print("Figure 7a")
n_obs_dem <- summary(lm(Q10_binary ~ treat_dem,df%>%filter(presvote20post%in%c(1),
                                                           rand_ab3%in%c(2,5)) ))$df[2] 
dem_effect <- data.frame(summary(lm(Q10_binary ~ treat_dem,df%>%filter(presvote20post%in%c(1),
                                                                       rand_ab3%in%c(2,5)) ))$coefficients) %>%
  mutate(treatment="Democrat",presvote20post="All",n_obs=n_obs_dem) %>%filter(!rownames(.)%in%"(Intercept)") 

n_obs_rep <- summary(lm(Q10_binary ~ treat_rep,df%>%filter(presvote20post%in%c(1),
                                                           rand_ab3%in%c(3,6)) ))$df[2]
rep_effect <- data.frame(summary(lm(Q10_binary ~ treat_rep,df%>%
                                      filter(presvote20post%in%c(1),
                                             rand_ab3%in%c(3,6)) ))$coefficients) %>%
  mutate(treatment="Republican",presvote20post="All",n_obs=n_obs_rep) %>%filter(!rownames(.)%in%"(Intercept)")

n_obs_neutral <- summary(lm(Q10_binary ~ treat,df%>%filter(presvote20post%in%c(1),
                                                           rand_ab3%in%c(1,4)) ))$df[2]
neutral_effect <- data.frame(summary(lm(Q10_binary ~ treat,df%>%filter(presvote20post%in%c(1),
                                                                       rand_ab3%in%c(1,4)) ))$coefficients) %>%
  mutate(treatment="Neutral",presvote20post="All",n_obs=n_obs_neutral) %>%filter(!rownames(.)%in%"(Intercept)") 

df.plot <- rbind(dem_effect,rep_effect,neutral_effect) 
dem.diff <-  df.plot$Estimate[df.plot$treatment=="Neutral"]-df.plot$Estimate[df.plot$treatment=="Democrat"]
w<-n_obs_dem/(n_obs_neutral+n_obs_dem)
dem.se <- sqrt(df.plot$Std..Error[df.plot$treatment=="Neutral"]^2*(w)+df.plot$Std..Error[df.plot$treatment=="Democrat"]^2*(1-w))
dem.t.test.biden.pooled <- dem.diff/dem.se
w<-n_obs_rep/(n_obs_neutral+n_obs_rep)
rep.diff <-  df.plot$Estimate[df.plot$treatment=="Neutral"]-df.plot$Estimate[df.plot$treatment=="Republican"]
rep.t.test.biden.pooled  <- rep.diff/rep.se

custom.pallette <- c(brewer.pal(3,"Blues")[1:2],brewer.pal(3,"Reds")[1:2])

df.plot <- rbind(dem_effect,rep_effect,neutral_effect) %>%
  select(Estimate,Std..Error)%>%
  mutate(Estimate = Estimate,
         se =Std..Error,
         Effect = c("Democratic","Republican","treat")) %>%
  select(-Std..Error) %>%
  add_row(Estimate = dem.diff,se=dem.se,Effect="Democratic") %>%
  add_row(Estimate = rep.diff,se=rep.se,Effect="Republican") %>%
  #filter(!Effect%in%c("treat")) %>%
  mutate(Type =c("Partisan","Partisan","Neutral","Difference","Difference")) %>%
  mutate(Type=paste(Effect,Type,sep=" "))

g <- ggplot(df.plot%>%filter(!Effect%in%c("treat")), aes(y = Estimate, 
                                                         x = Effect, 
                                                         fill = Type,
                                                         color=Type,
                                                         label=Effect)) + 
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "black")+
  scale_fill_manual( values=custom.pallette)+
  scale_color_manual( values=custom.pallette)+
  ylab("Treatment Effect")+
  theme_bw()+
  geom_text(aes(label=c("Latent","Latent","Direct","Direct"),
                y=c(0.1,0.1,0.02,0.02),
                x=c(0.8,2.2,0.8,2.2)),vjust=2,size=5)+
  geom_text(aes(x=1.5,y=0.13,label="Total Treatment Effect"),size=5,color="black")+
  geom_hline(yintercept = df.plot$Estimate[3], linetype = "dashed", color = "black")+
  scale_y_continuous(limits = c(-0.06,0.3),n.breaks = 10)+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

pdf("Plots/biden.study2_treatment_effects_soils.pdf")
plot(g)
dev.off()

print("Figure 7b")
g <- ggplot(df.plot%>%filter(!Effect%in%c("treat"),Type%in%c("Republican Difference","Democratic Difference")), aes(y = Estimate, 
                                                                                                                    x = Effect, 
                                                                                                                    fill = Type,
                                                                                                                    color=Type,
                                                                                                                    label=Effect)) + 
  geom_bar(stat = "identity") +
  geom_pointrange(data=df.plot%>%filter(Type%in%c("Republican Difference","Democratic Difference")),aes(ymin = Estimate - 1.96*se, 
                                                                                                        ymax = Estimate + 1.96*se,
                                                                                                        x=Effect),fatten=5,
                  alpha = 0.5,fill="white",color=c("blue","red"),linewidth = 1.5)+ 
  scale_fill_manual( values=custom.pallette[c(1,3)])+
  scale_color_manual( values=custom.pallette[c(2,4)])+
  geom_text(aes(label=c("Latent","Latent"),
                y=c(0.05,0.1),
                x=c(0.8,2.2)),vjust=2,size=4.5)+
  geom_hline(yintercept = 0, color = "black")+
  theme_bw()+
  ylab("Latent Effect")+
  scale_y_continuous(limits = c(-0.06,0.3),n.breaks = 10)+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))
pdf("Plots/biden.study2_latent_effects_soils.pdf")
plot(g)
dev.off()

## Figure 7c/d – Study 2: Treatment and latent effects for Trump voters ----
## Estimate and plot treatment and latent effects among Trump voters in the pooled data.
print("Figure 7c")
n_obs_dem <- summary(lm(Q10_binary ~ treat_dem,df%>%filter(presvote20post%in%c(2),
                                                           rand_ab3%in%c(2,5)) ))$df[2] 
dem_effect <- data.frame(summary(lm(Q10_binary ~ treat_dem,df%>%filter(presvote20post%in%c(2),
                                                                       rand_ab3%in%c(2,5)) ))$coefficients) %>%
  mutate(treatment="Democrat",presvote20post="All",n_obs=n_obs_dem) %>%filter(!rownames(.)%in%"(Intercept)") 

n_obs_rep <- summary(lm(Q10_binary ~ treat_rep,df%>%filter(presvote20post%in%c(2),
                                                           rand_ab3%in%c(3,6)) ))$df[2]
rep_effect <- data.frame(summary(lm(Q10_binary ~ treat_rep,df%>%
                                      filter(presvote20post%in%c(2),
                                             rand_ab3%in%c(3,6)) ))$coefficients) %>%
  mutate(treatment="Republican",presvote20post="All",n_obs=n_obs_rep) %>%filter(!rownames(.)%in%"(Intercept)")

n_obs_neutral <- summary(lm(Q10_binary ~ treat,df%>%filter(presvote20post%in%c(2),
                                                           rand_ab3%in%c(1,4)) ))$df[2]
neutral_effect <- data.frame(summary(lm(Q10_binary ~ treat,df%>%filter(presvote20post%in%c(2),
                                                                       rand_ab3%in%c(1,4)) ))$coefficients) %>%
  mutate(treatment="Neutral",presvote20post="All",n_obs=n_obs_neutral) %>%filter(!rownames(.)%in%"(Intercept)") 

df.plot <- rbind(dem_effect,rep_effect,neutral_effect) 
dem.diff <-  df.plot$Estimate[df.plot$treatment=="Neutral"]-df.plot$Estimate[df.plot$treatment=="Democrat"]
w<-n_obs_dem/(n_obs_neutral+n_obs_dem)
dem.se <- sqrt(df.plot$Std..Error[df.plot$treatment=="Neutral"]^2*(w)+df.plot$Std..Error[df.plot$treatment=="Democrat"]^2*(1-w))
dem.t.test.trump.pooled <- dem.diff/dem.se
w<-n_obs_rep/(n_obs_neutral+n_obs_rep)
rep.diff <-  df.plot$Estimate[df.plot$treatment=="Neutral"]-df.plot$Estimate[df.plot$treatment=="Republican"]
rep.se <- sqrt(df.plot$Std..Error[df.plot$treatment=="Neutral"]^2*(w)+df.plot$Std..Error[df.plot$treatment=="Republican"]^2*(1-w))
rep.t.test.trump.pooled  <- rep.diff/rep.se

custom.pallette <- c(brewer.pal(3,"Blues")[1:2],brewer.pal(3,"Reds")[1:2])

df.plot <- rbind(dem_effect,rep_effect,neutral_effect) %>%
  select(Estimate,Std..Error)%>%
  mutate(Estimate = Estimate,
         se =Std..Error,
         Effect = c("Democratic","Republican","treat")) %>%
  select(-Std..Error) %>%
  add_row(Estimate = dem.diff,se=dem.se,Effect="Democratic") %>%
  add_row(Estimate = rep.diff,se=rep.se,Effect="Republican") %>%
  #filter(!Effect%in%c("treat")) %>%
  mutate(Type =c("Partisan","Partisan","Neutral","Difference","Difference")) %>%
  mutate(Type=paste(Effect,Type,sep=" "))

g <- ggplot(df.plot%>%filter(!Effect%in%c("treat")), aes(y = Estimate+c(0,0,df.plot$Estimate[1:2]), 
                                                         x = Effect, 
                                                         fill = Type,
                                                         color=Type,
                                                         label=Effect)) + 
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "black") +
  scale_fill_manual( values=custom.pallette) +
  scale_color_manual( values=custom.pallette) +
  scale_y_continuous(limits = c(-0.06,0.3),n.breaks = 10)+
  ylab("Treatment Effect")+
  theme_bw()+
  geom_text(aes(label=c("Latent","Latent","Direct","Direct"),
                y=c(0.03,0.03,0.002,0.002),
                x=c(0.8,2.2,0.8,2.2)),vjust=2,size=5)+
  geom_text(aes(x=1.5,y=0.13,label="Total Treatment Effect"),size=5,color="black")+
  geom_hline(yintercept = df.plot$Estimate[3], linetype = "dashed", color = "black")+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

pdf("Plots/trump.study2_treatment_effects_soils.pdf")
plot(g)
dev.off()

print("Figure 7d")
g <- ggplot(df.plot%>%filter(!Effect%in%c("treat"),Type%in%c("Republican Difference","Democratic Difference")), aes(y = Estimate, 
                                                                                                                    x = Effect, 
                                                                                                                    fill = Type,
                                                                                                                    color=Type,
                                                                                                                    label=Effect)) + 
  geom_bar(stat = "identity") +
  geom_pointrange(data=df.plot%>%filter(Type%in%c("Republican Difference","Democratic Difference")),aes(ymin = Estimate - 1.96*se, 
                                                                                                        ymax = Estimate + 1.96*se,
                                                                                                        x=Effect),fatten=5,
                  alpha = 0.5,fill="white",color=c("blue","red"),linewidth = 1.5)+ 
  scale_fill_manual( values=custom.pallette[c(1,3)])+
  scale_color_manual( values=custom.pallette[c(2,4)])+
  geom_text(aes(label=c("Latent","Latent"),
                y=c(0.05,0.1),
                x=c(0.8,2.2)),vjust=2,size=4.5)+
  geom_hline(yintercept = 0, color = "black")+
  theme_bw()+
  ylab("Latent Effect")+
  scale_y_continuous(limits = c(-0.06,0.3),n.breaks = 10)+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))
pdf("Plots/trump.study2_latent_effects_soils.pdf")
plot(g)
dev.off()

