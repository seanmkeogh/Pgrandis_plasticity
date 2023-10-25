{
  # Keogh et al. 2023 -
  # Sibling freshwater mussels in lentic and lotic gardens 
  # reveal phenotypic plasticity as mechanism of ecophenotypy (Unionidae: Pyganodon)

library(cowplot)
library(viridis)
library(readxl)
library(tidyverse)
library(car)
library(scales)
library(multcomp)

setwd('~/Box/Myprojects/Pgrandis/')
path_figs<-('figures/')

# Read data in

all <- read_xlsx('data/pgran_morph.xlsx',
                   sheet='all',na=c('NA','','No Data','Unknown'))
warnings()
}


#######
### START Growth trajectories scaling
#######
{
### Siblings at different sites
# Width
  
  exp<-all %>% filter(Gene_pool == 'Sibling',
                      !Site == 'Zoo',
                      !Site == 'exhibit')
  exp.zoo<-all %>% filter(Gene_pool == 'Sibling',
                      !Site == 'exhibit')
  
a<-ggplot(exp,color='black')+
  scale_fill_manual(values = c('pink','red','maroon','navy'))+
  geom_point(aes((Length),(Width),fill=Site),size=1.5,shape=21,alpha=0.7)+ 
  geom_smooth(aes((Length),(Width), color=Site), method=lm, se=FALSE)+
  scale_color_manual(values = c('pink','red','maroon','navy'))+theme_classic()+
  labs(x="Length (mm)", y="Width (mm)")+
    theme(legend.position='none')
# Height
b<-ggplot(exp,color='black')+
  scale_fill_manual(values = c('pink','red','maroon','navy'))+
  geom_point(aes((Length),(Height),fill=Site),size=1.5,shape=21,alpha=0.7)+ 
  geom_smooth(aes((Length),(Height), color=Site), method=lm, se=FALSE)+
  scale_color_manual(values = c('pink','red','maroon','navy'))+theme_classic()+
  labs(x="Length (mm)", y="Height (mm)")+
  theme(legend.position='none')

### Siblings at different habitats
# Width
c<-ggplot(exp,color='black')+
  scale_fill_manual(values = c('pink','red','maroon','navy'))+
  geom_point(aes((Length),(Width),fill=Site),size=1.5,shape=21,alpha=0.7)+ 
  geom_smooth(aes((Length),(Width), color=Habitat), method=lm, se=FALSE)+
  scale_color_manual(values = c('navy','red'))+theme_classic()+
  labs(x="Length (mm)", y="Width (mm)")+
  theme(legend.position='none')
# Height
d<-ggplot(exp,color='black')+
  scale_fill_manual(values = c('pink','red','maroon','navy'))+
  geom_point(aes((Length),(Height),fill=Site),size=1.5,shape=21,alpha=0.7)+ 
  geom_smooth(aes((Length),(Height), color=Habitat), method=lm, se=FALSE)+
  scale_color_manual(values = c('navy','red'))+theme_classic()+
  labs(x="Length (mm)", y="Height (mm)")+
  theme(legend.position='none')

### Siblings and wild mussels in different habitats
# Width

exp.wild<- all %>% filter(Habitat == 'lentic' | Habitat == 'lotic',
                          !Gene_pool == 'Mother',
                          !Site == 'Zoo')

exp.wild<-exp.wild %>% mutate(Habitat_condition = paste(Habitat, Gene_pool, sep = '_'))

e<-ggplot(exp.wild,color='black')+
  scale_fill_manual(values = c('navy','navy','red','red'))+
  geom_point(aes((Length),(Width),fill=Habitat_condition,shape=Habitat_condition),size=0.5,alpha=0.4)+
  scale_shape_manual(values = c(21,22,21,22))+
  geom_smooth(aes((Length),(Width), color=Habitat_condition, linetype=Habitat_condition),
              method=lm, se=FALSE)+
  scale_linetype_manual(values=c('solid','longdash','solid','longdash'))+
  scale_color_manual(values = c('navy','navy','red','red'))+theme_classic()+
  labs(x="Length (mm)", y="Width (mm)")+
  theme(legend.position='none')
# Height
f<-ggplot(exp.wild,color='black')+
  scale_fill_manual(values = c('navy','navy','red','red'))+
  geom_point(aes((Length),(Height),fill=Habitat_condition,shape=Habitat_condition),size=0.5,alpha=0.4)+
  scale_shape_manual(values = c(21,22,21,22))+
  geom_smooth(aes((Length),(Height), color=Habitat_condition, linetype=Habitat_condition),
              method=lm, se=FALSE)+
  scale_linetype_manual(values=c('solid','longdash','solid','longdash'))+
  scale_color_manual(values = c('navy','navy','red','red'))+theme_classic()+
  labs(x="Length (mm)", y="Height (mm)")+
  theme(legend.position='none')


ggsave(paste0(path_figs,"growth_traj_F.png"),f,
  width=4, height=3.25, units='in', dpi=400)


a.zoo<-ggplot()+
  scale_fill_manual(values = c('pink','red','maroon','navy','purple'))+
  geom_point(data=exp.zoo,aes((Length),(Width),fill=Site),size=2,shape=21,alpha=0.7,color='black')+ 
  geom_smooth(data=exp.zoo,aes((Length),(Width), color=Site), method=lm, se=FALSE)+
  scale_color_manual(values = c('pink','red','maroon','navy','purple'))+
  #geom_point(data=mother,aes(Length,Width), size=3, shape=8,color='black')+
  theme_classic()+
  labs(x="Length (mm)", y="Width (mm)")+
  theme(legend.position='none')
# Height
b.zoo<-ggplot(exp.zoo,color='black')+
  scale_fill_manual(values = c('pink','red','maroon','navy','purple'))+
  geom_point(aes((Length),(Height),fill=Site),size=2,shape=21,alpha=0.7)+ 
  geom_smooth(aes((Length),(Height), color=Site), method=lm, se=FALSE)+
  scale_color_manual(values = c('pink','red','maroon','navy','purple'))+
  theme_classic()+
  labs(x="Length (mm)", y="Height (mm)")+
  theme(legend.position='none')
growth_grid<-plot_grid(a.zoo,b.zoo,
                       labels='auto',nrow=1,ncol = 2)
ggsave(paste0(path_figs,"growth_traj_zoo.png"),growth_grid,dpi=400)

}
#######
### END Growth trajectories scaling
#######


#####
# START STATISTICAL ANALYSIS
#####
{

  exp<-all %>% filter(Gene_pool == 'Sibling',
                      !Site == 'Zoo',
                      !Site == 'exhibit')
  exp.zoo<-all %>% filter(Gene_pool == 'Sibling',
                      !Site == 'exhibit')
  
#### Q1: Do sibling mussels reared at different sites have different shell morphology?
# W/O zoo animals
q1_model_h <- lm(Height ~ Location + Length + Location:Length, data = exp)
summary(q1_model_h)
Anova(q1_model_h, type="II")
q1_model_w <- lm(Width ~ Location + Length + Location:Length, data = exp)
summary(q1_model_w)
Anova(q1_model_w, type="II")
# With zoo animals
q1_model_h_zoo <- lm(Height ~ Location + Length + Location:Length, data = exp.zoo)
summary(q1_model_h_zoo)
Anova(q1_model_h_zoo, type="II")
q1_model_w_zoo <- lm(Width ~ Location + Length + Location:Length, data = exp.zoo)
summary(q1_model_w_zoo)
Anova(q1_model_w_zoo, type="II")

#### Q2: Do sibling mussels reared in lotic versus lentic environments have different shell morphology?
q2_model_h <- lm(Height ~ Habitat + Length + Habitat:Length, data = exp)
summary(q2_model_h)
Anova(q2_model_h, type="II")
q2_model_w <- lm(Width ~ Habitat + Length + Habitat:Length, data = exp)
summary(q2_model_w)
Anova(q2_model_w, type="II")

#### Q3: Do sibling mussels reared at different sites but same habitat (lentic vs. lotic) have the same shell morphology?
## Subset lotic
# height
nat.lotic<- exp %>% filter(Habitat == 'lotic')
nat.lotic$Location<-as.factor(nat.lotic$Location)
lotic_model_h <- lm(Height ~ Location + Length + Location:Length, data = nat.lotic)
Anova(lotic_model_h, type='II')
# don't need the interaction term for the Tukey's contrast
lotic_model_h <- lm(Height ~ Location + Length, data = nat.lotic)
q3_lotic_model_h <- glht(lotic_model_h, linfct = mcp(Location = "Tukey"))
summary(q3_lotic_model_h)
# width
lotic_model_w <- lm(Width ~ Location + Length + Location:Length, data = nat.lotic)
Anova(lotic_model_w, type='II')
# don't need the interaction term for the Tukey's contrast
lotic_model_w <- lm(Width ~ Location + Length, data = nat.lotic)
q3_lotic_model_w <- glht(lotic_model_w, linfct = mcp(Location = "Tukey"))
summary(q3_lotic_model_w)

# Subset lentic - not totally comparable because of unique zoo environment
# nat.lentic<- all %>% filter(Gene_pool == 'Sibling', Habitat == 'lentic)
# nat.lentic$Location<-as.factor(nat.lentic$Location)
# lentic_model <- aov(Height + Width ~ Location + Length, data = nat.lentic)
# q3_lentic_model <- glht(lentic_model, linfct = mcp(Location = "Tukey"))
# summary(q3_lentic_model)

#### Q4: Do experimental mussels morphology differ from wild P. grandis reared at the same site?
## Subset Shields Lake
# height
shields.all<- all %>% filter(Site == 'Shields')
q4_model_shields_h<- lm(Height ~ Gene_pool + Length + Gene_pool:Length, data = shields.all)
summary(q4_model_shields_h)
Anova(q4_model_shields_h, type="II")
# width
q4_model_shields_w<- lm(Width ~ Gene_pool + Length + Gene_pool:Length, data = shields.all)
summary(q4_model_shields_w)
Anova(q4_model_shields_w, type="II")

# Subset Maple Creek
maple.all<- all %>% filter(Site == 'Maple')
table(maple.all$Location)
# height
q4_model_maple_h<- lm(Height ~ Gene_pool + Length + Gene_pool:Length, data = maple.all)
summary(q4_model_maple_h)
Anova(q4_model_maple_h, type="II")
# width
q4_model_maple_w<- lm(Width ~ Gene_pool + Length + Gene_pool:Length, data = maple.all)
summary(q4_model_maple_w)
Anova(q4_model_maple_w, type="II")

## Subset Chub Creek
chub.all<- all %>% filter(Site == 'Chub', !Gene_pool == 'Mother')
# remove mother because point of test is to test effect of non-family members
table(chub.all$Location)
# height
q4_model_chub_h<- lm(Height ~ Gene_pool + Length + Gene_pool:Length, data = chub.all)
summary(q4_model_chub_h)
Anova(q4_model_chub_h, type="II")
# width
q4_model_chub_w<- lm(Width ~ Gene_pool + Length + Gene_pool:Length, data = chub.all)
summary(q4_model_chub_w)
Anova(q4_model_chub_w, type="II")

## Subset Belle Creek
belle.all<- all %>% filter(Site == 'Belle')
table(belle.all$Location)
# height
q4_model_belle_h<- lm(Height ~ Gene_pool + Length + Gene_pool:Length, data = belle.all)
summary(q4_model_belle_h)
Anova(q4_model_belle_h, type="II")
# width
q4_model_belle_w<- lm(Width ~ Gene_pool + Length + Gene_pool:Length, data = belle.all)
summary(q4_model_belle_w)
Anova(q4_model_belle_w, type="II")

#Significant, so lets take a look at raw data
# ggplot(belle.all,aes(Length,Height, color=Gene_pool))+
#   geom_point()+
#   geom_smooth(aes((Length),(Height), color=Gene_pool), method=lm, se=FALSE)+theme_classic()+
#   labs(x="Length (mm)", y="Height (mm)")+
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=13),
#         legend.position='none')

#### Q5: Do experimental mussels morphology differ from wild P. grandis reared in the same habitat?
## Subset lotic
all.lotic<- exp.wild %>% filter(Habitat == 'lotic',
                                      !Gene_pool == 'lotic_mother')
table(all.lotic$Habitat_condition)
# height
q5_lotic_model_h <- lm(Height ~ Habitat_condition + Length + Habitat_condition:Length, data = all.lotic)
summary(q5_lotic_model_h)
Anova(q5_lotic_model_h, type="II")
# width
q5_lotic_model_w <- lm(Width ~ Habitat_condition + Length + Habitat_condition:Length, data = all.lotic)
summary(q5_lotic_model_w)
Anova(q5_lotic_model_w, type="II")

#### Q6: Do full sibship families have different morphologies? 
sibs<-exp %>% drop_na(Sibship)
table(sibs$Sibship) 

# height
q6_lotic_model_h <- lm(Height ~ Sibship + Length + Habitat, data = sibs)
summary(q6_lotic_model_h)
Anova(q6_lotic_model_h, type="II")
# width
q6_lotic_model_w <- lm(Width ~ Sibship + Length + Habitat, data = sibs)
summary(q6_lotic_model_w)
Anova(q6_lotic_model_w, type="II")

}
#####
# END STATISTICAL ANALYSIS
#####


#######
## SIBSHIP PLOTS
#######

lakes<-exp %>% filter(Habitat=='lentic')
streams<-exp %>% filter(Habitat=='lotic')
sibship<-exp %>% drop_na(Sibship)

lakes$variable<-lakes$Habitat
streams$variable<-streams$Habitat
sibship$variable<-sibship$Sibship

sibship_data<-rbind.data.frame(lakes,streams,sibship)

ggplot()+
  geom_point(data=sibship,aes(Length,Width,fill=Habitat,color=Habitat,shape=Sibship),size=5,alpha=1)+ 
  geom_smooth(data=streams,aes(Length,Width),method='lm',se=FALSE,linetype='solid',linewidth=1.25,colour='red')+
  geom_smooth(data=lakes,aes(Length,Width),method='lm',se=FALSE,linetype='solid',linewidth=1.25,colour='navy')+
  scale_shape_manual(values = c(15,19,17,23,8,13,0,1))+
  scale_color_manual(values = c('navy','red'))+
  scale_fill_manual(values = c('navy','red'))+
  theme_classic()+
  labs(x="Length (mm)", y="Width (mm)")+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        legend.position="none")

ggplot()+
  geom_point(data=sibship,aes(Length,Height,fill=Habitat,color=Habitat,shape=Sibship),size=5,alpha=1)+ 
  geom_smooth(data=streams,aes(Length,Height),method='lm',se=FALSE,linetype='solid',linewidth=1.25,colour='red')+
  geom_smooth(data=lakes,aes(Length,Height),method='lm',se=FALSE,linetype='solid',linewidth=1.25,colour='navy')+
  scale_shape_manual(values = c(15,19,17,23,8,13,0,1))+
  scale_color_manual(values = c('navy','red'))+
  scale_fill_manual(values = c('navy','red'))+
  theme_classic()+
  labs(x="Length (mm)", y="Height (mm)")+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        legend.position="none")


#######################################################################
## END of code for paper
#######################################################################


######
## PLOTS FOR TALKS
######
exp$Site
ggplot(exp %>% filter(Site %in% c('Belle', 'Chub', 'Maple','Shields')),color='black')+
  scale_fill_manual(values = c('pink','red','maroon','navy'))+
  geom_point(aes((Length),(Width),fill=Site),size=1.5,shape=21,alpha=0.7)+ 
  geom_smooth(aes((Length),(Width), color=Site), method=lm, se=FALSE)+
  scale_color_manual(values = c('pink','red','maroon','navy'))+theme_classic()+
  labs(x="Length (mm)", y="Width (mm)")+
  xlim(38,91)+
  ylim(10,37)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        legend.position='none')
# Height
ggplot(exp %>% filter(Site %in% c('Belle', 'Chub', 'Maple','Shields')),color='black')+
  scale_fill_manual(values = c('pink','red','maroon','navy'))+
  geom_point(aes((Length),(Height),fill=Site),size=1.5,shape=21,alpha=0.7)+ 
  geom_smooth(aes((Length),(Height), color=Site), method=lm, se=FALSE)+
  scale_color_manual(values = c('pink','red','maroon','navy'))+theme_classic()+
  labs(x="Length (mm)", y="Height (mm)")+
  xlim(38,91)+
  ylim(19,49)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        legend.position='none')

### Siblings at different habitats
# Width
c<-ggplot(exp,color='black')+
  scale_fill_manual(values = c('pink','red','maroon','navy'))+
  geom_point(aes((Length),(Width),fill=Site),size=1.5,shape=21,alpha=0.7)+ 
  geom_smooth(aes((Length),(Width), color=Habitat), method=lm, se=FALSE)+
  scale_color_manual(values = c('navy','red'))+theme_classic()+
  labs(x="Length (mm)", y="Width (mm)")+
  theme(legend.position='none')
# Height
d<-ggplot(exp,color='black')+
  scale_fill_manual(values = c('pink','red','maroon','navy'))+
  geom_point(aes((Length),(Height),fill=Site),size=1.5,shape=21,alpha=0.7)+ 
  geom_smooth(aes((Length),(Height), color=Habitat), method=lm, se=FALSE)+
  scale_color_manual(values = c('navy','red'))+theme_classic()+
  labs(x="Length (mm)", y="Height (mm)")+
  theme(legend.position='none')
######
## END PLOTS FOR TALKS
######




# ###########################################################################
# ## OLD STATS - type III
# ###########################################################################
# #### Q1: Do sibling mussels reared at different sites have different shell morphology?
# # W/O zoo animals
# q1_model_h <- aov(Height  ~ Location + Length, data = exp)
# q1_model_h <- lm(Height  ~ Location + Length, data = exp)
# q1_model_h <- aov(Height  ~ Location + Length + Location*Length, data = exp)
# summary(q1_model_h)
# Anova(q1_model_h, type="II")
# q1_model_w <- aov(Width ~ Location + Length, data = exp)
# summary(q1_model_w)
# Anova(q1_model_w, type="III")
# # With zoo animals
# q1_model_h_zoo <- aov(Height  ~ Location + Length, data = exp.zoo)
# summary(q1_model_h_zoo)
# Anova(q1_model_h_zoo, type="III")
# q1_model_w_zoo <- aov(Width ~ Location + Length, data = exp.zoo)
# summary(q1_model_w_zoo)
# Anova(q1_model_w_zoo, type="III")
# 
# #### Q2: Do sibling mussels reared in lotic versus lentic environments have different shell morphology?
# q2_model_h <- aov(Height ~ Habitat + Length, data = exp)
# summary(q2_model_h)
# Anova(q2_model_h, type="III")
# q2_model_w <- aov(Width ~ Habitat + Length, data = exp)
# summary(q2_model_w)
# Anova(q2_model_w, type="III")
# 
# #### Q3: Do sibling mussels reared at different sites but same habitat (lentic vs. lotic) have the same shell morphology?
# ## Subset lotic
# # height
# nat.lotic<- exp %>% filter(Habitat == 'lotic')
# nat.lotic$Location<-as.factor(nat.lotic$Location)
# lotic_model_h <- aov(Height ~ Location + Length, data = nat.lotic)
# Anova(lotic_model_h, type='III')
# q3_lotic_model_h <- glht(lotic_model_h, linfct = mcp(Location = "Tukey"))
# summary(q3_lotic_model_h)
# # width
# lotic_model_w <- aov(Width ~ Location + Length, data = nat.lotic)
# Anova(lotic_model_w, type='III')
# q3_lotic_model_w <- glht(lotic_model_w, linfct = mcp(Location = "Tukey"))
# summary(q3_lotic_model_w)
# 
# # Subset lentic - not totally comparable because of unique zoo environment
# # nat.lentic<- all %>% filter(Gene_pool == 'Sibling', Habitat == 'lentic)
# # nat.lentic$Location<-as.factor(nat.lentic$Location)
# # lentic_model <- aov(Height + Width ~ Location + Length, data = nat.lentic)
# # q3_lentic_model <- glht(lentic_model, linfct = mcp(Location = "Tukey"))
# # summary(q3_lentic_model)
# 
# #### Q4: Do experimental mussels morphology differ from wild P. grandis reared at the same site?
# ## Subset Shields Lake
# # height
# shields.all<- all %>% filter(Site == 'Shields')
# q4_model_shields_h<- aov(Height ~ Gene_pool + Length, data = shields.all)
# summary(q4_model_shields_h)
# Anova(q4_model_shields_h, type="III")
# # width
# q4_model_shields_w<- aov(Width ~ Gene_pool + Length, data = shields.all)
# summary(q4_model_shields_w)
# Anova(q4_model_shields_w, type="III")
# 
# # Subset Maple Creek
# maple.all<- all %>% filter(Site == 'Maple')
# table(maple.all$Location)
# # height
# q4_model_maple_h<- aov(Height ~ Gene_pool + Length, data = maple.all)
# summary(q4_model_maple_h)
# Anova(q4_model_maple_h, type="III")
# # width
# q4_model_maple_w<- aov(Width ~ Gene_pool + Length, data = maple.all)
# summary(q4_model_maple_w)
# Anova(q4_model_maple_w, type="III")
# 
# ## Subset Chub Creek
# chub.all<- all %>% filter(Site == 'Chub', !Gene_pool == 'Mother')
# # remove mother because point of test is to test effect of non-family members
# table(chub.all$Location)
# # height
# q4_model_chub_h<- aov(Height ~ Gene_pool + Length, data = chub.all)
# summary(q4_model_chub_h)
# Anova(q4_model_chub_h, type="III")
# # width
# q4_model_chub_w<- aov(Width ~ Gene_pool + Length, data = chub.all)
# summary(q4_model_chub_w)
# Anova(q4_model_chub_w, type="III")
# 
# ## Subset Belle Creek
# belle.all<- all %>% filter(Site == 'Belle')
# table(belle.all$Location)
# # height
# q4_model_belle_h<- aov(Height ~ Gene_pool + Length, data = belle.all)
# summary(q4_model_belle_h)
# Anova(q4_model_belle_h, type="III")
# # width
# q4_model_belle_w<- aov(Width ~ Gene_pool + Length, data = belle.all)
# summary(q4_model_belle_w)
# Anova(q4_model_belle_w, type="III")
# 
# #Significant, so lets take a look at raw data
# ggplot(belle.all,aes(Length,Height, color=Gene_pool))+
#   geom_point()+
#   geom_smooth(aes((Length),(Height), color=Gene_pool), method=lm, se=FALSE)+theme_classic()+
#   labs(x="Length (mm)", y="Height (mm)")+
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=13),
#         legend.position='none')
# 
# #### Q5: Do experimental mussels morphology differ from wild P. grandis reared in the same habitat?
# ## Subset lotic
# all.lotic<- exp.wild %>% filter(Habitat == 'lotic',
#                                 !Gene_pool == 'lotic_mother')
# table(all.lotic$Habitat_condition)
# # height
# q5_lotic_model_h <- aov(Height ~ Habitat_condition + Length, data = all.lotic)
# summary(q5_lotic_model_h)
# Anova(q5_lotic_model_h, type="III")
# # width
# q5_lotic_model_w <- aov(Width ~ Habitat_condition + Length, data = all.lotic)
# summary(q5_lotic_model_w)
# Anova(q5_lotic_model_w, type="III")
# 
# # Remove Belle Creek and retest
# all.lotic.nobelle<-all.lotic %>% filter(!Site =='Belle')
# # height
# q5_nobelle_lotic_model_h <- aov(Height ~ Habitat_condition + Length, data = all.lotic.nobelle)
# summary(q5_nobelle_lotic_model_h)
# Anova(q5_nobelle_lotic_model_h, type="III")
# # width
# q5__nobelle_lotic_model_w <- aov(Width ~ Habitat_condition + Length, data = all.lotic.nobelle)
# summary(q5__nobelle_lotic_model_w)
# Anova(q5__nobelle_lotic_model_w, type="III")
# 
# 
# 
##########
## OLD PLOTS
##########
# 
# # ALL POPS RELATIVE SHELL SHAPE
# 
# A<-ggplot(exp.wild.zoo,aes(x= factor(Site), y = log(Height/Length)))+
#   geom_boxplot(aes(color = Gene_pool,fill=Habitat),alpha=c(0.9,0.5,0.9,0.5,0.9,0.5,0.9,0.5,0.9),outlier.shape = NA) +
#   #geom_jitter(shape = 15,color = "black",position = position_jitter(0.25)) +
#   geom_point(size=1,position=position_jitterdodge(jitter.width = 0.25), aes(color = Gene_pool))+
#   scale_color_manual(values = c("black","gray"))+
#   scale_fill_manual(values = c("blue","red","purple"))+
#   ggtitle("Relative shell height")+
#   xlab("Site") + ylab("ln Height/Length")+
#   geom_point(data = mother,aes(Site,log(Height/Length)),shape=21,fill='red',color='black',size=4)+
#   theme_classic()+
#   theme(plot.title = element_text(hjust = 0.5, size = 16),
#         axis.text.x=element_text(size=12),
#         axis.title.x = element_text(size=14),
#         axis.title.y = element_text(size=14),
#         legend.position="none")
# 
# # pretty sure this plot should be used
# # ggplot(exp.wild.zoo,aes(x= factor(Site), y = log(Height)/log(Length)))+
# #   geom_boxplot(aes(color = Gene_pool,fill=Habitat),alpha=c(0.9,0.5,0.9,0.5,0.9,0.5,0.9,0.5,0.9),outlier.shape = NA) +
# #   #geom_jitter(shape = 15,color = "black",position = position_jitter(0.25)) +
# #   geom_point(size=1,position=position_jitterdodge(jitter.width = 0.25), aes(color = Gene_pool))+
# #   scale_color_manual(values = c("black","gray"))+
# #   scale_fill_manual(values = c("blue","red","purple"))+
# #   ggtitle("Relative shell height")+
# #   xlab("Site") + ylab("ln Height/Length")+
# #   geom_point(data = mother,aes(Site,log(Height)/log(Length)),shape=21,fill='red',color='black',size=4)+
# #   theme_classic()+
# #   theme(plot.title = element_text(hjust = 0.5, size = 16),
# #         axis.text.x=element_text(size=12),
# #         axis.title.x = element_text(size=14),
# #         axis.title.y = element_text(size=14),
# #         legend.position="none")
# 
# 
# # ALL POPS RELATIVE SHELL INFLATION
# B<-ggplot(exp.wild.zoo,aes(x= factor(Site), y = log(Width/Length)))+
#   geom_boxplot(aes(color = Gene_pool,fill=Habitat),alpha=c(0.9,0.5,0.9,0.5,0.9,0.5,0.9,0.5,0.9),outlier.shape = NA) +
#   #geom_jitter(shape = 15,color = "black",position = position_jitter(0.25)) +
#   geom_point(size=1,position=position_jitterdodge(jitter.width = 0.25), aes(color = Gene_pool))+
#   scale_color_manual(values = c("black","gray"))+
#   scale_fill_manual(values = c("blue","red","purple"))+
#   ggtitle("Relative shell width")+
#   xlab("Site") + ylab("ln Width/Length")+
#   geom_point(data = mother,aes(Site,log(Width/Length)),shape=21,fill='red',color='black',size=4)+
#   theme_classic()+
#   theme(plot.title = element_text(hjust = 0.5, size = 16),
#         axis.text.x=element_text(size=12),
#         axis.title.x = element_text(size=14),
#         axis.title.y = element_text(size=14),
#         legend.position="none")
# 
# shape_boxplots<-plot_grid(A,B,
#                           labels='auto',nrow=2,ncol = 1)
# ggsave(paste0(path_figs,"shape_boxplots.png"),shape_boxplots,dpi=400)
# 
# 
# #######
# # END Morphological variation for ALL mussels
# #######
# 
#
# #######
# # START Visualize Morphological variation for EXPERIMENTAL mussels
# #######
# {
# # RELATIVE SHELL HEIGHT
# ggplot(exp.zoo,aes(Location,log(Height/Length),fill=Location))+
#   geom_boxplot(alpha=0.9,outlier.shape = NA) +
#   geom_jitter(shape = 15,
#               color = "black",
#               position = position_jitter(0.25)) +
#   ggtitle("Relative shell height")+
#   xlab("Site") + ylab("ln Height/Length")+
#   geom_point(data = mother,aes(Location,log(Height/Length)),shape=21,fill='red',color='black',size=4)+
# theme(plot.title = element_text(hjust = 0.5, size = 16),axis.text.x=element_text(size=14),
#       axis.title.x = element_text(size=16),
#       axis.title.y = element_text(size=16),
#       legend.position="none")
# 
# #RELATIVE SHELL INFLATION
# ggplot(exp.zoo,aes(Location,log(Width/Length),fill=Location))+
#   geom_boxplot(alpha=0.9,outlier.shape = NA) +
#   geom_jitter(shape = 15,
#               color = "black",
#               position = position_jitter(0.25)) +
#   ggtitle("Relative shell inflation")+
#   xlab("Site") + ylab("ln Width/Length")+
#   geom_point(data = mother,aes(Location,log(Width/Length)),shape=21,fill='red',color='black',size=4)+
#   theme(plot.title = element_text(hjust = 0.5, size = 16),axis.text.x=element_text(size=14),
#         axis.title.x = element_text(size=16),
#         axis.title.y = element_text(size=16),
#         legend.position="none")
# }
#######
# END Morphological variation for EXPERIMENTAL mussels
#######
