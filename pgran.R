{
  # Keogh et al. XXXX -
  # Phenotypic plasticity, multiple paternity, and shell shape divergence across 
  # lake-stream habitats in a single freshwater mussel brood (Pyganodon grandis)

library(cowplot)
library(viridis)
library(readxl)
library(tidyverse)
library(car)
library(scales)
library(multcomp)

# Set working directory
#setwd('~/Box/Myprojects/Pgrandis/')
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
  scale_fill_manual(values = c('pink','red','maroon','#0F67B7'))+
  geom_point(aes((Length),(Width),fill=Site),size=1.75,shape=21,alpha=0.7)+ 
  geom_smooth(aes((Length),(Width), color=Site), method=lm, se=FALSE)+
  scale_color_manual(values = c('pink','red','maroon','#0F67B7'))+
  theme_bw()+
  labs(x="Length (mm)", y="Width (mm)")+
  theme(
    panel.grid.minor=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(hjust=0),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    text = element_text(family = "Times New Roman"),
    legend.position="none")

ggsave(paste0(path_figs,"growth_traj_a.png"),a,
       width=4, height=3.25, units='in', dpi=400)
# Height
b<-ggplot(exp,color='black')+
  scale_fill_manual(values = c('pink','red','maroon','#0F67B7'))+
  geom_point(aes((Length),(Height),fill=Site),size=1.75,shape=21,alpha=0.7)+ 
  geom_smooth(aes((Length),(Height), color=Site), method=lm, se=FALSE)+
  scale_color_manual(values = c('pink','red','maroon','#0F67B7'))+
  theme_bw() +
  labs(x="Length (mm)", y="Height (mm)")+
  theme(
    panel.grid.minor=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(hjust=0),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    text = element_text(family = "Times New Roman"),
    legend.position="none")

ggsave(paste0(path_figs,"growth_traj_b.png"),b,
       width=4, height=3.25, units='in', dpi=400)
  

### Siblings at different habitats
# Width
c<-ggplot(exp,color='black')+
  scale_fill_manual(values = c('#FF225A','#FF225A','#FF225A','#0F67B7'))+
  geom_point(aes((Length),(Width),fill=Site),size=1.75,shape=21,alpha=0.7)+ 
  geom_smooth(aes((Length),(Width), color=Habitat), method=lm, se=FALSE)+
  scale_color_manual(values = c('#0F67B7','#FF225A'))+
  theme_bw()+
  labs(x="Length (mm)", y="Width (mm)")+
  theme(
    panel.grid.minor=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(hjust=0),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    text = element_text(family = "Times New Roman"),
    legend.position="none")

ggsave(paste0(path_figs,"growth_traj_c.png"),c,
       width=4, height=3.25, units='in', dpi=400)
# Height
d<-ggplot(exp,color='black')+
  scale_fill_manual(values = c('#FF225A','#FF225A','#FF225A','#0F67B7'))+
  geom_point(aes((Length),(Height),fill=Site),size=1.75,shape=21,alpha=0.7)+ 
  geom_smooth(aes((Length),(Height), color=Habitat), method=lm, se=FALSE)+
  scale_color_manual(values = c('#0F67B7','#FF225A'))+
  theme_bw()+
  labs(x="Length (mm)", y="Height (mm)")+
  theme(
    panel.grid.minor=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(hjust=0),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    text = element_text(family = "Times New Roman"),
    legend.position="none")

ggsave(paste0(path_figs,"growth_traj_d.png"),d,
       width=4, height=3.25, units='in', dpi=400)

### Siblings and wild mussels in different habitats
# Width

exp.wild<- all %>% filter(Habitat == 'lentic' | Habitat == 'lotic',
                          !Gene_pool == 'Mother',
                          !Site == 'Zoo')

exp.wild<-exp.wild %>% mutate(Habitat_condition = paste(Habitat, Gene_pool, sep = '_'))

# Belle - recap & wild
Belle<- exp.wild %>% filter(Site == 'Belle')
belle_width<-ggplot(Belle,color='black')+
  scale_fill_manual(values = c('#FF225A','gray75'))+
  geom_point(aes((Length),(Width),fill=Gene_pool),size=1.75,shape=21,alpha=0.7)+
  geom_smooth(aes((Length),(Width), color=Gene_pool),
              method=lm, se=FALSE,linewidth=1.1)+
  scale_linetype_manual(values=c('solid','solid'))+
  scale_color_manual(values = c('#FF225A','gray75'))+
  theme_bw()+
  labs(x="Length (mm)", y="Width (mm)")+
  xlim(31,149)+
  ylim(8,69)+
  theme(
    panel.grid.minor=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(hjust=0),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    text = element_text(family = "Times New Roman"),
    legend.position="none")

ggsave(paste0(path_figs,"belle_width.png"),belle_width,
       width=4, height=3.25, units='in', dpi=400)

belle_height<-ggplot(Belle,color='black')+
  scale_fill_manual(values = c('#FF225A','gray75'))+
  geom_point(aes((Length),(Height),fill=Gene_pool),size=1.75,shape=21,alpha=0.7)+
  geom_smooth(aes((Length),(Height), color=Gene_pool),
              method=lm, se=FALSE,linewidth=1.1)+
  scale_linetype_manual(values=c('solid','solid'))+
  scale_color_manual(values = c('#FF225A','gray75'))+
  theme_bw()+
  labs(x="Length (mm)", y="Height (mm)")+
  xlim(31,149)+
  ylim(15,98)+
  theme(
    panel.grid.minor=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(hjust=0),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    text = element_text(family = "Times New Roman"),
    legend.position="none")

ggsave(paste0(path_figs,"belle_height.png"),belle_height,
       width=4, height=3.25, units='in', dpi=400)

# Chub - recap & wild
Chub<- exp.wild %>% filter(Site == 'Chub')
chub_width<-ggplot(Chub,color='black')+
  scale_fill_manual(values = c('#FF225A','gray75'))+
  geom_point(aes((Length),(Width),fill=Gene_pool),size=1.75,shape=21,alpha=0.7)+
  geom_smooth(aes((Length),(Width), color=Gene_pool),
              method=lm, se=FALSE,linewidth=1.1)+
  scale_linetype_manual(values=c('solid','solid'))+
  scale_color_manual(values = c('#FF225A','gray75'))+
  theme_bw()+
  labs(x="Length (mm)", y="Width (mm)")+
  xlim(31,149)+
  ylim(8,69)+
  theme(
    panel.grid.minor=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(hjust=0),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    text = element_text(family = "Times New Roman"),
    legend.position="none")

ggsave(paste0(path_figs,"chub_width.png"),chub_width,
       width=4, height=3.25, units='in', dpi=400)

chub_height<-ggplot(Chub,color='black')+
  scale_fill_manual(values = c('#FF225A','gray75'))+
  geom_point(aes((Length),(Height),fill=Gene_pool),size=1.75,shape=21,alpha=0.7)+
  geom_smooth(aes((Length),(Height), color=Gene_pool),
              method=lm, se=FALSE,linewidth=1.1)+
  scale_linetype_manual(values=c('solid','solid'))+
  scale_color_manual(values = c('#FF225A','gray75'))+
  theme_bw()+
  labs(x="Length (mm)", y="Height (mm)")+
  xlim(31,149)+
  ylim(15,98)+
  theme(
    panel.grid.minor=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(hjust=0),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    text = element_text(family = "Times New Roman"),
    legend.position="none")

ggsave(paste0(path_figs,"chub_height.png"),chub_height,
       width=4, height=3.25, units='in', dpi=400)

# Maple - recap & wild
Maple<- exp.wild %>% filter(Site == 'Maple')
maple_width<-ggplot(Maple,color='black')+
  scale_fill_manual(values = c('#FF225A','gray75'))+
  geom_point(aes((Length),(Width),fill=Gene_pool),size=1.75,shape=21,alpha=0.7)+
  geom_smooth(aes((Length),(Width), color=Gene_pool),
              method=lm, se=FALSE,linewidth=1.1)+
  scale_linetype_manual(values=c('solid','solid'))+
  scale_color_manual(values = c('#FF225A','gray75'))+
  theme_bw()+
  labs(x="Length (mm)", y="Width (mm)")+
  xlim(31,149)+
  ylim(8,69)+
  theme(
    panel.grid.minor=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(hjust=0),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    text = element_text(family = "Times New Roman"),
    legend.position="none")

ggsave(paste0(path_figs,"maple_width.png"),maple_width,
       width=4, height=3.25, units='in', dpi=400)

maple_height<-ggplot(Maple,color='black')+
  scale_fill_manual(values = c('#FF225A','gray75'))+
  geom_point(aes((Length),(Height),fill=Gene_pool),size=1.75,shape=21,alpha=0.7)+
  geom_smooth(aes((Length),(Height), color=Gene_pool),
              method=lm, se=FALSE,linewidth=1.1)+
  scale_linetype_manual(values=c('solid','solid'))+
  scale_color_manual(values = c('#FF225A','gray75'))+
  theme_bw()+
  labs(x="Length (mm)", y="Height (mm)")+
  xlim(31,149)+
  ylim(15,98)+
  theme(
    panel.grid.minor=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(hjust=0),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    text = element_text(family = "Times New Roman"),
    legend.position="none")

ggsave(paste0(path_figs,"maple_height.png"),maple_height,
       width=4, height=3.25, units='in', dpi=400)

# Shields - recap & wild
Shields<- exp.wild %>% filter(Site == 'Shields')
shields_width<-ggplot(Shields,color='black')+
  scale_fill_manual(values = c('#0F67B7','gray75'))+
  geom_point(aes((Length),(Width),fill=Gene_pool),size=1.75,shape=21,alpha=0.7)+
  geom_smooth(aes((Length),(Width), color=Gene_pool),
              method=lm, se=FALSE,linewidth=1.1)+
  scale_linetype_manual(values=c('solid','solid'))+
  scale_color_manual(values = c('#0F67B7','gray75'))+
  theme_bw()+
  labs(x="Length (mm)", y="Width (mm)")+
  xlim(31,149)+
  ylim(8,69)+
  theme(
    panel.grid.minor=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(hjust=0),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    text = element_text(family = "Times New Roman"),
    legend.position="none")

ggsave(paste0(path_figs,"shields_width.png"),shields_width,
       width=4, height=3.25, units='in', dpi=400)

shields_height<-ggplot(Shields,color='black')+
  scale_fill_manual(values = c('#0F67B7','gray75'))+
  geom_point(aes((Length),(Height),fill=Gene_pool),size=1.75,shape=21,alpha=0.7)+
  geom_smooth(aes((Length),(Height), color=Gene_pool),
              method=lm, se=FALSE,linewidth=1.1)+
  scale_linetype_manual(values=c('solid','solid'))+
  scale_color_manual(values = c('#0F67B7','gray75'))+
  theme_bw()+
  labs(x="Length (mm)", y="Height (mm)")+
  xlim(31,149)+
  ylim(15,98)+
  theme(
    panel.grid.minor=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(hjust=0),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    text = element_text(family = "Times New Roman"),
    legend.position="none")

ggsave(paste0(path_figs,"shields_height.png"),shields_height,
       width=4, height=3.25, units='in', dpi=400)

### Old end of FIGURE 4
e<-ggplot(exp.wild,color='black')+
  scale_fill_manual(values = c('#0F67B7','#0F67B7','#FF225A','#FF225A'))+
  geom_point(aes((Length),(Width),fill=Habitat_condition,shape=Habitat_condition),size=1,alpha=0.4)+
  scale_shape_manual(values = c(21,22,21,22))+
  geom_smooth(aes((Length),(Width), color=Habitat_condition, linetype=Habitat_condition),
              method=lm, se=FALSE,linewidth=1.1)+
  scale_linetype_manual(values=c('solid','longdash','solid','longdash'))+
  scale_color_manual(values = c('#0F67B7','#0F67B7','#FF225A','#FF225A'))+
  theme_bw()+
  labs(x="Length (mm)", y="Width (mm)")+
  theme(
    panel.grid.minor=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(hjust=0),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    text = element_text(family = "Times New Roman"),
    legend.position="none")

ggsave(paste0(path_figs,"growth_traj_e.png"),e,
       width=4, height=3.25, units='in', dpi=400)
# Height
f<-ggplot(exp.wild,color='black')+
  scale_fill_manual(values = c('#0F67B7','#0F67B7','#FF225A','#FF225A'))+
  geom_point(aes((Length),(Height),fill=Habitat_condition,shape=Habitat_condition),size=1,alpha=0.4)+
  scale_shape_manual(values = c(21,22,21,22))+
  geom_smooth(aes((Length),(Height), color=Habitat_condition, linetype=Habitat_condition),
              method=lm, se=FALSE,linewidth=1.1)+
  scale_linetype_manual(values=c('solid','longdash','solid','longdash'))+
  scale_color_manual(values = c('#0F67B7','#0F67B7','#FF225A','#FF225A'))+
  theme_bw()+
  labs(x="Length (mm)", y="Height (mm)")+
  theme(
    panel.grid.minor=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(hjust=0),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    text = element_text(family = "Times New Roman"),
    legend.position="none")

ggsave(paste0(path_figs,"growth_traj_f.png"),f,
       width=4, height=3.25, units='in', dpi=400)





a.zoo<-ggplot()+
  scale_fill_manual(values = c('pink','red','maroon','#0F67B7','purple'))+
  geom_point(data=exp.zoo,aes((Length),(Width),fill=Site),size=2,shape=21,alpha=0.45,color='black')+ 
  geom_smooth(data=exp.zoo,aes((Length),(Width), color=Site), method=lm, se=FALSE, linewidth=1.2)+
  scale_color_manual(values = c('pink','red','maroon','#0F67B7','purple'))+
  #geom_point(data=mother,aes(Length,Width), size=3, shape=8,color='black')+
  theme_classic()+
  labs(x="Length (mm)", y="Width (mm)")+
  theme(legend.position='none')
# Height
b.zoo<-ggplot(exp.zoo,color='black')+
  scale_fill_manual(values = c('pink','red','maroon','#0F67B7','purple'))+
  geom_point(aes((Length),(Height),fill=Site),size=2,shape=21,alpha=0.45)+ 
  geom_smooth(aes((Length),(Height), color=Site), method=lm, se=FALSE, linewidth=1.2)+
  scale_color_manual(values = c('pink','red','maroon','#0F67B7','purple'))+
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
sibs<-exp %>% drop_na(Sibship_90)
table(sibs$Sibship_90)
table(sibs$Sibship_80)
table(sibs$Sibship_70)

# Colony results from 904 data completeness
# height
q6_sibship_model_h_90 <- lm(Height ~ Sibship_90 + Length + Habitat, data = sibs)
summary(q6_sibship_model_h_90)
Anova(q6_sibship_model_h_90, type="II")
# width
q6_sibship_model_w_90 <- lm(Width ~ Sibship_90 + Length + Habitat, data = sibs)
summary(q6_sibship_model_w_90)
Anova(q6_sibship_model_w_90, type="II")

# Colony results from 80% data completeness
# height
q6_sibship_model_h_80 <- lm(Height ~ Sibship_80 + Length + Habitat, data = sibs)
summary(q6_sibship_model_h_80)
Anova(q6_sibship_model_h_80, type="II")
# width
q6_sibship_model_w_80 <- lm(Width ~ Sibship_80 + Length + Habitat, data = sibs)
summary(q6_sibship_model_w_80)
Anova(q6_sibship_model_w_80, type="II")

# Colony results from 70% data completeness
# height
q6_sibship_model_h_70 <- lm(Height ~ Sibship_70 + Length + Habitat, data = sibs)
summary(q6_sibship_model_h_70)
Anova(q6_sibship_model_h_70, type="II")
# width
q6_sibship_model_w_70 <- lm(Width ~ Sibship_70 + Length + Habitat, data = sibs)
summary(q6_sibship_model_w_70)
Anova(q6_sibship_model_w_70, type="II")

}
#####
# END STATISTICAL ANALYSIS
#####


#######
## SIBSHIP PLOTS
#######

lakes<-exp %>% filter(Habitat=='lentic')
streams<-exp %>% filter(Habitat=='lotic')
sibship<-exp %>% drop_na(Sibship_70)

library(stringr)
sibship<-sibship %>%
  filter(str_detect(Sibship_70, '^full'))

lakes$variable<-lakes$Habitat
streams$variable<-streams$Habitat
sibship$variable<-sibship$Sibship_70

sibship_data<-rbind.data.frame(lakes,streams,sibship)
unique(sibship$Sibship_70)
g<-ggplot()+
  geom_point(data=sibship,aes(Length,Width,fill=Habitat,color=Habitat,shape=Sibship_70),size=5,alpha=1)+ 
  geom_smooth(data=streams,aes(Length,Width),method='lm',se=FALSE,linetype='solid',linewidth=1.25,colour='#FF225A')+
  geom_smooth(data=lakes,aes(Length,Width),method='lm',se=FALSE,linetype='solid',linewidth=1.25,colour='#0F67B7')+
  scale_shape_manual(values = c(15,19,17,23,8,13,0,1,12,2,9,14))+
  scale_color_manual(values = c('#0F67B7','#FF225A'))+
  scale_fill_manual(values = c('#0F67B7','#FF225A'))+
  theme_bw()+
  labs(x="Length (mm)", y="Width (mm)")+
  theme(
    panel.grid.minor=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(hjust=0),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    text = element_text(family = "Times New Roman"),
    legend.position="none")

ggsave(paste0(path_figs,"growth_traj_g.png"),g,
       width=4, height=3.25, units='in', dpi=400)

h<-ggplot()+
  geom_point(data=sibship,aes(Length,Height,fill=Habitat,color=Habitat,shape=Sibship_70),size=5,alpha=1)+ 
  geom_smooth(data=streams,aes(Length,Height),method='lm',se=FALSE,linetype='solid',linewidth=1.25,colour='#FF225A')+
  geom_smooth(data=lakes,aes(Length,Height),method='lm',se=FALSE,linetype='solid',linewidth=1.25,colour='#0F67B7')+
  scale_shape_manual(values = c(15,19,17,23,8,13,0,1,12,2,9,14))+
  scale_color_manual(values = c('#0F67B7','#FF225A'))+
  scale_fill_manual(values = c('#0F67B7','#FF225A'))+
  theme_bw()+
  labs(x="Length (mm)", y="Height (mm)")+
  theme(
    panel.grid.minor=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(hjust=0),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    text = element_text(family = "Times New Roman"),
    legend.position="none")

ggsave(paste0(path_figs,"growth_traj_h.png"),h,
       width=4, height=3.25, units='in', dpi=400)


#######################################################################
## END of code for paper
#######################################################################