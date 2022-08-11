library(dplyr)
library(readr)
library(ggplot2)
library(lme4)
library(sjPlot)
library(lmerTest)
library(MuMIn)


# read clean data

data<-read_csv("clean_data/PFTC6_Norway_Leaf_traits_2022_cleaned_v1.csv")

# summary numbers for Joshua

# data %>% distinct(taxon) %>% summarise(total_species = n()) # 41 species
# data %>% summarise(total_leaves = sum(bulk_nr_leaves, na.rm=T)) # 3401 leaves


# read sampling sheet for priority species list

sp_list<-read_csv("raw_data/pftc6_group1_species.csv")

# filter the sampling sheet to the 16 focal species, and select the species column only

sp_list<-sp_list %>% filter(Site == "ULV", Proj. == "*", Trt == "OTC") %>% select(Species, `G/F`)

# fix sallix mispelling
data<-data %>% mutate(taxon = ifelse(taxon == "Salix herbaceae", "Salix herbacea", taxon))

#remove non priority species from data

data<-data %>% filter(taxon %in% sp_list$Species, project == "Incline")



# some NAs for treatment, and an N experiment, and sites, remove for now but will be fixed in proper cleaning

data<-data %>% filter(!is.na(experiment), experiment != "N", siteID %in% c("Ulv", "Skj", "Gud"))

data$siteID<-factor(data$siteID, levels = c("Ulv", "Gud", "Skj"))

data <- left_join(data, select(sp_list, c(Species, `G/F`)), by = c("taxon" = "Species")) %>% rename(lifeform = `G/F`)
data <- data %>% mutate(mean_thickness = rowMeans(select(., c(leaf_thickness_1_mm,leaf_thickness_2_mm,leaf_thickness_3_mm)), na.rm=T))


xx<-data %>% group_by(siteID, taxon, experiment) %>% summarise(average_height = mean(plant_height, na.rm = T), average_thickness = mean(mean_thickness, na.rm = T),
                                                               average_leafarea = mean(leaf_area_cm2, na.rm = T), average_sla = mean(sla_cm2_g, na.rm = T))

xx<-left_join(xx, select(sp_list, c(Species, `G/F`)), by = c("taxon" = "Species")) %>% rename(lifeform = `G/F`)


# ----------------------------------------- #
# trait differences by experiment and sites #
# ----------------------------------------- #

# plant height
ggplot(data, aes(experiment, plant_height, fill = siteID)) +geom_boxplot() +facet_wrap(~siteID) + theme_classic() +
  scale_fill_manual(values = c("#FF6F59","#AB92BF","#C2E9AF"))+labs(y = "Plant height (mm)", x = "Treatment")
#ggsave("outputs/height_experiment_site.png", height = 4, width = 5)

# thickness
ggplot(data, aes(experiment, mean_thickness, fill = siteID)) +geom_boxplot() +facet_wrap(~siteID) + theme_classic() +
  scale_fill_manual(values = c("#FF6F59","#AB92BF","#C2E9AF"))+labs(y = "Mean leaf thickness (mm)", x = "Treatment")
#ggsave("outputs/thickness_experiment_site.png", height = 4, width = 5)

# leaf area
ggplot(data, aes(experiment, leaf_area_cm2, fill = siteID)) +geom_boxplot() +facet_wrap(~siteID) + theme_classic() +
  scale_fill_manual(values = c("#FF6F59","#AB92BF","#C2E9AF"))+labs(y = "Leaf area (cm2)", x = "Treatment")
#ggsave("outputs/leafarea_experiment_site.png", height = 4, width = 5)

# sla
ggplot(data, aes(experiment, sla_cm2_g, fill = siteID)) +geom_boxplot() +facet_wrap(~siteID) + theme_classic() +
  scale_fill_manual(values = c("#FF6F59","#AB92BF","#C2E9AF"))+labs(y = "Specific leaf area (cm2/g)", x = "Treatment")
ggsave("outputs/sla_experiment_site.png", height = 5, width = 5)


# individual plant heights by species and site

ggplot(data, aes(experiment, plant_height, fill = siteID)) +geom_boxplot() +facet_wrap(~taxon, scales = "free") + theme_classic()

# same but thickness

ggplot(data, aes(experiment, mean_thickness, fill = siteID)) +geom_boxplot() +facet_wrap(~taxon, scales = "free") + theme_classic()

# same but leaf area

ggplot(data, aes(experiment, log(leaf_area_cm2), fill = siteID)) +geom_boxplot() +facet_wrap(~taxon, scales = "free") + theme_classic()

# same but SLA

ggplot(data, aes(experiment, sla_cm2_g, fill = siteID)) +geom_boxplot() +facet_wrap(~taxon, scales = "free") + theme_classic()


# global mean difference between OTC and C by site - plant height

ggplot(data, aes(siteID, plant_height, fill = experiment)) + geom_boxplot()+
  theme_classic()+
  #ggtitle("Bistorta vivipara") +
  labs(y = "Plant height (mm)", x = "Site", fill = "Treatment")+
  scale_fill_manual(values = c("#FF6F59","#AB92BF"))

# same but leaf area

ggplot(data, aes(siteID, log(leaf_area_cm2), fill = experiment)) + geom_boxplot()+
  theme_classic()+
  #ggtitle("Bistorta vivipara") +
  labs(y = "Leaf area (cm2)", x = "Site", fill = "Treatment")+
  scale_fill_manual(values = c("#FF6F59","#AB92BF"))



#ggsave("outputs/global_mean_leaf_area.png", height = 5, width  = 5)


# --------------------------------------------------- #
# create plots of individual species for presentation #
# --------------------------------------------------- #
# height

data %>% filter(taxon %in% c("Bistorta vivipara", "Carex bigelowii", "Poa alpina", "Veronica alpina")) %>% ggplot(., aes(experiment, plant_height, fill = siteID)) +
  geom_boxplot() +
  theme_classic()+ facet_wrap(~taxon)+#, scales = "free")+
  #ggtitle("Bistorta vivipara") +
  labs(y = "Plant height (mm)", x = "Treatment", fill = "Site")+
  scale_fill_manual(values = c("#FF6F59","#AB92BF","#C2E9AF"))

#ggsave("outputs/cherry_picked_height.png", height = 6, width = 6)

# thickness

data %>% filter(taxon %in% c("Bistorta vivipara", "Carex bigelowii", "Poa alpina", "Veronica alpina")) %>% ggplot(., aes(experiment, mean_thickness, fill = siteID)) +
  geom_boxplot() +
  theme_classic()+ facet_wrap(~taxon)+#, scales = "free")+
  #ggtitle("Bistorta vivipara") +
  labs(y = "Mean leaf thickness (mm)", x = "Treatment", fill = "Site")+ggtitle("THIQUE CLIQUE!!!!!")+
  scale_fill_manual(values = c("#FF6F59","#AB92BF","#C2E9AF"))

#ggsave("outputs/cherry_picked_thickness.png", height = 6, width = 6)

# leaf area

data %>% filter(taxon %in% c("Bistorta vivipara", "Carex bigelowii", "Poa alpina", "Veronica alpina")) %>% ggplot(., aes(experiment, leaf_area_cm2, fill = siteID)) +
  geom_boxplot() +# coord_trans(y = "log") +
  theme_classic()+ facet_wrap(~taxon, scales = "free")+
  #ggtitle("Bistorta vivipara") +
  labs(y = "Leaf area (cm2)", x = "Treatment", fill = "Site")+
  scale_fill_manual(values = c("#FF6F59","#AB92BF","#C2E9AF"))

#ggsave("outputs/cherry_picked_leafarea.png", height = 6, width = 6)

# (wet) SLA

data %>% filter(taxon %in% c("Bistorta vivipara", "Carex bigelowii", "Poa alpina", "Veronica alpina")) %>% ggplot(., aes(experiment, sla_cm2_g, fill = siteID)) +
  geom_boxplot() +# coord_trans(y = "log") +
  theme_classic()+ facet_wrap(~taxon, scales = "free")+
  #ggtitle("Bistorta vivipara") +
  labs(y = "(Wet) Specific leaf area (cm2/g)", x = "Treatment", fill = "Site")+
  scale_fill_manual(values = c("#FF6F59","#AB92BF","#C2E9AF"))

#ggsave("outputs/cherry_picked_sla.png", height = 6, width = 6)

#---------------------------#
# ULV variance partitioning #

# height

data_ulv <- data %>% filter(siteID == 'Ulv')
data_gud <- data %>% filter(siteID == 'Gud')
data_skj <- data %>% filter(siteID == 'Skj')


ft_height<-lmer(plant_height ~ 1+ (1|lifeform/taxon/experiment), data = data_ulv)
summary(ft_height)

r.squaredGLMM(ft_height)

var_height <- select(as.data.frame(VarCorr(ft_height)), c(grp,vcov)) %>% rename(level = grp)

var_height <- var_height %>% mutate(RE = c( 'Treatment', 'Species', 'Functional group', 'Residual')) %>%
  select(-level) %>% rename(variance_height = vcov)

var_height$RE<-factor(var_height$RE, levels = c( 'Treatment', 'Species', 'Functional group', 'Residual'))

ggplot(var_height, aes(fill=RE,x="", y=variance_height))+
  geom_col(position = "fill") + theme_classic()

# leaf area


ft_la<-lmer(log(leaf_area_cm2) ~ 1+ (1|lifeform/taxon/experiment), data = data_ulv)
summary(ft_la)

r.squaredGLMM(ft_la)

var_la <- select(as.data.frame(VarCorr(ft_la)), c(grp,vcov)) %>% rename(level = grp)

var_la <- var_la %>% mutate(RE = c('Treatment', 'Species', 'Functional group', 'Residual')) %>%
  select(-level) %>% rename(variance_la = vcov)

var_la$RE<-factor(var_la$RE, levels = c('Treatment', 'Species', 'Functional group', 'Residual'))


ggplot(var_la, aes(fill=RE,x="", y=variance_la))+
  geom_col(position = "fill") + theme_classic()


# thickness


ft_thick<-lmer(mean_thickness ~ 1+ (1|lifeform/taxon/experiment), data = data_ulv)
summary(ft_thick)

r.squaredGLMM(ft_thick)

var_thick <- select(as.data.frame(VarCorr(ft_thick)), c(grp,vcov)) %>% rename(level = grp)

var_thick <- var_thick %>% mutate(RE = c('Treatment', 'Species', 'Functional group', 'Residual')) %>%
  select(-level) %>% rename(variance_thick = vcov)

var_thick$RE<-factor(var_thick$RE, levels = c('Treatment', 'Species', 'Functional group', 'Residual'))

ggplot(var_thick, aes(fill=RE,x="", y=variance_thick))+
  geom_col(position = "fill") + theme_classic()

all <- bind_cols(var_thick, val_la = var_la$variance_la, var_height=var_height$variance_height)

all <- pivot_longer(all, cols=c(1,3,4), names_to = "trait")

ulv_plots<-ggplot(all, aes(fill=RE,x=trait, y=value))+
  geom_col(position = "fill") + theme_classic()+
  ylab("Proportion variance explained")+
  xlab("")+ggtitle('Ulvehagen')+scale_x_discrete(labels = c('Leaf area', 'Height', 'Leaf thickness'))


#---------------------------------#
# GUD variance partitioning plots #
ft_height<-lmer(plant_height ~ 1+ (1|lifeform/taxon/experiment), data = data_gud)
summary(ft_height)

r.squaredGLMM(ft_height)

var_height <- select(as.data.frame(VarCorr(ft_height)), c(grp,vcov)) %>% rename(level = grp)

var_height <- var_height %>% mutate(RE = c( 'Treatment', 'Species', 'Functional group', 'Residual')) %>%
  select(-level) %>% rename(variance_height = vcov)

var_height$RE<-factor(var_height$RE, levels = c( 'Treatment', 'Species', 'Functional group', 'Residual'))

ggplot(var_height, aes(fill=RE,x="", y=variance_height))+
  geom_col(position = "fill") + theme_classic()

# leaf area


ft_la<-lmer(log(leaf_area_cm2) ~ 1+ (1|lifeform/taxon/experiment), data = data_gud)
summary(ft_la)

r.squaredGLMM(ft_la)

var_la <- select(as.data.frame(VarCorr(ft_la)), c(grp,vcov)) %>% rename(level = grp)

var_la <- var_la %>% mutate(RE = c('Treatment', 'Species', 'Functional group', 'Residual')) %>%
  select(-level) %>% rename(variance_la = vcov)

var_la$RE<-factor(var_la$RE, levels = c('Treatment', 'Species', 'Functional group', 'Residual'))


ggplot(var_la, aes(fill=RE,x="", y=variance_la))+
  geom_col(position = "fill") + theme_classic()


# thickness


ft_thick<-lmer(mean_thickness ~ 1+ (1|lifeform/taxon/experiment), data = data_gud)
summary(ft_thick)

r.squaredGLMM(ft_thick)

var_thick <- select(as.data.frame(VarCorr(ft_thick)), c(grp,vcov)) %>% rename(level = grp)

var_thick <- var_thick %>% mutate(RE = c('Treatment', 'Species', 'Functional group', 'Residual')) %>%
  select(-level) %>% rename(variance_thick = vcov)

var_thick$RE<-factor(var_thick$RE, levels = c('Treatment', 'Species', 'Functional group', 'Residual'))

ggplot(var_thick, aes(fill=RE,x="", y=variance_thick))+
  geom_col(position = "fill") + theme_classic()

all <- bind_cols(var_thick, val_la = var_la$variance_la, var_height=var_height$variance_height)

all <- pivot_longer(all, cols=c(1,3,4), names_to = "trait")

gud_plots<-ggplot(all, aes(fill=RE,x=trait, y=value))+
  geom_col(position = "fill") + theme_classic()+
  ylab("Proportion variance explained")+
  xlab("")+ggtitle('Gudmedalen')+scale_x_discrete(labels = c('Leaf area', 'Height', 'Leaf thickness'))

#---------------------------#
# SKJ variance partitioning #

ft_height<-lmer(plant_height ~ 1+ (1|lifeform/taxon/experiment), data = data_skj)
summary(ft_height)

r.squaredGLMM(ft_height)

var_height <- select(as.data.frame(VarCorr(ft_height)), c(grp,vcov)) %>% rename(level = grp)

var_height <- var_height %>% mutate(RE = c( 'Treatment', 'Species', 'Functional group', 'Residual')) %>%
  select(-level) %>% rename(variance_height = vcov)

var_height$RE<-factor(var_height$RE, levels = c( 'Treatment', 'Species', 'Functional group', 'Residual'))

ggplot(var_height, aes(fill=RE,x="", y=variance_height))+
  geom_col(position = "fill") + theme_classic()

# leaf area


ft_la<-lmer(log(leaf_area_cm2) ~ 1+ (1|lifeform/taxon/experiment), data = data_skj)
summary(ft_la)

r.squaredGLMM(ft_la)

var_la <- select(as.data.frame(VarCorr(ft_la)), c(grp,vcov)) %>% rename(level = grp)

var_la <- var_la %>% mutate(RE = c('Treatment', 'Species', 'Functional group', 'Residual')) %>%
  select(-level) %>% rename(variance_la = vcov)

var_la$RE<-factor(var_la$RE, levels = c('Treatment', 'Species', 'Functional group', 'Residual'))


ggplot(var_la, aes(fill=RE,x="", y=variance_la))+
  geom_col(position = "fill") + theme_classic()


# thickness


ft_thick<-lmer(mean_thickness ~ 1+ (1|lifeform/taxon/experiment), data = data_skj)
summary(ft_thick)

r.squaredGLMM(ft_thick)

var_thick <- select(as.data.frame(VarCorr(ft_thick)), c(grp,vcov)) %>% rename(level = grp)

var_thick <- var_thick %>% mutate(RE = c('Treatment', 'Species', 'Functional group', 'Residual')) %>%
  select(-level) %>% rename(variance_thick = vcov)

var_thick$RE<-factor(var_thick$RE, levels = c('Treatment', 'Species', 'Functional group', 'Residual'))

ggplot(var_thick, aes(fill=RE,x="", y=variance_thick))+
  geom_col(position = "fill") + theme_classic()

all <- bind_cols(var_thick, val_la = var_la$variance_la, var_height=var_height$variance_height)

all <- pivot_longer(all, cols=c(1,3,4), names_to = "trait")

skj_plots<-ggplot(all, aes(fill=RE,x=trait, y=value))+
  geom_col(position = "fill") + theme_classic()+
  ylab("Proportion variance explained")+
  xlab("")+ggtitle('Skjellingahaugen')+scale_x_discrete(labels = c('Leaf area', 'Height', 'Leaf thickness'))

combined<-ulv_plots+gud_plots+skj_plots& theme(legend.position = "right")
combined + plot_layout(guides = "collect")

ggsave('combined_variance_partitioning.png', height = 5, width = 10)
