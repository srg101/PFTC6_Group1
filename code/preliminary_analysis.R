# read clean data

data<-read_csv("clean_data/clean_data.csv")

# read sampling sheet for priority species list

sp_list<-read_csv("raw_data/pftc6_group1_species.csv")

# filter the sampling sheet to the 16 focal species, and select the species column only

sp_list<-sp_list %>% filter(Site == "ULV", Proj. == "*", Trt == "OTC") %>% select(Species, `G/F`)

# fix sallix mispelling
data<-data %>% mutate(taxon = ifelse(taxon == "Salix herbaceae", "Salix herbacea", taxon))

#remove non priority species from data

data<-data %>% filter(taxon %in% sp_list$Species)

# some NAs for treatment, and an N experiment, and sites, remove for now but will be fixed in proper cleaning

data<-data %>% filter(!is.na(experiment), experiment != "N", siteID %in% c("Ulv", "Skj", "Gud"))

data$siteID<-factor(data$siteID, levels = c("Ulv", "Gud", "Skj"))

data <- left_join(data, select(sp_list, c(Species, `G/F`)), by = c("taxon" = "Species")) %>% rename(lifeform = `G/F`)
data <- data %>% mutate(mean_thickness = rowMeans(select(., c(leaf_thickness_1_mm,leaf_thickness_2_mm,leaf_thickness_3_mm)), na.rm=T))


xx<-data %>% group_by(siteID, taxon, experiment) %>% summarise(average_height = mean(plant_height), average_thickness = mean(mean_thickness))

xx<-left_join(xx, select(sp_list, c(Species, `G/F`)), by = c("taxon" = "Species")) %>% rename(lifeform = `G/F`)

# species average height by site
ggplot(xx, aes(experiment, average_height, group = taxon, colour = taxon)) +geom_point() +geom_line(col = "grey") +facet_wrap(~factor(siteID, levels = c("Ulv", "Gud", "Skj")))

# species average height by site and lifeform
ggplot(xx, aes(experiment, average_height, group = taxon, colour = taxon)) +geom_point() +geom_line(col = "grey") +facet_wrap(~factor(siteID, levels = c("Ulv", "Gud", "Skj"))~lifeform)

# individual plant heights (not averaged) from raw data

ggplot(yy, aes(experiment, plant_height, fill = lifeform)) +geom_boxplot() +facet_wrap(~siteID)

# individual plant heights by species and site

ggplot(data, aes(experiment, plant_height, fill = siteID)) +geom_boxplot() +facet_wrap(~taxon, scales = "free") + theme_classic()

# same but thickness

ggplot(data, aes(experiment, mean_thickness, fill = siteID)) +geom_boxplot() +facet_wrap(~taxon, scales = "free") + theme_classic()

# global mean difference between OTC and C by site

ggplot(data, aes(siteID, plant_height, fill = experiment)) + geom_boxplot()+
  theme_classic()+
  #ggtitle("Bistorta vivipara") +
  labs(y = "Plant height (mm)", x = "Site", fill = "Treatment")+
  scale_fill_manual(values = c("#FF6F59","#AB92BF"))

ggsave("outputs/global_mean.png", height = 5, width  = 5)

data %>% filter(taxon %in% c("Bistorta vivipara", "Carex bigelowii", "Poa alpina", "Veronica alpina")) %>% ggplot(., aes(experiment, plant_height, fill = siteID)) +
  geom_boxplot() +
  theme_classic()+ facet_wrap(~taxon)+#, scales = "free")+
  #ggtitle("Bistorta vivipara") +
  labs(y = "Plant height (mm)", x = "Treatment", fill = "Site")+
 scale_fill_manual(values = c("#FF6F59","#AB92BF","#C2E9AF"))


data %>% filter(taxon %in% c("Bistorta vivipara", "Carex bigelowii", "Poa alpina", "Veronica alpina")) %>% ggplot(., aes(experiment, mean_thickness, fill = siteID)) +
  geom_boxplot() +
  theme_classic()+ facet_wrap(~taxon)+#, scales = "free")+
  #ggtitle("Bistorta vivipara") +
  labs(y = "Mean leaf thickness (mm)", x = "Treatment", fill = "Site")+ggtitle("THIQUE CLIQUE!!!!!")+
  scale_fill_manual(values = c("#FF6F59","#AB92BF","#C2E9AF"))

ggsave("outputs/cherry_picked_thickness.png", height = 6, width = 6)

zz<-left_join(zz, select(sp_list, c(Species, `G/F`)), by = c("taxon" = "Species")) %>% rename(lifeform = `G/F`)


ggplot(xx, aes(experiment, average_thickness, group = taxon, colour = taxon)) +geom_point() +geom_line(col = "grey") +facet_wrap(~factor(siteID, levels = c("Ulv", "Gud", "Skj")))

ggplot(zz, aes(experiment, mean_thickness #, fill = lifeform
)) +geom_boxplot() +facet_wrap(~siteID)


library(lme4)
library(sjPlot)
library(lmerTest)

hist(sqrt(data$plant_height))
hist(log(data$mean_thickness))

ft<-lmer((plant_height) ~ experiment + siteID+(experiment|taxon), data = data)
summary(ft)

ft<-lmer(average_thickness ~ experiment*lifeform + siteID+(experiment|taxon), data = xx)
summary(ft)



# calculate the average thickness



