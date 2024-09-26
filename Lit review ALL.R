#install.packages("tidyverse")

library(ggplot2)
library(tidyverse)
library(dplyr)


## -----------------------------------------------------------------------------

setwd("C:/Users/samue/Desktop")

data <- read.csv("relatedness_literature_review_working2.csv", stringsAsFactors = T)

data <- data %>% 
  filter(Index == "Keep")%>%
  droplevels

## We now have all the data to assess general patterns in species, conservation and order etc...

## How many pubs...

nlevels(data$Title)

## How many species

nlevels(data$Species)

## Across how many orders

nlevels(data$Order)

## Now lets look at what papers did what...

table(data$Order)

## Super order

table(data$Super_Order)

species_superorder <- data %>%
  group_by(Super_Order) %>%
  count(Species)

print(species_superorder, n = 49)

species_order <- data %>%
  group_by(Super_Order, Order, Species, Focus) %>%
  count(Order)

print(species_order, n = 49)

order_plot <- ggplot(species_order, 
       aes(forcats::fct_reorder(Order, -n, sum), n, fill = Focus)) +
  geom_col() + 
  xlab("Order") +
  ylab("No. Papers") +
  scale_fill_brewer(palette = "RdPu") +
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, by = 5)) + 
  ggtitle("Number of marker types used in relation to research focus") + 
  theme_classic()

order_plot + theme(axis.text.x = element_text(angle = 90),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                  plot.title = element_text(hjust = 0.5, size = 16))


species <- data %>%
  group_by(Order) %>%
  count(Species)


## ------------------------------------------------------------------------------

## Now lets redo the conservation data 

IUCN <- data.frame(data %>% 
  group_by(IUCN.Status) %>%
  count(IUCN.Status, sort = T))


## make a function to create a % value of data
IUCN <- IUCN %>% mutate(SUM=sum(n),
                Percent=n/SUM*100) 

IUCN$Percent <- round(IUCN$Percent, 2)

IUCN

CC.THR <- 21 + 14 + 12 #num threatened 

(CC.THR/74)*100
## --------------------------------------------------------------------

data$Kinship.Method <- recode_factor(data$Kinship.Method, "Allele counts  " = "Allele counts", "GERUD & COLONY " = "GERUD & COLONY", "COLONY + CERVUS" = "COLONY & CERVUS", "Allele counts & GERUD 1" = "Allele counts & GERUD", "Allele counts & GERUD 2.0" = "Allele counts & GERUD", "GERUD 2.0" = "GERUD", "GERUD 2.0 & COLONY" = "GERUD & COLONY", "Allele counts, GERUD 2.0 & COLONY2" = "Allele counts, GERUD 2.0 & COLONY", "IR Values & KINSHIP 1.3" = "Kinship 1.3", "Kinship 1.3 + Cervus 2.0 "= "Kinship 1.3 & Cervus", "GERUD 1.0, COLONY, STORM & allele counts" = "Allele counts, COLONY, GERUD & STORM", "GERUD 2.0, CERVUS 3.0.7 and COLONY" = "CERVUS, COLONY, & GERUD", "Allele counts, GERUD 2.0, COLONY" = "Allele counts, COLONY & GERUD", "GERUD & COLONY" = "COLONY & GERUD", "Allele counts, GERUD 2.0 & COLONY" = "Allele counts, COLONY & GERUD", "Sequoia, COLONY, dartR*" = "COLONY, dartR, & Sequoia", "KINFERENCE" = "Kinference", "COLONY2" = "COLONY", "COLONY v.2." = "COLONY", "MLRELATE, COLONY v1.2, KINGROUP 1" = "ML-Relate, COLONY & KinGroup", "KinGroup  " = "KinGroup")

estimator <- data %>% 
  group_by(Kinship.Method, Focus)%>%
  count(Kinship.Method, sort= T)

print(estimator, n = 45)

order <- estimator$n

## Do it this shitty way until I remember the r solution

colony <- 11+4+4+3+2+2+2+10
colony

(38/74)*100

AC <- 8+6+4+1+1
AC
(AC/74)*100


GER <- 8+4+4+4+3+1+1+1
GER
(GER/74)*100

COA <- 2+2+1+1+1+1+1
COA

(COA/74)*100

MLR <- 5
(MLR/74)*100

CERV <- 

## Calculate %
estimator <- data.frame(estimator)
estimator <- estimator %>% mutate(SUM=sum(n),
                        Percent=n/SUM*100) 

estimator$Percent <- round(estimator$Percent, 2)

estimator

est_plot <- 
ggplot(estimator, 
       aes(forcats::fct_reorder(Kinship.Method, -n, sum), n, fill = Focus)) +
  geom_col(colour="black") + 
  xlab("Estimator") +
  ylab("No. Papers") +
  scale_fill_brewer(palette = "GnBu") +
  scale_y_continuous(breaks = 1:12) + 
  ggtitle("Summary of estimators used by each research focus") + 
  theme_classic()

est_plot + theme(axis.text.x = element_text(angle = 90), 
                 plot.title = element_text(hjust = 0.5, size = 16)) 


print(estimator, n = 31)
## No _ estimator 

no_estimators <- data %>% 
  group_by(No..analyses)%>%
  count(No..analyses, sort= T)

print(no_estimators, n = 45)

## Calculate %
no_estimators <- data.frame(no_estimators)

no_estimators <- no_estimators %>% mutate(SUM=sum(n),
                                  Percent=n/SUM*100) 

no_estimators$Percent <- round(no_estimators$Percent, 2)

no_estimators

##------------------------- 

all_order <- data %>% 
  group_by(Order, Species)%>% #add Focus back in SAM
  count(Order)

print(all_order,n = 60)

####Order

all_fams <- data %>% 
  group_by(Family)%>% #add Focus back in SAM
  count(Family)


all_fams$Carch <- ifelse(all_fams$Family == "Carcharhinidae", "Carcharhinidae", "Other")

all_fams$Carch

print(all_fams, n = 100)

sp_plot <- ggplot(all_fams, 
         aes(forcats::fct_reorder(Family, -n, sum), n, fill = Focus)) +
  geom_col(colour="black") + 
  xlab("Species") +
  ylab("No. Papers") +
  scale_fill_brewer(palette = "GnBu") +
  scale_y_continuous(limits = c(0, 40), n.breaks = 20) + 
  ggtitle("Species studied in relation to research focus") + 
  theme_classic()

sp_plot + theme(axis.text.x = element_text(angle = 90), 
                 plot.title = element_text(hjust = 0.5, size = 16)) 

## Try the Carch vs Other plot 

sp_plot <- ggplot(all_fams, 
                  aes(forcats::fct_reorder(Carch, n, sum), n, fill = Focus)) +
  geom_col() + 
  xlab("Species") +
  ylab("No. Papers") +
  scale_fill_brewer(palette = "RdYlGn") +
  scale_y_continuous(limits = c(0, 48), n.breaks = 24) + 
  ggtitle("Species studied in relation to research focus") + 
  theme_classic()

sp_plot + theme(axis.text = element_text(size = 12), 
                axis.title = element_text(size = 13.5),
                legend.text = element_text(size = 12), 
                legend.title = element_text(size = 12), 
                plot.title = element_text(hjust = 0.5, size = 16)) 


d <- c(16,8,10, 11,7)
sd(d)
mean(d)

## Markers

## Calculation for the review document 

marker_rep <- data.frame(data %>% 
                       group_by(Markers)%>%
                       count(Markers))


print(marker_rep)

msat_perc <- (marker_rep[1,2]/sum(marker_rep$n))*100
msat_perc

snp_perc <- (marker_rep[2,2]/sum(marker_rep$n))*100
snp_perc

## Lets calculate the avergae number of mSats and SNPs


## Now lets make sme nice plots...

data$Markers <- recode_factor(data$Markers, "mSats + mtDNA " = "mSats + mtDNA", "SNPS" = "SNPs", "mSats + mtDna_genome" = "mSats + mtDNA", "SNPs + mtDNA_genome" = "SNPs + mtDNA", "SNPs + mtDNA " = "SNPs + mtDNA")

marker <- data.frame(data %>% 
  group_by(Markers, Focus)%>%
  count(Markers))

marker

marker_plot <- ggplot(marker, 
                  aes(forcats::fct_reorder(Markers, -n, sum), n, fill = Focus)) +
  geom_col() + 
  xlab("Marker") +
  ylab("No. Papers") +
  scale_fill_brewer(palette = "RdYlGn") +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 5)) + 
  ggtitle("Markers used in relation to research focus") + 
  theme_classic()

marker_plot + theme(axis.text = element_text(size = 12), 
                    axis.title = element_text(size = 13.5),
                    legend.title = element_text(size = 12.5), 
                    legend.text = element_text(size = 11),
                plot.title = element_text(hjust = 0.5, size = 16)) 

## Now we claculate % to 2 DP for everything...

marker2 <- marker %>% mutate(SUM=sum(n),
                        Percent=n/SUM*100) 

marker2$Percent <- round(marker2$Percent, 2)

marker2

## Number of markers 
no_marker <- data %>% 
  group_by(No..Marker.Types, Focus)%>%
  count(No..Marker.Types, sort= T)

print(no_marker, n = 45)

## Calculate %
no_marker <- data.frame(no_marker)

no_marker <- no_marker %>% mutate(SUM=sum(n),
                                  Percent=n/SUM*100) 

no_marker$Percent <- round(no_marker$Percent, 2)

no_marker$No..Marker.Types <- as.factor(no_marker$No..Marker.Types)

nomarker_plot <- ggplot(no_marker, 
                      aes(forcats::fct_reorder( No..Marker.Types, -n, sum), n, fill = Focus)) +
  geom_col(colour="black") + 
  xlab("No. Marker Types Used") +
  ylab("No. Papers") +
  scale_fill_brewer(palette = "BuGn") +
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, by = 5)) + 
  ggtitle("Number of marker types used in relation to research focus") + 
  theme_classic()

nomarker_plot + theme(, 
                    plot.title = element_text(hjust = 0.5, size = 16))

## -----------------------------------------------

snps <- data %>%
 select(No..SNPs)

mean(snps$No..SNPs, na.rm = T)
summary(snps, na.rm = T)
sd(snps$No..SNPs, na.rm = T)


snps 

levels(data$Markers)
## Need to work out what year the SNP papers were published

snp_year <- data %>%
  group_by(Markers, Year)%>%
  count(Year, sort= T) %>%
  filter(Markers != "mSats", Markers != "mSats + mtDNA") %>%
  na.omit()

snp_year

##-----------------------------------------------------------------------------

## Microsat nitty gritty

microsat <- data %>%
  group_by(No..mSats) %>%
  count(No..mSats) %>%
  na.omit()

microsat

msat_plot <- ggplot(data = microsat, aes(x = No..mSats, y = Focus, fill = Focus)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, height = 0.2, size = 2) +
  scale_fill_brewer(palette = "BuGn")+
  ggtitle("Number of mSat loci used in studies of relatedness") +
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 26, by = 4)) +
  xlab("Number of mSat loci") +
  theme_classic()


msat_plot <- msat_plot + theme(
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(hjust = 0.5, size = 16))

print(msat_plot)

## No samples used 

samples <- data %>% 
  group_by(n_samples, Focus) %>% 
  count(n_samples) %>%
  na.omit()

summary(samples)

samples$n_samples <- as.numeric(as.character(samples$n_samples))

n_samples <- ggplot(data = samples, aes(x = n_samples, y = Focus, fill = Focus)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, height = 0.2, size = 2.5, alpha = 0.7) +
  scale_fill_brewer(palette = "BuGn")+
  ggtitle("Sample sizes of studies investigating relatedness") +
  scale_x_continuous(limits = c(0, 2000), breaks = seq(0, 2000, by = 100)) +
  xlab("Number of Individuals") +
  geom_vline(xintercept = 50, linetype = 5, linewidth = 1, col = "red") +
  theme_classic()


n_samples <- n_samples + theme(axis.text.x = element_text(angle = 90),
  axis.text = element_text(size = 12),
  axis.title = element_text(size = 14),
  plot.title = element_text(hjust = 0.5, size = 18))

print(n_samples)

##------------------------------------------------------------------------------

install.packages("egg")
install.packages("cowplot")

library(egg)
library(cowplot)

## Marker power plot 

Marker_PWR <- data %>%
  select(Power_comp, Year, Markers)

Marker_PWR

PWR_comp <- 
  ggplot(data = Marker_PWR, aes(x = Year, y = Power_comp, shape = Markers)) + 
  geom_point(stat = "identity", size = 3, aes(col = Markers, alpha = 0.7)) +
  scale_colour_brewer(palette = "Dark2") +
  ggtitle("Comparison of Marker Power between mSats and SNPs") +
  scale_x_continuous(limits = c(2002, 2024), breaks = seq(2002, 2024, by = 2)) +
  scale_y_continuous(limits = c(0, 9000), breaks = seq(0, 9000, by = 750)) +
  xlab("Year") +
  ylab("Power in terms of no. of SNPs") +
  theme_bw()

PWR_comp <- PWR_comp + theme(
  plot.title = element_text(size = 16, hjust = 0.5), 
  axis.title = element_text(size= 13), 
  axis.text = element_text(size = 12), 
  legend.title = element_text(size = 12.5), 
  legend.text = element_text(size = 11.5)
)

print(PWR_comp)

## Now the inset plot for the mSat pattern 

PWR_comp2 <- 
  ggplot(data = Marker_PWR, aes(x = Year, y = Power_comp, shape = Markers, alpha = 0.7)) + 
  geom_point(stat = "identity", size = 3, aes(col = Markers)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_continuous(limits = c(2002, 2024), breaks = seq(2002, 2024, by = 4)) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 25)) +
  theme_bw()

PWR_comp2 <- PWR_comp2 + theme(
  plot.title = element_blank(), 
  axis.title = element_blank(), 
  axis.text = element_text(size = 8, angle = 0),
  legend.position = "none"
) 

print(PWR_comp2)

plot.with.inset <-
  ggdraw() +
  draw_plot(PWR_comp) +
  draw_plot(PWR_comp2, x = 0.07, y = 0.52, width = .5, height = .42)

print(plot.with.inset)

## ----------------------------------------------------------------------------

## How many estimators did each study use? 
## Of those with > 1, did they use a diverse range i.e. Ped and Mark? 

colnames(data)

estimator_type <- data %>% 
  group_by(No..analyses, Method_PedvsMark) %>%
  count(No..analyses)

estimator_type

## some lazy calculations 

estimator_type$Two <- ifelse(estimator_type$No..analyses > 1, "T", "F")

estimator_type %>%
  group_by(Two)

tapply(estimator_type$n, estimator_type$Two, FUN=sum)
tapply(estimator_type$n, estimator_type$Method_PedvsMark, FUN=sum)

