

# Goal: use NEON data to figure out species composition at each site
# - compare dominant species
# - determine which model might be most appropriate for TALL site


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(vegan)



# Read in NEON data -------------------------------------------------------

files <- list.files("./data/NEON_struct-woody-plant/",
                    recursive = TRUE,
                    full.names = TRUE)

files

# Just files with info about individuals

files <- grep("mappingandtagging.basic", files, value = TRUE)
files

dat <- files %>%
       map_df(~read_csv(., guess_max = 100000))


dat


# Filter to unique records by individual
dat <- dat %>%
  distinct(individualID, .keep_all = TRUE)

dim(dat)


## Turn to plot by species matrix

plot <- dat %>%
  select(plotID, taxonID) %>%
  pivot_longer(-plotID) %>%
  group_by(plotID, value) %>%
  summarise(n = length(value)) %>%
  ungroup() %>%
  rename("species" = value , "count" =  n) %>%
  pivot_wider(names_from = "species", values_from = "count")

plot[is.na(plot)] <- 0

plot

# Remove outlier plot
plot <- plot[-c(48), ]


# Filter to just species found in training dataset
sp_train <- read_csv("./data/train/Field/train_data.csv")


plot <- plot[, c("plotID", levels(factor(sp_train$taxonID)))]
dim(plot)

plot <- plot[-c(52), ]



# Run NMDS
nmds <- metaMDS(plot[, -1])

ordiplot(nmds,type="n")
orditorp(nmds,display="species",col="steelblue2",air=0.01)
 # orditorp(nmds,display="sites",cex=1.25,air=0.01)
ordihull(nmds,groups=str_sub(plot$plotID, start = 1, end = 4),
         draw="polygon",
         col="grey90",label=T)




## Sum of stems across each site
plot_sum <- plot %>%
  mutate(siteID = str_sub(plotID, start = 1, end = 4)) %>%
  select(-plotID) %>%
  select(siteID, everything()) %>%
  group_by(siteID) %>%
  summarise_all(sum)

plot_sum


## Relative frequency

plot_freq <- plot_sum
plot_freq = tibble(siteID = plot_sum$siteID, 
           bind_rows(plot_freq[1, -1] / sum(plot_freq[1, -1]),
                    plot_freq[2, -1] / sum(plot_freq[2, -1]),
                    plot_freq[3, -1] / sum(plot_freq[3, -1])))

plot_freq[, -1] <- round(plot_freq[, -1], 2)

View(plot_freq)



# Calculate shared species ------------------------------------------------


site <- dat %>%
  select(plotID, taxonID) %>%
  pivot_longer(-plotID) %>%
  mutate(site = str_sub(plotID, start = 1, end = 4)) %>%
  group_by(site) %>%
  distinct(value) %>%
  rename("species" = value) %>%
  group_by(site, species) %>%
  tally() %>%
  pivot_wider(names_from = site, values_from = n)
  
  
  group_by(plotID, value) %>%
  summarise(n = length(value)) %>%
  ungroup() %>%
  rename("species" = value , "count" =  n)

