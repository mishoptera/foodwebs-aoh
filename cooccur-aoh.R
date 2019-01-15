# Author: Misha Leong
# Date: January 2019
# Project: By room type pairwise cooccurence of AOH dataset

# *************************************************************
# FIRST THINGS FIRST
# *************************************************************
# load libraries
library(tidyverse)
library(cooccur)

# load files
load('data/aoh.Rdata') # large 7roomsFamilies dataset from Bertone PeerJ paper
load('data/core.Rdata') # list of 47 core species from Leong Sci Reports paper

# take a peek and combine into one file
head(aoh)
head(core)
aoh_core <- semi_join(aoh, core, by = "familyLong")


# *************************************************************
# FUNCTIONS
# *************************************************************
# For each room to create a presence absence matrix and run cooccur program
run_cooccur <- function (room_data, room) {
  core_matrix <- room_data %>%
    group_by(roomUnique, familyLong) %>%
    summarise(obs = n()) %>%
    spread(familyLong, obs, fill = 0) %>%
    as.data.frame() 
  rownames(core_matrix) <-  core_matrix[,1]
  core_matrix <- core_matrix[, -1]
  core_matrix <- t(core_matrix) # transpose data frame
  cooccur_room <- cooccur(mat = core_matrix, type = "spp_site", 
                          spp_names = TRUE, thresh = TRUE)
  return(cooccur_room)
}

# To pull out the siginifcantly positive pairs for each room
pos <- function (cooccur_room, i) { 
  pos_pairs <- as.tibble(cooccur_room$results) %>% 
    arrange(p_gt) %>%
    filter(p_gt < 0.05) %>%
    mutate(room = i)%>%
    select (room, sp1_name, sp2_name, everything())
  return (pos_pairs)
}

# To pull out the siginifcantly negative pairs for each room
neg <- function (cooccur_room, i) { 
  neg_pairs <- as.tibble(cooccur_room$results) %>% 
    arrange(p_lt) %>%
    filter(p_lt < 0.05) %>%
    mutate(room = i) %>%
    select (room, sp1_name, sp2_name, everything())
  return (neg_pairs)
}

# *************************************************************
# Call functions for each room and save
# *************************************************************
# calling the above fuction
room_names <- c("basement", "bath", "bed", "common", "kitchen", "attic")

# create simple ranking tables for each taxa (landcover collapsed)
lapply(room_names, function(i){
  room_results <- run_cooccur(aoh_core %>% filter (roomType == i), i)
  summary(room_results)
  plot(room_results)
  assign(paste0("posPairs_", i) , pos(room_results, i), 
         envir = .GlobalEnv)
  assign(paste0("negPairs_", i) , neg(room_results, i), 
         envir = .GlobalEnv)
})

pos_rooms <- posPairs_basement %>%
  bind_rows(posPairs_bath, posPairs_bed, posPairs_common, posPairs_kitchen, 
            posPairs_attic)
write.csv(neg_rooms, "pos_rooms.csv")

neg_rooms <- negPairs_basement %>%
  bind_rows(negPairs_bath, negPairs_bed, negPairs_common, negPairs_kitchen, 
            negPairs_attic)
write.csv(neg_rooms, "neg_rooms.csv")
