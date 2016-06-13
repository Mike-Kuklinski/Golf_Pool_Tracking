# Project: Boomer's Pool - Golf Major Tracking
# Author: Mike Kuklinski
# Date: 5/20/16
# Description: Code to clean up initial list of pool entries, payouts, and 
# player selection combinations

#setwd('~/R Scripts/Boomer Pool/')

# Load Libraries
library(dplyr)
library(stringr)
library(reshape2)

# ==============================================================================
# Re-Load Necessary Info
# ==============================================================================

#payouts <- read.csv('Data/Payout.csv', header = T, stringsAsFactors = F)
#entries <- read.csv('Data/revised_entries.csv', header = T, stringsAsFactors = F)
#id_num <- 2493
#entry_combinations <- read.csv('Data/entry_combinations.csv', header = T, stringsAsFactors = F)

# ==============================================================================
# Clean Entries
# ==============================================================================

# Load entries
#entry_list <- read.csv("Data/Boomer's 2016 Masters Pool Participants.csv", header = T, stringsAsFactors = F)
#entry_list_15 <- as.data.frame(read_excel("Data/Boomer's 2015 Masters Pool Participants.xlsx", sheet = 1))
#entry_list_16 <- as.data.frame(read_excel("Data/Boomer's 2016 Masters Pool Participants.xlsx", sheet = 1))


# Helper Function to reverse last and first names
reverse_name <- function(rev_name){
    last_first <- unlist(str_split(rev_name, pattern = ', '))
    first_last <- paste(last_first[2], last_first[1], sep = ' ')
    first_last
}

# Function which takes an entry list and cleans up the names
clean_entries <- function(entry_list){
    names(entry_list) <- c('EntryName', 
                           'Player1', 
                           'Player2', 
                           'Player3', 
                           'Player4', 
                           'Player5', 
                           'TieBreak1', 
                           'TieBreak2')
    # Reverse Names of entries
    for(col in 2:7){
        entry_list[,col] <- mapply(reverse_name, entry_list[,col])
        entry_list[,col] <- mapply(function(x){gsub("(?<=[[:alpha:]])-(?=[[:alpha:]])",
                                                    ' ', x, perl = T)}, 
                                   entry_list[,col])
    }
    # Check for and flag duplicates
    for(row_idx in 1:nrow(entry_list)){
        entry <- c(entry_list[row_idx,])
        dup_check <- duplicated(entry)
        entry_list[row_idx, dup_check] <- paste("DUPLICATE_",entry_list[row_idx, dup_check],sep = '')
    }
    entry_list
}

# ==============================================================================
# Calculate All Entry Combinations 
# ==============================================================================

# Function which takes list of entries and groups all selection combinations
entry_combos <- function(entry_list){
    # Source NAnal functions
    #source('~/R Scripts/Boomer Pool/Code/NAnal.R')
    source('Code/NAnal.R')
    # Create Pivot Table of entries
    entry_melt <- melt(entry_list, 
                       id.vars = 'EntryName', 
                       measure.vars = c('Player1',
                                        'Player2',
                                        'Player3',
                                        'Player4',
                                        'Player5')
                       )
    entry_pivot <- dcast(entry_melt, EntryName ~ value, fun.aggregate = function(x){x[1]})
    # Remove EntryName from pivot table
    entry_pivot <- entry_pivot[,-1]
    # Get combinations of players
    plyr_combos <- NAnal.reduce_comb(entry_pivot, 
                                     dup_reduce = F, 
                                     na_col_only = F)
    plyr_combos <- plyr_combos$lists
    # Subset by combinations with 5 or less selected players
    appl_combos <- foreach(idx = 1:length(plyr_combos), .combine = c) %do% {
        length(plyr_combos[[idx]]) <= 5
    }
    appl_combos <- which(appl_combos)
    plyr_combos <- plyr_combos[appl_combos]
    # Aggretates counts of each combination
    plyr_combos <- NAnal.score(entry_pivot, plyr_combos, 0)
    plyr_combos <- plyr_combos[complete.cases(plyr_combos),]
    plyr_combos$ADJVarNames <- as.character(levels(plyr_combos$ADJVarNames))
    plyr_combos
}

#entry_combinations <- entry_combos(entries)
#write.csv(entry_combinations, 'Data/entry_combinations.csv', row.names = F)  
