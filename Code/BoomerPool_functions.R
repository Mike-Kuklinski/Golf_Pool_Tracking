

# Project: Boomer's Pool - Golf Major Tracking
# Title: Project Functions
# Major: 2016 Masters
# Author: Mike Kuklinski
# Date: 5/20/16
# Description: Functions to be used in the shiny app

#setwd('~/R Scripts/Boomer Pool/')

# Load Libraries
library(dplyr)
library(stringr)
library(httr)
library(XML)
library(optimx)
library(reshape2)
library(foreach)

# ==============================================================================
# Re-Load Necessary Info
# ==============================================================================

payouts <- read.csv('Data/Payout.csv', header = T, stringsAsFactors = F)
entries <- read.csv('Data/revised_entries.csv', header = T, stringsAsFactors = F)
id <- 2493
entry_combinations <- read.csv('Data/entry_combinations.csv', header = T, stringsAsFactors = F)


display_idx <- list('Keep' = c(1,2,3,7,11,15,19,23),
                    'Ties' = c(4,5,6),
                    'Projected Cash' = c(10,14,18,22,26),
                    'Current Positions' = c(8,12,16,20,24),
                    'Current Strokes' = c(9,13,17,21,25)) 


# ==============================================================================
# Update Player Positions and Projected Winnings
# ==============================================================================

# Function to get updated player positions and provide projected winnings
update_trny_ranks <- function(id, payout){
    cur_trny_pos <- suppressWarnings(get_trny_pos(id))
    cur_money <- get_trny_pos_money(cur_trny_pos$Position, payouts)
    trny_ranks <- left_join(cur_trny_pos, cur_money, by = c('Position' = 'position_list'))
    trny_ranks
}

#===========================HELPER FUNCTIONS====================================


# Function which returns current positions of players given a tournament id
get_trny_pos <- function(id){
    # Get html
    url <- paste('http://espn.go.com/golf/leaderboard?tournamentId=', id, sep = '')
    html_text <- GET(url)
    html_text <- content(html_text, as = 'text')
    parsed_html <- htmlParse(html_text, asText = TRUE)
    # Extract table statistics
    pos_table <- xpathSApply(parsed_html, '//*[(@id = "regular-leaderboard")]//td', xmlValue)
    # Get header titles
    headers <- c('Position', 'skip', 'PLAYER_NAME', 'To Par', 'R1', 'R2', 'R3', 'R4', 'Total', 'MONEY', 'FEDEX PTS')
    # Get finish results
    pos_table <- data.frame(matrix(data = pos_table, ncol = length(headers), byrow = T), stringsAsFactors = F)
    names(pos_table) <- headers
    # Clean up column classes
    for(idx in 5:9){
        pos_table[,idx] <- as.numeric(pos_table[,idx])
    }
    pos_table$Position <- as.factor(pos_table$Position)
    pos_table$Sort_Pos <- as.numeric(gsub("^T",'', pos_table$Position))
    pos_table <- pos_table[,c(3,1,12,4,5,6,7,8,9,10)]
    pos_table$THRU <- 36
    pos_table
}


# Function to calculate projected winnings based on positions, including ties
get_trny_pos_money <- function(position_list, payout){
    pos_money <- data.frame(table(position_list))
    pos_money$adj_pos <- suppressWarnings(as.numeric(gsub('^T', '', pos_money$position_list)))
    for(idx in 1:nrow(pos_money)){
        #idx <- 6
        pos_freq <- pos_money$Freq[idx]
        pos <- pos_money$adj_pos[idx]
        if(!is.na(pos_money$adj_pos[idx]) && 
            pos_freq > 1){
            moneys <- subset(payout, Rank >= pos & Rank < (pos+pos_freq))
            moneys <- mean(moneys[,2])
        }else if(is.na(pos)){
            moneys <- 0
        }else {moneys <- payout[which(payout$Rank == pos),2]
        }
        pos_money$proj_cash[idx] <- as.integer(round(moneys,-2))
    }
    pos_money$position_list <- as.factor(pos_money$position_list)
    pos_money
}


# ==============================================================================
# Update Entry Standings
# ==============================================================================

# Takes a list of entries and aggregates the project winnings for the players
update_pool_ranks <- function(trny_ranks, pool_entries){
    # Join in Project Money for each player
    trny_ranks <- trny_ranks[,c("PLAYER_NAME", "Position", "To Par", "proj_cash")]
    pool_ranks <- left_join(pool_entries, trny_ranks, by = c("Player1" = "PLAYER_NAME"))
    pool_ranks <- hid_update_names(pool_ranks, 1)
    pool_ranks <- left_join(pool_ranks, trny_ranks, by = c("Player2" = "PLAYER_NAME"))
    pool_ranks <- hid_update_names(pool_ranks, 2)
    pool_ranks <- left_join(pool_ranks, trny_ranks, by = c("Player3" = "PLAYER_NAME"))
    pool_ranks <- hid_update_names(pool_ranks, 3)
    pool_ranks <- left_join(pool_ranks, trny_ranks, by = c("Player4" = "PLAYER_NAME"))
    pool_ranks <- hid_update_names(pool_ranks, 4)
    pool_ranks <- left_join(pool_ranks, trny_ranks, by = c("Player5" = "PLAYER_NAME"))
    pool_ranks <- hid_update_names(pool_ranks, 5)
    pool_ranks <- left_join(pool_ranks, trny_ranks, by = c("TieBreak1" = "PLAYER_NAME"))
    pool_ranks <- hid_update_names(pool_ranks, 'TB')
    pool_ranks <- pool_ranks[,c(1,2,9,10,11,3,12,13,14,4,15,16,17,5,18,19,20,6,21,22,23,7,24,25,26,8)]
    pool_ranks[is.na(pool_ranks)] <- 0
    pool_ranks <- mutate(pool_ranks, Total_proj_cash = Plyr1_proj_cash +
                             Plyr2_proj_cash + Plyr3_proj_cash + Plyr4_proj_cash +
                             Plyr5_proj_cash)
    pool_ranks <- pool_ranks[order(pool_ranks$Total_proj_cash, 
                                   pool_ranks$PlyrTB_proj_cash, 
                                   decreasing = T),
                             c(1,27,25,26, 2:24)]
    pool_ranks$Entry_Rank <- 1:nrow(pool_ranks)
    #pool_ranks2 <- pool_ranks[,c(1,28,2,25,3,4,5,8,9,12,13,16,17,20,21,24)]
    pool_ranks <- pool_ranks[,c(1,28,2,25,3,4,5:24)]
    names(pool_ranks) <- c('Entry', 'Rank', 'Total Money', 
                           'Tie.1', 'Tie.1 Money', 'Tie.2', 
                           'Player.1', 'Player.1 Pos',  'Player.1 Stroke', 'Player.1 Money',
                           'Player.2', 'Player.2 Pos',  'Player.2 Stroke', 'Player.2 Money', 
                           'Player.3', 'Player.3 Pos',  'Player.3 Stroke', 'Player.3 Money',
                           'Player.4', 'Player.4 Pos',  'Player.4 Stroke', 'Player.4 Money', 
                           'Player.5', 'Player.5 Pos',  'Player.5 Stroke', 'Player.5 Money')
    #class(pool_ranks$PlyrTB_proj_cash) <- c('money', class(pool_ranks$PlyrTB_proj_cash))
    pool_ranks 
}


#===========================HELPER FUNCTIONS====================================

# Function which updates names of df after join table
hid_update_names <- function(df, pnum){
    temp_names <- names(df)
    adj_names <- mapply(function(x){paste('Plyr',pnum,x,sep = '')}, c('_Pos', '_To Par', '_proj_cash'))
    temp_names[(length(temp_names)-2):length(temp_names)] <- adj_names
    names(df) <- temp_names
    df
}


# ==============================================================================
# Get Best Case / Optimized Scenario for a given entry Functions
# ==============================================================================

# Function which returns the best case scenario for a given entry
optimize_pool_entry <- function(entry_name, id, lb = 3, ub = 3, est_bound = F){
    # Clean up initial player positions    
    cur_trny_pos <- suppressWarnings(get_trny_pos(id))
    cur_trny_pos <- subset(cur_trny_pos, `To Par` != 'CUT')
    cur_trny_strokes <- as.integer(gsub('E', '0', cur_trny_pos$`To Par`))
    # Establish parameters for optimization function
    stroke_delta <- rep(0, length(cur_trny_strokes))
    # Determine upper and low bounds based on holes through
    if(est_bound){
        lower <- -round((lb/18) * (72 - cur_trny_pos$THRU),0)
        upper <- round((ub/18) * (72 - cur_trny_pos$THRU),0)
    }else{
        lower <- rep(-lb,length(stroke_delta))
        upper <- rep(ub,length(stroke_delta))
    }
    # Check to make sure the tournament is in progress and optimize
    if(sum(lower) == 0 && sum(upper) == 0 ){
        message('Tournament Complete, nothing returned')
        optim_list <- NULL
    }else{
        optim_info <- optimx(stroke_delta, 
                             simulate_pool_change, 
                             cur_trny_pos = cur_trny_pos,
                             cur_trny_strokes = cur_trny_strokes, 
                             entry_name = entry_name,
                             method = 'nmkb', 
                             lower = lower,
                             upper = upper)
        # Record best rank
        best_optim1 <- data.frame(optim_info)
        best_rank1 <- best_optim1$value
        # Rerun optimization to see if better result can be found
        new_strokes <- as.integer(round(best_optim1[, c(1:length(stroke_delta))]))
        lower <- lower - new_strokes
        upper <- upper - new_strokes
        new_trny_ranks <- simulate_trny_pos_change(new_strokes, cur_trny_pos, cur_trny_strokes)
        new_trny_ranks <- new_trny_ranks[, names(cur_trny_pos)]
        new_trny_ranks$Position <- as.factor(new_trny_ranks$Position)
        optim_info2 <- optimx(stroke_delta, 
                             simulate_pool_change, 
                             cur_trny_pos = new_trny_ranks,
                             cur_trny_strokes = new_trny_ranks$`To Par`, 
                             entry_name = entry_name,
                             method = c('spg', 'nmkb'),
                             lower = lower,
                             upper = upper)
        # Record best rank
        best_optim2 <- data.frame(optim_info2)
        best_rank_idx2 <- which(best_optim2$value == min(best_optim2$value))[1]
        best_rank2 <- best_optim2$value[best_rank_idx2]
        # Compare ranks and take the optimal
        if(best_rank2 < best_rank1){
            new_strokes <- new_strokes + as.integer(round(best_optim2[best_rank_idx2, c(1:length(stroke_delta))]))
            optim_info <- optim_info2
        }
        # Run optimal case and return player positions and pool rankings
        new_trny_ranks <- simulate_trny_pos_change(new_strokes, cur_trny_pos, cur_trny_strokes)
        new_pool_ranks <- update_pool_ranks(new_trny_ranks, entries)
        new_trny_ranks$Pos_Moved <- new_trny_ranks$Sort_Pos - new_trny_ranks$Position
        new_trny_ranks <- new_trny_ranks[,c(1,2,15,3,14)]
        names(new_trny_ranks) <- c('PLAYER NAME', 'Best Case Position', 'Positions Moved', 'Current Position', 'Best Case Cash')
        new_trny_ranks <- new_trny_ranks[order(new_trny_ranks$`Best Case Position`),]
        new_entry_rank <- simulate_pool_change(new_strokes, cur_trny_pos, cur_trny_strokes, entry_name)
        optim_list <- list(new_strokes = new_strokes, 
             new_trny_ranks = new_trny_ranks,
             new_pool_ranks = new_pool_ranks,
             new_entry_rank = new_entry_rank,
             optim_info = optim_info)
    }
    optim_list
}


#=========================FUNCTION TO BE OPTIMIZED==============================
# Function which takes a stroke delta and determines its affect on pool entry's rank
simulate_pool_change <- function(stroke_delta, cur_trny_pos, cur_trny_strokes, entry_name){
    # Get revised standings/winnings for player based on stroke delta
    stroke_delta <- ceiling(stroke_delta)
    temp_trny_ranks <- simulate_trny_pos_change(stroke_delta, cur_trny_pos, cur_trny_strokes)
    temp_pool_ranks <- update_pool_ranks(temp_trny_ranks, entries)
    entry_pool_rank <- temp_pool_ranks$Rank[which(temp_pool_ranks$Entry == entry_name)]
    #list(entry_pool_rank = entry_pool_rank, pool_ranks = temp_pool_ranks) 
    entry_pool_rank
}

#================================HELPER FUNCTION================================

# Function which takes a list of stroke deltas and return update position list
# with new projected winnings
simulate_trny_pos_change <- function(stroke_delta, cur_trny_pos, cur_trny_strokes){
    # Change player strokes and determine new ranking
    new_strokes <- cur_trny_strokes + stroke_delta
    # Prevent two first places
    first_place <- which(new_strokes == min(new_strokes))
    if(length(first_place) > 1){
        second_place <- first_place[-1]
        new_strokes[second_place] <- new_strokes[second_place] + 1
    }
    # Determine new ranking
    new_ranks <- rank(new_strokes, ties.method = 'min')
    new_trny_pos_money <- get_trny_pos_money(new_ranks, payouts)
    # Change new_trny_pos_money rankings from factor to integer
    new_trny_pos_money$position_list <- as.numeric(levels(new_trny_pos_money$position_list))
    # Update Player Positions based on new ranks and expected winnings
    new_trny_pos <- cur_trny_pos
    new_trny_pos$Position <- as.integer(new_ranks)
    new_trny_pos$`To Par` <- new_strokes
    new_trny_ranks <- left_join(new_trny_pos, new_trny_pos_money, by = c('Position' = 'position_list'))
    new_trny_ranks
}

#undebug(optimize_pool_entry)
#t <- optimize_pool_entry('Pimp Master Flex Face', id, est_bound = T)

# ==============================================================================
# Compare Pool Entry Picks with other entries
# ==============================================================================

# Function which take an entry name and returns all combinations of players with number of identical picks
compare_entry_combos <- function(entry_name, entries, entry_combinations){
    # Get Entry Players
    entry_players <- subset(entries, EntryName == entry_name)
    entry_players <- sort(entry_players[,c(2:6)])
    combos <-foreach(idx = 1:5, .combine = rbind) %do% {
        # Get combinations of player picks
        plyr_combos <- combn(5, idx)
        foreach(idx2 = 1:ncol(plyr_combos), .combine = rbind) %do% {
            # Check number of identical combination picks
            temp_idxs <- plyr_combos[,idx2]
            return_players <- paste(entry_players[temp_idxs], collapse = ' / ')
            temp_players <- paste(entry_players[temp_idxs], collapse = ' ')
            temp_players <- paste("^",temp_players, "$", sep = '')
            combo_idx <- grep(temp_players, entry_combinations$ADJVarNames)
            temp_players_combo_cnt <- entry_combinations[combo_idx, c(2,3)]
            temp_players_combo_cnt[1,1] <- return_players
            temp_players_combo_cnt
        }
    }
    names(combos) <- c("Player Combinations", "Number of Entries with Combo")
    combos <- combos[order(combos$`Number of Entries with Combo`),]
    combos
}



