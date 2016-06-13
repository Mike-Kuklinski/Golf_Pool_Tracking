# Project: Boomer's Pool - Golf Major Tracking
# Author: Mike Kuklinski
# Date: 5/20/16
# Description: Functions to be used in the shiny app for tracking major and pool standings

#setwd('~/R Scripts/Boomer Pool/')

# Load Libraries
library(dplyr)
library(plyr)
library(stringr)
library(httr)
library(XML)
library(optimx)
library(reshape2)
library(foreach)


# Create Multiplier Table
mult_tbl <- data.frame('num_plyr' = c(1,2,3,4,5), 'multplyr' = c(5,2.5,1.7,1.3,1))

# ==============================================================================
# Re-Load Necessary Info
# ==============================================================================

# Needed information for new tournament
#entry_list
#major <- 'Masters'
#year <- 2016
#id_num <- 2493
#id_num <- 2241
#payouts <- read.csv('Data/Payout.csv', header = T, stringsAsFactors = F)
#entries <- read.csv('Data/revised_entries.csv', header = T, stringsAsFactors = F)
#entry_combinations <- read.csv('Data/entry_combinations.csv', header = T, stringsAsFactors = F)

# ==============================================================================
# Update Player Positions and Projected Winnings
# ==============================================================================

# Function to get updated player positions and provide projected winnings
update_trny_ranks <- function(id_num, payout){
    cur_trny_pos <- suppressWarnings(get_trny_pos(id_num))
    cur_money <- get_trny_pos_money(cur_trny_pos$Position, payout)
    trny_ranks <- left_join(cur_trny_pos, cur_money, by = c('Position' = 'position_list'))
    trny_ranks
}

#===========================HELPER FUNCTIONS====================================

# Function which returns current positions of players given a tournament id_num
get_trny_pos <- function(id_num){
    # Get html
    url <- paste('http://espn.go.com/golf/leaderboard?tournamentId=', id_num, sep = '')
    html_text <- GET(url)
    html_text <- content(html_text, as = 'text')
    parsed_html <- htmlParse(html_text, asText = TRUE)
    # Extract table statistics and header
    pos_header <- xpathSApply(parsed_html, '//*[(@id = "regular-leaderboard")]//th', xmlValue)
    pos_header <- make.names(pos_header)
    pos_table <- xpathSApply(parsed_html, '//*[(@id = "regular-leaderboard")]//td', xmlValue)
    # Get finish results
    # Check for and remove projected cut and cut information
    if(length(grep('Cut:', pos_table)) > 0){
        cut_idx <- grep('Cut:', pos_table)
        proj_cut <- pos_table[cut_idx]
        proj_cut <- unlist(str_split(proj_cut, ': '))
        proj_cut_stroke <- proj_cut[2]
        pos_table <- pos_table[-cut_idx]
    }else{
        cut_idx <- NA
    }
    # Form table
    pos_table <- data.frame(matrix(data = pos_table, 
                                   ncol = length(pos_header), 
                                   byrow = T), 
                            stringsAsFactors = F)
    names(pos_table) <- pos_header
    # Check is tournament is ongoing and Adjust THRU stat
    if("THRU" %in% pos_header == F){
        trn_ongoing <- F
        pos_table$THRU <- 72
        day_fin <- 'F'
    }else{
        trn_ongoing <- T
        day_fin <- 0
    }
    # Subset Table
    pos_table <- pos_table[,c('PLAYER',
                              'POS',
                              'THRU',
                              'TO.PAR',
                              'R1',
                              'R2',
                              'R3',
                              'R4',
                              'TOT')]
    # Rename variables
    names(pos_table) <- c('Player', 
                          'Position', 
                          'THRU', 
                          'To Par', 
                          'R1', 
                          'R2', 
                          'R3', 
                          'R4', 
                          'Total')
    # Adjust variables
    pos_table$Position <- as.factor(pos_table$Position)
    pos_table$`Sort Pos` <- as.numeric(gsub("^T",'', pos_table$Position))
    pos_table$Day_THRU <- day_fin
    # If tournament is ongoing, adjust THRU information to account for rounds played
    if(trn_ongoing){
        for(idx in 1:nrow(pos_table)){
            rnd_chk <- pos_table[idx, c('To Par', 'THRU', 'R1', 'R2', 'R3', 'R4')]
            cur_rnd <- rnd_chk[,'THRU']
            CUT_chk <- ('CUT' %in% rnd_chk[,c('To Par', 'THRU')] || 'WD' %in% rnd_chk[,'THRU'])
            # Check if current round complete
            if(grepl("F|WD|AM|PM|CUT", cur_rnd)){cur_rnd <- 0}
            Round_lnth <- length(grep('[0-9]', rnd_chk[,c(3:ncol(rnd_chk))]))
            aggr_fin <- Round_lnth*18 + as.integer(cur_rnd)
            if(Round_lnth*18 == aggr_fin){
                day_fin <- 'F'
            }else{
                day_fin <- aggr_fin %% 18}
            # If player is CUT, adjust `To Par` 
            if(CUT_chk){pos_table[idx, 'To Par'] <- 'CUT'}
            pos_table[idx, 'THRU'] <- as.character(aggr_fin)
            pos_table[idx, 'Day_THRU'] <- as.character(day_fin)
        }
    }
    # Re-sort Table
    pos_table <- pos_table[,c('Player',
                              'Position',
                              'Sort Pos', 
                              'Day_THRU',
                              'To Par',
                              'R1',
                              'R2',
                              'R3',
                              'R4',
                              'THRU',
                              'Total')]
    pos_table
}



# Function to calculate projected winnings based on positions, including ties
get_trny_pos_money <- function(position_list, payout){
    pos_money <- data.frame(table(position_list))
    pos_money$adj_pos <- suppressWarnings(as.numeric(gsub('^T', '', pos_money$position_list)))
    for(idx in 1:nrow(pos_money)){
        pos_freq <- pos_money$Freq[idx]
        pos <- pos_money$adj_pos[idx]
        if(!is.na(pos) && pos_freq > 1){
            moneys <- subset(payout, Rank >= pos & Rank < (pos+pos_freq))
            moneys <- mean(moneys[,'Payout'])
            if(is.nan(moneys)){moneys <- 0}
        }else if(is.na(pos)){
            moneys <- 0
        }else {
            if(pos %in% payout$Rank){
                moneys <- payout[which(payout$Rank == pos),'Payout']
            }else{
                moneys <- 0
            }
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
    trny_ranks <- trny_ranks[,c("Player", "Position", "To Par", "proj_cash")]
    pool_ranks <- left_join(pool_entries, trny_ranks, by = c("Player1" = "Player"))
    pool_ranks <- hid_update_names(pool_ranks, 1)
    pool_ranks <- left_join(pool_ranks, trny_ranks, by = c("Player2" = "Player"))
    pool_ranks <- hid_update_names(pool_ranks, 2)
    pool_ranks <- left_join(pool_ranks, trny_ranks, by = c("Player3" = "Player"))
    pool_ranks <- hid_update_names(pool_ranks, 3)
    pool_ranks <- left_join(pool_ranks, trny_ranks, by = c("Player4" = "Player"))
    pool_ranks <- hid_update_names(pool_ranks, 4)
    pool_ranks <- left_join(pool_ranks, trny_ranks, by = c("Player5" = "Player"))
    pool_ranks <- hid_update_names(pool_ranks, 5)
    pool_ranks <- left_join(pool_ranks, trny_ranks, by = c("TieBreak1" = "Player"))
    pool_ranks <- hid_update_names(pool_ranks, 'TB')
    # Add Sum of player payouts for each entry
    pool_ranks[is.na(pool_ranks)] <- 0
    pool_ranks <- mutate(pool_ranks, 
                         Total_proj_cash = Plyr1_proj_cash +
                             Plyr2_proj_cash + 
                             Plyr3_proj_cash + 
                             Plyr4_proj_cash +
                             Plyr5_proj_cash)
    # Adjust tiebreaker to be a normalized decimal
    
    pool_ranks$Entry_Rank <- rank(-apply(X = pool_ranks[,c('Total_proj_cash', 'PlyrTB_proj_cash')],
                                         MARGIN = 1, 
                                         FUN = function(r){r[1] + adj_TB(r[2], 10)}), 
                                  ties.method = 'min')
    # Re-order pool standings
    pool_ranks <- pool_ranks[order(pool_ranks$Entry_Rank),]
    # Re-order Columns
    pool_ranks <- pool_ranks[,c("EntryName",
                                "Entry_Rank",
                                "Total_proj_cash",
                                "TieBreak1",
                                "PlyrTB_proj_cash",
                                "TieBreak2",
                                "Player1",
                                "Plyr1_Pos",
                                "Plyr1_To Par",
                                "Plyr1_proj_cash",
                                "Player2",
                                "Plyr2_Pos",
                                "Plyr2_To Par",
                                "Plyr2_proj_cash",
                                "Player3",
                                "Plyr3_Pos",
                                "Plyr3_To Par",
                                "Plyr3_proj_cash",
                                "Player4",
                                "Plyr4_Pos",
                                "Plyr4_To Par",
                                "Plyr4_proj_cash",
                                "Player5",
                                "Plyr5_Pos",
                                "Plyr5_To Par",
                                "Plyr5_proj_cash")]
    # Re-name pool ranks
    names(pool_ranks) <- c('Entry', 'Rank', 'Total Money', 
                           'Tie.1', 'Tie.1 Money', 'Tie.2', 
                           'Player.1', 'Player.1 Pos',  'Player.1 Stroke', 'Player.1 Money',
                           'Player.2', 'Player.2 Pos',  'Player.2 Stroke', 'Player.2 Money', 
                           'Player.3', 'Player.3 Pos',  'Player.3 Stroke', 'Player.3 Money',
                           'Player.4', 'Player.4 Pos',  'Player.4 Stroke', 'Player.4 Money', 
                           'Player.5', 'Player.5 Pos',  'Player.5 Stroke', 'Player.5 Money')
    pool_ranks[is.na(pool_ranks)] <- as.factor('-')
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

# Function which takes a number and creates a decimal value of length n
adj_TB <- function(TB_num, n){
    as.numeric(paste('.', paste(rep(0, n-nchar(TB_num)), collapse = ''), 
                     TB_num, 
                     sep = ''))
}

# ==============================================================================
# Get Best Case / Optimized Scenario for a given entry Functions
# ==============================================================================

# Function which returns the best case scenario for a given entry
optimize_pool_entry <- function(entry_name, id_num, lb = 3, ub = 3, est_bound = F){
    # Clean up initial player positions    
    cur_trny_pos <- suppressWarnings(get_trny_pos(id_num))
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
        new_trny_ranks$Pos_Moved <- new_trny_ranks$`Sort Pos` - new_trny_ranks$Position
        new_trny_ranks <- new_trny_ranks[,c('Player', 
                                            'Position', 
                                            'Pos_Moved',
                                            'Sort Pos',
                                            'proj_cash')]
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
    combos$num_plyr <- str_count(combos$`Player Combinations`, '/') + 1
    combos <- left_join(combos, mult_tbl, by = 'num_plyr')
    combos <- mutate(combos, Score = round(multplyr/(num_plyr + `Number of Entries with Combo`),2))
    combos <- combos[order(combos$Score, decreasing = T),]
    combos
}

# ==============================================================================
# Estimate Payouts 
# ==============================================================================

# Testing values
#id_num <- 1013 #US Open 2012
#old_purse <- 8000000

# Function which returns current positions of players given a tournament id_num
create_payout <- function(id_num, old_purse){
    # Get html
    url <- paste('http://espn.go.com/golf/leaderboard?tournamentId=', id_num, sep = '')
    html_text <- GET(url)
    html_text <- content(html_text, as = 'text')
    parsed_html <- htmlParse(html_text, asText = TRUE)
    # Extract table statistics and header
    pos_header <- xpathSApply(parsed_html, '//*[(@id = "regular-leaderboard")]//th', xmlValue)
    pos_header <- make.names(pos_header)
    pos_table <- xpathSApply(parsed_html, '//*[(@id = "regular-leaderboard")]//td', xmlValue)
    # Form table
    pos_table <- data.frame(matrix(data = pos_table, 
                                   ncol = length(pos_header), 
                                   byrow = T), 
                            stringsAsFactors = F)
    names(pos_table) <- pos_header
    # Subset Table
    pos_table <- pos_table[,c('POS','EARNINGS')]
    # Adjust variables
    pos_table$POS <- as.numeric(gsub("^T", '', pos_table$POS))
    pos_table$EARNINGS <- as.numeric(gsub("[[:punct:]]",'', pos_table$EARNINGS))
    pos_table <- na.exclude(pos_table)
    pos_table <- subset(pos_table, EARNINGS != 0 & POS >= 10)
    con_pos_table <- count(pos_table)
    con_pos_table$freq <- con_pos_table$freq - 1
    con_pos_table$adj_POS <- con_pos_table$POS + con_pos_table$freq/2
    # Create regresssion line
    per_table <- con_pos_table
    per_table$EARNINGS <- per_table$EARNINGS/old_purse
    per_table <- rbind(data.frame(POS = 10, EARNINGS = .027, freq = 1, adj_POS = 10), per_table)
    per_reg <- lm(EARNINGS ~ poly(adj_POS, 6, raw = T), data = per_table)
    fit_points <- data.frame('fit' = per_reg$fitted.values)
    #g <- ggplot(data = per_table, aes(y = EARNINGS, x = adj_POS)) + geom_point()
    #g <- g + geom_point(data = fit_points, aes(y = fit, x = per_table$adj_POS), colour = 'red')
    # Predict continous values for all positions
    pred_fit <- predict(per_reg, newdata = data.frame('adj_POS' = 10:69))
    #g <- g + geom_point(data = data.frame('new' = pred_fit), aes(y = new, x = 10:69), colour = 'green')
    #g
    # Join known percentages for first 10 places
    finish_perc <- c(c(.18, .1080, .068,.048,.04,.036, .0335,.031,.029,.027), pred_fit[-1])
    names(finish_perc) <- NULL
    payout_perc <- data.frame('Rank' = 1:length(finish_perc), 'Payout Perc' = finish_perc)
    write.csv(payout_perc, 'Data/payout_perc.csv', row.names = F)
}

