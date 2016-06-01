

# Title: Boomer's Pool - Golf Major Tracking
# Author: Mike Kuklinski
# Date: 5/20/16
# server.R file

# Source Functions

#setwd('Boomer Pool/')
source('./Code/BoomerPool_functions.R')

# load libraries
library(shiny)
# begin shiny server
shinyServer(
    function(input, output) {
        # Button Update golfer positions
        cur_trny_pos <- reactive({
            input$b_update_golfers
            get_trny_pos(id)
        })
        # Button Update golfer ranks and projected winnings
        cur_trny_ranks <- reactive({
            input$b_update_golfers
            update_trny_ranks(id)
        })
        # Update pool entry ranks
        cur_pool_ranks <- reactive({
            c_p_r <- update_pool_ranks(cur_trny_ranks(), entries)
            cur_var <- c('Total Money', 'Tie.1 Money', 'Player.1 Money', 'Player.2 Money',
                         'Player.3 Money', 'Player.4 Money', 'Player.5 Money')
            # Adjust Pool Rank Winnings to Currency Format
            for(idx in seq_along(cur_var)){
                c_p_r[,cur_var[idx]] <- prettyNum(c_p_r[,cur_var[idx]], big.mark = ',')
                c_p_r[,cur_var[idx]] <- paste('$', c_p_r[,cur_var[idx]], sep = '')
            }
            c_p_r
        })
        # Render output table showing pool entry standings
        output$o_standings <- renderDataTable({
                     # Subset data table by selected variables to show
                     disp_vars <- sort(unlist(display_idx[c('Keep', input$i_show_vars)]))
                     cur_pool_ranks()[,disp_vars]
                 },
                 options = list(
                     lengthMenu = list(c(25,50,-1), c('25','50','All')),
                     autoWidth = TRUE
                 )
        )
        # Render output table showing current golfer standings
        output$o_golfer_pos <- renderDataTable({
            cur_trny_pos()},
            options = list(
                lengthMenu = list(c(25,50,-1), c('25','50','All')),
                autoWidth = TRUE,
                columnDefs = list(list(width = '50px', targets = c(2:8)))
        ))
        # Button update list of golfers for a given entry name
        entry_players <- eventReactive(input$b_update_entry_stats, {
            # Get list of golfers for entry
            entry_players <- entries[which(entries$EntryName == input$i_entry), c(2:6)]
            # Subset golfer positions by entry golfers
            entry_player_pos <- subset(cur_trny_pos(), PLAYER_NAME %in% entry_players)
            entry_player_pos$Player_Idx <- 1:nrow(entry_player_pos)
            for(idx in c(3,5,6,7,8,9,11)){
                entry_player_pos[, idx] <- as.integer(entry_player_pos[, idx]) 
            }
            entry_player_pos[,c(12,1,2,4:11)]
        })
        # Render outlist table of golfers and respective positions for a given entry
        output$o_entry_players <- renderTable({
            entry_players()
        })
        # Button update entry's player combinations
        entry_comb_comp <- eventReactive(input$b_update_entry_stats,{
            compare_entry_combos(entry_name = input$i_entry, entries, entry_combinations)
        })
        # Render output table for entry combination comparisons given index list to display
        output$o_entry_comb <- renderDataTable({
            show_plyrs_idx <- as.integer(gsub('Player ', '', input$i_show_plyrs))
            rmv_plyrs_names <- entry_players()[-show_plyrs_idx,2]
            rmv_combo_idx <- c()
            if(length(rmv_plyrs_names) > 0){
                for(idx in seq_along(rmv_plyrs_names)){
                    name_check <- rmv_plyrs_names[idx] 
                    rmv_combo_idx <- c(rmv_combo_idx, grep(name_check, entry_comb_comp()[,1]))
                }
                rmv_combo_idx <- unique(rmv_combo_idx)
                return_comb <- entry_comb_comp()[-rmv_combo_idx,]
            }else {
                return_comb <- entry_comb_comp()
            }
            return_comb
            },
            options = list(
                paging = FALSE,
                autoWidth = F)
        )
        # Render Output text showing the current position for entry
        cur_entry_stand <- eventReactive(input$b_update_entry_stats, {
            entry_idx <- which(cur_pool_ranks()[,1] == input$i_entry)
            cur_pool_ranks()[entry_idx, 'Rank']
        })
        output$o_cur_entry_stand <- renderText({
            paste('Current Entry Standing: ',cur_entry_stand(), sep = '')
        })
        
        #### OPTIMIZATION OUTPUTS
        
        # Button update optimization information
        entry_opt <- eventReactive(input$b_optim_entry,{
            optimize_pool_entry(input$i_entry, id, est_bound = T)
        })
        # Render output table of new golfer standings 
        output$o_new_trny_ranks <- renderDataTable({
            entry_opt <- entry_opt()
            entry_opt$new_trny_ranks 
        },
        options = list(
            lengthMenu = list(c(25,50,-1), c('25','50','All')),
            autoWidth = TRUE
        ))
        # Render output table of new pool standings 
        output$o_new_pool_ranks <- renderDataTable({
            entry_opt <- entry_opt()
            disp_vars <- sort(unlist(display_idx[c('Keep','Projected Cash', 'Ties')]))
            entry_opt_pool_ranks <- entry_opt$new_pool_ranks
            cur_var <- c('Total Money', 'Tie.1 Money', 'Player.1 Money', 'Player.2 Money',
                         'Player.3 Money', 'Player.4 Money', 'Player.5 Money')
            # Adjust Pool Rank Winnings to Currency Format
            for(idx in seq_along(cur_var)){
                entry_opt_pool_ranks[,cur_var[idx]] <- prettyNum(entry_opt_pool_ranks[,cur_var[idx]], big.mark = ',')
                entry_opt_pool_ranks[,cur_var[idx]] <- paste('$', entry_opt_pool_ranks[,cur_var[idx]], sep = '')
            }
            entry_opt_pool_ranks <-entry_opt_pool_ranks[, disp_vars]
        },
        options = list(
            lengthMenu = list(c(25,50,-1), c('25','50','All')),
            autoWidth = TRUE
        ))
        # Render output text of new pool entry standing 
        output$o_new_entry_rank <- renderText({
            entry_opt <- entry_opt()
            paste('Best case finish for entry is: ', 
                  as.character(entry_opt$new_entry_rank), sep = '') 
        })

    }
)
