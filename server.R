# Title: Boomer's Pool - Golf Pool Major Tracking
# Author: Mike Kuklinski
# Date: 5/20/16
# server.R file

# load libraries
library(shiny)
library(readxl)
#setwd('Boomer Pool/')

# # Source Helper Information
# source('~/R Scripts/Boomer Pool/Code/BoomerPool_clean_data.R')
# source('~/R Scripts/Boomer Pool/Code/BoomerPool_functions.R')
# # Source membership information
# source('~/R Scripts/Boomer Pool/authorization/membership.R')

# Source Helper Information
source('Code/BoomerPool_clean_data.R')
source('Code/BoomerPool_functions.R')
# Source membership information
source('authorization/membership.R')



# Set initial parameters
check_credentials <- 'Submit Username and Password'

display_idx <- list('Keep' = c("Entry","Rank",
                               "Total Money"),
                    'Players' = c("Player.1",
                                  "Player.2","Player.3",
                                  "Player.4","Player.5"),
                    'Ties' = c("Tie.1","Tie.1 Money",
                               "Tie.2"),
                    'Projected Cash' = c("Player.1 Money","Player.2 Money",
                                         "Player.3 Money","Player.4 Money",
                                         "Player.5 Money"),
                    'Current Positions' = c("Player.1 Pos","Player.2 Pos",
                                            "Player.3 Pos","Player.4 Pos",
                                            "Player.5 Pos"),
                    'Current Strokes' = c("Player.1 Stroke","Player.2 Stroke",
                                          "Player.3 Stroke","Player.4 Stroke",
                                          "Player.5 Stroke")) 

# begin shiny server
shinyServer(
    function(input, output) {

# ##############################################################################
# TAB 2 - STANDINGS 
# ##############################################################################

# ==============================================================================
# sub tab 1 - Pool Entry Standings
# ==============================================================================

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
        #Render output table showing pool entry standings
        output$o_standings <- renderDataTable({
            # Subset data table by selected variables to show
            disp_vars <- sort(unlist(display_idx[c('Players', input$i_show_vars)]))
            disp_vars <- c(display_idx$Keep, disp_vars)
            cur_pool_ranks()[,disp_vars]
        },
        options = list(
            lengthMenu = list(c(25,50,-1), c('25','50','All')),
            autoWidth = TRUE
        )
        )

# ==============================================================================
# sub tab 2 - Golfer Standings
# ==============================================================================

        # Button Update golfer positions
        cur_trny_pos <- reactive({
            input$b_update_golfers
            suppressWarnings(update_trny_ranks(id_num, payouts))
        })
        # Render output table showing current golfer standings
        output$o_golfer_pos <- renderDataTable({
            display_golfer_pos <- cur_trny_pos()[,c('Player',
                              'Position',
                              'Sort Pos',
                              'Day_THRU',
                              'To Par',
                              'Today',
                              'R1',
                              'R2',
                              'R3',
                              'R4',
                              'Total',
                              'THRU',
                              'proj_cash')]
            display_golfer_pos <- rename(display_golfer_pos, c('Day_THRU' = 'Day Thru'))
            display_golfer_pos[,'proj_cash'] <- prettyNum(display_golfer_pos[,'proj_cash'],
                                                          big.mark = ',')
            display_golfer_pos[,'proj_cash'] <- paste('$', display_golfer_pos[,'proj_cash'],
                                                      sep = '')
            display_golfer_pos
            },
            options = list(
                lengthMenu = list(c(25,50,-1), c('25','50','All')),
                autoWidth = TRUE,
                columnDefs = list(list(width = '50px', targets = c(2:8)))
            ))
        #Button Update golfer ranks and projected winnings
        cur_trny_ranks <- reactive({
            input$b_update_golfers
            update_trny_ranks(id_num, payouts)
        })

        
# ==============================================================================
# sub tab 3 - Estimated Position Payout
# ==============================================================================

        output$o_pos_payout <- renderDataTable({
            payouts[,'Payout'] <- prettyNum(payouts[,'Payout'],
                                            big.mark = ',')
            payouts[,'Payout'] <- paste('$', payouts[,'Payout'], sep = '')
            payouts[,c('Rank','Payout')]
            },
            options = list(
                lengthMenu = list(c(25,50,-1), 
                                 c('25','50','All')),
                       autoWidth = F
            )
        )
        
        
        
# ##############################################################################
# TAB 3 - Pool Entry Info 
# ##############################################################################        

#################### Left hand side of page ####################################

# ==============================================================================
# Left Side - sub tab 1 - Select Entry
# ==============================================================================
        
        # Button update entry's current standing
        #cur_entry_stand <- eventReactive(input$b_update_entry_stats, {
        cur_entry_stand <- reactive({
            entry_idx <- which(cur_pool_ranks()[,'Entry'] == input$i_entry)
            cur_pool_ranks()[entry_idx, 'Rank']
        })
        # Render Output text showing the current position for entry
        output$o_cur_entry_stand <- renderText(paste('Current Entry Standing is: ', 
                                                     cur_entry_stand(), sep = ''))
        
        # Button update list of golfers for a given entry name
        #entry_players <- eventReactive(input$b_update_entry_stats, {
        entry_players <- reactive({
            # Get list of golfers for entry
            entry_players <- entries[which(entries$EntryName == input$i_entry), 
                                     c('Player1',
                                       'Player2',
                                       'Player3',
                                       'Player4',
                                       'Player5')]
            # Subset golfer positions by entry golfers
            entry_player_pos <- subset(cur_trny_pos(), Player %in% entry_players)
            entry_player_pos$`Plyr Idx` <- 1:nrow(entry_player_pos)
            for(idx in c('Sort Pos','R1','R2','R3','R4','THRU','Total')){
                entry_player_pos[, idx] <- as.integer(entry_player_pos[, idx]) 
            }
            entry_player_pos <- rename(entry_player_pos, c('Day_THRU' = 'Day Thru'))
            entry_player_pos[,c('Plyr Idx', 
                                'Player', 
                                'Position', 
                                'Day Thru', 
                                'To Par',
                                'Today',
                                'R1',
                                'R2',
                                'R3',
                                'R4',
                                #'THRU',
                                'Total')]
        })
        # Render output table of golfers and respective positions for a given entry
        output$o_entry_players <- renderTable({
            entry_players()
            }, 
            include.rownames = F, 
            options = list(autoWidth = T)
        )
        # Button update entry's player combinations
        #entry_comb_comp <- eventReactive(input$b_update_entry_stats,{
        entry_comb_comp <- reactive({    
            compare_entry_combos(entry_name = input$i_entry, entries, entry_combinations)
        })
        
        
#################### Right hand side of page ####################################

# ==============================================================================
# Right Side - sub tab 1 - Entry Players and Combinations Counts
# ==============================================================================

        # Render output table for entry combination comparisons given index list to display
        output$o_entry_comb <- renderDataTable({
            show_plyrs_idx <- as.integer(gsub('Player ', '', input$i_show_plyrs))
            rmv_plyrs_names <- entry_players()[-show_plyrs_idx,'Player']
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
                paging = F,
                autoWidth = F)
        )

# ==============================================================================
# Right Side - sub tab 2 - Golfer Finish for Best Case
# ==============================================================================

        # Button update optimization information
        entry_opt <- eventReactive(input$b_optim_entry,{
            suppressWarnings(optimize_pool_entry(input$i_entry, 
                                                 id_num, 
                                                 lb = low_bnd, 
                                                 ub = up_bnd, 
                                                 est_bound = T))
        })
        
        observeEvent(input$i_entry, {
            show_best <<- FALSE
        })
        observeEvent(input$b_optim_entry, {
            show_best <<- TRUE
        })
        
        # Render output table of new golfer standings 
        output$o_new_trny_ranks <- renderDataTable({
            input$i_entry
            entry_opt <- entry_opt()
            if(show_best == T){
                return_t <- entry_opt$new_trny_ranks[,c('PLAYER NAME',
                                                        'Best Case Position',
                                                        'Positions Moved',
                                                        'Current Position')]
            }else{
                return_t <- data.frame('message' = 'New entry, re-run best case.')
            }
            return_t
            },
            options = list(
                paging = F,
                autoWidth = F)
        )
        
# ==============================================================================
# Right Side - sub tab 3 - Pool Finishes for Best Case
# ==============================================================================

        # Render output table of new pool standings 
        output$o_new_pool_ranks <- renderDataTable({
            input$i_entry
            entry_opt <- entry_opt()
            if(show_best == T){
                disp_vars <- sort(unlist(display_idx[c('Players','Current Positions', 'Ties')]))
                disp_vars <- c(display_idx$Keep, disp_vars)
                entry_opt_pool_ranks <- entry_opt$new_pool_ranks
                cur_var <- c('Total Money', 'Tie.1 Money', 'Player.1 Money', 'Player.2 Money',
                             'Player.3 Money', 'Player.4 Money', 'Player.5 Money')
                # Adjust Pool Rank Winnings to Currency Format
                for(idx in seq_along(cur_var)){
                    entry_opt_pool_ranks[,cur_var[idx]] <- prettyNum(entry_opt_pool_ranks[,cur_var[idx]], big.mark = ',')
                    entry_opt_pool_ranks[,cur_var[idx]] <- paste('$', entry_opt_pool_ranks[,cur_var[idx]], sep = '')
                }
                return_t <- entry_opt_pool_ranks[, disp_vars]
            }else{
                return_t <- data.frame('message' = 'New entry, re-run best case.')
            }
            },
            options = list(
                autoWidth = F,
                paging = F)
        )
        # Render output text of new pool entry standing 
        output$o_new_entry_rank <- renderText({
            input$i_entry
            entry_opt <- entry_opt()
            if(show_best == T){
                paste('Best Entry Standing is: ', 
                      as.character(entry_opt$new_entry_rank), sep = '')
            }
        })
        
# ##############################################################################
# TAB 4 - MAINTENANCE
# ##############################################################################

        # Check is user input information is acceptable
        check_credentials <- eventReactive(input$b_check_credentials, {
            if(input$i_user_n == user_nm && input$i_user_pw == user_pw){
                'Access Granted'}else{'Invalid Username and/or Password'}
        })
        output$o_access_granted <- reactive({check_credentials()})
        
        # Rename googlesheet and update id information
        observeEvent(input$b_submit, {
            # Rename Google Sheet File for tournament
            major_ws_title <- paste(input$i_major, "_", input$i_year, sep = '')
            gs_rename(gs_entries, to = major_ws_title, verbose = F)
            # Update 'Id" sheet with id, year, major name
            trny_id <- data.frame('id' = input$i_id, 
                                  'year' = input$i_year, 
                                  'major' = input$i_major,
                                  'low_bnd' = input$i_lb,
                                  'up_bnd' = input$i_ub,
                                  'purse' = input$i_purse)
            gs_edit_cells(gs_entries, 
                          ws = 'id', 
                          input = trny_id, 
                          trim = T, 
                          verbose = F)
        })
        #  Update 'entries' sheet with uploaded file
        observeEvent(input$b_submit_entries, {
            # Load Excel File
            entries_file <- input$f_entry_list
            if(is.null(entries_file))
                return(NULL)
            file.rename(entries_file$datapath,
                        paste(entries_file$datapath, ".xlsx", sep = ""))
            xl_entries_file <- as.data.frame(read_excel(paste(entries_file$datapath, 
                                                              ".xlsx", 
                                                              sep = ""), 1))
            # Clean up file names
            xl_entries_file <- clean_entries(xl_entries_file)
            # Determine combinations
            xl_entries_file_combos <- entry_combos(xl_entries_file)
            # Update googlesheet with entries and combinations
            gs_edit_cells(gs_entries, 
                          ws = 'entries', 
                          input = xl_entries_file, 
                          trim = T, 
                          verbose = F)
            gs_edit_cells(gs_entries, 
                          ws = 'entry combinations', 
                          input = xl_entries_file_combos, 
                          trim = T, 
                          verbose = F)
        })
        #  Update payout percentages with uploaded file
        observeEvent(input$b_submit_payout, {
            # Load Excel File
            payout_file <- input$f_payout
            if(is.null(payout_file))
                return(NULL)
            csv_payout_file <- read.csv(payout_file$datapath,
                                        header = T,
                                        stringsAsFactors = F)
            # Update googlesheet with entries and combinations
            gs_edit_cells(gs_entries, 
                          ws = 'payouts', 
                          input = csv_payout_file, 
                          trim = T, 
                          verbose = F)
        })
    }
)
