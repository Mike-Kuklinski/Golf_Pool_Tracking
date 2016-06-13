# Title: Boomer's Pool - Golf Pool Major Tracking
# Author: Mike Kuklinski
# Date: 5/20/16
# ui.R User Interface Code

# Set working directory accordingly
# to run app, enter runapp() into command line
#setwd('Boomer Pool/')

# load libraries
library(shiny)
library(dplyr)

# # Source Credential Information
#source('~/R Scripts/Boomer Pool/authorization/auth.R')
source('authorization/auth.R')

# Authorize
gs_entries <<- gs_key(key_lu)

#Get id information
ids <<- data.frame(gs_read_csv(gs_entries, ws = 'id', verbose = F))
id_num <<- as.integer(ids$id)
year <<- as.integer(ids$year)
major <<- ids$major
low_bnd <<- as.integer(ids$low_bnd)
up_bnd <<- as.integer(ids$up_bnd)
purse <<- as.integer(ids$purse)

# Load Pool Entries, Id, Combinations, etc. from google sheets
entries <<- data.frame(gs_read_csv(gs_entries, ws = 'entries', verbose = F))
payouts <<- data.frame(gs_read_csv(gs_entries, ws = 'payouts', verbose = F))
# Add payout based on purse size
payouts <<- mutate(payouts, Payout = round(Payout.Perc*purse, -2))
entry_combinations <<- data.frame(gs_read_csv(gs_entries, ws = 'entry combinations', verbose = F))


# Manually load hard files for information
#major <- 'Masters'
#year <- 2016
#id_num <- 2493
#payouts <- read.csv('Data/Payout.csv', header = T, stringsAsFactors = F)
#entries <- read.csv('Data/revised_entries.csv', header = T, stringsAsFactors = F)
#entry_combinations <- read.csv('Data/entry_combinations.csv', header = T, stringsAsFactors = F)

# Start shiny user interface
shinyUI(
navbarPage("Boomer's Pool - Golf Major Tracking", 
           position = 'fixed-top', 
           collapsible = T, 
           style = 'color:white',

# ##############################################################################
# TAB 1 - WELCOME
# ##############################################################################

    tabPanel("Welcome",
    # Set Color scheme for the league
        tags$head(
            tags$style(HTML(
            "
            .navbar-default {
            background-color: #0d3d0d;
            border-color: #0d3d0d;
            }
            .navbar-default .navbar-nav > li > a {
            color: white;
            font-size: 15px;
            }
            .navbar-default .navbar-brand {
            color:#ecf8ec;
            font-size: 20px;
            }
            body {
            background-color:white;
            color: #1a3300
            }"
            ))
        ),
        br(),
        br(),
        br(),
        br(),
        h1(paste(year, ' - ', major, ' Edition', sep = '')),
        h3("Welcome to Boomer's Pool!"),
        tags$hr(),
        h4("Hello all and welcome!"),
        p("If you're like me, you look forward to Boomer's Golf Pools every year 
for the four PGA majors. I've only participated in the past few years and have 
never come close to winning... but find that the pool certainly still adds to the 
enjoyment of the PGA Major Weekends. Up until now, Boomer and Tom have been kind enough 
to provide updates via Excel Spreadsheets which have been great, but I felt it'd 
be nice to have a website where anyone can check in on the live estimated standings 
at any time during the tournament."),
        p('The standings are projected in the same fashion as the Excel sheets.
Based on current golfer strokes, the golfer payouts are estimated and added together 
for the five golfers listed in a given entry. The entry with the highest total golfer 
payout in the end wins. Tiebreakers come into play if necessary.'),
        p(strong('Please note: '), 'All projected standings, projected winnings, 
and similar information included in this application is an ', strong("estimate based
off of the previous year's payouts."), ' Actual pool standings will be determined 
via the normal channels. This is just helpful tracking tool.'),
        p('Finally, if you have any comments, suggestions for improvement, or notice 
any glitches occuring, please feel free to let me know so I can get them corrected.'),
        p('My email: mike.kuklinski@gmail.com.')
    ),

# ##############################################################################
# TAB 2 - STANDINGS 
# ##############################################################################

    tabPanel("Standings", 
        br(), 
        br(), 
        br(),
        br(),
        h3('Standings'),
        p(strong('Pool Entry Standings '), "tab shows all the entries, selected 
players, tie breakers, and other stats. Just check the boxes for which stats you 
want to be included in the view. Please note the projected Money Value listed is 
subject to change as it is only based on the previous year's payouts. However, it 
should still be accurate enough to determine the estimated standings."),
        p(strong('Golfer Standings '), "tab shows the current golfer positions in 
the tournament."),
        h4("Click the UPDATE button to refresh the Pool Entry and Golfer Standings"),
        # Button to update standings from ESPN.com
        actionButton('b_update_golfers', label = 'UPDATE'),
        br(),
        br(),
        tabsetPanel(

# ==============================================================================
# sub tab 1 - Pool Entry Standings
# ==============================================================================

            tabPanel('Pool Entry Standings',
                # Provide Check Box to select which variables to show
                checkboxGroupInput('i_show_vars', 'Information to Include in Table',
                    c('Ties',
                    'Projected Cash',
                    'Current Positions',
                    'Current Strokes'), 
                    inline = T, 
                    selected = 'Projected Cash'),
                h3('You can type a name into the Search bar to locate an entry'),
                div(dataTableOutput('o_standings'),
                style = 'font-size:85%')
                ),
                

# ==============================================================================
# sub tab 2 - Golfer Standings
# ==============================================================================

            tabPanel('Golfer Standings',
                dataTableOutput('o_golfer_pos')
                ),
        

# ==============================================================================
# sub tab 3 - Estimated Position Payouts
# ==============================================================================
    
            tabPanel('Estimated Position Payouts',
                column(3,
                    dataTableOutput('o_pos_payout')
                )
            )
        )
    ),

# ##############################################################################
# TAB 3 - POOL ENTRY INFO
# ##############################################################################

    tabPanel("Pool Entry Info",
        br(), 
        br(), 
        br(),
        br(),
        h3('Pool Entry Analysis'),

#################### Left hand side of page ####################################

        absolutePanel(fixed = F, width = 550, height = 'auto',
        tabsetPanel(

# ==============================================================================
# Left Side - sub tab 1 - Select Entry
# ==============================================================================

            tabPanel('Select Entry',
                wellPanel(style = 'background-color:#f4faf7',
                    #br(),
                    selectInput('i_entry', 'Select an Entry', sort(entries$EntryName)),
                    #br(),
                    # Update and Optimization Buttons
                    #actionButton('b_update_entry_stats', label = 'Get Pool Entry Info'),
                    actionButton('b_optim_entry', label = 'Get Best Case'), 
                    p(style = "font-size: 10px; font-style: italic", "Give the 
application up to 60 secs to optimize for the ", tags$strong('Get Best Case'), 
" button. If no results are displayed, it is an indication the tournament is over."),
                    # Display Current and best case ranks for entry
                    tags$strong(textOutput('o_cur_entry_stand')),
                    tags$strong(textOutput('o_new_entry_rank')),
                    br(),
                    # Checkbox to select which players to include in combinations
                    checkboxGroupInput('i_show_plyrs', 'Plyr Idx to Include in Combinations',
                        c('Player 1',
                        'Player 2',
                        'Player 3',
                        'Player 4',
                        'Player 5'),
                        inline = T,
                        selected = c('Player 1',
                        'Player 2',
                        'Player 3',
                        'Player 4',
                        'Player 5'))
                ),
                h4('Current Entry Player Breakdown'),
                tableOutput('o_entry_players')
            ),


# ==============================================================================
# Left Side - sub tab 2 - Instructions and Descriptions
# ==============================================================================

            tabPanel('Instructions and Descriptions',
                wellPanel(style = 'background-color:#f4faf7',
                    br(),
                    h4(tags$u(tags$strong('Instructions'))),
                    p('Using ', strong('Select an Entry,'), ' highlight the entry 
you want to inspect to display entry player breakdown and combinations. Click on
', strong('Get Best Case'), 'to inspect how well the entry could potentially perform in the pool.
See information below for additional descriptions of result tables.'),
                    p(tags$strong('Please note' ), 'if the entry selected is changed,
the player breakdown and combinations will automatically be updated. However,
the best case result tables will ', tags$strong('NOT'),' be updated. The 
respective button must be re-clicked.'),
                    #br(),
                    h4(tags$u(tags$strong('Result Table Descriptions'))),
                    p(strong('Current Entry Player Breakdown'), "table displays 
the 5 golfers associated with a pool entry, along with their current standings."),
                    p(strong('Entry Players and Combination Counts'), "table lists 
every combination of the 5 golfers within a pool entry. In addition, it lists the 
number of pool entries with identical combinations as well as a weighted score 
trading off between the number of players in a combination and the number of
combinations. If the number for a combination is 1, this means only the currently 
selected pool entry has this combination. This should help identify players groupings 
you need to do well in order to perform better in the overall pool."),
                    p(strong('Golfer Finish for Best Case'), "table shows an 
estimated best case finishes golfers would need to have to maximize a pool entry 
finsh. It takes all golfers' current strokes (to par) and assumes each one could 
potentially move up to ",  low_bnd, "strokes down and ", up_bnd, "strokes up per 
day for the remainder of the tournament. 
Using this range of movement for each player, it attempts to optimize the best 
case scenario for a given entry. This is handy near the end of the tournament since 
it lets you see if you have a realistic chance at finishing near the top. The 
Positions Moved value is in the direction towards 1st place (e.g. +9 is 9 positions 
towards 1st)."),
                    p(strong('Pool Finishes for Best Case'), "table shows the 
resulting pool finishes associated with the best case situation for a given entry, 
similar to the ", strong('Standings'), "tab.")
                )
            )
        )
        ),

#################### Right hand side of page ####################################

        absolutePanel(width = 'auto', left = 600, height = '100%',
        tabsetPanel(

# ==============================================================================
# Right Side - sub tab 1 - Entry Players and Combinations Counts
# ==============================================================================

            tabPanel('Entry Players and Combination Counts',
                wellPanel(style = "overflow-y:scroll; max-height: 775px; background-color:#f4faf7",
                    dataTableOutput('o_entry_comb')
                )
            ),

# ==============================================================================
# Right Side - sub tab 2 - Golfer Finish for Best Case
# ==============================================================================

            tabPanel('Golfer Finish for Best Case',
                wellPanel(style = "overflow-y:scroll; max-height: 775px; background-color:#f4faf7",     
                    dataTableOutput('o_new_trny_ranks')
                )
            ),

# ==============================================================================
# Right Side - sub tab 3 - Pool Finishes for Best Case
# ==============================================================================

            tabPanel('Pool Finishes for Best Case',
                wellPanel(style = "overflow-y:scroll; max-height: 775px; background-color:#f4faf7",
                    dataTableOutput('o_new_pool_ranks')
                )
            )
        )
        )
    ),

# ##############################################################################
# TAB 4 - MAINTENANCE
# ##############################################################################

    tabPanel('Maintenance',
        br(),
        br(),
        br(),
        br(),
        h3('Maintenance'),
        p('This page is intended for updating the entry list, payout, and tournament 
information. Only authorize users are allowed.'),
        # Request username and password
        textInput('i_user_n', 'Enter User Name'),
        textInput('i_user_pw', 'Enter User Password'),
        actionButton('b_check_credentials', 'Submit'),
        h4('Enter Log In information'),
        verbatimTextOutput('o_access_granted'),
        # With correct log-in, show tournament update options
        conditionalPanel(
            condition="output.o_access_granted=='Access Granted'",
            h4('Enter New Tournament Data'),
            # Input for updating major information
            textInput('i_id', 'enter id'),
            textInput('i_year', 'enter year'),
            textInput('i_major', 'enter major'),
            textInput('i_lb', 'enter optimization lower bound', value = 3),
            textInput('i_ub', 'enter optimization upper bound', value = 3),
            textInput('i_purse', 'enter purse'),
            # Upload entry file information
            fileInput('f_entry_list', 'Select Entry List File (.xlsx)',
                accept = c(".xlsx")),
            # Upload payout file information
            fileInput('f_payout', 'Select Payout Percent File (.csv)',
                      accept = c(".csv")),
            # Submit buttons
            actionButton('b_submit', 'Update Id information'),
            actionButton('b_submit_entries', 'Update Entries'),
            actionButton('b_submit_payout', 'Update Payouts')
        )
    )
)
)