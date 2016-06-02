

# Title: Boomer's Pool - Golf Major Tracking
# Author: Mike Kuklinski
# Date: 5/20/16
# ui.R User Interface Code

# Set working directory accordingly
# to run app, enter runapp() into command line

library(shiny)
# begin shiny UI

source('./Code/BoomerPool_functions.R')

shinyUI(navbarPage("Boomer's Pool - Golf Major Tracking",
# HEAD TAB 1 - WELCOME #########################################################

                   tabPanel("Welcome",
                        h1("2016 - Masters Edition"),
                        h3("Welcome to Boomer's Pool!"),
                        tags$hr(),
                        h4("Hello all and welcome!"),
                        p("If you're like me, you look
forward to Boomer's Golf Pools every year for the four PGA majors. I've only participated 
in the past few years and have never come close to winning... but find that the pool certainly still 
adds to the enjoyment of the PGA Major Weekends. Up until now, Boomer and Tom have been kind enough 
to provide updates via Excel Spreadsheets which have been great, but I felt it'd be nice to have a website
where anyone can check in on the live estimated standings at any time during the tournament."),
                        p('The standings are projected in the same fashion as the Excel sheets.
Based on current golfer strokes, the golfer payouts are estimated and added together for
the five golfers listed in a given entry. The entry with the highest total golfer payout in the end wins.
Tiebreakers come into play if necessary.'),
                        p(strong('Please note: '), 'All projected standings, projected winnings, 
and similar information included in this application is an ', strong('estimate and its accuracy 
is not guaranteed.'), ' Actual pool standings will be determined via the normal
channels. This is just helpful tracking tool.'),
                        p('Finally, if you do notice any glitches or errors occuring, please
feel free to let me know so I can get them corrected.')
                   ),

# HEAD TAB 2 - STANDINGS #######################################################
                   
                    tabPanel("Standings",
                            h3('Standings'),
                            p(strong('Pool Entry Standings '),
"tab shows all the entries, selected players, tie breakers, and other stats. 
Just check the boxes for which stats you want to be included in the view.
Please note the projected Money Value listed is subject to change as it is only based 
on the previous year's payouts. However, it should still be accurate enough to 
determine the estimated standings."),
                            p(strong('Golfer Standings '),
"tab shows the current golfer positions in the tournament."),
                            h4("Click the UPDATE button to refresh the Pool Entry and Golfer Standings"),
                            actionButton('b_update_golfers', label = 'UPDATE'),
                            br(),
                            br(),
                            tabsetPanel(
# sub tab 1 ====================================================================
                                tabPanel('Pool Entry Standings',
                                         checkboxGroupInput('i_show_vars', 'Information to Include in Table',
                                                            c('Ties',
                                                              'Projected Cash',
                                                              'Current Positions',
                                                              'Current Strokes'), inline = T, selected = 'Projected Cash'),
                                         h3('Type your Entry Name into the Search bar to locate entry'),
                                         div(dataTableOutput('o_standings'),
                                             style = 'font-size:85%')
                                         ),
# sub tab 2 ====================================================================
                                tabPanel('Golfer Standings',
                                         dataTableOutput('o_golfer_pos')
                            )
                            )
                   ),

# HEAD TAB 3 - POOL ENTRY STATS ################################################
                   
                   tabPanel("Pool Entry Info",
                            h3('Pool Entry Information'),
                            p(strong('Entry Players and Combination Counts'), "tab includes
two tables. The first table displays the 5 golfers associated 
with a pool entry, along with their current standings. The second table lists every combination of 
the 5 golfers within a pool entry. In addition, it lists the number of pool entries with identical
combinations as well as a weighted score trading off between the number of players in a combination and the number of
combinations. If the number for a combination is 1, this means only the currently selected pool
entry has this combination. This should help identify players groupings you need to do well
in order to perform better in the overall pool."),
                            p('You can also restrict the Players you wish to 
include in the list of combinations by checking the ', strong('Player_idx'),' boxes on/off.'),
                            p(strong('Best Case Scenario'), "tab shows an estimated best case finish a pool entry could get. It
takes all golfers' current strokes (to par) and assumes each one could potentially move 
up to 3 strokes up/down per day for the remainder of the tournament. Using this range of movement
for each player, it attempts to optimize the best case scenario for a given entry and returns 
the list of position changes needed by the golfers to accomplish this. This is handy near the end of
the tournament since it lets you see if you have a realistic chance at finishing near the top."),
                            p('To use, click the drop down list ',
                              strong('Select an Entry'), 
                              ' and highlight the entry you want to inspect. 
                              Next, click ', strong('Get Pool Entry Info.'), ' or ', 
                              strong('Get Best Case'),' in the respective tab.'),
                            br(),
                            selectInput('i_entry', 'Select an Entry', entries$EntryName),
                            br(),
                            tabsetPanel(
# sub tab 1 ====================================================================
                                tabPanel('Entry Players and Combination Counts',
                                         br(),
                                         actionButton('b_update_entry_stats', label = 'Get Pool Entry Info'),
                                br(),
                                br(),
                                textOutput('o_cur_entry_stand'),
                                h4('Current Entry Player Breakdown'),
                                tableOutput('o_entry_players'),
                                checkboxGroupInput('i_show_plyrs', 'Player_Idx to Include',
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
                                                               'Player 5')),
                                h4('Current Player Combination Breakdown'),
                                dataTableOutput('o_entry_comb')
                                ),
# sub tab 2 ====================================================================
                                tabPanel('Best Case Scenario',
                                         br(),
                                         p("Give the application up to 60 secs to 
                                          optimize. If no results are displayed, it
                                           is an indication the tournament is over. 
                                           To keep the feature working in the meantime,
                                           I've set the current THRU hole to be 36 for each golfer."),
                                         actionButton('b_optim_entry', label = 'Get Best Case'),
                                         br(),
                                         br(),
                                         tabsetPanel(
    # sub-sub tab 1 ============================================================
                                             tabPanel('Golfer Finish for Best Case',
                                                 br(),
                                                 p("The table generated in this tab represents
                                                   the best case finishes the golfers would have to
                                                   have to optimize the entry's standing in the pool."),
                                                 textOutput('o_new_entry_rank'),
                                                 br(),
                                                 p('The Positions Moved value is in the direction towards
                                                   1st place(e.g. +9 is 9 positions towards 1st).'),
                                                 dataTableOutput('o_new_trny_ranks')
                                             ),
    # sub-sub tab 2 ============================================================
                                             tabPanel('Pool Finishes for Best Case',
                                                 br(),
                                                 p("The table generated in this tab represents
                                                   the resulting pool finishes from the best
                                                   case golfer finishes."),
                                                 br(),
                                                 dataTableOutput('o_new_pool_ranks')
                                             )
                                         )
                                )
)
)
)
)