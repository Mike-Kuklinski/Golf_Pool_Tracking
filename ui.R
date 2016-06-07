

# Title: Boomer's Pool - Golf Major Tracking
# Author: Mike Kuklinski
# Date: 5/20/16
# ui.R User Interface Code

# Set working directory accordingly
# to run app, enter runapp() into command line


library(shiny)

source('./Code/BoomerPool_functions.R')

shinyUI(


navbarPage("Boomer's Pool - Golf Major Tracking", position = 'fixed-top', collapsible = T, style = 'color:white',
# HEAD TAB 1 - WELCOME #########################################################
                   tabPanel("Welcome",
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
                            br(), 
                            br(), 
                            br(),
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
                            br(), 
                            br(), 
                            br(),
                            h3('Pool Entry Analysis'),
                                # Left hand side of page
                                absolutePanel(fixed = F, width = 550, height = 'auto',
                                    tabsetPanel(
                                        tabPanel('Select Entry',
                                                 wellPanel(style = 'background-color:#f4faf7',
                                                     #br(),
                                                     selectInput('i_entry', 'Select an Entry', entries$EntryName),
                                                     #br(),
                                                     actionButton('b_update_entry_stats', label = 'Get Pool Entry Info'),
                                                     actionButton('b_optim_entry', label = 'Get Best Case'), 
                                                     p(style = "font-size: 10px; font-style: italic", "Give the application up to 60 secs to
optimize for the ", tags$strong('Get Best Case'), " button. If no results are displayed, it is an indication the tournament is over.
To keep the feature working in the meantime, I've set the current THRU hole to be 36 for each golfer."),
                                                     # Display Current and best case ranks for entry
                                                     tags$strong(textOutput('o_cur_entry_stand')),
                                                     tags$strong(textOutput('o_new_entry_rank')),
                                                     br(),
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
                                                                                     'Player 5'))),
                                                 h4('Current Entry Player Breakdown'),
                                                 tableOutput('o_entry_players')
                                                 ),
                                        tabPanel('Instructions and Descriptions',
                                            wellPanel(style = 'background-color:#f4faf7',
                                            br(),
                                            h4(tags$u(tags$strong('Instructions'))),
                                            p('Using ', strong('Select an Entry,'), 
' highlight the entry you want to inspect and click ', strong('Get Pool Entry Info'), ' or ', 
strong('Get Best Case'), 'to display results. See information below for description of result tables. '),
                                            p(tags$strong('Please note' ), 'if the entry selected is changed,
the result tables will ', tags$strong('NOT'),' automatically be updated. The respective buttons must be re-clicked.'),
                                            #br(),
                                            h4(tags$u(tags$strong('Result Table Descriptions'))),
                                            p(strong('Current Entry Player Breakdown'), 
"table displays the 5 golfers associated with a pool entry, along with their current standings."),
                                            p(strong('Entry Players and Combination Counts'), 
"table lists every combination of the 5 golfers within a pool entry. In addition, it lists the number of 
pool entries with identical combinations as well as a weighted score trading off between the number of 
players in a combination and the number of
combinations. If the number for a combination is 1, this means only the currently selected pool
entry has this combination. This should help identify players groupings you need to do well
in order to perform better in the overall pool."),
                                            p(strong('Golfer Finish for Best Case'), "table shows 
an estimated best case finishes golfers would need to have to maximize a pool entry finsh. It
takes all golfers' current strokes (to par) and assumes each one could potentially move 
up to 3 strokes up/down per day for the remainder of the tournament. Using this range of movement
for each player, it attempts to optimize the best case scenario for a given entry. This is handy near the end of
the tournament since it lets you see if you have a realistic chance at finishing near the top.
The Positions Moved value is in the direction towards 1st place (e.g. +9 is 9 positions towards 1st)."),
                                            p(strong('Pool Finishes for Best Case'), "table shows the resulting pool
finishes associated with the best case situation for a given entry, similar to the ", strong('Standings'), "tab."))))),
################################################################################
                                # Right hand side of page
                                    absolutePanel(width = 'auto', left = 600, height = '100%',
                                        tabsetPanel(
                                            tabPanel('Entry Players and Combination Counts',
                                                wellPanel(style = "overflow-y:scroll; max-height: 775px; background-color:#f4faf7",
                                                dataTableOutput('o_entry_comb')
                                            )),
                                            tabPanel('Golfer Finish for Best Case',
                                                wellPanel(style = "overflow-y:scroll; max-height: 775px; background-color:#f4faf7",     
                                                dataTableOutput('o_new_trny_ranks')
                                            )),
                                            tabPanel('Pool Finishes for Best Case',
                                                wellPanel(style = "overflow-y:scroll; max-height: 775px; background-color:#f4faf7",
                                                dataTableOutput('o_new_pool_ranks')
                                            ))
                                        ))
                   )
)
)