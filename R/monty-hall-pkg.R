#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'    Contestant Selects A Door
#' @description
#'    The function 'function()' creates a custom function to preform a task.
#'    'sample' is used to select 1 option in the vector, randomly
#' @details
#'    This section creates a vector of three doors that the contestant
#'    can choose from, and then randomizes the selection
#' @param ... no arguments are used by the function.
#' @return The return will be the randomly selected door from the created
#' @examples select_door
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'    Host Opens Goat Door
#' @description
#'    The function 'if()' is used to create a if
#'    statement to filter the doors
#' @details
#'    The host will randomly selected one of the doors
#'    remaining that is NOT a car door. If the contestant
#'    has selected the car door, the host will randomly
#'    select one of the remaining two goat doors.
#' @param ... no arguments are used by the function.
#' @return The function will return the door selected by the host
#' @examples open_goat_door
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'    Change Doors
#' @description
#'    'if(stay)' will be used in conjunction with
#'    'if( ! stay)' for the contestant to choose
#'    whether or not to switch doors
#' @details
#'    The contestant will have the option to switch doors, or keep
#'    their previously selected door. A if function will be used to 
#'    create an option if the contestant chooses STAY, or if the
#'    contestant chooses SWAP
#' @param ... no arguments are used by the function.
#' @return The return will be the remaining door selected by the contestant
#' @examples change_door
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title Determine if Contestant Has Won
#' @description
#'    'if(final.pick, game' will be used to create a
#'    text return based on the outcome of the game.
#' @details
#'    The contestant will either win or lose, based on
#'    their choice whether or not to swap. This will be
#'    created using a if function with a return of either
#'    'win' or 'lose'
#' @param ... no arguments are used by the function.
#' @return 'win' or 'lose'
#' @examples determine_winner
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title 
#'    Simplify to a Single Function
#' @description
#'    Simplify to a single function that can be
#'    used to run the game
#' @details 
#'    By storing the game in a single 'play game' function,
#'    the process of running the game can be simplified.
#' @param ... no arguments are used by the function.
#' @return 'win' or 'lose' will be returned
#' @examples play_game
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title Create a loop to run the game 100 times
#' @description
#'    Prove whether it is better to swap or stay
#'    with data by running the game multiple times
#' @details.
#'    By creating a loop, the game can be ran several
#'    times which will record data to justify whether
#'    it is better to swap or stay during the game.
#' @param ... no arguments are used by the function.
#' @return 
#'    The return will be a table with the relevant game data
#' @examples play_n_games
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
