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
create_game <- function(num.goats=2,num.cars=1)
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}

#' @title
#'  Contestant will select a door
#'  
#' @description    
#' `select_door()` generates a number that correlates to one of the three doors. 
#' 
#' @details
#' This number represents the door the contestant chooses initially at the beginning
#' of the game.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a number that represents 
#' what door the contestant chose.
#' 
#' @examples
#'   select.door()
#'   
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'  Host will open a goat door
#'  
#' @description 
#' `open_goat_door()` generates a number that represents a goat door that the host opens. 
#' 
#' @details
#' The number that is generated correlates to the door that the host chooses.
#' It will always be a goat door and it will never be the same door that the contestant
#' has already chosen. 
#' 
#' @param create_game a vector representing the current game setup in the form of
#' how many goats there are and how many cars there are (ex: c("goat", "goat", "car"))
#' 
#' @param select.door a numeric representing the door the contestant chooses which is 
#' correlated to the vector that represents the game setup. 
#' 
#' @return a number that is correlated to a goat door but is not 
#' a door the contestant has chosen. 
#' 
#' @examples
#' game.output <- create_game()
#' game.output
#' my.initial.pick <- select.door()
#' my.initial.pick
#' 
#' open_goat_door( game.output, my.initial.pick )
#' 
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
#'  Contestant stays or switches their initial door
#'  
#' @description 
#' `change_door()` is a function that determines if the contestant will change the door
#' and what their next chosen door is. It will generate a numeric. 
#' 
#' @details
#' This number that is generated will either be the same number as the initial pick
#' if the contestant stays or it will be a number that is not what they initially chose nor
#' a number that correlates to the door the host has chosen.
#' 
#' @param stay=T A logical where if stay is T or TRUE, then the code will keep the contestant's
#' initial choice, that is, the previous parameter "select.door".
#' 
#' @param stay=F A logical where if stay is F or FALSE, then the code will change the contestant's
#' initial choice, that is, the previous parameter "select.door" will be changed to a 
#' numeric that is not "initial.pick" or "open_goat_door".
#' 
#' @param open_goat_door A numeric representing the door that the host has already opened.
#' 
#' @param initial.pick A numeric representing the door that the contestant initially chose. 
#' 
#' @return Either a new numeric representing a new door the contestant chose or the same number as "initial.pick" 
#' if the contestant decides to stay. 
#' 
#' @examples
#' game.output <- create_game()
#' my.initial.pick <- select.door()
#' opened.goat.door <- open_goat_door( game.output, my.initial.pick )
#' 
#' my.final.pick.stay <- change_door( stay=T, 
#' opened.door=opened.goat.door, 
#' a.pick=my.initial.pick )
#' 
#' my.final.pick.switch <- change_door( stay=F, 
#' opened.door=opened.goat.door, 
#' a.pick=my.initial.pick )
#' 
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


#' @title
#'  Determine if the contestant wins
#'  
#' @description 
#' `determine_winner()` checks whether or not the door number with the car matches with
#' the contestant's final pick. It will generate a character string of either "WIN" or "LOSE". 
#' 
#' @details
#' If the 
#' 
#' @param
#' 
#' @return Either returns "WIN" or "LOSE" based on whether or not the final door 
#' correlated with a number the contestant chooses is the door that 
#' has the car behind it or a goat behind it.
#' 
#' @examples
#' game.output
#' my.initial.pick
#' 
#' my.final.pick <- change_door( stay=T, 
#' opened.door=opened.door, 
#' a.pick=my.initial.pick )
#' 
#' determine_winner( final.pick=my.final.pick, 
#'game=game.output )
#'
#'my.final.pick <- change_door( stay=F, 
#'opened.door=opened.door, 
#'a.pick=my.initial.pick )
#'
#'determine_winner( final.pick=my.final.pick, 
#'game=game.output )
#'
#' @examples if contestant stays
#' paste0( "GAME SETUP" )
#' game.output
#' paste0( "My initial selection: ", my.initial.pick )
#' paste0( "The opened goat door: ", opened.goat.door )
#' paste0( "My final selection: ", my.final.pick.stay )
#' paste0( "GAME OUTCOME:" )
#' determine_winner( final.pick=my.final.pick.stay, 
#' game=game.output )
#' 
#' @examples if contestant switches
#' paste0( "GAME SETUP" )
#' game.output
#' paste0( "My initial selection: ", my.initial.pick )
#' paste0( "The opened goat door: ", opened.goat.door )
#' paste0( "My final selection: ", my.final.pick.switch )
#' paste0( "GAME OUTCOME:" )
#' determine_winner( final.pick=my.final.pick.switch, 
#'                  game=game.output )
#'
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
#'  Let's play a game!
#'  
#' @description `play_game()` is a function that generates a full Monty Hall
#' game with the default being 2 goat doors and 1 car door. 
#' 
#' @details
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return A data frame representing the set up, the initial door the contestant
#' chooses, the door the host chooses, if the contestant switches doors, and if
#' the contestant wins or loses. 
#' 
#' @examples
#' 
#' play_game()
#' 
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


#' @title
#'  Let's play 100 games!!
#'  
#' @description
#' `play_n_games` generates a new game that consists of two doors
#'   with goats behind them, and one with a car and repeats the same set up
#'   for n amount of times.
#' 
#' @details
#' 
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
#' 
#' @param n a numeric that represents how many simulations or runs of the game
#' we should do. The default is n=100, or 100 rounds. 
#' 
#' @return A table with decimal numerics representing the percentage of how many wins and 
#' losses the contestant got categorized by if they switched their initial door choice or
#' if they stayed with their initial choice. 
#' 
#' @examples
#' play_n_games()
#' 
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

#' @title
#' Sum of vector elements.
#'
#' @description
#' `sum(x)` returns the sum of all the values present in its arguments.
#'
#' @details
#' This is a generic function: methods can be defined for it directly
#' or via the [Summary] group generic. For this to work properly,
#' the arguments `...` should be unnamed, and dispatch is on the
#' first argument.
#'
#' @param x Numeric, complex, or logical vectors.
#' @param na.rm A logical scalar. Should missing values (including `NaN`)
#'   be removed?
#' @return If all inputs are integer and logical, then the output
#'   will be an integer. Otherwise it will be a length-one numeric or
#'   complex vector.
#'
#'   Zero-length vectors have sum 0 by definition. See
#'   <http://en.wikipedia.org/wiki/Empty_sum> for more details.
#'
#' @examples
#' sum(1:10)
#' sum(1:5, 6:10)
#' sum(F, F, F, T, T)
#'
#' sum(.Machine$integer.max, 1L)
#' sum(.Machine$integer.max, 1)
#'
#' \dontrun{
#' sum("a")
#' }
sum <- function(..., na.rm = TRUE) {}
