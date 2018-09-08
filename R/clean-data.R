#' Clean the matches data from deuce atp_maches and wta_matches
#'
#' @param atp_matches atp_matches object from R package deuce (Men's).  Default is NULL
#' @param wta_matches atp_matches object from R package deuce (Women's).  Default is NULL
#' @param start_year start year (earliest is 1968)
#' @param end_year end year (latest available is 2017)
#' @param include_quals logical to include qualifying rounds.  Default is FALSE
#' @param level level of tournament, corresponding to column from atp_matches/wta_matches.  Default is "Grand Slams"
#' @return cleaned and subsetted data set, combined if there is both an atp_matches and wta_data set.  A new column called "league" is added and has value "ATP" or "WTA".  We also clean the tourney_name making a new column called tournament
#' @importFrom magrittr %>%
clean_matches_data <- function(atp_matches = NULL, wta_matches = NULL,
                               start_year,
                               end_year,
                               include_quals = FALSE,
                               level = "Grand Slams"){

    ## Combine the two and add a "league" variable
    matches <- NULL
    if(is.null(atp_matches) & is.null(wta_matches)) {
        stop("Both atp_matches and wta_matches are NULL")
    }
    if(!is.null(atp_matches)) {
        atp_matches$league <- "ATP"
    }
    if(!is.null(wta_matches)){
        wta_matches$league <- "WTA"
    }
    matches <- rbind(wta_matches, atp_matches)

    ## Subset to proper years
    matches <- matches  %>% dplyr::filter(tourney_level == level &
                                   year >= start_year & year <= end_year)

    ## Get rid of qualifiers if appropriate
    if(!include_quals){
        matches <- matches %>% dplyr::filter(!(round %in% c("Q1", "Q2", "Q3", "Q4")))
    }

    matches$tournament <- factor(matches$tourney_name) %>%
        forcats::fct_collapse("US Open" = c("US Open", "Us Open"),
                     "French Open" = c("French Open", "Roland Garros"))

    return(matches)

}


#' Extract set, games, and points for winner and loser
#'
#' @param matches object from deuce package or clean_matches_data.
#' @return matches with extra columns w_pointswon, l_pointswon, w_gameswon, l_gameswon, w_setswon, l_setswon
extract_sgp<- function(matches){

    browser()
    ## Points

    matches <- matches %>% dplyr::mutate(
                                      w_pointswon = w_1stWon + w_2ndWon +
                                          l_svpt - l_1stWon - l_2ndWon,
                                      l_pointswon = l_1stWon +
                                          l_2ndWon + w_svpt - w_1stWon - w_2ndWon)

    ## Games
    score_list <- strsplit(matches$score, " ")
    games_won <- lapply(score_list, strsplit, split = "-|\\(")
    matches$w_gameswon <- sapply(games_won, function(list){
      win_list <- lapply(list, "[", 1)
      win_list <- unlist(win_list)
      win_list <- gsub("RET|W\\/O", "0", win_list)  ## Walkover or retirement
      if(any(is.na(win_list)))  win_list <-  0
      sum(as.integer(win_list))
    })


    matches$l_gameswon <- sapply(games_won, function(list){
      win_list <- lapply(list, "[", 2)
      win_list <- unlist(win_list)
      win_list <- gsub("RET|W\\/O", "0", win_list)  ## Walkover or retirement
      if(any(is.na(win_list)))  win_list <-  0
      sum(as.integer(win_list))
    })
    ## Sets

    ## a set is won if they scored more games in the set than opponent
    matches$w_setswon <-  sapply(games_won, function(list){
        sum(sapply(list, function(vec){
            if(is.null(dim(vec))){
               return((as.integer(vec[1]) - as.integer(vec[2])) > 0)
            } else {
                return((as.integer(vec[,1]) - as.integer(vec[, 2])) > 0)
            }
        }), na.rm = TRUE)
    })

     matches$l_setswon <-  sapply(games_won, function(list){
        sum(sapply(list, function(vec){
            if(is.null(dim(vec))){
               return((as.integer(vec[1]) - as.integer(vec[2])) < 0)
            } else {
                return((as.integer(vec[,1]) - as.integer(vec[, 2])) < 0)
            }
        }), na.rm = TRUE)
    })



    return(matches)


}



#' Turn the matches data into player data for modelling
#'
#' @param matches object from deuce package or clean_matches_data.
#' @param w_opp_vars opponent variable base name for the winner to keep.  Default is "loser_rank"
#' @param l_opp_vars opponent variable base name for the loser to keep.  Default is "winner_rank"
#' @param opp_var_names what to rename the opponent variables
#' @return formatted matches so each row refers to one player, their attributes, and their opponents variables along with the other tournament variables
#' @details All variables to be removed must start with "w_" "W" or "winner_" or "l_", "L", or "loser_"
matches_to_player_data <- function(matches,
                                   w_opp_vars = "loser_rank",
                                   l_opp_vars = "winner_rank",
                                   opp_var_names = "opponent_rank"){

    ## Copy data frames
    matches_l <- matches
    matches_w <- matches

    ## Get rid of winner attributes for the loser except for l_opp_vars
    remove_from_loser <- names(matches)[grep("^w|^W", names(matches))] %>%
        .[-which(. == l_opp_vars)]
    rename_w_inds <- which(names(matches) %in% l_opp_vars)
    names(matches_l)[rename_w_inds] <- opp_var_names

    ## Get rid of loser attributes for the winner except for w_opp_vars
    remove_from_winner <- names(matches)[grep("^l|^L", names(matches))] %>%
        .[-which(. %in% c(w_opp_vars, "league"))]
    rename_l_inds <- which(names(matches) %in% w_opp_vars)
    names(matches_w)[rename_l_inds] <- opp_var_names

    ## Filter and rename variables
    matches_l <- dplyr::select(matches_l, -remove_from_loser)
    matches_w <- dplyr::select(matches_w, -remove_from_winner)
    names(matches_w) <- gsub("^winner_|^W|^w_", "", names(matches_w))
    names(matches_l) <- gsub("^loser_|^L|^l_", "", names(matches_l))

    ## Combine
    player_matches <- dplyr::bind_rows(matches_w, matches_l)

    return(player_matches)


}
