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

    ## Points

    matches <- matches %>% dplyr::mutate(
                                      w_pointswon = w_1stWon + w_2ndWon +
                                          l_svpt - l_1stWon - l_2ndWon,
                                      l_pointswon = l_1stWon +
                                          l_2ndWon + w_svpt - w_1stWon - w_2ndWon)

    ## Games
    ## Get rid of retirements and walkover, the leading space, and any tiebreaker scores
    score_no_retire <- gsub("RET|W\\/O| RET| W\\/O|\\([0-9]+\\)", "", matches$score)
    score_list <- strsplit(score_no_retire, " ")
    matches$w_gameswon <- sapply(score_list, function(vec){
      sum(as.integer(gsub("\\-[0-9]+$","", vec)))
    })


    matches$l_gameswon <- sapply(score_list, function(vec){
      sum(as.integer(gsub("^[0-9]+\\-","", vec)))
    })

    ## Sets
    ## a set is won if they scored more games in the set than opponent
    matches$w_setswon <-  sapply(score_list, function(vec){
        bools <- sapply(vec, function(score){
          eval(parse(text = score)) > 0
        })
        bools <- ifelse(is.na(bools), 0, bools)
        sum(bools)
    })

     matches$l_setswon <-  sapply(score_list, function(vec){
       bools <- sapply(vec, function(score){
         eval(parse(text = score)) < 0
       })
       bools <- ifelse(is.na(bools), 0, bools)
       sum(bools)
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


    matches$total_points <- matches$w_pointswon + matches$l_pointswon
    ## Copy data frames
    matches_l <- matches
    matches_w <- matches
    matches_l$did_win <- 0 # no did not win
    matches_w$did_win <- 1 # yes did win

    ## Get rid of winner attributes for the loser except for l_opp_vars
    remove_from_loser <- names(matches)[grep("^w|^W", names(matches))]
    remove_from_loser <- remove_from_loser[-which(remove_from_loser == l_opp_vars)]
    rename_w_inds <- which(names(matches) %in% l_opp_vars)
    names(matches_l)[rename_w_inds] <- opp_var_names

    ## Get rid of loser attributes for the winner except for w_opp_vars
    remove_from_winner <- names(matches)[grep("^l|^L", names(matches))]
    remove_from_winner <- remove_from_winner[-which(remove_from_winner %in% c(w_opp_vars, "league"))]
    rename_l_inds <- which(names(matches) %in% w_opp_vars)
    names(matches_w)[rename_l_inds] <- opp_var_names

    ## Filter and rename variables
    matches_l <- matches_l %>% dplyr::select(-remove_from_loser)
    matches_w <- matches_w %>% dplyr::select(-remove_from_winner)
    names(matches_w) <- gsub("^winner_|^W|^w_", "", names(matches_w))
    names(matches_l) <- gsub("^loser_|^L|^l_", "", names(matches_l))

    ## Combine
    player_matches <- dplyr::bind_rows(matches_w, matches_l)

    return(player_matches)


}


#' Summarize the point by point data
#'
#' @param pbp point by point data from deuce package
#' @return summary of point by point data in terms of unique matches
#' @importFrom magrittr %>%
summarize_pbp <- function(pbp,
                          start_year = 2013, end_year = 2017){

    ## Omit non-complete columns
    na_col_inds <- colSums(is.na(pbp))
    complete_cols <- which(na_col_inds <= 1000)
    pbp <- pbp %>% dplyr::select(names(pbp[complete_cols])) %>%
        dplyr::filter(year >= start_year& year <= end_year)
    ## Of these, which rows have NAs?
    na_row_inds <- which(rowSums(is.na(pbp)) > 0)
    ## Find matches associated with NAs
    missing_match_ids <- unique(pbp$match_id[na_row_inds])
    ## Exclude entire matches which have any missing values of above relevant columns
    pbp <- pbp %>% dplyr::filter(!(match_id %in% missing_match_ids))
    

    ## Change tournament to match the names of gs data
    pbp$tournament <- factor(pbp$slam) %>%
        forcats::fct_collapse(
                     "US Open" = c("usopen"),
                     "French Open" = c("frenchopen"),
                     "Wimbledon" = c("wimbledon"),
                     "Australian Open" = c("ausopen"))


    ## Group by unique matches

    pbp_matches <- pbp %>% group_by(match_id, player1, player2, 
                                    slam, year, tournament) %>%
        dplyr::summarize(ave_serve_speed_p1 = mean(Speed_MPH[PointServer == 1]),
                         ave_serve_speed_p2 = mean(Speed_MPH[PointServer == 2]),
                         n_aces_p1 = sum(P1Ace),
                         n_aces_p2 = sum(P2Ace),
                         n_winners_p1 = sum(P1Winner),
                         n_winners_p2 = sum(P2Winner),
                         n_netpt_w_p1 = sum(P1NetPointWon),
                         n_netpt_w_p2 = sum(P1NetPointWon),
                         n_netpt_p1 = sum(P1NetPoint),
                         n_netpt_p2 =  sum(P1NetPoint),
                         n_bp_w_p1 = sum(P1BreakPointWon),
                         n_bp_w_p2 = sum(P2BreakPointWon),
                         n_bp_p1 =  sum(P1BreakPoint),
                         n_bp_p2 = sum(P2BreakPoint),
                         n_ue_p1 = sum(P1UnfErr),
                         n_ue_p2 = sum(P2UnfErr),
                         n_sv_w_p1 = sum((PointServer == 1) * (PointWinner == 1 )),
                         n_sv_w_p2 = sum((PointServer == 2) * (PointWinner == 2 )),
                         n_sv_p1 =   sum(PointServer == 1),
                         n_sv_p2 =   sum(PointServer == 2),
                         a1 = sort(c(player1, player2))[1],
                         a2 = sort(c(player1, player2), decreasing = TRUE)[1]
                         )
    return(pbp_matches)


}


#' Join gs and gs_pbp together
#'
#' @param gs subsetted gs object
#' @param gs_pbp point by point summarized by matches
#' @return joined data frame by tournament, year, and players, and added a week column
#' @importFrom magrittr %>%
join_gs_pbp <- function(gs_pbp, gs){


    ## Subset gs
    gs_sub <- gs %>% group_by(match_id, tournament, year, winner_name, loser_name, winner_rank,
                              loser_rank, winner_age, winner_ioc, Retirement,
                              w_pointswon, w_gameswon, w_setswon,
                              l_pointswon, l_gameswon, l_setswon,
                              round, league) %>%
        summarize(a1 = sort(c(winner_name, loser_name))[1],
                  a2 = sort(c(winner_name, loser_name), decreasing = TRUE)[1])


    gs_join <- na.omit(left_join(gs_sub, gs_pbp, by = c("tournament", "year", "a1", "a2")))

    ## Add week variable
    gs_join$week <- gs_join$round %>%
    forcats::fct_collapse("Week 1" = c("R128","R32", "R64" ),
                          "Week 2" = c("R16", "QF", "SF", "F"))

    return(gs_join)

}

#' Combine fields to have easier access
#'
#' @param gs_joint result from join_gs_pbp
#' @return combined fields
combine_fields <- function(gs_join){


    player_inds <- grep("_p[0-9]$", colnames(gs_join), value = TRUE)
    player_inds

    player_inds <- grep("_p[0-9]$", colnames(gs_join))
    mat <- matrix(0, nrow = nrow(gs_join), ncol = length(player_inds))
    df <- data.frame(mat)
    unique_col_names <- gsub("_p[0-9]$", "", colnames(gs_join)[player_inds])
    colnames(df) <- paste0(rep(c("w","l"), length(player_inds) / 2), "_", unique_col_names)
    for(ii in 1:(ncol(df)/2)){
        for(jj in 1:nrow(df)){
            col_name <- unique_col_names[2 * ii]
            ## Winner column
            df[jj, 2 * ii - 1] <- ifelse(gs_join$player1[jj] == gs_join$winner_name[jj],
                                       gs_join[jj, paste0(col_name, "_p1")],
                                       gs_join[jj, paste0(col_name, "_p2")])

            ## Loser column
            df[jj, 2 * ii] <- ifelse(gs_join$player1[jj] == gs_join$winner_name[jj],
                                   gs_join[jj, paste0(col_name, "_p2")],
                                   gs_join[jj, paste0(col_name, "_p1")])


        }

    }
    out <- dplyr::bind_cols(gs_join[, -player_inds], df)
    return(out)

}
