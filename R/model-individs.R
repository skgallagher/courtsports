
#' Model the individuals using stepwise linear regression
#'
#' @param player_name player name
#' @param data default is gs_partial_players
#' @param ref reference court.  Default is French Open
#' @param start_lower logical indicating whether the base model should be the lower model.
#' @return list step model output, the final model on the training set,
#' the subsetted data, the training indices, and the number of total observations in the subsetted data (n_obs), the number of training obs (n_train), and number of testing obs (n_test)
#' @export
model_individual <- function(player_name = "Roger Federer",
                             data = gs_partial_players,
                             ref = "Wimbledon",
                             start_lower = FALSE,
                             test_prop = 0,
                             min_year = 2013,
                             max_year = 2017,
                             seed = NULL){

    if(!is.null(seed)){
        set.seed(seed)
    }
    ### Subset data to player
    data_sub <- dplyr::filter(data, name %in% c(player_name) &
                                    year >= min_year &
                                    year <= max_year
                              ) %>%
        dplyr::mutate(pct_ace = ifelse( n_sv > 0,
                                       n_aces / n_sv,
                                       0),
                      pct_netpt = ifelse(n_netpt > 0,
                                         n_netpt_w / n_netpt,
                                         0),
                      pct_bp = ifelse(n_bp > 0,
                                      n_bp_w / n_bp,
                                      0),
                      wue_ratio = ifelse(n_ue > 0,
                                         n_winners / n_ue,
                                         100), ## magic number
                      pct_pts = ifelse(total_points > 0,
                                       pointswon / total_points,
                                       0))

    ## Make the tournament of reference
    data_sub$court <- relevel(data_sub$tournament, ref = ref)
    
    ## Split into train and test data
    if(test_prop > 0){
        n_obs <- nrow(data_sub)
        n_inds <- ceiling(n_obs * ( 1 - test_prop))
        inds <- sample(1:n_obs, size = n_inds)
        data_train <- data_sub[inds,]
        data_test <- data_sub[-inds,]
        n_train <- nrow(data_train)
        n_test <- nrow(data_test)
    } else{
        data_train <- data_sub
        data_test <- data_sub
        n_obs <- nrow(data_sub)
        n_train <- n_obs
        n_test <- n_obs
        inds <- 1:n_obs
    }
    

    mod_data_sub_low <- glm(pct_pts ~ I(court) + opponent_rank,
                            data = data_train, family = gaussian)

    mod_data_sub_full <- glm(pct_pts ~ I(court) + opponent_rank  +
                                 wue_ratio +
                                 ave_serve_speed +
                                 pct_ace +
                                 pct_bp +
                                 pct_netpt +
                          wue_ratio * I(court)  +
                          ave_serve_speed * I(court) +
                          pct_ace* I(court) +
                          pct_bp * I(court) +
                          pct_netpt * I(court),
                        data = data_train, family = gaussian)

  object <- mod_data_sub_full
  if(start_lower){
    object <- mod_data_sub_low
  }
    step_model <- MASS::stepAIC(object = mod_data_sub_full,
                                scope = list(upper = mod_data_sub_full,
                                             lower = mod_data_sub_low),
                                direction = "both",
                                trace = FALSE)

    final_model <- glm(data = data_test, formula = step_model$formula,
                       family = gaussian)

    out_list <- list(step_model = step_model,
                     final_model = final_model,
                     n_obs = n_obs,
                     n_train = n_train,
                     n_test = n_test,
                     train_inds = inds,
                     data_sub = data_sub)
    return(out_list)
}
