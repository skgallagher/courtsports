
#' Model the individuals using stepwise linear regression
#'
#' @param player_name player name
#' @param data default is gs_partial_players
#' @param ref reference court.  Default is French Open
#' @param start_lower logical indicating whether the base model should be the lower model.
#' @return best model fit
model_individual <- function(player_name = "Rafael Nadal", data = gs_partial_players,
                             ref = "French Open",
                             start_lower = FALSE){

  data_sub <- dplyr::filter(gs_partial_players, name %in% c(player_name)) %>%
    dplyr::mutate(pct_ace = n_aces / n_netpt,
                  pct_netpt = n_netpt_w / n_netpt,
                  pct_bp = n_bp_w / n_bp,
                  wue_ratio = n_winners / n_ue,
                  pct_pts = pointswon / total_points)

  data_sub$court <- relevel(data_sub$tournament, ref = ref)

  mod_data_sub_low <- glm(pct_pts ~ I(court) + opponent_rank, data = data_sub, family = gaussian)

  mod_data_sub_full <- glm(pct_pts ~ I(court) + opponent_rank  +
                          wue_ratio +
                          ave_serve_speed +
                          pct_ace+
                          pct_bp +
                          pct_netpt +
                          wue_ratio * I(court)  +
                          ave_serve_speed * I(court) +
                          pct_ace* I(court) +
                          pct_bp * I(court) +
                          pct_netpt * I(court),
                        data = data_sub, family = gaussian)

  object <- mod_data_sub_full
  if(start_lower){
    object <- mod_data_sub_low
  }
  step_model <- MASS::stepAIC(object = mod_data_sub_full,
                        scope = list(upper = mod_data_sub_full,
                                     lower = mod_data_sub_low),
                        direction = "both",
                        trace = FALSE)

  return(step_model)
  }
