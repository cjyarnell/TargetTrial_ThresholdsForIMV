# functions for predicting outcomes

# functions for every rule

extract_traj <- function(y_pred, i, a){
  traj <- filter(y_pred, iter == i, traj == a) %>%
    select(-iter, -traj, -var) %>%
    pivot_wider(names_from = variable,
                values_from = value)
  traj
}

usual_care_imv <- function(traj){
  max(traj$imv)
}

time_of_usual_care_imv <- function(traj){
  which.max(traj$imv)
}

discharge <- function(traj){
  max(traj$icu_dc)
}

time_of_discharge <- function(traj){
  which.max(traj$icu_dc)
}

death <- function(traj){
  max(traj$death)
}

time_of_death <- function(traj){
  which.max(traj$death)
}

pressor <- function(traj){
  prs <- traj$pressor + lag(traj$pressor) + lag(lag(traj$pressor)) + lag(lag(lag(traj$pressor)))    
  (max(as.numeric(prs), na.rm = T) == 4)
}

time_of_pressor <- function(traj){
  prs <- traj$pressor + lag(traj$pressor, k=1) + lag(traj$pressor, k=2) + lag(traj$pressor, k=3) + lag(traj$pressor, k = 4)    
  which.max(prs==4)
}

GCSthresh <- function(traj){
  gcs <- traj$GCS + lag(traj$GCS)
  (max(as.numeric(gcs), na.rm=T) == 2)
}

time_of_GCSthresh <- function(traj){
  gcs <- (traj$GCS<9) + lag(traj$GCS<9)
  which.max(gcs == 2)
}


# function to load all the trajectories for the given patient
load_y_pred <- function(pt_stay_id){
  # load a trajectory
  y_pred <- readRDS(paste0(dir,
                           "/y_pred",
                           pt_stay_id,
                           ".rds"))
  y_pred <- y_pred %>%
    ungroup()
}           

load_eta <- function(pt_stay_id){
  eta <- readRDS(paste0(dir, "/etasim",pt_stay_id, ".rds"))
  eta
}


# ROX
rox <- function(traj){
  100*traj$spo2/traj$fio2/traj$resp_rate
}

rox_threshold <- function(traj, x){
  (rox(traj) < x) & (traj$niv == 1 | traj$hfnc == 1 | traj$stdo2 == 1)
}

# S:F 
sf <- function(traj){
  100*traj$spo2/traj$fio2
}

sf_threshold <- function(traj, x){
  (sf(traj) < x) & (traj$niv == 1 | traj$hfnc == 1 | traj$stdo2 == 1)
}

delta_threshold <- function(traj, x){
    if(nrow(traj) == 1){
        return(0)
    } else {
        delta <- (sf(traj) - lag(sf(traj)))/2
        return((sf(traj) + delta*x < 88) &
                (traj$niv == 1 | traj$hfnc == 1 |  traj$stdo2 == 1) &
                (lag(traj$niv) == 1 | lag(traj$hfnc) == 1 | lag(traj$stdo2 == 1)))
    }
}

# SF and RR

sfrr_threshold <- function(traj, x){
     (sf(traj) < 98) & 
     (traj$resp_rate > x) &
     (traj$niv == 1 | traj$hfnc == 1 | traj$stdo2 == 1)
}

sfwob_threshold <- function(traj, x){
     (sf(traj) < 98) & 
     (traj$wob == 1) &
     (traj$niv == 1 | traj$hfnc == 1 | traj$stdo2 == 1)
}


# HACOR
# use Pandharipande 2009 to convert sf to pf
# Log10(PF)= 0.06 + 0.94 × Log10(SF)
# 10^((log10(200)-0.06)/0.94) = 242
# 10^((log10(175)-0.06)/0.94) = 210
# 10^((log10(150)-0.06)/0.94) = 178
# 10^((log10(125)-0.06)/0.94) = 147
# 10^((log10(125)-0.06)/0.94) = 115


hacor <- function(traj){
  hr_points <- ifelse(traj$heart_rate > 120,1,0)
  ph_points <- ifelse(is.na(traj$ph), 0,  
                      ifelse(traj$ph >= 7.35, 0,
                      ifelse(traj$ph >= 7.30, 2,
                             ifelse(traj$ph >= 7.25, 3, 4))))
  gcs_points <- ifelse(traj$GCS == 15, 0,
                       ifelse(traj$GCS >= 13, 2,
                              ifelse(traj$GCS >= 11, 5, 10)))
  sf_points <- ifelse(sf(traj) > 242, 0,
                      ifelse(sf(traj) > 210, 2,
                             ifelse(sf(traj) > 178, 3,
                                    ifelse(sf(traj) > 147, 4,
                                           ifelse(sf(traj) > 115, 5, 6)))))
  rr_points <- ifelse(traj$resp_rate <= 30, 0,
                      ifelse(traj$resp_rate <= 35, 1,
                             ifelse(traj$resp_rate <= 40, 2, 
                                    ifelse(traj$resp_rate <= 45, 3, 4))))
  
  hacor_points <- hr_points + ph_points + gcs_points + sf_points + rr_points
  hacor_points
}

hacor_threshold <- function(traj, x){
  (hacor(traj) > x) & (traj$niv == 1 | traj$hfnc == 1 | traj$stdo2 == 1)
}

# FLORALI

florali <- function(traj){
  f_sat <- (traj$spo2 < 90) & (traj$fio2 >= 90)
  f_rr  <- traj$resp_rate > 40
  f_wob <- traj$wob == 1
  f_ph  <- (traj$ph < 7.35) %in% TRUE
  f_gcs <- traj$GCS < 9
  f_prs <- traj$pressor == 1
  
  f_resp <- (f_sat + f_rr + f_wob + f_ph)>=2
  f_all <- f_resp | f_gcs | f_prs
  f_all
}

florali_threshold <- function(traj,x){
  florali(traj)  & (traj$niv == 1 | traj$hfnc == 1 | traj$stdo2 == 1)
}

# DeMontmollin / Bauer

demontb <- function(traj){
  dmb_sf <- sf(traj) < 120
  dmb_rr <- traj$resp_rate > 30
  dmb_wob<- traj$wob == 1
  dmb_all <- dmb_sf & dmb_rr & dmb_wob
  dmb_all
}

demontb_threshold <- function(traj,x){
  (demontb(traj) > 0)  & (traj$niv == 1 | traj$hfnc == 1 | traj$stdo2 == 1)
}

# MOD

mod <- function(traj){
  mod_resp <- (sf(traj) < 120) & (traj$wob == 1)
  mod_rr <- traj$resp_rate > 35
  mod_phpco2 <- (traj$ph < 7.35) & (traj$pco2 > 50)
  mod_lactate <- traj$lactate > 3
  mod_gcs <- traj$GCS < 12 
  mod <- mod_resp & (mod_rr | mod_phpco2 | mod_lactate | mod_gcs)
}

mod_threshold <- function(traj,x){
  (mod(traj) > 0)  & (traj$niv == 1 | traj$hfnc == 1 | traj$stdo2 == 1)
}

modAnd <- function(traj){
  f_sat <- (traj$spo2 < 90) & (traj$fio2 >= 90)
  f_rr  <- traj$resp_rate > 40
  f_wob <- traj$wob == 1
  f_ph  <- (traj$ph < 7.35) %in% TRUE
  f_gcs <- traj$GCS < 9
  f_prs <- traj$pressor == 1
  
  f_resp <- (f_sat + f_rr + f_wob + f_ph)>=2
  f_all <- f_resp & ( f_gcs | f_prs )
  f_all
}

modAnd_threshold <- function(traj,x){
  (modAnd(traj) > 0)  & (traj$niv == 1 | traj$hfnc == 1 | traj$stdo2 == 1)
}


# function for the respiratory failure IMV rules
# takes the trajectory and rule number
# outputs time of IMV (or N_pred + 1 if none)
imv_rule <- function(traj, r){
  
  # Usual care threshold rules
  if(rules$type[r] == "UC"){
    imv_marks <- traj$imv
  }
  
  # SF threshold rules
  if(rules$type[r] == "SF"){
    imv_marks <- sf_threshold(traj, rules$threshold[r])
  }

  # SFRR threshold rules
  if(rules$type[r] == "SFRR"){
    imv_marks <- sfrr_threshold(traj, rules$threshold[r])
  }

    # SFWoB threshold rules
  if(rules$type[r] == "SFWoB"){
    imv_marks <- sfwob_threshold(traj, rules$threshold[r])
  }
  
  # ROX threshold rules
  if(rules$type[r] == "ROX"){
    imv_marks <- rox_threshold(traj, rules$threshold[r])
  }
  
  # HACOR threshold rules
  if(rules$type[r] == "HACOR"){
    imv_marks <- hacor_threshold(traj, rules$threshold[r])
  }
  
  # FLORALI threshold rules
  if(rules$type[r] == "FLORALI"){
    imv_marks <- florali_threshold(traj, rules$threshold[r])
  }
  
  # DeMontmollin Bauer threshold rules
  if(rules$type[r] == "DeMontBauer"){
    imv_marks <- demontb_threshold(traj, rules$threshold[r])
  }
  
  # change in SF threshold rules
  if(rules$type[r] == "Delta"){
      imv_marks <- delta_threshold(traj, rules$threshold[r])
  }
    
  # MOD threshold rules
  if(rules$type[r] == "MOD"){
    imv_marks <- mod_threshold(traj, rules$threshold[r])
  }
    
  # Vasopressor threshold rules
  if(rules$type[r] == "Vasopressor"){
    imv_marks <- traj$pressor
  }

  # multiorgan OR rule
  if(rules$type[r] == "MultiOrgan_Or"){
    imv_marks <- florali_threshold(traj, rules$threshold[r])
  }
    
  # multiorgan AND rule
     # FLORALI threshold rules
  if(rules$type[r] == "MultiOrgan_And"){
    imv_marks <- modAnd_threshold(traj, rules$threshold[r])
  }
 
  
  
  # check if duration criteria is met
  if (rules$duration[r] > 1){
    lags =  rep(0, length(imv_marks))
    for(d in 1:(rules$duration[r])){
      lags <- lags + lag(imv_marks, d-1)
    }
    imv_marks <- lags
  }
  imv_mark <- ifelse(max(imv_marks, na.rm = T) == rules$duration[r],which.max(imv_marks),N_pred+1)
  imv_mark
}

