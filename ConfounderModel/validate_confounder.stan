// multioutput GP with Hilbert space approximation, 
// multiple patients multiple covariates

functions {
  
  real partial_sum_joint_lpdf(real[] slice_k,
                        int start,
                        int end,
                        real[] y_c,
                        int[] y_b,
                        int[] d_obs,
                        matrix d_obs_matrix,
                        matrix[] PHI,
                        vector diagSPD,
                        int M,
                        matrix[] eta,
                        matrix L_C_alpha,
                        vector sigma,
                        vector[] Beta,
                        int[] N_k,
                        int[] N_kc,
                        int[] N_kb,
                        int D,
                        vector d_ones
                        ) {
 
    // first row of first patient relative to whole dataset
    int startn = sum(N_k[1:start])- N_k[start] + 1;
    int startc = sum(N_kc[1:start])- N_kc[start] + 1;
    int startb = sum(N_kb[1:start])- N_kb[start] + 1;
    
    int length = sum(N_k[start:end]);
    int lengthc = sum(N_kc[start:end]);
    int lengthb = sum(N_kb[start:end]);

    matrix[D,end-start+1] mu;
    vector[D] mu_temp;
    vector[length] f_temp;
    array[lengthc] int d_obs_temp;
    array[lengthc] int fc_ind;
    array[lengthb] int fb_ind;
    matrix[M,D] SPD_eta;    
    int pos = 0;
    int posc = 0;
    int posb = 0;
 
    for (k in start:end){
  
    // mean    
      mu_temp = Beta[k];
      
    // HSGP
      SPD_eta = diag_pre_multiply(diagSPD, eta[k]);

      f_temp[(pos + 1):(pos + N_k[k])] = mu_temp[segment(d_obs, pos + startn, N_k[k])]
                + (((PHI[k,1:N_k[k],] * SPD_eta * L_C_alpha')
                .* block(d_obs_matrix, pos + startn, 1, N_k[k], D)) * d_ones);
                
      d_obs_temp[(posc + 1):(posc + N_kc[k])] = segment(d_obs, pos + startn, N_kc[k]);
      
      for (nc in 1:N_kc[k]){fc_ind[posc + nc] = nc + pos;}
      for (nb in 1:N_kb[k]){fb_ind[posb + nb] = nb + N_kc[k] + pos;}

      pos  = pos + N_k[k];
      posc = posc + N_kc[k];
      posb = posb + N_kb[k];
    }
    // joint likelihood
      return normal_lupdf(segment(y_c, startc, lengthc) | f_temp[fc_ind], sigma[d_obs_temp]) 
           + bernoulli_logit_lupmf(segment(y_b, startb, lengthb) | f_temp[fb_ind]);
  }

// from https://github.com/gabriuma/basis_functions_approach_to_GP/blob/master/uni_dimensional/simulated_dataset/stancode_BF_1dim.stan
 real lambda(real L, int m) {
		real lam;
		lam = ((m*pi())/(2*L))^2;
				
		return lam;
	}
	real spd(real alpha, real rho, real w) {
		real S;
		S = (alpha^2) * sqrt(2*pi()) * rho * exp(-0.5*(rho^2)*(w^2));
				
		return S;
	}
	vector phi(real L, int m, vector x) {
		vector[rows(x)] fi;
		fi = 1/sqrt(L) * sin(m*pi()/(2*L) * (x+L));
				
		return fi;
	}
}
data {
  int<lower=1> N_obs;
  int<lower=1> K; // number of patients
  int<lower=1> D; //  output dimensions (continuous first, then binary)
  int<lower=1> B; // number of baseline covariates
  
  // baseline characteristics
  matrix[K,B] dfb;
  
  int N_wb; // number of the baseline variables used for each confounder mean
  array[D,N_wb] int which_b; // which baseline data points to use for each confounder
  
  // number of observations per patient
  // organize data sequentially by patient
  array[K] int<lower=1> N_k;
  array[K] int<lower=1> N_kc; // continuous obs
  array[K] int<lower=1> N_kb; // binary obs

  // observation times
  array[N_obs] real x_obs;
  array[N_obs] int<lower=1,upper=D> d_obs;
  matrix[N_obs, D] d_obs_matrix;
  array[N_obs] int<lower=1,upper=K> k_obs;
  
  // continuous outcomes
  int<lower=0> N_c; // number of continuous observations
  int<lower=0> D_c; // number of continuous outcome dimensions 
  array[N_c] real y_c; // outcome values
 
  // binary outcomes 
  int<lower=0> N_b; // number of continuous observations
  int<lower=0> D_b; // number of binary outcome dimensions
  array[N_b] int<lower=0,upper=1> y_b; // outcome values
  
  // for interpolations, ppc, and simulated confounder trajectories
  int<lower=0> N_pred; // total predictions
  array[K] int N_k_pred; // predictions per patient
  array[N_pred] real x_pred; // prediction times for each patient
  array[N_pred] int d_pred; // which variable to predict at each time
  array[N_pred] real y_obs; // true observed value
  array[N_pred] int k_pred; // which patient is this prediction for
  array[N_pred] int obk_pred; // which obs number for each patient
  
  // for Hilbert space basis function approximation
  int M; // number of basis functions
  real L; // boundary condition factor
  
  vector<lower=0>[D] alpha;
  real<lower=0> rho;

  // noise and offset 
  vector<lower=0>[D_c] sigma;
  vector[D_b] offset;
  
  matrix[D,D] L_C; // correlation matrix across outputs

  matrix[11,D] beta_mu; // coefficients of baseline variables

}

transformed data {
  
// baseline data
  // make matrix of baseline data arranged for each patient
  // because we only use a subset of variables for each confounder
  
  array[K] matrix[D,N_wb] B_k;
  array[K] vector[D] Beta;
  
  vector[D] mu_hat = append_row(rep_vector(0.0, D_c),
                                offset);
  
  for (k in 1:K){
    for (d in 1:D){
      B_k[k,d,] = dfb[k,which_b[d,]];
    }
    Beta[k] = diagonal(B_k[k] * beta_mu) + mu_hat;
  }

// because they are data we can precalculate covariances

  matrix[D,D] L_C_alpha = diag_pre_multiply(alpha, L_C);

// make dummy arrays for reduce sum
  array[K] real id_real = rep_array(1.0, K);
  
  vector[D] d_ones = rep_vector(1.0, D);
  
  real delta = 1e-5;
// make an array of matrices big enough to accommodate each patient
  array[K] int<lower=0> phi_size;

// center the x for HSGP performance
  real xmean = 0.3; 
  real xsd = 0.4; 
  array[N_obs]  real x1;
  array[N_pred] real x2;
  for (n in 1:N_obs)  x1[n] = (x_obs[n]-xmean)/xsd;
  for (n in 1:N_pred) x2[n] = (x_pred[n]-xmean)/xsd;

// build a counter for observations by patient
  array[N_obs] int obs_by_k;
  {
    int posk = 0;
    for (k in 1:K){
      for (n in 1:N_k[k]){
        obs_by_k[posk+n] = n;
      }
      posk = posk + N_k[k];
    }
  }

// build time-based covariance matrices  

   for (k in 1:K){
    phi_size[k] = N_k[k] + N_k_pred[k];
    }
  array[K] matrix[max(phi_size), M] PHI;

  // build the approximate matrices patient by patient
  {
    int pos = 1;
    for (k in 1:K){
      for (m in 1:M){
        PHI[k,1:(phi_size[k]),m] = 
          phi(L, m, append_row(
            segment(to_vector(x1), pos, N_k[k]), to_vector(x2[1:N_k_pred[k]])));
      }
      pos = pos + N_k[k];
    }
  }

// build diagonal SPD vector
    vector[M] diagSPD;
    for (m in 1:M){
      diagSPD[m] = sqrt(spd(1.0, rho, sqrt(lambda(L, m))));
      }
      
}

parameters {
//  intercept
  
  // HSGP 
  array[K] matrix[M,D] eta;
  
}

transformed parameters{

}

model {

// PRIORS

  // HSGP
  for(k in 1:K){
    to_vector(eta[k]) ~ std_normal();
  }


// MODEL  
  {
    target += reduce_sum(partial_sum_joint_lpdf,
                        id_real, 1, 
                        y_c, y_b, 
                        d_obs, d_obs_matrix,
                        PHI, diagSPD, M, eta, L_C_alpha, sigma,
                        Beta,
                        N_k, N_kc, N_kb, D, d_ones);
  }     
}

generated quantities{

  vector[N_pred] f_pred;
  array[N_pred] real y_pred;
  array[N_pred] real error;

  {
    array[K] vector[D] mu_temp;
    array[K] matrix[M,D] SPD_eta;    
    
    for (k in 1:K){
      SPD_eta[k] = diag_pre_multiply(diagSPD, eta[k]);
      mu_temp[k] = Beta[k];
    }
    
    for (n in 1:N_pred){
  
    // HSGP
    
      f_pred[n] = mu_temp[k_pred[n],d_pred[n]] 
                + PHI[k_pred[n], N_k[k_pred[n]] + obk_pred[n], ]
                * SPD_eta[k_pred[n]]
                * L_C_alpha[d_pred[n],]';
      
      if(d_pred[n] <= D_c){
        y_pred[n] = normal_rng(f_pred[n], sigma[d_pred[n]]);
      } else {
        y_pred[n] = inv_logit(f_pred[n]);
      }
      error[n] = y_pred[n]-y_obs[n];
    }
  }     
}
