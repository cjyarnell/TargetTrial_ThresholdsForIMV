// multioutput GP with Hilbert space approximation, 
// multiple patients multiple covariates
// no predictions
// includes IMV variable

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
                        matrix L_C,
                        vector alpha,
                        vector sigma,
                        vector mu_hat,
                        matrix[] B_k,
                        matrix beta_mu,
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
      mu_temp = mu_hat + diagonal(B_k[k] * beta_mu);
      
    // HSGP
      SPD_eta = diag_pre_multiply(diagSPD, eta[k]);

      f_temp[(pos + 1):(pos + N_k[k])] = mu_temp[segment(d_obs, pos + startn, N_k[k])]
                + (((PHI[k,1:N_k[k],] * SPD_eta * diag_pre_multiply(alpha, L_C)')
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
  int<lower=1> K; // number of patients - train set first, test set second
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
  
  // for Hilbert space basis function approximation
  int M; // number of basis functions
  real L; // boundary condition factor
  
}

transformed data {
  
// baseline data
  // make matrix of baseline data arranged for each patient
  // because we only use a subset of variables for each confounder
  
  array[K] matrix[D,N_wb] B_k;
  
  for (k in 1:K){
    for (d in 1:D){
      B_k[k,d,] = dfb[k,which_b[d,]];
    }
  }
  
// make dummy arrays for reduce sum
  array[K] real id_real = rep_array(1.0, K);
  vector[D] d_ones = rep_vector(1.0, D);

// make an array of matrices big enough to accommodate each patient
  array[K] int<lower=0> phi_size;

// center the x for HSGP performance
// range is -0.25 to 2
// so subtract 0.75 and divide by 1
// then it goes from -1 to 1.25
  real xmean = 0.75; 
  real xsd = 1;
  array[N_obs]  real x1;
  for (n in 1:N_obs)  x1[n] = (x_obs[n]-xmean)/xsd;
  

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
    phi_size[k] = N_k[k];
    }

  array[K] matrix[max(phi_size), M] PHI;

  // build the approximate matrices patient by patient
  {
    int pos = 1;
    for (k in 1:K){
      for (m in 1:M){
        PHI[k,1:(phi_size[k]),m] = 
          phi(L, m, segment(to_vector(x1), pos, N_k[k]));
      }
      pos = pos + N_k[k];
    }
  }
// define offsets and multipliers
  // rho
  real logrho_offset = -1.46;
  real logrho_sd     = 0.0053;
  
  // alpha
  real logalpha1_offset = -0.44;
  real logalpha1_sd = 0.05;
  real logalpha2_offset = -0.44;
  real logalpha2_sd = 0.04;
  real logalpha3_offset = -0.32;
  real logalpha3_sd = 0.03;
  real logalpha4_offset = -0.41;
  real logalpha4_sd = 0.04;
  real logalpha5_offset = -0.07;
  real logalpha5_sd = 0.04;
  real logalpha6_offset = -0.38;
  real logalpha6_sd = 0.07;
  real logalpha7_offset = -0.40;
  real logalpha7_sd = 0.06;
  real logalpha8_offset = -0.45;
  real logalpha8_sd = 0.06;
  real logalpha9_offset = -0.43;
  real logalpha9_sd = 0.05;
  real logalpha10_offset = 1.29;
  real logalpha10_sd = 0.2;
  real logalpha11_offset = 2.7;
  real logalpha11_sd = 0.2;
  real logalpha12_offset = 2.7;
  real logalpha12_sd = 0.2;
  real logalpha13_offset = 2.9;
  real logalpha13_sd = 0.2;
  real logalpha14_offset = 2.8;
  real logalpha14_sd = 0.2;
  real logalpha15_offset = 0.46;
  real logalpha15_sd = 0.3;
  real logalpha16_offset = 1.2;
  real logalpha16_sd = 0.5;
  real logalpha17_offset = 1.2;
  real logalpha17_sd = 0.5;
  
  // noise
  real logsigma1_offset = -0.56;
  real logsigma1_sd = 0.03;
  real logsigma2_offset = -0.4;
  real logsigma2_sd = 0.03;
  real logsigma3_offset = -1;
  real logsigma3_sd = 0.03;
  real logsigma4_offset = -0.33;
  real logsigma4_sd = 0.03;
  real logsigma5_offset = -1;
  real logsigma5_sd = 0.03;
  real logsigma6_offset = -1.05;
  real logsigma6_sd = 0.1;
  real logsigma7_offset = -0.96;
  real logsigma7_sd = 0.1;
  real logsigma8_offset = -1.15;
  real logsigma8_sd = 0.1;
  real logsigma9_offset =  -0.63;
  real logsigma9_sd = 0.04;


  // offset
  real offset1_offset = -3;
  real offset1_sd = 0.5;
  real offset2_offset = -20;
  real offset2_sd = 3;
  real offset3_offset = -14;
  real offset3_sd = 1.7;
  real offset4_offset = -7;
  real offset4_sd = 1.1;
  real offset5_offset = -14;
  real offset5_sd = 1.5;
  real offset6_offset = -6;
  real offset6_sd = 0.5;
  real offset7_offset = -13;
  real offset7_sd = 2;
  real offset8_offset = -8;
  real offset8_sd = 1.5;

  // prior sd
  
  real prior_sd_rho = 0.2;
  real prior_sd_alpha_cont = 1;    
  real prior_sd_alpha_bin = 2;
  real prior_sd_sigma = 1;
  real prior_sd_offset = 10;
  
  // continuous global means (centered already)
  vector[D_c] zeros = rep_vector(0.0, D_c);
}

parameters {
//  intercept
  matrix[11,D] beta_mu; // coefficients of baseline variables

  // HSGP 
  real<offset=logrho_offset,multiplier = logrho_sd> logrho;
  
  real<offset=logalpha1_offset,multiplier=logalpha1_sd> logalpha1;
  real<offset=logalpha2_offset,multiplier=logalpha2_sd> logalpha2;
  real<offset=logalpha3_offset,multiplier=logalpha3_sd> logalpha3;
  real<offset=logalpha4_offset,multiplier=logalpha4_sd> logalpha4;
  real<offset=logalpha5_offset,multiplier=logalpha5_sd> logalpha5;
  real<offset=logalpha6_offset,multiplier=logalpha6_sd> logalpha6;
  real<offset=logalpha7_offset,multiplier=logalpha7_sd> logalpha7;
  real<offset=logalpha8_offset,multiplier=logalpha8_sd> logalpha8;
  real<offset=logalpha9_offset,multiplier=logalpha9_sd> logalpha9;
  real<offset=logalpha10_offset,multiplier=logalpha10_sd> logalpha10;
  real<offset=logalpha11_offset,multiplier=logalpha11_sd> logalpha11;
  real<offset=logalpha12_offset,multiplier=logalpha12_sd> logalpha12;
  real<offset=logalpha13_offset,multiplier=logalpha13_sd> logalpha13;
  real<offset=logalpha14_offset,multiplier=logalpha14_sd> logalpha14;
  real<offset=logalpha15_offset,multiplier=logalpha15_sd> logalpha15;
  real<offset=logalpha16_offset,multiplier=logalpha16_sd> logalpha16;
  real<offset=logalpha17_offset,multiplier=logalpha17_sd> logalpha17;
  
  array[K] matrix[M,D] eta;
  
  // noise
  real<offset=logsigma1_offset, multiplier=logsigma1_sd> logsigma1;
  real<offset=logsigma2_offset, multiplier=logsigma2_sd> logsigma2;
  real<offset=logsigma3_offset, multiplier=logsigma3_sd> logsigma3;
  real<offset=logsigma4_offset, multiplier=logsigma4_sd> logsigma4;
  real<offset=logsigma5_offset, multiplier=logsigma5_sd> logsigma5;
  real<offset=logsigma6_offset, multiplier=logsigma6_sd> logsigma6;
  real<offset=logsigma7_offset, multiplier=logsigma7_sd> logsigma7;
  real<offset=logsigma8_offset, multiplier=logsigma8_sd> logsigma8;
  real<offset=logsigma9_offset, multiplier=logsigma9_sd> logsigma9;
  
  // offset
  real<offset=offset1_offset, multiplier=offset1_sd> offset1;
  real<offset=offset2_offset, multiplier=offset2_sd> offset2;
  real<offset=offset3_offset, multiplier=offset3_sd> offset3;
  real<offset=offset4_offset, multiplier=offset4_sd> offset4;
  real<offset=offset5_offset, multiplier=offset5_sd> offset5;
  real<offset=offset6_offset, multiplier=offset6_sd> offset6;
  real<offset=offset7_offset, multiplier=offset7_sd> offset7;
  real<offset=offset8_offset, multiplier=offset8_sd> offset8;
  
  // correlation matrices
  cholesky_factor_corr[D] L_C; // correlation matrix across outputs

}

transformed parameters{
  real<lower=0> rho =exp(logrho);
  vector<lower=0>[D] alpha = exp([ logalpha1
                                 , logalpha2
                                 , logalpha3
                                 , logalpha4
                                 , logalpha5
                                 , logalpha6
                                 , logalpha7
                                 , logalpha8
                                 , logalpha9
                                 , logalpha10
                                 , logalpha11
                                 , logalpha12
                                 , logalpha13
                                 , logalpha14
                                 , logalpha15
                                 , logalpha16
                                 , logalpha17
                                 ]');

  // noise and offset 
  vector<lower=0>[D_c] sigma = exp([
      logsigma1
    , logsigma2
    , logsigma3
    , logsigma4
    , logsigma5
    , logsigma6
    , logsigma7
    , logsigma8
    , logsigma9
  ]');

  // noise and offset 
  vector[D_b] offset = [
      offset1
    , offset2
    , offset3
    , offset4
    , offset5
    , offset6
    , offset7
    , offset8
  ]';

   vector[M] diagSPD;
    for (m in 1:M){
      diagSPD[m] = sqrt(spd(1.0, rho, sqrt(lambda(L, m))));
    }
    
    vector[D] mu_hat = append_row(zeros, offset);

}

model {

// PRIORS

  // means / offsets
  to_vector(beta_mu) ~ normal(0,0.5);

  // HSGP
  logrho ~ normal(logrho_offset,logrho_sd); 
  
  logalpha1  ~ normal(logalpha1_offset , prior_sd_alpha_cont);
  logalpha2  ~ normal(logalpha2_offset , prior_sd_alpha_cont);
  logalpha3  ~ normal(logalpha3_offset , prior_sd_alpha_cont);
  logalpha4  ~ normal(logalpha4_offset , prior_sd_alpha_cont);
  logalpha5  ~ normal(logalpha5_offset , prior_sd_alpha_cont);
  logalpha6  ~ normal(logalpha6_offset , prior_sd_alpha_cont);
  logalpha7  ~ normal(logalpha7_offset , prior_sd_alpha_cont);
  logalpha8  ~ normal(logalpha8_offset , prior_sd_alpha_cont);
  logalpha9  ~ normal(logalpha9_offset , prior_sd_alpha_cont);
  logalpha10 ~ normal(logalpha10_offset, prior_sd_alpha_bin);
  logalpha11 ~ normal(logalpha11_offset, prior_sd_alpha_bin);
  logalpha12 ~ normal(logalpha12_offset, prior_sd_alpha_bin);
  logalpha13 ~ normal(logalpha13_offset, prior_sd_alpha_bin);
  logalpha14 ~ normal(logalpha14_offset, prior_sd_alpha_bin);
  logalpha15 ~ normal(logalpha15_offset, prior_sd_alpha_bin);
  logalpha16 ~ normal(logalpha16_offset, prior_sd_alpha_bin);
  logalpha17 ~ normal(logalpha17_offset, prior_sd_alpha_bin);

  for(k in 1:K){
    to_vector(eta[k]) ~ std_normal();
  }

  // noise in continuous variables
  logsigma1  ~ normal(logsigma1_offset , prior_sd_sigma);
  logsigma2  ~ normal(logsigma2_offset , prior_sd_sigma);
  logsigma3  ~ normal(logsigma3_offset , prior_sd_sigma);
  logsigma4  ~ normal(logsigma4_offset , prior_sd_sigma);
  logsigma5  ~ normal(logsigma5_offset , prior_sd_sigma);
  logsigma6  ~ normal(logsigma6_offset , prior_sd_sigma);
  logsigma7  ~ normal(logsigma7_offset , prior_sd_sigma);
  logsigma8  ~ normal(logsigma8_offset , prior_sd_sigma);
  logsigma9  ~ normal(logsigma9_offset , prior_sd_sigma);

  // offset in binary variables
  offset1  ~ normal(offset1_offset , prior_sd_offset);
  offset2  ~ normal(offset2_offset , prior_sd_offset);
  offset3  ~ normal(offset3_offset , prior_sd_offset);
  offset4  ~ normal(offset4_offset , prior_sd_offset);
  offset5  ~ normal(offset5_offset , prior_sd_offset);
  offset6  ~ normal(offset6_offset , prior_sd_offset);
  offset7  ~ normal(offset7_offset , prior_sd_offset);
  offset8  ~ normal(offset8_offset , prior_sd_offset);

  // correlation across variables
  L_C ~ lkj_corr_cholesky(3);

  
// MODEL  
  

    target += reduce_sum(partial_sum_joint_lpdf,
                        id_real, 1, 
                        y_c, y_b, 
                        d_obs, d_obs_matrix,
                        PHI, diagSPD, M, eta, L_C, alpha, sigma,
                        mu_hat, B_k, beta_mu,
                        N_k, N_kc, N_kb, D, d_ones);
       
}
