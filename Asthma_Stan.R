# Stan code to model asthma attacks rates by zip code in CA

# No year effect
stancode1 <- "
data{
int<lower=1> N;
int<lower=1> N_year;
int<lower=1> N_zip;
int rate[N];
real no2[N];
real co[N];
real o3[N];
real pm10[N];
real pm25[N];
real no2_sq[N];
real co_sq[N];
real o3_sq[N];
real pm10_sq[N];
real pm25_sq[N];
real no2_co[N];
real no2_o3[N];
real no2_pm10[N];
real no2_pm25[N];
real co_o3[N];
real co_pm10[N];
real co_pm25[N];
real o3_pm10[N];
real o3_pm25[N];
real pm10_pm25[N];
int zip[N];
int year[N];
matrix[N_zip,N_zip] Dmat;
}


parameters{
real a;
real bno2;
real bco;
real bo3;
real bpm10;
real bpm25;
real bno2_sq;
real bco_sq;
real bo3_sq;
real bpm10_sq;
real bpm25_sq;
real bno2_co;
real bno2_o3;
real bno2_pm10;
real bno2_pm25;
real bco_o3;
real bco_pm10;
real bco_pm25;
real bo3_pm10;
real bo3_pm25;
real bpm10_pm25;
real<lower=0> theta;
vector[N_zip] a_zip_raw;
real<lower=0> etasq;
real<lower=0> rhosq;
real<lower=0> sigma;
}

transformed parameters{
vector[N_zip] a_zip;
matrix[N_zip,N_zip] SIGMA_Dmat;

for ( i in 1:(N_zip-1) )
for ( j in (i+1):N_zip ) {
SIGMA_Dmat[i,j] = etasq*exp(-rhosq*pow(Dmat[i,j],2));
SIGMA_Dmat[j,i] = SIGMA_Dmat[i,j];
}

for ( k in 1:N_zip )
SIGMA_Dmat[k,k] = etasq + sigma;

a_zip = cholesky_decompose(SIGMA_Dmat) * a_zip_raw;

}

model{
vector[N] lambda;
rhosq ~ cauchy( 0 , 1 );
etasq ~ cauchy( 0 , 1 );
sigma ~ cauchy( 0 , 1 );
a_zip_raw ~ normal(0,1);
theta ~ exponential( 1 );
bno2 ~normal(0,1);
bco ~normal(0,1);
bo3 ~normal(0,1);
bpm10 ~normal(0,1);
bpm25 ~normal(0,1);
bno2_sq ~normal(0,1);
bco_sq ~normal(0,1);
bo3_sq ~normal(0,1);
bpm10_sq ~normal(0,1);
bpm25_sq ~normal(0,1);
bno2_co ~normal(0,1);
bno2_o3 ~normal(0,1);
bno2_pm10 ~normal(0,1);
bno2_pm25 ~normal(0,1);
bco_o3 ~normal(0,1);
bco_pm10 ~normal(0,1);
bco_pm25 ~normal(0,1);
bo3_pm10 ~normal(0,1);
bo3_pm25 ~normal(0,1);
bpm10_pm25 ~normal(0,1);
a ~ normal( 0 , 5 );

for ( i in 1:N ) {
lambda[i] = a + a_zip[zip[i]] + bno2 * no2[i] + bco * co[i] 
+ bo3 * o3[i] + bpm10 * pm10[i] + bpm25 * pm25[i] + bno2_sq * no2_sq[i]
+ bco_sq * co_sq[i] + bo3_sq * o3_sq[i] + bpm10_sq * pm10_sq[i] 
+ bpm25_sq * pm25_sq[i] + bno2_co * no2[i] * co[i] + bno2_o3 * no2[i] * o3[i] 
+ bno2_pm10 * no2[i] * pm10[i] + bno2_pm25 * no2[i] * pm25[i] 
+ bco_o3 * co[i] * o3[i] + bco_pm10 * co[i] * pm10[i] + bco_pm25 * co[i] * pm25[i]
+ bo3_pm10 * o3[i] * pm10[i] + bo3_pm25 * o3[i] * pm25[i] 
+ bpm10_pm25 * pm10[i] * pm25[i];
lambda[i] = exp(lambda[i]);
}
rate ~ neg_binomial_2(lambda , theta);
}

generated quantities{
matrix[N_zip,N_zip] SIGMA_Dmat_1;
vector[N] lambda;
vector[N] log_lik;
vector[N] y_pred;

for ( i in 1:(N_zip-1) ) 
for ( j in (i+1):N_zip ) {
SIGMA_Dmat_1[i,j] = etasq*exp(-rhosq*pow(Dmat[i,j],2));
SIGMA_Dmat_1[j,i] = SIGMA_Dmat_1[i,j];
}

for ( k in 1:N_zip )
SIGMA_Dmat_1[k,k] = etasq + sigma;

for ( i in 1:N ) {
lambda[i] = a + a_zip[zip[i]] + bno2 * no2[i] + bco * co[i] 
+ bo3 * o3[i] + bpm10 * pm10[i] + bpm25 * pm25[i] + bno2_sq * no2_sq[i]
+ bco_sq * co_sq[i] + bo3_sq * o3_sq[i] + bpm10_sq * pm10_sq[i] 
+ bpm25_sq * pm25_sq[i] + bno2_co * no2[i] * co[i] + bno2_o3 * no2[i] * o3[i] 
+ bno2_pm10 * no2[i] * pm10[i] + bno2_pm25 * no2[i] * pm25[i] 
+ bco_o3 * co[i] * o3[i] + bco_pm10 * co[i] * pm10[i] + bco_pm25 * co[i] * pm25[i]
+ bo3_pm10 * o3[i] * pm10[i] + bo3_pm25 * o3[i] * pm25[i] 
+ bpm10_pm25 * pm10[i] * pm25[i];
lambda[i] = exp(lambda[i]);

log_lik[i] = neg_binomial_2_lpmf( rate[i] | lambda[i] , theta );
y_pred[i] = neg_binomial_2_rng(lambda[i], theta);

}

}


"

# zip and Year effect
stancode2 <- "
data{
int<lower=1> N;
int<lower=1> N_year;
int<lower=1> N_zip;
int rate[N];
real no2[N];
real co[N];
real o3[N];
real pm10[N];
real pm25[N];
real no2_sq[N];
real co_sq[N];
real o3_sq[N];
real pm10_sq[N];
real pm25_sq[N];
real no2_co[N];
real no2_o3[N];
real no2_pm10[N];
real no2_pm25[N];
real co_o3[N];
real co_pm10[N];
real co_pm25[N];
real o3_pm10[N];
real o3_pm25[N];
real pm10_pm25[N];
int zip[N];
int year[N];
matrix[N_zip,N_zip] Dmat;
//matrix[N_year,N_year] Dmat2;
}


parameters{
real a;
real bno2;
real bco;
real bo3;
real bpm10;
real bpm25;
real bno2_sq;
real bco_sq;
real bo3_sq;
real bpm10_sq;
real bpm25_sq;
real bno2_co;
real bno2_o3;
real bno2_pm10;
real bno2_pm25;
real bco_o3;
real bco_pm10;
real bco_pm25;
real bo3_pm10;
real bo3_pm25;
real bpm10_pm25;
real<lower=0> theta;
vector[N_zip] a_zip;
//vector[N_year] a_year;
real<lower=0> etasq;
real<lower=0> rhosq;
//real<lower=0> etasq2;
//real<lower=0> rhosq2;
real<lower=0> sigma;
//real<lower=0> sigma2;
}


model{
matrix[N_zip,N_zip] SIGMA_Dmat;
vector[N] lambda;
rhosq ~ cauchy( 0 , 1 );
etasq ~ cauchy( 0 , 1 );
sigma ~ cauchy( 0 , 1 );
for ( i in 1:(N_zip-1) )
for ( j in (i+1):N_zip ) {
SIGMA_Dmat[i,j] = etasq*exp(-rhosq*pow(Dmat[i,j],2));
SIGMA_Dmat[j,i] = SIGMA_Dmat[i,j];
}
for ( k in 1:N_zip )
SIGMA_Dmat[k,k] = etasq + sigma;
a_zip ~ multi_normal( rep_vector(0,N_zip) , SIGMA_Dmat );
theta ~ exponential( 1 );
bno2 ~normal(0,1);
bco ~normal(0,1);
bo3 ~normal(0,1);
bpm10 ~normal(0,1);
bpm25 ~normal(0,1);
bno2_sq ~normal(0,1);
bco_sq ~normal(0,1);
bo3_sq ~normal(0,1);
bpm10_sq ~normal(0,1);
bpm25_sq ~normal(0,1);
bno2_co ~normal(0,1);
bno2_o3 ~normal(0,1);
bno2_pm10 ~normal(0,1);
bno2_pm25 ~normal(0,1);
bco_o3 ~normal(0,1);
bco_pm10 ~normal(0,1);
bco_pm25 ~normal(0,1);
bo3_pm10 ~normal(0,1);
bo3_pm25 ~normal(0,1);
bpm10_pm25 ~normal(0,1);
a ~ normal( 0 , 5 );

for ( i in 1:N ) {
lambda[i] = a + a_zip[zip[i]] + bno2 * no2[i] + bco * co[i] 
+ bo3 * o3[i] + bpm10 * pm10[i] + bpm25 * pm25[i] + bno2_sq * no2_sq[i]
+ bco_sq * co_sq[i] + bo3_sq * o3_sq[i] + bpm10_sq * pm10_sq[i] 
+ bpm25_sq * pm25_sq[i] + bno2_co * no2[i] * co[i] + bno2_o3 * no2[i] * o3[i] 
+ bno2_pm10 * no2[i] * pm10[i] + bno2_pm25 * no2[i] * pm25[i] 
+ bco_o3 * co[i] * o3[i] + bco_pm10 * co[i] * pm10[i] + bco_pm25 * co[i] * pm25[i]
+ bo3_pm10 * o3[i] * pm10[i] + bo3_pm25 * o3[i] * pm25[i] 
+ bpm10_pm25 * pm10[i] * pm25[i];
lambda[i] = exp(lambda[i]);
}
rate ~ neg_binomial_2(lambda , theta);
}

generated quantities{
matrix[N_zip,N_zip] SIGMA_Dmat;
vector[N] lambda;
vector[N] log_lik;
vector[N] y_pred;

for ( i in 1:(N_zip-1) ) 
for ( j in (i+1):N_zip ) {
SIGMA_Dmat[i,j] = etasq*exp(-rhosq*pow(Dmat[i,j],2));
SIGMA_Dmat[j,i] = SIGMA_Dmat[i,j];
}

for ( k in 1:N_zip )
SIGMA_Dmat[k,k] = etasq + sigma;

for ( i in 1:N ) {
lambda[i] = a + a_zip[zip[i]] + bno2 * no2[i] + bco * co[i] 
+ bo3 * o3[i] + bpm10 * pm10[i] + bpm25 * pm25[i] + bno2_sq * no2_sq[i]
+ bco_sq * co_sq[i] + bo3_sq * o3_sq[i] + bpm10_sq * pm10_sq[i] 
+ bpm25_sq * pm25_sq[i] + bno2_co * no2[i] * co[i] + bno2_o3 * no2[i] * o3[i] 
+ bno2_pm10 * no2[i] * pm10[i] + bno2_pm25 * no2[i] * pm25[i] 
+ bco_o3 * co[i] * o3[i] + bco_pm10 * co[i] * pm10[i] + bco_pm25 * co[i] * pm25[i]
+ bo3_pm10 * o3[i] * pm10[i] + bo3_pm25 * o3[i] * pm25[i] 
+ bpm10_pm25 * pm10[i] * pm25[i];
lambda[i] = exp(lambda[i]);

log_lik[i] = neg_binomial_2_lpmf( rate[i] | lambda[i] , theta );
y_pred[i] = neg_binomial_2_rng(lambda[i], theta);

}

}


"