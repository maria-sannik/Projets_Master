clear all
cls
***PROJET A RENDRE - SERIES TEMPORELLES***
******************************************************************
**************************** PARTIE 1 ****************************
******************************************************************

******************************************************************
*cd "/Users/mariasannikov/Desktop/Nice\ EE/S2/Time-series"
import excel "/Users/mariasannikov/Desktop/NiceEE/S2/Time-series/namq_10_exi_PROJ.xls", sheet("X+M") firstrow

use "Portugal_X_M.dta", clear 

gen Temps= tq(1995q1)+ _n-1
list Temps in 1
format %tq Temps
list Temps in 1

tsset Temps, quarterly
order Temps
drop YEARQ

gen Trend= _n


tsline X M

gen delta_x = D.X 
gen delta_m = D.M


tsline delta_x delta_m

** Modèle 1- Unit root test***
gen X_lag1 = X[_n-1]
gen X_lag2=X[_n-2]

gen deltaX_L1= delta_x[_n-1]
gen deltaX_L2= delta_x[_n-2]
gen deltaX_L3= delta_x[_n-3]
gen deltaX_L4= delta_x[_n-4]

gen deltaX_L5= delta_x[_n-5]
gen deltaX_L6= delta_x[_n-6]

reg delta_x X_lag1 deltaX_L1 deltaX_L2 deltaX_L3 deltaX_L4 deltaX_L5 deltaX_L6 Trend
reg delta_x X_lag1 deltaX_L1 deltaX_L2 deltaX_L3 deltaX_L4 deltaX_L5 Trend
reg delta_x X_lag1 deltaX_L1 deltaX_L2 deltaX_L3 deltaX_L4 Trend
reg delta_x X_lag1 deltaX_L1 deltaX_L2 deltaX_L3 Trend
reg delta_x X_lag1 deltaX_L1 deltaX_L2 Trend
reg delta_x X_lag1 deltaX_L1 Trend



dfuller X, trend regress lags(6)
dfuller X, trend regress lags(5)
dfuller X, trend regress lags(4)
dfuller X, trend regress lags(3)
dfuller X, trend regress lags(2)
asdoc dfuller X, trend regress lags(1)

varsoc delta_x delta_m, maxlag(6)
varsoc delta_x
varsoc delta_m


* Modèle 1' - Tester la présence éventuelle du trend (sans racine unitaire)
asdoc reg delta_x deltaX_L1 Trend

* Modèle 2 - Tester la présence de racine unitaire dans le modèle sans trend
dfuller X ,regress lags(1)

* Modèle 2' - Tester la présence de constante
reg delta_x deltaX_L1

**
**IMPORTATIONS***
**

gen M_lag1 = M[_n-1]
gen M_lag2=M[_n-2]


gen deltaM_L1= delta_m[_n-1]
gen deltaM_L2= delta_m[_n-2]
gen deltaM_L3= delta_m[_n-3]
gen deltaM_L4= delta_m[_n-4]
gen deltaM_L5= delta_m[_n-5]
gen deltaM_L6= delta_m[_n-6]



reg delta_m M_lag1 deltaM_L1 deltaM_L2 deltaM_L3 deltaM_L4 deltaM_L5 deltaM_L6 Trend
reg delta_m M_lag1 deltaM_L1 deltaM_L2
dfuller M, trend regress lags(2)

* Modèle 1' 
reg delta_m deltaM_L1 deltaM_L2 Trend
* Modèle 2
dfuller M ,regress lags(2)
* Modèle 2'
reg delta_m deltaM_L1 deltaM_L2


******************************************************************
**************************** PARTIE 2 ****************************
******************************************************************

* Question 3 - Modèle VAR

varsoc delta_x delta_m, maxlag(6)

var delta_m delta_x , lags(1/2)

* Question 4 - VAR à la main
reg delta_x deltaX_L1 deltaM_L1  deltaM_L2
reg delta_m deltaX_L1 deltaM_L1 deltaM_L2

* Question 5 - Test de causalité de Granger
var delta_m delta_x , lags(1/3)
vargranger

//null hypo: lagged X(2) does not cause M
//alt: lagged X causes M

//WE ARE INTERESTED IN THE GRANGER CAUSALITY OF M ON X

//null" lagges (2) M does not cause X
//alt: lagged M (2) causes X
// we have a p-value > 5%, meaning we cannot reject the null hypothesis. 
//it means that according to granger causality, M does not cause X.

//no let's try with 3 lags

var delta_m delta_x , lags(3/3)
vargranger

var delta_m delta_x, lags(1)
vargranger

var delta_m delta_x , lags(1/5)
vargranger

var delta_m delta_x , lags(1/6)

var delta_m delta_x , lags(1/50)

******************************************************************
**************************** PARTIE 3 ****************************
******************************************************************

reg M X
*Le vecteur candidat est alors:(1 ; -0.80)


predict residu,res

tsline residu

ac residu
pac residu

gen residu_L1=residu[_n-1]
gen delta_res=residu-residu_L1
gen delta_res_L1=delta_res[_n-1]

reg delta_res residu_L1 delta_res_L1
reg delta_res residu_L1
* On accepte H0 ce qui signifie la présence de racine unitaire.
*Donc il faudra faire le VAR en différence.

gen lnX=ln( X )

gen lnM=ln( M )

gen ln_dX= lnX[_n]-lnX[_n-1]
gen ln_dM= lnM[_n]-lnM[_n-1]


******************************************************************
**************************** PARTIE 4 ****************************
******************************************************************

***PREDICTION***

gen lnX=ln( X )

gen lnM=ln( M )

gen ln_dX= lnX[_n]-lnX[_n-1]
gen ln_dM= lnM[_n]-lnM[_n-1]

var ln_dX ln_dM,lags(1/2)

fcast compute m1_, step(10)

fcast graph m1_ln_dX m1_ln_dM , observed

twoway (tsline m1_ln_dX m1_ln_dM, legend( label(1 "Variation X" 2 "Variation M"))) ///
		(rarea m1_ln_dX_LB m1_ln_dX_UB Temps,  color(blue%20) legend( label(3 "" 4 ""))) ///
		(rarea m1_ln_dM_LB m1_ln_dM_UB Temps, color(red%20) legend( label(5 "" 6 "")) ) ///
		(tsline ln_dX ln_dM, legend( label(7 "" 8 "")))

		
		

* autre moyen de prédiction
var ln_dX ln_dM, lag(1/4)
fcast compute m1_, step(10)
fcast graph m1_ln_dM m1_ln_dX, observed

twoway (tsline m1_ln_dM m1_ln_dX, legend( label(1 "M" 2 "X"))) ///
		(rarea m1_ln_dM_LB m1_ln_dM_UB Temps,  color(blue%20) legend( label(3 "" 4 ""))) ///
		(rarea m1_ln_dX_LB m1_ln_dX_UB Temps, color(red%20) legend( label(5 "" 6 "")) ) ///
		(tsline ln_dM ln_dX, legend( label(7 "" 8 "")))


tsappend, add(10)
		
arima lnX, arima(0,1,0)		
