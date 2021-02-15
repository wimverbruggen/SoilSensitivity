rm(list = ls())

library(SoilSensitivity)

sand = 0.8 # fractions
clay = 0.1

#psi = -10**seq(0.001,5,length.out = 1000)/100 #cm to m
SP <- get_soilproperties(sand = sand,clay = clay, model = "ORCHIDEE", orc_map = "zobler")
theta<-seq(SP$theta_res,SP$theta_sat,length.out = 1000)
#theta <- theta_Psi(psi,SP$psi_sat,SP$theta_sat,SP$b) # m³/m³
psi<-Psi_theta_orc(theta=theta,
                   theta_sat = SP$theta_sat,
                   theta_res = SP$theta_res,
                   n = SP$n,
                   alpha = SP$alpha)

k <- k_theta_orc(theta=theta,
                 theta_sat = SP$theta_sat,
                 theta_res = SP$theta_res,
                 n = SP$n,
                 k_sat = SP$k_sat,
                 z = 1) # m/s

plot(-psi*100,theta,log = 'x',type = 'l')
plot(theta,k,log = 'y',type = 'l')
