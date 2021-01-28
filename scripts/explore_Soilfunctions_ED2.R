rm(list = ls())

library(SoilSensitivity)

sand = 0.8 # fractions
clay = 0.1

psi = -10**seq(0.001,5,length.out = 1000)/100 #cm to m

SP <- get_soilproperties(sand = sand,clay = clay)

theta <- theta_Psi(psi,SP$psi_sat,SP$theta_sat,SP$b) # m³/m³
k <- k_theta(theta,SP$k_sat,SP$theta_sat,SP$b) # m/s

plot(-psi*100,theta,log = 'x',type = 'l')
plot(theta,k,log = 'y',type = 'l')
