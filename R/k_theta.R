k_theta <- function(theta,k_sat,theta_sat,b,l = 1){
  return(k_sat*(10**(-7*(1-l)))*(theta/theta_sat)**(2*b+3))
}
