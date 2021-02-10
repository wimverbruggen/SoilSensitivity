# Hydraulic conductivity K as defined in ORCHIDEE following
# the Mualem (1976) - Van Genuchten(1980) model
k_theta_orc <- function(theta,theta_sat,theta_res,n,k_sat,z=1){
  m<-1-1/n
  theta_f<-(theta-theta_res)/(theta_sat-theta_res)
  k=k_sat*sqrt(theta_f)*(1-(1-theta_f**(1/m))**m)**2
  return(k)
}
