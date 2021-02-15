# link between the matric potential Psi (m) involved in the hydraulic
# diffusivity and the volumetic water content theta
# as defined in ORCHIDEE Mualem (1976) - Van Genuchten (1980)
Psi_theta_orc <- function(theta,theta_sat,theta_res,n,alpha){
  m<-1-1/n
  theta_f<-(theta-theta_res)/(theta_sat-theta_res)
  psi<-(-1/alpha)*(theta_f**(-1/m)-1)**(1/n)
  return(psi)
}
