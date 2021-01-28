get_soilproperties <- function(sand = 0.5,clay = 0.5){

  theta_sat = (50.5 - 14.2*sand - 3.7*clay)/100 # m³/m³
  psi_sat = -0.01*10^(2.17-1.58*sand-0.63*clay) # m
  b = 3.1-0.3*sand + 15.7*clay #unitless
  k_sat = 7.055556e-6*10^{-0.6+1.26*sand-0.64*clay} # m/s

  wdns = 1.000e3
  grav = 9.80665
  day_sec = 86400

  fieldcp_K = 0.1 # kg/m²/day
  fieldcp_K_unit = fieldcp_K/wdns/day_sec # m/s
  soilwp_MPa  = -1.5

  # WC at field capacity
  theta_fc = theta_sat*(fieldcp_K_unit/k_sat)**(1/(2*b+3)) # m³/m³

  # WC at wilting point
  theta_wp = theta_sat*(psi_sat/(soilwp_MPa * wdns / grav))^(1/b) # m³/m³

  return(list(theta_sat = theta_sat,
              theta_wp = theta_wp,
              theta_fc = theta_fc,
              b = b,
              k_sat = k_sat,
              psi_sat = -0.01))
}
