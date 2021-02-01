get_soilproperties <- function(sand = 0.5,clay = 0.5,model="ED"){

  if(model=="ED"){

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

  if(model=="LPJ-GUESS"){

    # Derivation of soil properties for LPJ-GUESS.
    # These calculations can be found in the model code: /source/modules/soilinput.cpp::get_mineral()

    silt = 1-sand-clay

    # Some comment in the code:
        # Equation 1 from Cosby 1984
        # Psi = Psi_s * (Theta/Theta_s)^b
        # Psi is the pressure head in cm
        # *_s is the values at saturation
        # Theta is the volumetric moisture content in percent
        # Re-arranged to get the Theta
        # Theta = Theta_s * (Psi/Psi_s)^(1/b)

    b = 3.10 + 15.7*clay - 0.3*sand # from Cosby 1984 (Table 4) (empirical parameter in perc equation, mm/day)
    logPsi_s = 1.54 - 0.95*sand + 0.63*silt
    theta_sat = 0.01 * (50.5 - 14.2 * sand - 3.7 * clay) # saturation capacity, in Cosby expressed as %
    psi_sat = 10^(-logPsi_s)
    psi_wilt = 10^-4.2
    psi_whc = 10^-2
    theta_whc  = theta_sat * (psi_whc  / psi_sat)^(1/b)
    theta_wp = theta_sat * (psi_wilt / psi_sat)^(1/b) # wilting point as fraction of depth (Prentice 1992)

    # Then later in the code, for actual use in the rest of the model,
    # b is re-calculated as below, with the following explanation:
      # "A linear dependence between the percolation coefficient from Haxeltine 1996a
      # and the texture dependent parameter b from Cosby 1984 was established
      # K = 5.87 - 0.29*b"

    b_orig = b # saving it for "return"
    b = 5.87 - 0.29*b

    volumetric_whc_field_capacity = theta_whc - theta_wp # volWHC at field capacity minus volWHC at wilting point (Hmax), as fraction of soil depth

    # Thermal diffusivities follow van Duin (1963), Jury et al (1991) Fig 5.11
    thermal_wilting_point = 0.2 # thermal diffusivity (mm2/s) at wilting point (0% WHC)
    thermal_15_whc = 0.15 * b + 0.05  # thermal diffusivity (mm2/s) at 15% WHC
    thermal_field_capacity = 0.4 # thermal diffusivity at field capacity (100% WHC), these di

    # Other values are also set, they are read directly from the soil input map.
    # Only organic content fraction (orgC) is really required.
    #   pH
    #   orgC
    #   soilC
    #   C:N
    #   bulkdensity

    return(list(theta_sat = theta_sat,
                theta_wp = theta_wp,
                theta_fc = theta_whc, # I suppose this is the same
                b = b_orig,
                k_sat = NA, # not calculated I guess?
                psi_sat = psi_sat)) # maybe we'll need to add a minus sign?


  }

}
