# What vaccination coverage level do you expect to achieve?	50%

# Test frequency for unvaccinated persons	Never

# Test frequency for vaccinated persons	Never

# What are your data assumptions?
#
#   Vaccine preventive efficacy at time 0	85%
preventive_0 <- 0.85
#
#   Vaccine preventive efficacy at 6 months 	66%
preventive <- 0.66
#
#   Vaccine transmission efficacy at time 0	25%
transmission_0 <- 0.25
#
#   Vaccine transmission efficacy at 6 months 	25%
transmission <- 0.25
#
#   Initial population size	5,000
population_size <- 5000
#
#   Asymptomatic infections at time 0	5
Asymptomatic_infections_0 <- 5
#
#   Frequency of exogenous shocks (days)	7
Frequency_of_exogenous_shocks <- 7
#
#   Exogenous shocks (unadjusted for vaccination)	5
Exogenous_shocks <- 5

# What is your maximum infections tolerance (vertical axis)?"	5%
maximum_infections_tolerance <- 5
#
# What reproduction number do you expect to achieve (with NPIs, pre-vaccine, pre-testing) on campus?"	5
expected_reproduction_number <- 5
