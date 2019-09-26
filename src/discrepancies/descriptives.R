library(psych)

# Get data from discr_regress_county_fcc-ms
datadesc <- discr %>% select(availability_adv, usage, 
                             dis_rel_fcc_ms, dis_cat_fcc_ms, dis_bin_fcc_ms,
                             RUCC_2013, ru_binary, 
                             hs_r_ls, poverty, ag_65_l, hispanc, black, density, family, foreign)

describeBy(datadesc, digits = 2)
describeBy(datadesc, datadesc$ru_binary, digits = 2)

# Get data from predictFCC_ACS.R
datadesc_acs <- rf_data_full %>% select(-state, -population) %>%
                                 mutate(availability = availability*100,
                                        subscription = subscription*100,
                                        hs_or_less = hs_or_less*100,
                                        poverty = poverty*100,
                                        age_65_older = age_65_older*100,
                                        hispanic = hispanic*100,
                                        black = black*100,
                                        family = family*100,
                                        foreign = foreign*100)
describeBy(datadesc_acs, digits = 2)
describeBy(datadesc_acs, datadesc_acs$rural, digits = 2)