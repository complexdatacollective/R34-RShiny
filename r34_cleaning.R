

data_cleaning <- function(indat,interviewperiod = 12) {
    
    filenames <- unzip(indat,list=TRUE)$Name
    # Read in and clean the ego data
    egofile <- filenames[grep("ego.csv",filenames)]
    egodat <- read.csv(unz(indat,egofile))
    egodat <- egodat %>%
        dplyr::mutate(condoms12m = case_when(condoms_anal_insertive %in% c("sometimes","no") |
                                                 condoms_anal_receptive %in% c("sometimes","no") |
                                                 condoms_vaginal %in% c("sometimes","no") ~ "Y - Yes, Anal or Vaginal intercourse (with or without oral sex) [YAV]",
                                             condoms_oral_receptive %in% c("sometimes","no") | 
                                                 condoms_oral %in% c("sometimes","no") ~ "O - Oral sex only [O]",
                                             condoms_anal_insertive=="yes" & condoms_anal_receptive=="yes" &
                                                 condoms_vaginal=="yes" & condoms_oral_receptive=="yes" &
                                                 condoms_oral=="yes" ~ "N - No [N]",
                                             TRUE ~ "UNK - Unknown [UNK]"),
                      condoms_anal12m = ifelse(condoms_anal_insertive %in% c("sometimes","no") |
                                                condoms_anal_receptive %in% c("sometimes","no"), "A - Anal [A]",""),
                      condoms_vaginal12m = ifelse(condoms_vaginal %in% c("sometimes","no"), "V - Vaginal [V]",""),
                      condoms_oral12m = ifelse(condoms_oral_receptive %in% c("sometimes","no") |
                                                condoms_oral %in% c("sometimes","no"), "O - Oral [O]",""),
                      # condoms_sextype12m = paste0(condoms_anal12m,condoms_vaginal12m,condoms_oral12m),
                      condoms12m_pos = case_when(condoms_anal_insertive %in% c("sometimes","no") &
                                                     condoms_anal_receptive %in% c("sometimes","no") ~ "B - Both [B]",
                                                 condoms_anal_insertive %in% c("sometimes","no") ~ "I - Insertive [I]",
                                                 condoms_anal_receptive %in% c("sometimes","no") ~ "R - Receptive [R]",
                                                 TRUE ~ NA_character_  ),
                      sexual_identity = recode(sexual_identity, `1` = "Heterosexual", `2` = "Gay or Lesbian",
                                               `3` = "Bisexual", `4` = "Unknown", `5` = "Other", .default=NA_character_),
                      role = recode(role_self, "top" = "I - Insertive [I]", "bottom" = "R - Receptive [R]",
                                    "vers" = "B - Both [B]"),
                      sex_high = recode(sex_under_influence, "y_anal_vaginal" = "Y - Yes, Anal or Vaginal intercourse (with or without oral sex) [YAV]",
                                        "y_oral" = "O - Oral sex only [O]", "n" = "N - No [N]")) %>%
        dplyr::mutate_at(vars("drug_use","alcohol_use","drug_specific_crack","drug_specific_cocaine",
                              "drug_specific_heroin","drug_specific_meth","drug_specific_nitrate",
                              "drug_specific_erectile_dysfunciton","drug_specific_marijuana","drug_specific_other",
                              "injection_drug_use"),
                         ~recode(.,`false` = "N - No",`true` = "Y - Yes", .default=NA_character_))
    
    egodat$condoms_sextype12m <- apply(cbind(egodat$condoms_anal12m,egodat$condoms_vaginal12m,egodat$condoms_oral12m),1,
                                       function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    
    # Read in and clean the person attribute data
    person_attr_file <- filenames[grep("attributeList_Person.csv",filenames)]
    person_attr <- read.csv(unz(indat,person_attr_file))
    person_attr <- person_attr %>%
        dplyr::mutate_at(vars("sex_partner","race_white","race_black","race_asian","race_hisp",
                           "gender_cis_male","gender_cis_female","gender_trans_male",
                           "gender_trans_female","pregnant_yes","pregnant_no","partner_type_regular",
                           "partner_type_fwb","partner_type_anon","venue_met_internet","venue_met_bar",
                           "venue_met_bath","condoms_yes","condoms_sometimes","condoms_no","benefit_from_test",
                           "bar_specific_1","bar_specific_2","bar_specific_3","bar_specific_4",
                           "bar_specific_5","bar_specific_6","bar_specific_7","internet_specific_1",
                           "internet_specific_2","internet_specific_3","internet_specific_4",
                           "internet_specific_5","internet_specific_6","internet_specific_7","drug_use_partner",
                           "HIV_positive","partner_sex_role_bottom","partner_sex_role_top",
                           "partner_sex_role_vers","partner_sex_type_female_anal",
                           "partner_sex_type_female_oral","partner_sex_type_female_vaginal",
                           "partner_sex_type_male_anal","partner_sex_type_male_oral","partner_type_spouse"),
                         ~recode(., `false`= FALSE, `true` = TRUE, .default = NA))
    person_attr <- person_attr %>%
        ######## contact_basis - THIS VARIABLE IS NOT CODED CORRECTLY!!!!!!!!!!!!!
        mutate(contact_basis = case_when(sex_partner==TRUE & is.na(drug_use_partner) ~ "P1 - Sex partner [P1]",
                                         drug_use_partner==TRUE & is.na(sex_partner) ~ "P2 - Needle sharing partner [P2]",
                                         sex_partner==TRUE & drug_use_partner==TRUE ~ "P3 - Both sex and needle sharing partner [P3]",
                                         TRUE ~ NA_character_),
               spouse = ifelse(partner_type_spouse==TRUE, "Yes [YES]", "No [NO]"),
               gender = case_when(gender_cis_male==TRUE ~ "Male [MALE]",
                                  gender_cis_female==TRUE ~ "Female [FEMALE]",
                                  gender_trans_female==TRUE ~ "Transgender MTF [MTF]",
                                  gender_trans_male==TRUE ~ "Transgender - FTM [FTM]",
                                  TRUE ~ as.character(gender_other)),
               pregnant = case_when(pregnant_yes==TRUE ~ "Yes [YES]",
                                    pregnant_no == TRUE ~ "No [NO]",
                                    TRUE ~ "Not Applicable"),
               bar1 = ifelse(bar_specific_1==TRUE, "Rosecoe's Tavern",""),
               bar2 = ifelse(bar_specific_2==TRUE, "Progress Bar",""),
               bar3 = ifelse(bar_specific_3==TRUE,"Sidetrack",""),
               bar4 = ifelse(bar_specific_4==TRUE, "Hydrate Nightclub",""),
               bar5 = ifelse(bar_specific_5==TRUE, "Scarlet Bar",""),
               bar6 = ifelse(bar_specific_6==TRUE, "Berlin Nightclub",""),
               bar7 = ifelse(bar_specific_7==TRUE, "Jeffrey Pub",""),
               internet1 = ifelse(internet_specific_1==TRUE, "Grindr",""),
               internet2 = ifelse(internet_specific_2==TRUE, "Tinder",""),
               internet3 = ifelse(internet_specific_3==TRUE, "Jack'd",""),
               internet4 = ifelse(internet_specific_4==TRUE, "Facebook", ""),
               internet5 = ifelse(internet_specific_5==TRUE, "Snapchat",""),
               internet6 = ifelse(internet_specific_6==TRUE, "Instagram",""),
               internet7 = ifelse(internet_specific_7==TRUE, "Scruff",""),
               met_internet = ifelse(venue_met_internet==TRUE,"Yes [YES]", "No [NO]"),
               asian = ifelse(race_asian==TRUE, "Asian",""),
               black = ifelse(race_black==TRUE, "Black",""),
               white = ifelse(race_white==TRUE,"White",""),
               latinx = ifelse(race_hisp==TRUE,"Hispanic/Latino",""),
               first_sex = format(as.Date(first_sex), "%b %d, %Y"),
               last_sex = format(as.Date(last_sex), "%b %d, %Y"))
    person_attr$venue_all <- apply(cbind(person_attr$bar1,person_attr$bar2,person_attr$bar3,
                                         person_attr$bar4,person_attr$bar5,person_attr$bar6,
                                         person_attr$bar7,person_attr$bar_specific_o,person_attr$internet1,
                                         person_attr$internet2,person_attr$internet3,person_attr$internet4,
                                         person_attr$internet5,person_attr$internet6,person_attr$internet7,
                                         person_attr$internet_specific_o),1,
                                   function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    person_attr$race <- apply(cbind(person_attr$asian,person_attr$black,person_attr$white,
                                    person_attr$latinx),1,
                              function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    allpartners_dat <- person_attr %>%
        summarise(sexw_cisf = case_when(sum(partner_sex_type_female_anal & gender_cis_female)>=1 |
                                         sum(partner_sex_type_female_vaginal & gender_cis_female)>=1 ~ 
                                            "Y - Yes, Anal or Vaginal Intercourse (with or without oral sex) [YAV]",
                                        sum(partner_sex_type_female_oral & gender_cis_female)>=1 ~ "O - Oral sex only [O]",
                                        TRUE ~ "N - No [N]"),
                  anal_cisf = ifelse(sum(partner_sex_type_female_anal & gender_cis_female)>=1,"A - Anal [A]",""),
                  vag_cisf = ifelse(sum(partner_sex_type_female_vaginal & gender_cis_female)>=1,"V - Vaginal [V]",""),
                  oral_cisf = ifelse(sum(partner_sex_type_female_oral & gender_cis_female)>=1,"O - Oral [O]",""),
                  sexw_cism = case_when(sum(partner_sex_type_male_anal & gender_cis_male)>=1 ~ 
                                            "Y - Yes, Anal Intercourse (with or without oral sex) [YAV]",
                                        sum(partner_sex_type_male_oral & gender_cis_male)>=1 ~ "O - Oral sex only [O]",
                                        TRUE ~ "N - No [N]"),
                  anal_cism = ifelse(sum(partner_sex_type_male_anal & gender_cis_male)>=1,"A - Anal [A]",""),
                  oral_cism = ifelse(sum(partner_sex_type_male_oral & gender_cis_male)>=1,"O - Oral [O]",""),
                  sexw_transg = case_when(sum(partner_sex_type_female_anal & gender_trans_female)>=1 |
                                              sum(partner_sex_type_female_vaginal & gender_trans_female)>=1 |
                                              sum(partner_sex_type_male_anal & gender_trans_male)>=1 ~ 
                                              "Y - Yes, Anal or Vaginal Intercourse (with or without oral sex) [YAV]",
                                          sum(partner_sex_type_female_oral & gender_cis_female)>=1 |
                                              sum(partner_sex_type_male_oral & gender_trans_male)>=1 ~ "O - Oral sex only [O]",
                                          TRUE ~ "N - No [N]"),
                  anal_transg = ifelse(sum(partner_sex_type_male_anal & gender_trans_male)>=1 |
                                           sum(partner_sex_type_female_anal & gender_trans_female)>=1,"A - Anal [A]",""),
                  vag_transg = ifelse(sum(partner_sex_type_female_vaginal & gender_trans_female)>=1, "V - Vaginal [V]",""),
                  oral_transg = ifelse(sum(partner_sex_type_male_oral & gender_trans_male)>=1 |
                                           sum(partner_sex_type_female_oral & gender_trans_female)>=1,"O - Oral [O]"),
                  sexw_anon = case_when(sum(partner_sex_type_female_anal & partner_type_anon)>=1 |
                                            sum(partner_sex_type_female_vaginal & partner_type_anon)>=1 |
                                            sum(partner_sex_type_male_anal & partner_type_anon)>=1 ~
                                            "Y - Yes, Anal or Vaginal intercourse (with or without oral sex) [YAV]",
                                        sum(partner_sex_type_female_oral & partner_type_anon)>=1 |
                                            sum(partner_sex_type_male_oral & partner_type_anon)>=1 ~ 
                                            "O - Oral sex only [O]",
                                        TRUE ~ "N - No [N]"),
                  anal_anon = ifelse(sum(partner_sex_type_male_anal & partner_type_anon)>=1 |
                                         sum(partner_sex_type_female_anal & partner_type_anon)>=1,"A - Anal [A]",""),
                  vag_anon = ifelse(sum(partner_sex_type_female_vaginal & partner_type_anon)>=1,"V - Vaginal [V]",""),
                  oral_anon = ifelse(sum(partner_sex_type_female_oral & partner_type_anon)>=1 |
                                         sum(partner_sex_type_male_oral & partner_type_anon)>=1, "O - Oral [O]",""),
                  n_cisf = sum(gender_cis_female==TRUE & sex_partner==TRUE,na.rm=TRUE),
                  n_cism = sum(gender_cis_male==TRUE & sex_partner==TRUE, na.rm=TRUE),
                  n_trans = sum((gender_trans_female==TRUE | gender_trans_male==TRUE) &
                                    sex_partner==TRUE, na.rm=TRUE),
                  n_anon = sum(partner_type_anon==TRUE & sex_partner==TRUE,na.rm=TRUE),
                  sexw_hivpos = ifelse(sum(HIV_positive & sex_partner,na.rm=TRUE)>=1,"Y - Yes [Y]","No or Unknown"))
    allpartners_dat$cisf_sextype <- apply(cbind(allpartners_dat$anal_cisf,allpartners_dat$vag_cisf,allpartners_dat$oral_cisf),1,
                              function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    allpartners_dat$cism_sextype <- apply(cbind(allpartners_dat$anal_cism,allpartners_dat$oral_cism),1,
                                          function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    allpartners_dat$transg_sextype <- apply(cbind(allpartners_dat$anal_transg,allpartners_dat$vag_transg,allpartners_dat$oral_transg),1,
                                          function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    allpartners_dat$anon_sextype <- apply(cbind(allpartners_dat$anal_anon,allpartners_dat$vag_anon,allpartners_dat$oral_anon),1,
                                            function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    allpartners_dat$hivpos_gender <- apply(t(person_attr$gender[person_attr$HIV_positive & person_attr$sex_partner]),1,
                                           function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    

    
    ### how to use "benefit_from_test" variable in person_attr - there's a dropdown in 
    ### contact's referral basis that is "A3 - Associate - anyone else who would benefit from an exam" and
    ### "S3 - Suspect - anyone else who would benefit from an exam"

    

    # Read in and clean the venue attribute data
    venue_attr_file <- filenames[grep("attributeList_Venue.csv",filenames)]
    venue_attr <- read.csv(unz(indat,venue_attr_file))
    
    venue_summary <- venue_attr %>%
        summarise(any_online = ifelse(sum(type=="app")>0,"Y - Yes","N - No"))
    
    # Read in and clean the edge list of sex partners
    sex_edgelist_file <- filenames[grep("edgeList_had_sex.csv",filenames)]
    sex_edgelist <- read.csv(unz(indat,sex_edgelist_file))
    
    # Read in and clean edge list of partners who know one another
    know_edgelist_file <- filenames[grep("edgeList_know.csv",filenames)]
    know_edgelist <- read.csv(unz(indat,know_edgelist_file))
    
    # Read in and clean edge list of shared needles
    needles_edgelist_file <- filenames[grep("edgeList_shared_needles.csv",filenames)]
    needles_edgelist <- read.csv(unz(indat,needles_edgelist_file))
    
    # no longer using graphml file
    # dat <- igraph::read_graph(indat,"graphml")
    # egodat <- igraph::graph_attr(dat)
    # egodat <- do.call(cbind.data.frame,egodat)
    # alterdat <- igraph::as_data_frame(dat,"vertices")
    # edgesdat <- igraph::as_data_frame(dat,"edges")
    
    # library(xml2)
    # dat <- xml2::read_xml(indat)
    # 
    # library(XML)
    # test <- xmlParse(indat)
    # test2 <- xmlToList(test)
    
    refperiod <- c("in past 12 months?", "during the interview period?")
    sexbehavqs <- list()
    for(i in 1:length(refperiod)) {
        sexbehavqs[[i]] <- c(paste0("<b>Had sex with a female ",refperiod[i],"</b>"),
                            "Type of Sex with female partner(s)",
                            "Number of female partners",
                            paste0("<b>Had sex with a male ",refperiod[i],"</b>"),
                            "Type of Sex with male partner(s)",
                            "Positioning (anal only)",
                            "Number of male partners",
                            "Heterosexual contact with bisexual male",
                            paste0("<b>Had sex with a transgender person ",refperiod[i],"</b>"),
                            "Type of Sex with transgender partner(s)",
                            "Number of transgender partners",
                            paste0("<b>Had sex with an anonymous partner ",refperiod[i],"</b>"),
                            "Type of Sex with anonymous partner(s)",
                            "Number of anonymous partners",
                            paste0("<b>Had sex without using a condom ",refperiod[i],"</b>"),
                            "Type of Sex without using a condom",
                            "Positioning (anal only)",
                            paste0("<b>Had sex while intoxicated or high on drugs ",refperiod[i],"</b>"),
                            "Type of Sex while intoxicated or high",
                            paste0("<b>Exchanged drugs/money/goods for sex ",refperiod[i],"</b>"),
                            "Type of Sex exchanged",
                            "Transaction Type",
                            paste0("<b>Had sex with a person who is known to be an MSM ",refperiod[i],"</b>"),
                            "Type of Sex with known MSM",
                            paste0("<b>Had sex with a person who is known to be an IDU ",refperiod[i],"</b>"),
                            "Type of Sex with known IDU",
                            paste0("<b>Had sex with a bisexual person ",refperiod[i],"</b>"),
                            "Gender of bisexual person",
                            paste0("<b>Had sex with a person with AIDS or documented HIV infection ",refperiod[i],"</b>"),
                            "Gender of person with HIV",
                            paste0("<b>Met partners through the Internet ",refperiod[i],"</b>"))
    }
    
    sexbehav12mind <- c(# A bunch of these outputs can't be filled yet because data collection not consistent
                        # with CHIMS... 
                        allpartners_dat$sexw_cisf,allpartners_dat$cisf_sextype,allpartners_dat$n_cisf,
                        allpartners_dat$sexw_cism,allpartners_dat$cism_sextype,egodat$role,allpartners_dat$n_cism,
                        "",allpartners_dat$sexw_transg,allpartners_dat$transg_sextype,allpartners_dat$n_trans,
                        allpartners_dat$sexw_anon,allpartners_dat$anon_sextype,allpartners_dat$n_anon,
                        egodat$condoms12m, egodat$condoms_sextype12m, egodat$condoms12m_pos,
                        egodat$sex_high,"",
                        "","","","","","","","","",allpartners_dat$sexw_hivpos,allpartners_dat$hivpos_gender,
                        venue_summary$any_online)
    
    # FOR ANY ONLINE PARTNERS LOOK AT person_attr$internet_specific1 etc
    
    sexbehav12m <- data.frame(Questions = sexbehavqs[[1]],
                              Responses = sexbehav12mind)
    
    druguseqs <- list()
    for(i in 1:length(refperiod)) {
        druguseqs[[i]] <- c(paste0("<b>Any alcohol or drug use ",refperiod[i]," (injection or non-injection)</b>"),
                            "<b>Alcohol</b>","Frequency of use","Units",
                            "<b>Crack</b>","Frequency of use","Units",
                            "<b>Cocaine</b>","Frequency of use","Units",
                            "<b>Heroin</b>","Frequency of use","Units",
                            "<b>Methamphetamine</b>","Frequency of use","Units",
                            "<b>Nitrates/poppers</b>","Frequency of use","Units",
                            "<b>Erectile dysfunction medications</b>","Frequency of use","Units",
                            "<b>Marijuana</b>","Frequency of use","Units",
                            "<b>Other drugs</b>","Specify drug(s)","Frequency of use","Units",
                            paste0("<b>Engaged in injection drug use ",refperiod[i],"</b>"),
                            paste0("Shared injection drug equipment ",refperiod[i]))
    }
    
    druguse12mind <- c(egodat$drug_use,egodat$alcohol_use,"","",
                       egodat$drug_specific_crack,"","",egodat$drug_specific_cocaine,
                       "","",egodat$drug_specific_heroin,"","",egodat$drug_specific_meth,
                       "","",egodat$drug_specific_nitrate,"","",egodat$drug_specific_erectile_dysfunciton,
                       "","",egodat$drug_specific_marijuana,"","",egodat$drug_specific_other,
                       "","","",egodat$injection_drug_use,"")
    
    druguse12m <- data.frame(Questions = druguseqs[[1]],
                              Responses = druguse12mind)
    
    contact_referralqs <- c("Contact's referral basis","Name or alias","Contact's first name",
                            "Contact's last name",
                            "Is this person the spouse of the original patient?",
                            "Person's age (years)","Gender","Pregnant","Number of weeks pregnant",
                            "Venue where OP met/had sex with this person",
                            "Was this partner met through the internet?",
                            "<b>Locating Information</b>",
                            "Phone number","<b>Physical Attributes</b>",
                            "Height","Weight","Visible identifiers",
                            "Hair color","Race",
                            "Exposure date (first)","Exposure date (last)",
                            "Exposure frequency - number")
    
    contact_referralind <- rbind(person_attr$contact_basis,person_attr$preferred_name,
                                 person_attr$first_name, person_attr$last_name,
                                 person_attr$spouse,person_attr$age,person_attr$gender,
                                 person_attr$pregnant,"",person_attr$venue_all,
                                 person_attr$met_internet,"",person_attr$phone,
                                 "",person_attr$height,person_attr$weight,
                                 person_attr$descrip_other,person_attr$hair,person_attr$race,
                                 person_attr$first_sex,person_attr$last_sex,
                                 person_attr$num_times)
    
    contact_referral <- data.frame(Questions = contact_referralqs,
                                   contact_referralind)

    
    alldat <- list(egodat = egodat, person_attr = person_attr,
                   venue_attr = venue_attr, sex_edgelist = sex_edgelist,
                   know_edgelist = know_edgelist, needles_edgelist = needles_edgelist,
                   sexbehav12m = sexbehav12m,druguse12m = druguse12m,
                   contact_referral = contact_referral)
    
    return(alldat)
}
