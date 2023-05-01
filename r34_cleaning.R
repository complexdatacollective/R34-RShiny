data_cleaning <- function(indat, interviewperiodstart) {
#     indat='/Users/dyl5635/Library/CloudStorage/OneDrive-NorthwesternUniversity/DAA Work Organization 1.0/5. CONNECT Projects/2. R34 Partner Services/DummyData/sample2.zip'
#    interviewperiodstart= '2023-04-04T14:55:20.764Z'
    # filenames will have the file names inside of th/Users/dyl5635/Library/CloudStorage/OneDrive-NorthwesternUniversity/DAA Work Organization 1.0/5. CONNECT Projects/2. R34 Partner Services/DummyData/sample2.zipe zipped data file that you've
    # put into "indat" - so all of the various filenames coming from a network canvas
    # interview

    filenames <- unzip(indat,list=TRUE)$Name
    # Read in and clean the ego data
    # figure out which one is the "ego" data and save that as egofile
    egofile <- filenames[grep("ego.csv",filenames)]
    #grep is a function in R used for searching a pattern or a regular expression in a character vector. It returns the index or indices of the elements in the vector that contain the pattern.
#In the given code grep("ego.csv", filenames) looks for the character string "ego.csv" in the vector of filenames and returns a vector of indices of the elements containing "ego.csv". This is used to identify the file name that contains "ego.csv" in the filenames vector, which is then stored in egofile.
    
    # read in the csv for that egofile
    egodat <- read.csv(unz(indat,egofile))
    # create some ego variables relevant for sexual behavior in the past year
  
    #temporarily make logical variables character so that recode function can be used
    egodat <- egodat%>%
      mutate_if(is.logical, as.character)
  
    # Regex/ recode a bunch of variables that were true/false as TRUE/FALSE to make them easiser to manipulate
    egodat <- egodat %>%
        dplyr::mutate_at(vars("sex_type_exchange_anal", "sex_type_exchange_vaginal", "sex_type_exchange_oral",
                              "sex_under_influence_anal", "sex_under_influence_oral", "sex_under_influence_vaginal"),
                         ~recode(., `false`= FALSE, `true` = TRUE, .default = NA))
    egodat <- egodat %>%
        
        
        dplyr::mutate(
                    # condoms12m is to fill in the question "Had sex without using a condom during the interview period?"
                    # classifies folks' non-condom usage in the past year at an ego level,
                    # using the CHIMS categories - uses variables condoms_anal_insertive, 
                    # condoms_anal_receptive, condoms_vaginal, condoms_oral, condoms_oral_receptive
                    # which all apply to the previous year
          #Corresponds to SEX_MALE_POSITIONING_IX_PD in Mapping
                    condoms12m = case_when(condoms_anal_insertive %in% c("sometimes","no") |
                                                 condoms_anal_receptive %in% c("sometimes","no") |
                                                 condoms_vaginal %in% c("sometimes","no") ~ "Y - Yes, Anal or Vaginal intercourse (with or without oral sex) [YAV]",
                                             condoms_oral_receptive %in% c("sometimes","no") | 
                                                 condoms_oral %in% c("sometimes","no") ~ "O - Oral sex only [O]",
                                             condoms_anal_insertive=="yes" & condoms_anal_receptive=="yes" &
                                                 condoms_vaginal=="yes" & condoms_oral_receptive=="yes" &
                                                 condoms_oral=="yes" ~ "N - No [N]",
                                             TRUE ~ "UNK - Unknown [UNK]"),
                    # condoms_anal12m, condoms_vaginal_12m, and condoms_oral12m are created
                    # to be pasted together below in condoms_sextype_12m which is to fill in:
                    # "Type of Sex without using a condom" - these variables are
                    # only filled if folks had sex without a condom, otherwise blank
                    condoms_anal12m = ifelse(condoms_anal_insertive %in% c("sometimes","no") |
                                                condoms_anal_receptive %in% c("sometimes","no"), "A - Anal [A]",""),
                    condoms_vaginal12m = ifelse(condoms_vaginal %in% c("sometimes","no"), "V - Vaginal [V]",""),
                    condoms_oral12m = ifelse(condoms_oral_receptive %in% c("sometimes","no") |
                                                condoms_oral %in% c("sometimes","no"), "O - Oral [O]",""),
                    # condoms12m_pos is created to answer the question "Positioning (anal only)"
                    # in the condom sequence
                    condoms12m_pos = case_when(condoms_anal_insertive %in% c("sometimes","no") &
                                                     condoms_anal_receptive %in% c("sometimes","no") ~ "B - Both [B]",
                                                 condoms_anal_insertive %in% c("sometimes","no") ~ "I - Insertive [I]",
                                                 condoms_anal_receptive %in% c("sometimes","no") ~ "R - Receptive [R]",
                                                 TRUE ~ NA_character_  ),
                    # sexual_identity currently doesn't get used - this is recoding the ego's sexual
                    # identity - need to figure out where this goes for CHIMS
                    sexual_identity = recode(sexual_identity, `1` = "Heterosexual", `2` = "Gay or Lesbian",
                                               `3` = "Bisexual", `4` = "Unknown", `5` = "Other", .default=NA_character_),
                    # role allows us to answer the ego's "Type of Sex with male partner(s)"
                    # recoding the network canvas options to match CHIMS
                    role = recode(role_self, "top" = "I - Insertive [I]", "bottom" = "R - Receptive [R]",
                                    "vers" = "B - Both [B]"),
                    # exchanged_sex answers the CHIMS q "Exchanged drugs/money/goods for sex in past 12 months"
                    # recodes network canvas output
                    exchanged_sex = case_when(sex_type_exchange_anal==TRUE | sex_type_exchange_vaginal==TRUE~ "Y - Yes, Anal or Vaginal intercourse (with or without oral sex) [YAV]",
                                              sex_type_exchange_oral == TRUE & sex_type_exchange_anal==FALSE & sex_type_exchange_vaginal==FALSE~ "O - Oral sex only [O]",
                                         TRUE ~ "N - No [N]"),
                    # exch_sex_anal, exch_sex_vaginal, exch_sex_oral are created
                    # to be pasted together below in exch_sextype which is to fill in:
                    # "Type of sex exchanged"
                    exch_sex_anal = ifelse(sex_type_exchange_anal == TRUE, "A - Anal [A]",""),
                    exch_sex_vaginal = ifelse(sex_type_exchange_vaginal == TRUE , "V - Vaginal [V]",""),
                    exch_sex_oral = ifelse(sex_type_exchange_oral == TRUE, "O - Oral [O]",""),
                    # sex_high answers the CHIMS q "Had sex while intoxicated or high on drugs during the interview period?"
                    # recodes the output from network canvas
                    sex_high = case_when(sex_under_influence_anal==TRUE | sex_under_influence_vaginal==TRUE~ "Y - Yes, Anal or Vaginal intercourse (with or without oral sex) [YAV]",
                                         sex_under_influence_oral == TRUE & sex_under_influence_anal==FALSE & sex_under_influence_vaginal==FALSE~ "O - Oral sex only [O]",
                                         TRUE ~ "N - No [N]")) %>%
        # this mutate_at recodes all of the ego level drug use variables from Network Canvas's output
        # (false/true) to CHIMS's version (N - No/Y - Yes)
        dplyr::mutate_at(vars("drug_use","alcohol_use","drug_specific_crack","drug_specific_cocaine",
                              "drug_specific_heroin","drug_specific_meth","drug_specific_nitrate",
                              "drug_specific_erectile_dysfunciton","drug_specific_marijuana","drug_specific_other",
                              "injection_drug_use"),
                         ~recode(.,`false` = "N - No",`true` = "Y - Yes", .default=NA_character_))
    
    # creates the pasted together version of the three condom use by type of sex questions created above
    # this is the best way I could find to collapse and paste a comma only when not blank
    egodat$condoms_sextype12m <- apply(cbind(egodat$condoms_anal12m,egodat$condoms_vaginal12m,egodat$condoms_oral12m),1,
                                       function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    # creates the pasted together version of the three exchanged sex by type of sex questions created above
    egodat$exch_sextype<- apply(cbind(egodat$exch_sex_anal,egodat$exch_sex_vaginal,egodat$exch_sex_oral),1,
                                       function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    
    
    # Read in and clean the person attribute data - this has the attributes for sexual partner alters
    # and needle sharing alters
    # figure out which one is the "attributeList_Person" data and save that as person_attr_file
    person_attr_file <- filenames[grep("attributeList_Person.csv",filenames)]
    # read in the person attribute data
    person_attr <- read.csv(unz(indat,person_attr_file))
    
    
    #temporarily make logical variables character so that recode function can be used
    person_attr <- person_attr %>%
      mutate_if(is.logical, as.character)
    
    # recode a bunch of variables that were true/false as TRUE/FALSE to make them easier to manipulate
    person_attr <- person_attr %>%
        dplyr::mutate_at(vars("sex_partner","race_white","race_black","race_asian","race_hisp",
                              "race_aian", "race_nhpi", "race_other", "race_uk", "race_refuse",
                           "gender_cis_male","gender_cis_female","gender_trans_male",
                           "gender_trans_female","pregnant_yes","pregnant_no","partner_type_regular",
                           "partner_type_fwb","partner_type_anon","venue_met_internet","venue_met_bar",
                           "venue_met_bath","condoms_yes","condoms_sometimes","condoms_no",
                           "bar_specific_1","bar_specific_2","bar_specific_3","bar_specific_4",
                           "bar_specific_5","bar_specific_6","bar_specific_7","internet_specific_1",
                           "internet_specific_2","internet_specific_3","internet_specific_4",
                           "internet_specific_5","internet_specific_6","internet_specific_7",
                           "HIV_positive","partner_sex_role_bottom","partner_sex_role_top",
                           "partner_sex_role_vers","partner_sex_type_anal",
                           "partner_sex_type_oral","partner_sex_type_vaginal",
                           "partner_type_spouse", "injection_drug_use"),
                         ~recode(., `false`= FALSE, `true` = TRUE, .default = NA))
    # create a bunch of variables for CHIMS based on the person_attr data
    person_attr <- person_attr %>%
        mutate(
            # contact_basis is needed for the referral categories
            ######## check that this coding is correct...
            ### not sure how to use "benefit_from_test" variable in person_attr - there's a dropdown in 
            ### contact's referral basis that is "A3 - Associate - anyone else who would benefit from an exam" and
            ### "S3 - Suspect - anyone else who would benefit from an exam"
            contact_basis = case_when(sex_partner==TRUE & is.na(ego_injection_drug_partner) ~ "P1 - Sex partner [P1]",
                                         ego_injection_drug_partner==TRUE & is.na(sex_partner) ~ "P2 - Needle sharing partner [P2]",
                                         sex_partner==TRUE & ego_injection_drug_partner==TRUE ~ "P3 - Both sex and needle sharing partner [P3]",
                                         TRUE ~ NA_character_),
            # recode the spouse variable to be consistent with CHIMS
            spouse = ifelse(partner_type_spouse==TRUE, "Yes [YES]", "No [NO]"),
            # recode partner's gender to be consistent with CHIMS
            gender = case_when(gender_cis_male==TRUE ~ "Male [MALE]",
                                  gender_cis_female==TRUE ~ "Female [FEMALE]",
                                  gender_trans_female==TRUE ~ "Transgender MTF [MTF]",
                                  gender_trans_male==TRUE ~ "Transgender - FTM [FTM]",
                                  TRUE ~ as.character(gender_other)),
            # recode pregnancy variables to be consistent with CHIMS
            pregnant = case_when(pregnant_yes==TRUE ~ "Yes [YES]",
                                    pregnant_no == TRUE ~ "No [NO]",
                                    TRUE ~ "Not Applicable"),
            # recode the bar and internet 'specific' variables from where a partner
            # was met to list the actual name, to be pasted together below in "venue_all"
            # variable
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
            # create a yes/no variable for whether the partner was met online
            met_internet = ifelse(venue_met_internet==TRUE,"Yes [YES]", "No [NO]"),
            # recode specific race/ethnicity variables to be able to be concatenated
            # below into "race" variable
            black = ifelse(race_black==TRUE, "Black or African American",""),
            white = ifelse(race_white==TRUE,"White",""),
            latinx = ifelse(race_hisp==TRUE,"Hispanic/Latino",""),
            aian = ifelse(race_aian==TRUE, "American Indian Alaskan Native",""),
            nhpi = ifelse(race_nhpi==TRUE, "Native Hawaiian or Pacific Islander",""),
            other = ifelse(race_other==TRUE, "Other",""),
            unknown = ifelse(race_uk==TRUE, "Unknown",""),
            refused = ifelse(race_refuse==TRUE, "Refused",""),
            
           
            # format the dates of first and last sex
            first_sex_formatted = format(as.Date(first_sex), "%b %d, %Y"),
            last_sex_formatted = format(as.Date(last_sex), "%b %d, %Y")
            )
    # concatenate all of the specific venues/internet places the person was met into 
    # a nicely formatted string, pasted together with commas
    person_attr$venue_all <- apply(cbind(person_attr$bar1,person_attr$bar2,person_attr$bar3,
                                         person_attr$bar4,person_attr$bar5,person_attr$bar6,
                                         person_attr$bar7,person_attr$bar_specific_o,person_attr$internet1,
                                         person_attr$internet2,person_attr$internet3,person_attr$internet4,
                                         person_attr$internet5,person_attr$internet6,person_attr$internet7,
                                         person_attr$internet_specific_o),1,
                                   function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    # concatenate all of the specific race/ethnicity variables together into a nicely
    # formatted string, pasted together with commas
    person_attr$race <- apply(cbind(person_attr$black,person_attr$white,
                                    person_attr$latinx,person_attr$aian,person_attr$nhpi,
                                    person_attr$other,person_attr$unknown, person_attr$refused),1,
                              function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))

    
    
    # Create a bunch of summary variables across each partner
    allpartners_dat <- person_attr %>%
        summarise(
                  # create a variable for CHIMS to tell us within the past 12 months whether the respondent
                  # has had sex w/ a cis female partner and type of sex - "Had sex with a female during the interview period?"
                  # i assumed that because they separate out transgender partners, that when they ask about
                  # male and female partners they're only asking about cis partners - might be worth confirming
                  sexw_cisf = case_when(sum(partner_sex_type_anal & gender_cis_female)>=1 |
                                         sum(partner_sex_type_vaginal & gender_cis_female)>=1 ~ 
                                            "Y - Yes, Anal or Vaginal Intercourse (with or without oral sex) [YAV]",
                                        sum(partner_sex_type_oral & gender_cis_female)>=1 ~ "O - Oral sex only [O]",
                                        TRUE ~ "N - No [N]"),
                  # these three variables - anal_cisf, vag_cisf, oral_cisf are created to be concatenated below
                  # into cisf_sextype
                  anal_cisf = ifelse(sum(partner_sex_type_anal & gender_cis_female)>=1,"A - Anal [A]",""),
                  vag_cisf = ifelse(sum(partner_sex_type_vaginal & gender_cis_female)>=1,"V - Vaginal [V]",""),
                  oral_cisf = ifelse(sum(partner_sex_type_oral & gender_cis_female)>=1,"O - Oral [O]",""),
                  # the next few variables are analagous for cis male, transgender, and anonymous partners as the above for cis female
                  # cis male partners
                  sexw_cism = case_when(sum(partner_sex_type_anal & gender_cis_male)>=1 ~ 
                                            "Y - Yes, Anal Intercourse (with or without oral sex) [YAV]",
                                        sum(partner_sex_type_oral & gender_cis_male)>=1 ~ "O - Oral sex only [O]",
                                        TRUE ~ "N - No [N]"),
                  anal_cism = ifelse(sum(partner_sex_type_anal & gender_cis_male)>=1,"A - Anal [A]",""),
                  oral_cism = ifelse(sum(partner_sex_type_oral & gender_cis_male)>=1,"O - Oral [O]",""),
                  # transgender partners
                  sexw_transg = case_when(sum(partner_sex_type_anal & gender_trans_female)>=1 |
                                              sum(partner_sex_type_vaginal & gender_trans_female)>=1 |
                                              sum(partner_sex_type_anal & gender_trans_male)>=1 ~ 
                                              "Y - Yes, Anal or Vaginal Intercourse (with or without oral sex) [YAV]",
                                          sum(partner_sex_type_oral & gender_cis_female)>=1 |
                                              sum(partner_sex_type_oral & gender_trans_male)>=1 ~ "O - Oral sex only [O]",
                                          TRUE ~ "N - No [N]"),
                  anal_transg = ifelse(sum(partner_sex_type_anal & gender_trans_male)>=1 |
                                           sum(partner_sex_type_anal & gender_trans_female)>=1,"A - Anal [A]",""),
                  vag_transg = ifelse(sum(partner_sex_type_vaginal & gender_trans_female)>=1, "V - Vaginal [V]",""),
                  oral_transg = ifelse(sum(partner_sex_type_oral & gender_trans_male)>=1 |
                                           sum(partner_sex_type_oral & gender_trans_female)>=1,"O - Oral [O]", ""),
                  # anonymous partners
                  sexw_anon = case_when(sum(partner_sex_type_anal & partner_type_anon)>=1 |
                                            sum(partner_sex_type_vaginal & partner_type_anon)>=1 |
                                            sum(partner_sex_type_anal & partner_type_anon)>=1 ~
                                            "Y - Yes, Anal or Vaginal intercourse (with or without oral sex) [YAV]",
                                        sum(partner_sex_type_oral & partner_type_anon)>=1 |
                                            sum(partner_sex_type_oral & partner_type_anon)>=1 ~ 
                                            "O - Oral sex only [O]",
                                        TRUE ~ "N - No [N]"),
                  anal_anon = ifelse(sum(partner_sex_type_anal & partner_type_anon)>=1 |
                                         sum(partner_sex_type_anal & partner_type_anon)>=1,"A - Anal [A]",""),
                  vag_anon = ifelse(sum(partner_sex_type_vaginal & partner_type_anon)>=1,"V - Vaginal [V]",""),
                  oral_anon = ifelse(sum(partner_sex_type_oral & partner_type_anon)>=1 |
                                         sum(partner_sex_type_oral & partner_type_anon)>=1, "O - Oral [O]",""),
                  # these variables (n_) total the number of cis female, male, transgender, and anonymous partners
                  # in the previous 12 months
                  n_cisf = sum(gender_cis_female==TRUE & sex_partner==TRUE,na.rm=TRUE),
                  n_cism = sum(gender_cis_male==TRUE & sex_partner==TRUE, na.rm=TRUE),
                  n_trans = sum((gender_trans_female==TRUE | gender_trans_male==TRUE) &
                                    sex_partner==TRUE, na.rm=TRUE),
                  n_anon = sum(partner_type_anon==TRUE & sex_partner==TRUE,na.rm=TRUE),
                  # variable to answer "Had sex with a person with AIDS or documented HIV infection during the interview period?"
                  # only ask if a partner is known HIV positive, not if their status is unknown, so 
                  # can't split up "No" and "Unknown"
                  sexw_hivpos = ifelse(sum(HIV_positive & sex_partner,na.rm=TRUE)>=1,"Y - Yes [Y]","No or Unknown"),
                  
                  #created to answer question "met partners through the internet"
                  met_internet = ifelse(sum(venue_met_internet==TRUE)>0,"Y - Yes","N - No"),
                  
                  #this variable created to answer "Shared injection drug equipment in past 12 months?" on substance use q package
                  shared_inj = case_when(sum(ego_injection_drug_partner==TRUE, na.rm=TRUE)>=1  ~ 
                                            "Y - Yes",
                                        TRUE ~ "N - No"),
                  
                  #sex_with_idu created to answer "Had sex with a person who is known to be an IDU in past 12 months?"
                  sex_with_idu = case_when(sum(injection_drug_use==TRUE, na.rm=TRUE)>=1 ~ "Y - Yes",
                                           TRUE ~ "N - No"),
                  
                  #these variables are created to be concatenated below to answer question "type of sex with known idu"
                  
                  anal_with_idu = ifelse(sum(partner_sex_type_anal & injection_drug_use)>0,"A - Anal [A]",""),
                  vag_with_idu = ifelse(sum(partner_sex_type_vaginal & injection_drug_use)>0, "V - Vaginal [V]",""),
                  oral_with_idu = ifelse(sum(partner_sex_type_oral & injection_drug_use)>0,"O - Oral [O]", ""),
                  )
                
    # create subsets of person attribute data for interview periods
    #### need to replace Sys.Date() with date of infection?
    
    #determines date 90 days, 6.5 months before today to establish interview period
    person_attr_90d <- person_attr[person_attr$last_sex >= interviewperiodstart, ]
    person_attr_7m <- person_attr[person_attr$last_sex >= interviewperiodstart, ]
    
    # Create a bunch of summary variables across each partner for 90 days interview period
    allpartners_dat_90d <- person_attr_90d %>%
        summarise(
            # create a variable for CHIMS to tell us within the past 90 days whether the respondent
            # has had sex w/ a cis female partner and type of sex - "Had sex with a female during the interview period?"
            sexw_cisf = case_when(sum(partner_sex_type_anal & gender_cis_female)>=1 |
                                      sum(partner_sex_type_vaginal & gender_cis_female)>=1 ~ 
                                      "Y - Yes, Anal or Vaginal Intercourse (with or without oral sex) [YAV]",
                                  sum(partner_sex_type_oral & gender_cis_female)>=1 ~ "O - Oral sex only [O]",
                                  TRUE ~ "N - No [N]"),
            # these three variables - anal_cisf, vag_cisf, oral_cisf are created to be concatenated below
            # into cisf_sextype
            anal_cisf = ifelse(sum(partner_sex_type_anal & gender_cis_female)>=1,"A - Anal [A]",""),
            vag_cisf = ifelse(sum(partner_sex_type_vaginal & gender_cis_female)>=1,"V - Vaginal [V]",""),
            oral_cisf = ifelse(sum(partner_sex_type_oral & gender_cis_female)>=1,"O - Oral [O]",""),
            # the next few variables are analagous for cis male, transgender, and anonymous partners as the above for cis female
            # cis male partners
            sexw_cism = case_when(sum(partner_sex_type_anal & gender_cis_male)>=1 ~ 
                                      "Y - Yes, Anal Intercourse (with or without oral sex) [YAV]",
                                  sum(partner_sex_type_oral & gender_cis_male)>=1 ~ "O - Oral sex only [O]",
                                  TRUE ~ "N - No [N]"),
            anal_cism = ifelse(sum(partner_sex_type_anal & gender_cis_male)>=1,"A - Anal [A]",""),
            oral_cism = ifelse(sum(partner_sex_type_oral & gender_cis_male)>=1,"O - Oral [O]",""),
            # transgender partners
            sexw_transg = case_when(sum(partner_sex_type_anal & gender_trans_female)>=1 |
                                        sum(partner_sex_type_vaginal & gender_trans_female)>=1 |
                                        sum(partner_sex_type_anal & gender_trans_male)>=1 ~ 
                                        "Y - Yes, Anal or Vaginal Intercourse (with or without oral sex) [YAV]",
                                    sum(partner_sex_type_oral & gender_cis_female)>=1 |
                                        sum(partner_sex_type_oral & gender_trans_male)>=1 ~ "O - Oral sex only [O]",
                                    TRUE ~ "N - No [N]"),
            anal_transg = ifelse(sum(partner_sex_type_anal & gender_trans_male)>=1 |
                                     sum(partner_sex_type_anal & gender_trans_female)>=1,"A - Anal [A]",""),
            vag_transg = ifelse(sum(partner_sex_type_vaginal & gender_trans_female)>=1, "V - Vaginal [V]",""),
            oral_transg = ifelse(sum(partner_sex_type_oral & gender_trans_male)>=1 |
                                     sum(partner_sex_type_oral & gender_trans_female)>=1,"O - Oral [O]", ""),
            # anonymous partners
            sexw_anon = case_when(sum(partner_sex_type_anal & partner_type_anon)>=1 |
                                      sum(partner_sex_type_vaginal & partner_type_anon)>=1 |
                                      sum(partner_sex_type_anal & partner_type_anon)>=1 ~
                                      "Y - Yes, Anal or Vaginal intercourse (with or without oral sex) [YAV]",
                                  sum(partner_sex_type_oral & partner_type_anon)>=1 |
                                      sum(partner_sex_type_oral & partner_type_anon)>=1 ~ 
                                      "O - Oral sex only [O]",
                                  TRUE ~ "N - No [N]"),
            anal_anon = ifelse(sum(partner_sex_type_anal & partner_type_anon)>=1 |
                                   sum(partner_sex_type_anal & partner_type_anon)>=1,"A - Anal [A]",""),
            vag_anon = ifelse(sum(partner_sex_type_vaginal & partner_type_anon)>=1,"V - Vaginal [V]",""),
            oral_anon = ifelse(sum(partner_sex_type_oral & partner_type_anon)>=1 |
                                   sum(partner_sex_type_oral & partner_type_anon)>=1, "O - Oral [O]",""),
            # these variables (n_) total the number of cis female, male, transgender, and anonymous partners
            # in the previous 12 months
            n_cisf = sum(gender_cis_female==TRUE & sex_partner==TRUE,na.rm=TRUE),
            n_cism = sum(gender_cis_male==TRUE & sex_partner==TRUE, na.rm=TRUE),
            n_trans = sum((gender_trans_female==TRUE | gender_trans_male==TRUE) &
                              sex_partner==TRUE, na.rm=TRUE),
            n_anon = sum(partner_type_anon==TRUE & sex_partner==TRUE,na.rm=TRUE),
            # variable to answer "Had sex with a person with AIDS or documented HIV infection during the interview period?"
            # only ask if a partner is known HIV positive, not if their status is unknown, so 
            # can't split up "No" and "Unknown"
            sexw_hivpos = ifelse(sum(HIV_positive & sex_partner,na.rm=TRUE)>=1,"Y - Yes [Y]","No or Unknown"),
            met_internet = ifelse(sum(venue_met_internet==TRUE)>0,"Y - Yes","N - No"),
            
            #sex_with_idu created to answer "Had sex with a person who is known to be an IDU in interview period?"
            sex_with_idu = case_when(sum(injection_drug_use==TRUE, na.rm=TRUE)>=1 ~ "Y - Yes",
                                     TRUE ~ "N - No"),
            
            #these variables are created to be concatenated below to answer question "type of sex with known idu"
            
            anal_with_idu = ifelse(sum(partner_sex_type_anal & injection_drug_use)>0,"A - Anal [A]",""),
            vag_with_idu = ifelse(sum(partner_sex_type_vaginal & injection_drug_use)>0, "V - Vaginal [V]",""),
            oral_with_idu = ifelse(sum(partner_sex_type_oral & injection_drug_use)>0,"O - Oral [O]", ""))
    
    # Create a bunch of summary variables across each partner for 7 months interview period
    allpartners_dat_7m <- person_attr_7m %>%
        summarise(
            # create a variable for CHIMS to tell us within the past 6.5 months whether the respondent
            # has had sex w/ a cis female partner and type of sex - "Had sex with a female during the interview period?"
            sexw_cisf = case_when(sum(partner_sex_type_anal & gender_cis_female)>=1 |
                                      sum(partner_sex_type_vaginal & gender_cis_female)>=1 ~ 
                                      "Y - Yes, Anal or Vaginal Intercourse (with or without oral sex) [YAV]",
                                  sum(partner_sex_type_oral & gender_cis_female)>=1 ~ "O - Oral sex only [O]",
                                  TRUE ~ "N - No [N]"),
            # these three variables - anal_cisf, vag_cisf, oral_cisf are created to be concatenated below
            # into cisf_sextype
            anal_cisf = ifelse(sum(partner_sex_type_anal & gender_cis_female)>=1,"A - Anal [A]",""),
            vag_cisf = ifelse(sum(partner_sex_type_vaginal & gender_cis_female)>=1,"V - Vaginal [V]",""),
            oral_cisf = ifelse(sum(partner_sex_type_oral & gender_cis_female)>=1,"O - Oral [O]",""),
            # the next few variables are analagous for cis male, transgender, and anonymous partners as the above for cis female
            # cis male partners
            sexw_cism = case_when(sum(partner_sex_type_anal & gender_cis_male)>=1 ~ 
                                      "Y - Yes, Anal Intercourse (with or without oral sex) [YAV]",
                                  sum(partner_sex_type_oral & gender_cis_male)>=1 ~ "O - Oral sex only [O]",
                                  TRUE ~ "N - No [N]"),
            anal_cism = ifelse(sum(partner_sex_type_anal & gender_cis_male)>=1,"A - Anal [A]",""),
            oral_cism = ifelse(sum(partner_sex_type_oral & gender_cis_male)>=1,"O - Oral [O]",""),
            # transgender partners
            sexw_transg = case_when(sum(partner_sex_type_anal & gender_trans_female)>=1 |
                                        sum(partner_sex_type_vaginal & gender_trans_female)>=1 |
                                        sum(partner_sex_type_anal & gender_trans_male)>=1 ~ 
                                        "Y - Yes, Anal or Vaginal Intercourse (with or without oral sex) [YAV]",
                                    sum(partner_sex_type_oral & gender_cis_female)>=1 |
                                        sum(partner_sex_type_oral & gender_trans_male)>=1 ~ "O - Oral sex only [O]",
                                    TRUE ~ "N - No [N]"),
            anal_transg = ifelse(sum(partner_sex_type_anal & gender_trans_male)>=1 |
                                     sum(partner_sex_type_anal & gender_trans_female)>=1,"A - Anal [A]",""),
            vag_transg = ifelse(sum(partner_sex_type_vaginal & gender_trans_female)>=1, "V - Vaginal [V]",""),
            oral_transg = ifelse(sum(partner_sex_type_oral & gender_trans_male)>=1 |
                                     sum(partner_sex_type_oral & gender_trans_female)>=1,"O - Oral [O]", ""),
            # anonymous partners
            sexw_anon = case_when(sum(partner_sex_type_anal & partner_type_anon)>=1 |
                                      sum(partner_sex_type_vaginal & partner_type_anon)>=1 |
                                      sum(partner_sex_type_anal & partner_type_anon)>=1 ~
                                      "Y - Yes, Anal or Vaginal intercourse (with or without oral sex) [YAV]",
                                  sum(partner_sex_type_oral & partner_type_anon)>=1 |
                                      sum(partner_sex_type_oral & partner_type_anon)>=1 ~ 
                                      "O - Oral sex only [O]",
                                  TRUE ~ "N - No [N]"),
            anal_anon = ifelse(sum(partner_sex_type_anal & partner_type_anon)>=1 |
                                   sum(partner_sex_type_anal & partner_type_anon)>=1,"A - Anal [A]",""),
            vag_anon = ifelse(sum(partner_sex_type_vaginal & partner_type_anon)>=1,"V - Vaginal [V]",""),
            oral_anon = ifelse(sum(partner_sex_type_oral & partner_type_anon)>=1 |
                                   sum(partner_sex_type_oral & partner_type_anon)>=1, "O - Oral [O]",""),
            # these variables (n_) total the number of cis female, male, transgender, and anonymous partners
            # in the previous 12 months
            n_cisf = sum(gender_cis_female==TRUE & sex_partner==TRUE,na.rm=TRUE),
            n_cism = sum(gender_cis_male==TRUE & sex_partner==TRUE, na.rm=TRUE),
            n_trans = sum((gender_trans_female==TRUE | gender_trans_male==TRUE) &
                              sex_partner==TRUE, na.rm=TRUE),
            n_anon = sum(partner_type_anon==TRUE & sex_partner==TRUE,na.rm=TRUE),
            # variable to answer "Had sex with a person with AIDS or documented HIV infection during the interview period?"
            # only ask if a partner is known HIV positive, not if their status is unknown, so 
            # can't split up "No" and "Unknown"
            sexw_hivpos = ifelse(sum(HIV_positive & sex_partner,na.rm=TRUE)>=1,"Y - Yes [Y]","No or Unknown"),
            met_internet = ifelse(sum(venue_met_internet==TRUE)>0,"Y - Yes","N - No"),
            
            #sex_with_idu created to answer "Had sex with a person who is known to be an IDU in interview period?"
            sex_with_idu = case_when(sum(injection_drug_use==TRUE, na.rm=TRUE)>=1 ~ "Y - Yes",
                                     TRUE ~ "N - No"),
            
            #these variables are created to be concatenated below to answer question "type of sex with known idu"
            
            anal_with_idu = ifelse(sum(partner_sex_type_anal & injection_drug_use)>0,"A - Anal [A]",""),
            vag_with_idu = ifelse(sum(partner_sex_type_vaginal & injection_drug_use)>0, "V - Vaginal [V]",""),
            oral_with_idu = ifelse(sum(partner_sex_type_oral & injection_drug_use)>0,"O - Oral [O]", ""))
            
    
    # these variables do the nice concatenation for sex type with different gender partners, anonymous partners,
    # and HIV positive partners
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
    
    allpartners_dat$sex_type_with_idu <- apply(cbind(allpartners_dat$anal_with_idu,allpartners_dat$vag_with_idu,allpartners_dat$oral_with_idu),1,
                                               function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    
    #same as above for 90 day interview period
    
    
    allpartners_dat_90d$cisf_sextype <- apply(cbind(allpartners_dat_90d$anal_cisf,allpartners_dat_90d$vag_cisf,allpartners_dat_90d$oral_cisf),1,
                                          function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    allpartners_dat_90d$cism_sextype <- apply(cbind(allpartners_dat_90d$anal_cism,allpartners_dat_90d$oral_cism),1,
                                          function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    allpartners_dat_90d$transg_sextype <- apply(cbind(allpartners_dat_90d$anal_transg,allpartners_dat_90d$vag_transg,allpartners_dat_90d$oral_transg),1,
                                            function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    allpartners_dat_90d$anon_sextype <- apply(cbind(allpartners_dat_90d$anal_anon,allpartners_dat_90d$vag_anon,allpartners_dat_90d$oral_anon),1,
                                          function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    allpartners_dat_90d$hivpos_gender <- apply(t(person_attr_90d$gender[person_attr_90d$HIV_positive & person_attr_90d$sex_partner]),1,
                                           function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    allpartners_dat_90d$sex_type_with_idu <- apply(cbind(allpartners_dat_90d$anal_with_idu,allpartners_dat_90d$vag_with_idu,allpartners_dat_90d$oral_with_idu),1,
                                               function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    
    #same as above for 7 month interview period
    allpartners_dat_7m$cisf_sextype <- apply(cbind(allpartners_dat_7m$anal_cisf,allpartners_dat_7m$vag_cisf,allpartners_dat_7m$oral_cisf),1,
                                              function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    allpartners_dat_7m$cism_sextype <- apply(cbind(allpartners_dat_7m$anal_cism,allpartners_dat_7m$oral_cism),1,
                                              function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    allpartners_dat_7m$transg_sextype <- apply(cbind(allpartners_dat_7m$anal_transg,allpartners_dat_7m$vag_transg,allpartners_dat_7m$oral_transg),1,
                                                function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    allpartners_dat_7m$anon_sextype <- apply(cbind(allpartners_dat_7m$anal_anon,allpartners_dat_7m$vag_anon,allpartners_dat_7m$oral_anon),1,
                                              function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    allpartners_dat_7m$hivpos_gender <- apply(t(person_attr_7m$gender[person_attr_7m$HIV_positive & person_attr_7m$sex_partner]),1,
                                               function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    allpartners_dat_7m$sex_type_with_idu <- apply(cbind(allpartners_dat_7m$anal_with_idu,allpartners_dat_7m$vag_with_idu,allpartners_dat_7m$oral_with_idu),1,
                                               function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))
    
    # Read in and clean the venue attribute data
    # same process with the venue data
    venue_attr_file <- filenames[grep("attributeList_Venue.csv",filenames)]
    venue_attr <- read.csv(unz(indat,venue_attr_file))
    
    venue_summary <- venue_attr %>%
        summarise(any_online = ifelse(sum(type=="app")>0,"Y - Yes","N - No"))
    
    #removes rows of venues file if venue name is "other" 
    venue_attr <- venue_attr %>%
        filter(name!="Other")
    
    venue_attr <- venue_attr %>%
        mutate(
            
            #recode venue_met, venue_sex, and venue_drugs to their names to be pasted together
            met = ifelse(venue_met=="true", "Met", ""),
            sex = ifelse(venue_sex=="true", "Sex", ""),
            drugs = ifelse(venue_drugs=="true", "Drug/needle-sharing", ""),
            
            #other_venue is name if type is missing. if type is present, other_venue is left blank
            
            other_venue = case_when(type=="App" | type=="Bar" | type=="Other" | 
                                        type == "Website" | type=="Park" ~ NA_character_,
                                    TRUE ~ venue_attr$name),
            
            #venue_name is name if type is present. if type is missing, venue_name is "Other"
            
            venue_name = case_when(type=="App" | type=="Bar" | type=="Other" |
                                       type == "Website" | type=="Park"~ venue_attr$name,
                                   TRUE ~ "Other")
            )
    
    
    
    # concatenate the activities of each venue
    # a nicely formatted string, pasted together with commas
    venue_attr$activity <- apply(cbind(venue_attr$met,venue_attr$sex,venue_attr$drugs),1,
                                   function(x) paste(x[!is.na(x) & x!=""], collapse = ", "))

    
    
    # This chunk of code writes out the different CHIMS questions to go into the table
    # since the questions are the same for the interview period and past 12 months, do
    # this for loop to create the two lists formatted the way we want
    # the <b> at the start and </b> at the end tell R that we want that string to be bolded 
    # tried to bold the groups of questions
    refperiod <- c("in past 12 months?", "during the interview period?")
    
    #Louis Added
    modelvariables_sexbehav90days <- c("SEX_FEMALE_IX_PD",
                                  "SEX_FEMALE_TYPE_IX_PD",
                                  "SEX_FEMALE_NUMBER_PARTNERS_IX_PD",
                                  "SEX_MALE_IX_PD",
                                  "SEX_MALE_TYPE_IX_PD",
                                  "SEX_MALE_POSITIONING_IX_PD",
                                  "SEX_MALE_NUMBER_PARTNERS_IX_PD",
                                  "SEX_MALE_HETEROSEXUAL_WITH_BISEXUAL_MALE_IX_PD",
                                  "SEX_TRANSGENDER_IX_PD",
                                  "SEX_TRANSGENDER_TYPE_IX_PD",
                                  "SEX_TRANSGENDER_NUMBER_PARTNERS_IX_PD",
                                  "SEX_ANONYMOUS_IX_PD",
                                  "SEX_ANONYMOUS_TYPE_IX_PD",
                                  "SEX_ANONYMOUS_NUMBER_PARTNERS_IX_PD",
                                  "SEX_USING_CONDOM_IX_PD",
                                  "SEX_USING_CONDOM_TYPE_IX_PD",
                                  "SEX_USING_CONDOM_POSITIONING_IX_PD",
                                  "SEX_WHILE_INTOXICATED_HIGH_IX_PD",
                                  "SEX_WHILE_INTOXICATED_HIGH_TYPE_IX_PD",
                                  "SEX_EXCHANGED_DRUGS_MONEY_IX_PD",
                                  "SEX_EXCHANGED_DRUGS_MONEY_TYPE_IX_PD",
                                  "SEX_EXCHANGED_DRUGS_MONEY_RELATIONSHIP_IX_PD",
                                  "SEX_WITH_MSM_IX_PD",
                                  "SEX_WITH_MSM_TYPE_IX_PD",
                                  "SEX_WITH_IDU_IX_PD",
                                  "SEX_WITH_IDU_TYPE_IX_PD",
                                  "SEX_WITH_BISEXUAL_PERSON_IX_PD",
                                  "SEX_WITH_BISEXUAL_PERSON_GENDER_IX_PD",
                                  "SEX_WITH_PERSON_WITH_AIDS_HIV_IX_PD",
                                  "SEX_WITH_PERSON_WITH_AIDS_HIV_GENDER_IX_PD",
                                  ""
                                  )
    
    modelvariables_sexbehav12m <- c(
                               "SEX_FEMALE_12_MOS",
                               "SEX_FEMALE_TYPE_IX_PD",
                               "SEX_FEMALE_NUMBER_PARTNERS_12_MOS",
                               "SEX_MALE_12_MOS",
                               "SEX_MALE_TYPE_12_MOS",
                               "SEX_MALE_POSITIONING_12_MOS",
                               "SEX_MALE_NUMBER_PARTNERS_12_MOS",
                               "SEX_MALE_HETEROSEXUAL_WITH_BISEXUAL_MALE_12_MOS",
                               "SEX_TRANSGENDER_12_MOS",
                               "SEX_TRANSGENDER_TYPE_12_MOS",
                               "SEX_TRANSGENDER_NUMBER_PARTNERS_12_MOS",
                               "SEX_ANONYMOUS_12_MOS",
                               "SEX_ANONYMOUS_TYPE_12_MOS",
                               "SEX_ANONYMOUS_NUMBER_PARTNERS_12_MOS",
                               "SEX_USING_CONDOM_12_MOS",
                               "SEX_USING_CONDOM_TYPE_12_MOS",
                               "SEX_USING_CONDOM_POSITIONING_12_MOS",
                               "SEX_WHILE_INTOXICATED_HIGH_12_MOS",
                               "SEX_WHILE_INTOXICATED_HIGH_TYPE_12_MOS",
                               "SEX_EXCHANGED_DRUGS_MONEY_12_MOS",
                               "SEX_EXCHANGED_DRUGS_MONEY_TYPE_12_MOS",
                               "SEX_EXCHANGED_DRUGS_MONEY_RELATIONSHIP_12_MOS",
                               "SEX_WITH_MSM_12_MOS",
                               "SEX_WITH_MSM_TYPE_12_MOS",
                               "SEX_WITH_IDU_12_MOS",
                               "SEX_WITH_IDU_TYPE_12_MOS",
                               "SEX_WITH_BISEXUAL_PERSON_12_MOS",
                               "SEX_WITH_BISEXUAL_PERSON_GENDER_12_MOS",
                               "SEX_WITH_PERSON_WITH_AIDS_HIV_12_MOS",
                               "SEX_WITH_PERSON_WITH_AIDS_HIV_GENDER_12_MOS",
                               "")
    #Louis Ended
    
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
    
    # This is a super clunky way to do this, but basically looking at the list of questions
    # from CHIMS in the sexbehavqs object, and putting the outputs from the different datasets we've created above
    # in the order that these questions are - pretty finnicky so probably good to triple check when editing!
    sexbehav12mind <- c(
                        allpartners_dat$sexw_cisf,allpartners_dat$cisf_sextype,allpartners_dat$n_cisf,
                        allpartners_dat$sexw_cism,allpartners_dat$cism_sextype,egodat$role,allpartners_dat$n_cism,
                        "",allpartners_dat$sexw_transg,allpartners_dat$transg_sextype,allpartners_dat$n_trans,
                        allpartners_dat$sexw_anon,allpartners_dat$anon_sextype,allpartners_dat$n_anon,
                        egodat$condoms12m, egodat$condoms_sextype12m, egodat$condoms12m_pos,
                        egodat$sex_high,"",egodat$exchanged_sex,egodat$exch_sextype,"","","",allpartners_dat$sex_with_idu,
                        allpartners_dat$sex_type_with_idu,"","",
                        allpartners_dat$sexw_hivpos,allpartners_dat$hivpos_gender,
                        allpartners_dat$met_internet)
    
    sexbehav90dind <- c(
            allpartners_dat_90d$sexw_cisf,allpartners_dat_90d$cisf_sextype,allpartners_dat_90d$n_cisf,
            allpartners_dat_90d$sexw_cism,allpartners_dat_90d$cism_sextype,"",allpartners_dat_90d$n_cism,
            "",allpartners_dat_90d$sexw_transg,allpartners_dat_90d$transg_sextype,allpartners_dat_90d$n_trans,
            allpartners_dat_90d$sexw_anon,allpartners_dat_90d$anon_sextype,allpartners_dat_90d$n_anon,
            "", "", "",
            "","","","","","","",allpartners_dat_90d$sex_with_idu,allpartners_dat_90d$sex_type_with_idu,"","",
            allpartners_dat_90d$sexw_hivpos,allpartners_dat_90d$hivpos_gender,
            allpartners_dat_90d$met_internet)
    
    sexbehav7mind <- c(
        allpartners_dat_7m$sexw_cisf,allpartners_dat_7m$cisf_sextype,allpartners_dat_7m$n_cisf,
        allpartners_dat_7m$sexw_cism,allpartners_dat_7m$cism_sextype,"",allpartners_dat_7m$n_cism,
        "",allpartners_dat_7m$sexw_transg,allpartners_dat_7m$transg_sextype,allpartners_dat_7m$n_trans,
        allpartners_dat_7m$sexw_anon,allpartners_dat_7m$anon_sextype,allpartners_dat_7m$n_anon,
        "", "", "",
        "","","","","","","",allpartners_dat_7m$sex_with_idu,allpartners_dat_7m$sex_type_with_idu,"","",
        allpartners_dat_7m$sexw_hivpos,allpartners_dat_7m$hivpos_gender,
        allpartners_dat_7m$met_internet)
        
    
    # put together the questions and responses for sexual behavior in the past 12 months
    sexbehav12m <- data.frame(modelvariables_sexbehav12m,
                              Questions = sexbehavqs[[1]],
                              Responses = sexbehav12mind)
    
    # put together the questions and responses for sexual behavior in interview period
    sexbehav90days <- data.frame(modelvariables_sexbehav90days,
                                 Questions = sexbehavqs[[2]],
                                 Responses = sexbehav90dind)
    
    sexbehav7mo <- data.frame(Questions = sexbehavqs[[2]],
                              Responses = sexbehav7mind)
    
    
    # Now go through the same process for drug use in the past 12 months
    #Louis added:
    modelvariables_druguse <- c("DRUGS_12_MOS",
                           "ALCOHOL_12_MOS",
                           "CRACK_12_MOS",
                           "COCAINE_12_MOS",
                           "HEROIN_12_MOS",
                           "METHAMPHETAMINE_12_MOS",
                           "NITRATES_POPPERS_12_MOS",
                           "ED_MEDS_12_MOS",
                           "MARIJUANA_12_MOS",
                           "OTHER_DRUGS_12_MOS",
                           "OTHER_DRUGS_12_MOS_SPECIFY",
                           "INJECTION_DRUGS_LAST_12_MONTHS",
                           "INJECTION_DRUGS_LAST_12_MONTHS_SHARED_EQUIPMENT",
                           "OTHER_DRUGS_12_MOS_FREQUENCY_UNITS"
                           )
    druguseqs <- list()
    for(i in 1:length(refperiod)) {
        druguseqs[[i]] <- c(paste0("<b>Any alcohol or drug use ",refperiod[i]," (injection or non-injection)</b>"),
                            "<b>Alcohol</b>",
                            "<b>Crack</b>",
                            "<b>Cocaine</b>",
                            "<b>Heroin</b>",
                            "<b>Methamphetamine</b>",
                            "<b>Nitrates/poppers</b>",
                            "<b>Erectile dysfunction medications</b>",
                            "<b>Marijuana</b>",
                            "<b>Other drugs</b>","Specify drug(s)",
                            paste0("<b>Engaged in injection drug use ",refperiod[i],"</b>"),
                            paste0("Shared injection drug equipment ",refperiod[i]),
                            "<b>Frequency of Use</b>")
    }
    
    druguse12mind <- c(egodat$drug_use,egodat$alcohol_use,
                       egodat$drug_specific_crack,egodat$drug_specific_cocaine,
                       egodat$drug_specific_heroin,egodat$drug_specific_meth,
                       egodat$drug_specific_nitrate,egodat$drug_specific_erectile_dysfunciton,
                       egodat$drug_specific_marijuana,egodat$drug_specific_other,
                       egodat$drug_other,egodat$injection_drug_use,allpartners_dat$shared_inj,egodat$drug_use_freq)
    
    druguse12m <- data.frame(modelvariables_druguse,
                             Questions = druguseqs[[1]],
                              Responses = druguse12mind)
    
    # Now go through the same process for contact referrals
    # haven't pulled all the questions from CHIMS here because there are quite
    # a lot of questions that aren't getting asked at present in Network Canvas - 
    # i think Howard Brown will guide whether there are more sections that should be filled
    # in or not
    
    #Louis added
    modelvariables_contact_referral <- c("CONTACT_REFERRAL_BASIS", 
                            "CONTACT_NAME_ALIAS", 
                            "CONTACT_FIRST_NAME",
                            "CONTACT_LAST_NAME",
                            "CONTACT_SPOUSE",
                            "CONTACT_AGE",
                            "CONTACT_GENDER",
                            "CONTACT_PREGNANT",
                            "CONTACT_PREGNANT_WEEKS",
                            "CONTACT_MET_VENUE",
                            "CONTACT_MET_ONLINE",
                            "CONTACT_LOCATING_INFO_SECTION",
                            "CONTACT_CELL_PHONE",
                            "CONTACT_STREET_ADDRESS_1",
                            "CONTACT_PHYSICAL_ATTRIBUTES_SECTION",
                            "CONTACT_HEIGHT",
                            "CONTACT_WEIGHT",
                            "CONTACT_VISIBLE_IDENTIFIERS_TATTOOS_DESCRIBE",
                            "CONTACT_HAIR_COLOR_OTHER_DESCRIBE",
                            "CONTACT_COMPLEXION",
                            "CONTACT_EXPOSURE_FROM_DATE",
                            "CONTACT_EXPOSURE_TO_DATE",
                            "CONTACT_EXPOSURE_FREQUENCY_NUMBER")
    
    contact_referralqs <- c("Contact's referral basis", 
                            "Name or alias", 
                            "Contact's first name",
                            "Contact's last name",
                            "Is this person the spouse of the original patient?",
                            "Person's age (years)",
                            "Gender",
                            "Pregnant",
                            "Number of weeks pregnant",
                            "Venue where OP met/had sex with this person",
                            "Was this partner met through the internet?",
                            "<b>Locating Information</b>",
                            "Phone number","Address",
                            "<b>Physical Attributes</b>",
                            "Height",
                            "Weight",
                            "Visible identifiers",
                            "Hair color",
                            "Race",
                            "Exposure date (first)","Exposure date (last)",
                            "Exposure frequency - number")
    
    contact_referralind <- rbind(person_attr$contact_basis,
                                 person_attr$preferred_name,
                                 person_attr$first_name, 
                                 person_attr$last_name,
                                 person_attr$spouse,
                                 person_attr$age,
                                 person_attr$gender,
                                 person_attr$pregnant,
                                 "",
                                 person_attr$venue_all,
                                 person_attr$met_internet,
                                 "",
                                 person_attr$phone,
                                 person_attr$address,
                                 "",
                                 person_attr$height,
                                 person_attr$weight,
                                 person_attr$descrip_other,
                                 person_attr$hair,person_attr$race,
                                 person_attr$first_sex_formatted,
                                 person_attr$last_sex_formatted,
                                 person_attr$num_times)
    
    contact_referral <- data.frame(modelvariables_contact_referral,
                                   Questions = contact_referralqs,
                                   contact_referralind)
 

    #same process for venues
   #Louis Added
    modelvariables_venues <- c("MET_SEX_PARTNERS_VENUE_TYPE", "MET_SEX_PARTNERS_VENUE", "MET_SEX_PARTNERS_VENUE_OTHER_SPECIFY", "MET_SEX_PARTNERS_VENUE_ACTIVITY")
    venues_qs <- c("Venue Type", "Venue", "Other venue", "Activity")
   
    venues_ind <- rbind(venue_attr$type, venue_attr$venue_name, venue_attr$other_venue, venue_attr$activity)
    
    venues <- data.frame(modelvariables_venues,
                         Questions = venues_qs,
                         venues_ind)

    # Now make a list of all of those datasets for us to be able to use in the Shiny app
    alldat <- list(egodat = egodat, 
                   person_attr = person_attr,
                   venues = venues,
                   sexbehav12m = sexbehav12m,
                   druguse12m = druguse12m,
                   contact_referral = contact_referral,
                   sexbehav90days = sexbehav90days,
                   sexbehav7mo = sexbehav7mo)
    return(alldat)

}
# 
# library(openxlsx)
# 
# # create a new workbook
# wb <- createWorkbook()
# 
# # add data frames to three different sheets in the workbook
# addWorksheet(wb, "All Partners Data")
# writeDataTable(wb, sheet = "All Partners Data", x = allpartners_dat, startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")
# 
# addWorksheet(wb, "Ego Data")
# writeDataTable(wb, sheet = "Ego Data", x = egodat, startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")
# 
# addWorksheet(wb, "Person Attributes")
# writeDataTable(wb, sheet = "Person Attributes", x = person_attr, startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")
# 
# # save the workbook
# saveWorkbook(wb, "/Users/dyl5635/Library/CloudStorage/OneDrive-NorthwesternUniversity/DAA Work Organization 1.0/5. CONNECT Projects/2. R34 Partner Services/DummyData/sample4", overwrite = TRUE)
# 
# wb2 <- createWorkbook()
# 
# # add data frames to three different sheets in the workbook
# addWorksheet(wb2, "Venues")
# writeDataTable(wb2, sheet = "Venues", x = venues, startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")
# 
# 
# addWorksheet(wb2, "Sexual Behavior 3 months")
# writeDataTable(wb2, sheet = "Sexual Behavior 3 months", x = sexbehav90days, startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")
# 
# addWorksheet(wb2, "Sexual Behavior 7 months")
# writeDataTable(wb2, sheet = "Sexual Behavior 7 months", x = sexbehav7mo, startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")
# 
# addWorksheet(wb2, "Sexual Behavior 12 months")
# writeDataTable(wb2, sheet = "Sexual Behavior 12 months", x = sexbehav90days, startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")
# 
# addWorksheet(wb2, "Substance Use within 12 months")
# writeDataTable(wb2, sheet = "Substance Use within 12 months", x = druguse12m, startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")
# 
# addWorksheet(wb2, "Referral Contacts")
# writeDataTable(wb2, sheet = "Referral Contacts", x = contact_referral, startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")
# saveWorkbook(wb2, "/Users/dyl5635/Library/CloudStorage/OneDrive-NorthwesternUniversity/DAA Work Organization 1.0/5. CONNECT Projects/2. R34 Partner Services/DummyData/sample4", overwrite = TRUE)
# 
