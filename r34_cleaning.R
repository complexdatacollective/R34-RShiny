

data_cleaning <- function(indat) {
    
    filenames <- unzip(indat,list=TRUE)$Name
    # Read in and clean the ego data
    egofile <- filenames[grep("ego.csv",filenames)]
    egodat <- read.csv(unz(indat,egofile))
    egodat <- egodat %>%
        dplyr::mutate(condoms12m = case_when(condoms_anal_insertive %in% c("sometimes","no") |
                                                 condoms_anal_receptive %in% c("sometimes","no") |
                                                 condoms_vaginal %in% c("sometimes","no") ~ "Y - Yes, Anal or Vaginal intercourse (with or without oral sex)",
                                             condoms_oral_receptive %in% c("sometimes","no") | 
                                                 condoms_oral %in% c("sometimes","no") ~ "O - Oral sex only",
                                             condoms_anal_insertive=="yes" & condoms_anal_receptive=="yes" &
                                                 condoms_vaginal=="yes" & condoms_oral_receptive=="yes" &
                                                 condoms_oral=="yes" ~ "N - No",
                                             TRUE ~ "UNK - Unknown"),
                      condoms12m_pos = case_when(condoms_anal_insertive %in% c("sometimes","no") &
                                                     condoms_anal_receptive %in% c("sometimes","no") ~ "B - Both",
                                                 condoms_anal_insertive %in% c("sometimes","no") ~ "I - Insertive",
                                                 condoms_anal_receptive %in% c("sometimes","no") ~ "R - Receptive",
                                                 TRUE ~ NA_character_  ),
                      sexual_identity = recode(sexual_identity, `1` = "Heterosexual", `2` = "Gay or Lesbian",
                                               `3` = "Bisexual", `4` = "Unknown", `5` = "Other", .default=NA_character_),
                      role = recode(role_self, "top" = "I - Insertive", "bottom" = "R - Receptive",
                                    "vers" = "B - Both"),
                      sex_high = recode(sex_under_influence, "y_anal_vaginal" = "Yes, Anal or Vaginal intercourse (with or without oral sex)",
                                        "y_oral" = "Oral sex only", "n" = "No")) %>%
        dplyr::mutate_at(vars("drug_use","alcohol_use","drug_specific_crack","drug_specific_cocaine",
                              "drug_specific_heroin","drug_specific_meth","drug_specific_nitrate",
                              "drug_specific_erectile_dysfunciton","drug_specific_marijuana","drug_specific_other",
                              "injection_drug_use"),
                         ~recode(.,`false` = "N - No",`true` = "Y - Yes", .default=NA_character_))
    
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
                           "internet_specific_5","internet_specific_6","internet_specific_7","drug_use_partner"),
                         ~recode(., `false`= FALSE, `true` = TRUE, .default = NA))
    allpartners_dat <- person_attr %>%
        summarise(n_cisf = sum(gender_cis_female==TRUE & sex_partner==TRUE,na.rm=TRUE),
                  n_cism = sum(gender_cis_male==TRUE & sex_partner==TRUE, na.rm=TRUE),
                  n_trans = sum((gender_trans_female==TRUE | gender_trans_male==TRUE) &
                                    sex_partner==TRUE, na.rm=TRUE),
                  n_anon = sum(partner_type_anon==TRUE & sex_partner==TRUE,na.rm=TRUE))

    

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
                            "Gender of bisexaul person",
                            paste0("<b>Had sex with a person with AIDS or documented HIV infection ",refperiod[i],"</b>"),
                            "Gender of person with HIV",
                            paste0("<b>Met partners through the Internet ",refperiod[i],"</b>"))
    }
    
    sexbehav12mind <- c(# A bunch of these outputs can't be filled yet because data collection not consistent
                        # with CHIMS... 
                        "","",allpartners_dat$n_cisf,
                        "","",egodat$role,allpartners_dat$n_cism,
                        "","","",allpartners_dat$n_trans,
                        "","",allpartners_dat$n_anon,
                        egodat$condoms12m, egodat$condoms12m_pos,
                        egodat$sex_high,"",
                        "","","","","","","","","","","",
                        venue_summary$any_online)
    
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

    
    alldat <- list(egodat = egodat, person_attr = person_attr,
                   venue_attr = venue_attr, sex_edgelist = sex_edgelist,
                   know_edgelist = know_edgelist, needles_edgelist = needles_edgelist,
                   sexbehav12m = sexbehav12m,druguse12m = druguse12m)
    
    return(alldat)
}
