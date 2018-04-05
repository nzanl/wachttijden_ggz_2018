clean_data <- function(mq) {
  
  mq[, aanbieder := ifelse(tolower(Groep) == 'n.v.t.', as.character(Naam_zorgaanbieder), as.character(Groep))]
  
  # AGBs
  mq[, AGBcode.1 := as.integer(as.character(AGBcode.1))]
  
  mq[is.na(AGBcode.2) & AGBcode.1 == 22220877 & Naam_zorgaanbieder == 'BLG Psychologen', AGBcode.2 := 94002286]
  mq[is.na(AGBcode.2) & AGBcode.1 == 22220861 & Naam_zorgaanbieder == 'Buro T3', AGBcode.2 := 94060590]
  mq[is.na(AGBcode.2) & AGBcode.1 == 22220855 & Naam_zorgaanbieder == 'Cognito Praktijk voor Psychotherapie en Gezinsbegeleiding', AGBcode.2 := 94055305]
  mq[is.na(AGBcode.2) & AGBcode.1 == 22220880 & Naam_zorgaanbieder == 'De Zorgkliniek', AGBcode.2 := 94058597]
  mq[is.na(AGBcode.2) & AGBcode.1 == 22220903 & Naam_zorgaanbieder == 'LAS Psychologie', AGBcode.2 := 94055570]
  mq[is.na(AGBcode.2) & AGBcode.1 == 22220888 & Naam_zorgaanbieder == 'Medipsy', AGBcode.2 := 94057558]
  mq[is.na(AGBcode.2) & AGBcode.1 == 22220887 & Naam_zorgaanbieder == 'Psychologisch Adviesbureau Heller', AGBcode.2 := 94055468]
  mq[is.na(AGBcode.2) & AGBcode.1 == 22220910 & Naam_zorgaanbieder == 'Psychotherapie Praktijk S. Günther', AGBcode.2 := 94000406]
  mq[is.na(AGBcode.3) & AGBcode.1 == 22220910 & Naam_zorgaanbieder == 'Psychotherapie Praktijk S. Günther', AGBcode.3 := 94004254]
  mq[is.na(AGBcode.2) & AGBcode.1 == 22220876 & Naam_zorgaanbieder == 'Prisma Praktjk voor therapie en coaching', AGBcode.2 := 94001915]
  mq[is.na(AGBcode.2) & AGBcode.1 == 73732510 & Naam_zorgaanbieder == 'Stichting Premiumzorg Zeeland', AGBcode.2 := 94001825]
  mq[is.na(AGBcode.2) & AGBcode.1 == 22220836 & Naam_zorgaanbieder == 'Vitaal mensenwerk', AGBcode.2 := 94055935]
  mq[is.na(AGBcode.2) & AGBcode.1 == 54540021 & Naam_zorgaanbieder == 'Symfora-Meander, Centrum voor psychiatrie', AGBcode.2 := 53530491]
  mq[is.na(AGBcode.1) & Naam_zorgaanbieder == 'Ambulatorium, FSW Universiteit Utrecht', AGBcode.1 := 94000496]
  mq[is.na(AGBcode.2) & AGBcode.1 == 22220789 & Naam_zorgaanbieder == 'Adagio Psychologen', AGBcode.2 := 94056485]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94013575 & Naam_zorgaanbieder == 'Mensano Psychologie', AGBcode.2 := 94004127]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94008576 & Naam_zorgaanbieder == 'Psychologenpraktijk Inspire', AGBcode.2 := 94058494]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94101769 & Naam_zorgaanbieder == 'De Rietkamp Praktijk voor Psychische Hulpverlening', AGBcode.2 := 94002058]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94002651 & Naam_zorgaanbieder == 'Instituut voor Rationele Therapie', AGBcode.2 := 94000177]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94007013 & Naam_zorgaanbieder == 'Psychologiepraktijk Hollands Noorden', AGBcode.2 := 94059070]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94004157 & Naam_zorgaanbieder == 'Hopman Praktijk voor Cognitieve Gedragstherapie', AGBcode.2 := 94001908]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94011625 & Naam_zorgaanbieder == 'Psychotherapiepraktijk Schallenberg', AGBcode.2 := 94059298]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94102299 & Naam_zorgaanbieder == 'Ströer psychotherapie', AGBcode.2 := 94003903]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94002715 & Naam_zorgaanbieder == 'Hamminga Advies', AGBcode.2 := 94002284]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94008400 & Naam_zorgaanbieder == 'Psychologenpraktijk Bommelerwaard', AGBcode.2 := 94000041]
  mq[is.na(AGBcode.3) & AGBcode.1 == 94008400 & Naam_zorgaanbieder == 'Psychologenpraktijk Bommelerwaard', AGBcode.3 := 94062353]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94008004 & Naam_zorgaanbieder == 'Psychologenpraktijk Derksen & Klein Herenbrink', AGBcode.2 := 94000594]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94008598 & Naam_zorgaanbieder == 'De Helper', AGBcode.2 := 94056825]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94011074 & Naam_zorgaanbieder == 'Psychologiepraktijk Emmen', AGBcode.2 := 94061853]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94013479 & Naam_zorgaanbieder == "Psychologenpraktijk Op 't Veld", AGBcode.2 := 94000477]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94009181 & Naam_zorgaanbieder == "Neomai Psychologenpraktijk", AGBcode.2 := 94055449]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94000731 & Naam_zorgaanbieder == "Klinische Psychologie Praktijk B. de Ruyter de Wildt", AGBcode.2 := 94059837]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94060953 & Naam_zorgaanbieder == "Praktijk Reflectief", AGBcode.2 := 94062281]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94011407 & Naam_zorgaanbieder == "Focus Psychologie en coaching", AGBcode.2 := 94059415]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94061692 & Naam_zorgaanbieder == "Praktijk Kramer", AGBcode.2 := 94002172]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94007672 & Naam_zorgaanbieder == "De Psychologiepraktijk", AGBcode.2 := 94062188]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94007004 & Naam_zorgaanbieder == "Samenwerkingsverband Psychologenpraktijk het Waardeel", AGBcode.2 := 94000081]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94101736 & Naam_zorgaanbieder == "Psychologenpraktijk de Mijnstreek", AGBcode.2 := 94056771]
  mq[is.na(AGBcode.2) & AGBcode.1 == 94400869 & Naam_zorgaanbieder == "AuroFocus Psychotherapie", AGBcode.2 := 94002312]
  
  # Niet-bestaande locatie-zorgcombinaties
  mq <- mq[!(grepl('TRTC', Naam_zorgaanbieder) & Hoofddiagnosegroep == 'Angststoornissen')]
  mq <- mq[!(aanbieder == 'St. Antonius Ziekenhuis' & grepl('Groepsbehandeling', Diagnosegroep_letterlijk) & Vestigingslocatie == 'Utrecht')]
  mq <- mq[Opmerking != 'Vestiging gestopt']
  
  # Type aanbieder
  mq[, PUKPAAZ := ifelse(Type_aanbieder == 'PUK/PAAZ', 1, 0)]
  mq[Type_aanbieder == 'PUK/PAAZ', Type_aanbieder := 'Instelling']
  
  # Aanmeldwachttijd
  mq[Opmerking %in% c('Aanmeldstop', 'Geen nieuwe aanmeldingen', 'Geen behandelmogelijkheid', 'Clientstop', 
                        'Algehele aanmeldstop', 'Aanmeldstop tot krokusvakantie', 'Clientstop tot januari',
                        'Clientstop tot 8 januari', 'Clientstop tot half januari', 'Aanmeldstop tot 11 december',
                        'Aanmeldstop tot januari', 'Aanmeldstop tot 1 juli 2018', 'Clientstop tot 15 januari',
                        'Vanaf april weer plek', 'Plek vanaf half januari', 'Aanmeldstop tot 1 december',
                        'Aanmeldstop tot januari 2018', 'Aanmelden vanaf januari 2018', 'Aanmeldstop tot 1 september',
                        'Wij nemen helaas geen nieuwe aanmeldingen aan tot 1 oktober 2017', 
                        'Op dit ogenblik hebben wij helaas voor de GGz een aanmeldstop.', 'Geen ruimte voor nieuwe patienten'),
       Aanmeldwachttijd := 'Stop']
  
  mq[Opmerking == 'Half oktober ruimte in Harderwijk, in Utrecht geen plek' & Plaatsnaam == 'Utrecht',
       Aanmeldwachttijd := 'Stop']
  mq[Opmerking == 'Tot 1 maart 2018 is het tijdelijk niet mogelijk om zich bij EDT Maastricht aan te melden, vanwege intakestop. Dit geldt niet voor onze locatie in Amsterdam.' & Plaatsnaam == 'Maastricht',
     Aanmeldwachttijd := 'Stop']
  mq[Opmerking == 'Trauma TRTC zet op dit moment geen patiënten op de wachtlijst voor individuele behandeling, omdat de duur van de wachttijd (1,5 jaar) voor de meeste patiënten niet verantwoord is.' & Naam_zorgaanbieder == 'Trauma TRTC',
     Aanmeldwachttijd := 'Stop']
  mq[Opmerking == 'In Zierikzee nemen wij op dit moment geen nieuwe patiënten op' & Plaatsnaam == 'Zierikzee',
     Aanmeldwachttijd := 'Stop']
  mq[Opmerking == 'Tijdelijk geen BGGZ verwijzingen' & Type_zorg == 'BGGZ',
     Aanmeldwachttijd := 'Stop']
  
  res <- mq[Opmerking == 'Voor diagnostiek en/of behandeling bij ADHD en ASS-problematiek geldt vanwege personele bezetting een stop op aanmeldingen / verwijzingen tot februari 2018' & Vestigingslocatie == 'Regio Twente - Samen 14']
  res[, Diagnosegroep_letterlijk := 'ADHD en ASS-problematiek']
  res[, Hoofddiagnosegroep := 'Aandachtstekort- en gedragsstoornissen']
  res[, Aanmeldwachttijd := 'Stop']
  res[, Zorgverzekeraar_toelichting := '']
  
  mq <- rbindlist(list(mq, res))
  
  res1 <- mq[Opmerking == 'Per therapeut: Heerbeek heeft aanmeldstop, Kokhuis en Ploegmakers 8 en 6 weken' & Naam_zorgaanbieder == 'Psychologenpraktijk Mind' & Circuit == 'Adolescenten']
  res1[, Naam_zorgaanbieder := 'Heerbeek']
  res1[, Groep := 'Psychologenpraktijk Mind']
  res1[, Aanmeldwachttijd := 'Stop']
  
  res2 <- mq[Opmerking == 'Per therapeut: Heerbeek heeft aanmeldstop, Kokhuis en Ploegmakers 8 en 6 weken' & Naam_zorgaanbieder == 'Psychologenpraktijk Mind' & Circuit == 'Adolescenten']
  res2[, Naam_zorgaanbieder := 'Kokhuis']
  res2[, Groep := 'Psychologenpraktijk Mind']
  
  res3 <- mq[Opmerking == 'Per therapeut: Heerbeek heeft aanmeldstop, Kokhuis en Ploegmakers 8 en 6 weken' & Naam_zorgaanbieder == 'Psychologenpraktijk Mind' & Circuit == 'Adolescenten']
  res3[, Naam_zorgaanbieder := 'Ploegmakers']
  res3[, Groep := 'Psychologenpraktijk Mind']
  
  res4 <- mq[Opmerking == "Smit 7 weken; Ploegmakers 6 wkn'; Kokhuis 8 wkn" & Naam_zorgaanbieder == 'Psychologenpraktijk Mind' & Circuit == 'Volwassenen']
  res4[, Naam_zorgaanbieder := 'Smit']
  res4[, Groep := 'Psychologenpraktijk Mind']
  res4[, Aanmeldwachttijd := '7']
  
  res5 <- mq[Opmerking == "Smit 7 weken; Ploegmakers 6 wkn'; Kokhuis 8 wkn" & Naam_zorgaanbieder == 'Psychologenpraktijk Mind' & Circuit == 'Volwassenen']
  res5[, Naam_zorgaanbieder := 'Ploegmakers']
  res5[, Groep := 'Psychologenpraktijk Mind']
  res4[, Aanmeldwachttijd := '6']
  
  res6 <- mq[Opmerking == "Smit 7 weken; Ploegmakers 6 wkn'; Kokhuis 8 wkn" & Naam_zorgaanbieder == 'Psychologenpraktijk Mind' & Circuit == 'Volwassenen']
  res6[, Naam_zorgaanbieder := 'Kokhuis']
  res6[, Groep := 'Psychologenpraktijk Mind']
  res4[, Aanmeldwachttijd := '8']
  
  mq <- mq[Naam_zorgaanbieder != 'Psychologenpraktijk Mind']
  mq <- rbindlist(list(mq, res1, res2, res3, res4, res5, res6))
  
  res <- mq[Opmerking == 'Voor bepaalde hulpvragen en therapietrajecten geldt helaas een tijdelijke aanmeldstop. Neemt u voor meer informatie contact op met het secretariaat.']
  res[, Aanmeldwachttijd := 'Stop']
  
  mq <- rbindlist(list(mq, res))
  
  res <- mq[Opmerking == 'Voor hoofddiagnose Autisme is in de Volwassenzorg de wachttijd momenteel vier weken langer dan hierboven aangegeven']
  res[, Diagnosegroep_letterlijk := 'Autisme']
  res[, Hoofddiagnosegroep := 'Pervasieve stoornissen']
  res[, Aanmeldwachttijd := as.character(as.numeric(as.character(Aanmeldwachttijd)) + 4)]
  
  mq <- rbindlist(list(mq, res))
  
  # Max: Check overige opmerkingen!
  mq[Aanmeldwachttijd != 'Stop' & grepl('stop', tolower(Opmerking)), .N, Opmerking]
  
  mq[, PAT_STOP := ifelse(Aanmeldwachttijd == 'Stop', 1, 0)]
  
  mq[, Aanmeldwachttijd := as.numeric(gsub(',', '.', Aanmeldwachttijd, fixed = T))]
  mq[Aanmeldwachttijd_letterlijk %in% c('0 weken', 'Geen wachtlijst', 'Geen', '0 dagen', 'Geen wachttijd', 'N.v.t.',
                                        'geen wachttijd', 'geen', 'Geen wachttijden', 'Direct hulp', 'Meteen', '0 weken '),
     Aanmeldwachttijd := 0]
  mq[Aanmeldwachttijd == 1 & Aanmeld.behandelwachttijd_letterlijk %in% c('Er is geen wachtlijst', 'Wij kennen geen wachtlijst', 
                                                                         'Geen wachtlijst ', 'Geen wachtlijsten', '0 dagen', 
                                                                         'Geen wachttijd', 'Geen', 'Geen wachttijden',
                                                                         'Geen wachtlijst', '0 weken'),
     Aanmeldwachttijd := 0]
  
  mq[Aanmeldwachttijd_letterlijk %in% c('0,14 (1 werkdag)', '<1 dag', '1 dag'), Aanmeldwachttijd := round(1 / 7, 1)]
  mq[Aanmeldwachttijd_letterlijk %in% c('2 dagen', '2 werkdagen') |
       Aanmeld.behandelwachttijd_letterlijk %in% c('2 dagen'), Aanmeldwachttijd := round(2 / 7, 1)]
  mq[Aanmeldwachttijd_letterlijk %in% c('3  dagen', 'Binnen 3 werkdagen'), Aanmeldwachttijd := round(3 / 7, 1)]
  mq[Aanmeldwachttijd_letterlijk %in% c('4 werkdagen'), Aanmeldwachttijd := round(4 / 7, 1)]
  mq[Aanmeldwachttijd_letterlijk %in% c('Maximaal 5 dagen', '5 kalenderdagen'), Aanmeldwachttijd := round(5 / 7, 1)]
  mq[Aanmeldwachttijd_letterlijk %in% c('6 dagen', '6 kalenderdagen'), Aanmeldwachttijd := round(6 / 7, 1)]
  mq[Opmerking %in% c('Geen wachttijden', 'Geen wachtlijst', 'Op dit moment zijn er geen wachtlijsten.', 
                        'Geen wachtlijsten', 'Geen of korte wachtlijst'), Aanmeldwachttijd := 0]
  
  # Max: Check wachttijden van 1 week!
  mq[Aanmeldwachttijd == 1, .N, .(Aanmeldwachttijd_letterlijk, Aanmeld.behandelwachttijd_letterlijk)]
  
  # Specifieke correcties aanmeldwachttijd
  mq[Opmerking == 'Volw en chr op dit moment niet in Barendrecht mogelijk' & Circuit == 'Jeugd' & 
         Datum_actualisatie_website == '13-11-2017', Aanmeldwachttijd := 4]
  mq[Opmerking %in% c('Leven & Zorg GGZ kent geen wachttijden en we kunnen cli?nten meteen helpen.'),
       Aanmeldwachttijd := 0]
  
  mq[(Naam_zorgaanbieder == 'PAAZ Tergooi ziekenhuizen' & Datum_actualisatie_website == '15-7-2017') | 
         (Naam_zorgaanbieder == 'PAAZ Elisabeth-TweeSteden Ziekenhuis' & Datum_actualisatie_website == '1-8-2017'), 
       Aanmeldwachttijd := Aanmeldwachttijd / 7]
  
  # mq[Naam_zorgaanbieder == 'Peters Psychotherapie' & Type_zorg == 'SGGZ', Aanmeldwachttijd_letterlijk := '8 - 12 weken']
  
  mq[, HEEFT_AANMELD := ifelse(is.na(Aanmeldwachttijd), 0, 1)]
  
  # Behandelwachttijd
  mq[Behandelwachttijd == 'Stop', PAT_STOP := 1]
  mq[, Behandelwachttijd := as.numeric(gsub(',', '.', Behandelwachttijd, fixed = T))]
  mq[Behandelwachttijd_letterlijk %in% c('0 weken', 'Geen wachttijd', 'Geen', 'Start Direct', 'Direct na intake',
                                         'Start direct na intake', 'Behandeling start direct', 'Aansluitend',
                                         '0  weken', 'Direct na onderzoek', '0 dagen', 'N.v.t.',
                                         'Behandeling start direct na intake.', 'Behandeling sluit aan op intake',
                                         'Behandeling start direct na intake. ', 'In principe geen wachttijd',
                                         'Behandeling direct aansluitend', 'Direct gestart', 'Geen wachttijden',
                                         'In principe wordt na de intake en het teamoverleg direct gestart met de behandeling.',
                                         'Direct', 'Start direct', 'Gevolgd door behandeling', 'Start aansluitend',
                                         'Behandeling start aansluitend', 'Geen behandelwachttijd', 'Geen wachtlijst',
                                         'Er is geen behandelingswachttijd', 'Gelijk', ' 0 weken', '0 week',
                                         'Start Direct na intake', 'Behandeling Direct aansluitend',
                                         'Behandeling start Direct na intake', 'Directe start na intake',
                                         'In principe Geen wachttijd, wel afhankelijk hulpvraag en behandeladvies',
                                         'Gevolgd door een behandeling'), 
       Behandelwachttijd := 0]
  mq[Behandelwachttijd_letterlijk %in% c('0-1 wwek'), Behandelwachttijd := 0.5]
  mq[Behandelwachttijd_letterlijk %in% c('5 dagen'), Behandelwachttijd := round(5 / 7, 1)]
  mq[Behandelwachttijd_letterlijk %in% c('6 dagen'), Behandelwachttijd := round(6 / 7, 1)]
  mq[Opmerking %in% c('Geen wachttijden', 'Geen wachtlijst', 'Op dit moment zijn er geen wachtlijsten.', 
                        'Geen wachtlijsten', 'Geen of korte wachtlijst'), Behandelwachttijd := 0]
  
  # Max: Check wachttijden van 1 week!
  mq[Behandelwachttijd == 1, .N, .(Behandelwachttijd_letterlijk, Aanmeld.behandelwachttijd_letterlijk)]
  
  mq[HEEFT_AANMELD == 0, Behandelwachttijd := NA] # Deze ook een keer uitzetten, kijken wat het effect is!
  
  mq[, HEEFT_BEHANDEL := ifelse(is.na(Behandelwachttijd), 0, 1)]
  
  # Data
  mq[, Datum_actualisatie_website := as.Date(mq$Datum_actualisatie_website, format = '%d-%m-%Y')]
  mq[, Peildatum := as.Date(mq$Peildatum, format = '%d-%m-%Y')]
  
  mq[Naam_zorgaanbieder == 'Voorzet Behandeling' & Peildatum == as.Date('2017-11-17'), Datum_actualisatie_website := as.Date('2017-08-01')]
  mq[Naam_zorgaanbieder == 'Solinge Coaching & Therapie ' & Peildatum == as.Date('2018-02-15') & grepl('vakantie-periode', Opmerking), Datum_actualisatie_website := as.Date('2017-07-01')]

  # Leeftijd / circuit
  mq[grepl('13 t/m 17', tolower(Diagnosegroep_letterlijk)), Circuit := 'Jeugd']
  mq[grepl('16 & 17-jarigen', tolower(Diagnosegroep_letterlijk)), Circuit := 'Jeugd']
  mq[Naam_zorgaanbieder == 'Horizon Jeugdzorg en Speciaal Onderwijs', Circuit := 'Jeugd']
  mq[Naam_zorgaanbieder == 'Kenter Jeugdhulp', Circuit := 'Jeugd']
  mq[Vestigingslocatie == 'Overschiestraat Jeugd/Assist', Circuit := 'Adolescenten']
  mq <- mq[!(Diagnosegroep_letterlijk == 'Angst en depressie, Somatisch gerelateerde klachten en 0 tot 6-jarigen' & Circuit == 'Adolescenten')]
  
  mq[Diagnosegroep_letterlijk %in% c('Specialistische Verslavingsbehandeling Jeugd', 
                                     'Verslavingspsychiatrie jeugd'),
     Circuit := 'Jeugd']
  
  res <- mq[Diagnosegroep_letterlijk %in% c('Semi acute psychiatrie 12 t/m 23 jaar')]
  res[, Circuit := 'Adolescenten']
  mq <- rbind(mq, res)
  
  # Max: Check leeftijd in diagnoses / zorgaanbiedernamen!
  mq[Circuit != 'Jeugd' & grepl('jeugd', tolower(Diagnosegroep_letterlijk)), .N, Diagnosegroep_letterlijk]
  mq[Circuit != 'Jeugd' & grepl('kinder', tolower(Diagnosegroep_letterlijk)), .N, Diagnosegroep_letterlijk]
  mq[Circuit != 'Jeugd' & grepl('jeugd', tolower(Naam_zorgaanbieder)), .N, Naam_zorgaanbieder]
  mq[Circuit != 'Jeugd' & grepl('kinder', tolower(Naam_zorgaanbieder)), .N, Naam_zorgaanbieder]
  mq[Circuit != 'Jeugd' & grepl('[0-9]', Diagnosegroep_letterlijk), .N, Diagnosegroep_letterlijk]
  
  mq[Diagnosegroep_letterlijk == 'Onder andere: Forensische zorg; intensieve behandeling/zorg; verslavingsreclassering', Circuit := 'Forensisch']
  mq[Naam_zorgaanbieder == 'Lentis, Poli Forensische Psychiatrie', Circuit := 'Forensisch']
  mq[Naam_zorgaanbieder == 'Palier, forensische en intensieve zorg', Circuit := 'Forensisch']
  mq[Naam_zorgaanbieder == 'Aventurijn, Wier', Circuit := 'Forensisch']
  
  # Max: Check fz in diagnoses / zorgaanbiedernamen!
  mq[Circuit != 'Forensisch' & grepl('forens', tolower(Diagnosegroep_letterlijk)), .N, Diagnosegroep_letterlijk]
  mq[Circuit != 'Forensisch' & grepl('straf', tolower(Diagnosegroep_letterlijk)), .N, Diagnosegroep_letterlijk]
  mq[Circuit != 'Forensisch' & grepl('forens', tolower(Naam_zorgaanbieder)), .N, Naam_zorgaanbieder]
  mq[Circuit != 'Forensisch' & grepl('straf', tolower(Naam_zorgaanbieder)), .N, Naam_zorgaanbieder]
  
  # Jeugd en forensische zorg eruit
  mq <- mq[!Circuit %in% c('Jeugd', 'Forensisch')]
  mq[, HEEFT_CIRCUIT := ifelse(Circuit %in% c("Adolescenten", "Ouderen", "Verslavingszorg", "Volwassenen"), 1, 0)]
  mq[HEEFT_AANMELD == 1 & HEEFT_CIRCUIT == 0, Circuit := "Volwassenen"]
  
  mq[, HEEFT_TYPEGGZ := ifelse(Type_zorg %in% c('SGGZ', 'Combi', 'BGGZ'), 1, 0)]
  
  mq[HEEFT_TYPEGGZ == 0] # alle drie zonder website
  
  # Nietverzekerde zorg
  mq[grepl('relati', tolower(Diagnosegroep_letterlijk)) |
       grepl('arbe', tolower(Diagnosegroep_letterlijk)) |
       grepl('dagbehandeling', tolower(Diagnosegroep_letterlijk)) |
       grepl('aanpas', tolower(Diagnosegroep_letterlijk)) |
       grepl('zzp', tolower(Diagnosegroep_letterlijk)) |
       grepl('nachtbehandeling', tolower(Diagnosegroep_letterlijk)) |
       grepl('verpl', tolower(Diagnosegroep_letterlijk)) |
       grepl('verz', tolower(Diagnosegroep_letterlijk)) |
       grepl('seksuo', tolower(Diagnosegroep_letterlijk)) |
       grepl('zwang', tolower(Diagnosegroep_letterlijk)) |
       grepl('hecht', tolower(Diagnosegroep_letterlijk)) |
       grepl('slaap', tolower(Diagnosegroep_letterlijk)) |
       grepl('pddb', tolower(Diagnosegroep_letterlijk)) |
       Diagnosegroep_letterlijk %in% c('Aandacht & Werkhoudingstraining', 'Werkgerelateerde problematiek',
                                       'Relatie, seksuele en werk problematiek', 'Studiebegeleiding',
                                       'Zorg voor clienten beschermd wonen', 'Misofonie', 'Intelligentiebepaling',
                                       'Rouw en verlies en problemen rondom vruchtbaarheid, zwangerschap en moederschap',
                                       'Dyslexie-onderzoek', 'Onderzoek naar enkel leerproblemen',
                                       'CogMed', 'Vrienden voor het leven (sociale vaardigheid)',
                                       'Begeleiding van mantelzorgers van verslaafde personen',
                                       'Mindfulness'), 
     Type_zorg := 'Niet Zvw']
  
  mq[Naam_zorgaanbieder %in% c('Cordaan', 'Kwintes', 'Vitalis WoonZorg Groep', 'CuraXL', 'Ministerie van Defensie , Militaire Geestelijke Gezondheidszorg'), 
     Type_zorg := 'Niet Zvw']
  
  mq[grepl('oggz', tolower(Vestigingslocatie)), Type_zorg := 'Niet Zvw']
  
  # Max: Check Zvw-zorg in diagnosegroepen!
  mq[Type_zorg != 'Niet Zvw', .N, .(Diagnosegroep_letterlijk)][1:100]
  
  mq <- mq[Type_zorg != 'Niet Zvw']
  
  # Diagnosegroepen
  
  mq[Naam_zorgaanbieder == 'Altrecht Stemmingstoornissen', Hoofddiagnosegroep := 'Depressieve stoornissen']
  mq[Naam_zorgaanbieder == 'Psychosomatiek Eikenboom', Hoofddiagnosegroep := 'Somatoforme stoornissen']
  mq[Naam_zorgaanbieder == 'HSK Groep' & Diagnosegroep_letterlijk == 'Expertisecentrum Tics', Hoofddiagnosegroep := 'Overige stoornissen']
  mq[Naam_zorgaanbieder == 'PsyQ' & Diagnosegroep_letterlijk == 'Somatiek en Psyche', Hoofddiagnosegroep := 'Overige stoornissen']
  mq[Naam_zorgaanbieder == 'PsyQ' & Diagnosegroep_letterlijk == 'Somatiek & Psyche', Hoofddiagnosegroep := 'Overige stoornissen']
  mq[Naam_zorgaanbieder == 'MET ggz' & Diagnosegroep_letterlijk == 'Somatiek & Psyche', Hoofddiagnosegroep := 'Overige stoornissen']
  mq[Naam_zorgaanbieder == 'Lievegoed' & Diagnosegroep_letterlijk == 'EMDR', Hoofddiagnosegroep := 'Angststoornissen']
  mq[Naam_zorgaanbieder == 'Psychotherapeutisch Centrum De Viersprong', Hoofddiagnosegroep := 'Persoonlijkheidsstoornissen']
  mq[Naam_zorgaanbieder == 'St. Antonius Ziekenhuis' & Diagnosegroep_letterlijk == 'Polikliniek psychiatrie en psychologie: Consult met arts bij seksuele stoornissen', Hoofddiagnosegroep := 'Overige stoornissen']
  mq[Naam_zorgaanbieder == "Centrum '45", Hoofddiagnosegroep := 'Angststoornissen']
  mq[Naam_zorgaanbieder == 'Pro Persona' & Diagnosegroep_letterlijk == 'Andere aandoeningen en problemen die een reden voor zorg kunnen zijn', Hoofddiagnosegroep := 'Overige stoornissen']
  mq[Naam_zorgaanbieder == 'De Vruchtenburg', Hoofddiagnosegroep := 'Overige stoornissen']
  mq[Naam_zorgaanbieder == 'FortaGroep' & Diagnosegroep_letterlijk == 'Alleen klachten bij kanker', Hoofddiagnosegroep := 'Overige stoornissen']
  mq[Naam_zorgaanbieder == 'Het Behouden Huys', Hoofddiagnosegroep := 'Overige stoornissen']
  mq[Naam_zorgaanbieder == 'Psy-Zo!', Hoofddiagnosegroep := 'Angststoornissen']
  mq[aanbieder == 'Voorzet Behandeling', Hoofddiagnosegroep := 'Pervasieve stoornissen']
  
  res1 <- mq[Naam_zorgaanbieder == 'Ready for Change']
  res1[, Hoofddiagnosegroep := 'Alcohol gebonden stoornissen']
  res2 <- mq[Naam_zorgaanbieder == 'Ready for Change']
  res2[, Hoofddiagnosegroep := 'Overige aan middel gebonden stoornissen']
  
  mq <- mq[Naam_zorgaanbieder != 'Ready for Change']
  mq <- rbindlist(list(mq, res1, res2))
  
  res1 <- mq[Naam_zorgaanbieder == 'Brijder']
  res1[, Hoofddiagnosegroep := 'Alcohol gebonden stoornissen']
  res2 <- mq[Naam_zorgaanbieder == 'Brijder']
  res2[, Hoofddiagnosegroep := 'Overige aan middel gebonden stoornissen']
  
  mq <- mq[Naam_zorgaanbieder != 'Brijder']
  mq <- rbindlist(list(mq, res1, res2))
  
  res1 <- mq[Naam_zorgaanbieder == 'Novadic-Kentron, GGZ Jeugd']
  res1[, Hoofddiagnosegroep := 'Alcohol gebonden stoornissen']
  res2 <- mq[Naam_zorgaanbieder == 'Novadic-Kentron, GGZ Jeugd']
  res2[, Hoofddiagnosegroep := 'Overige aan middel gebonden stoornissen']
  
  mq <- mq[Naam_zorgaanbieder != 'Novadic-Kentron, GGZ Jeugd']
  mq <- rbindlist(list(mq, res1, res2))
  
  res1 <- mq[Naam_zorgaanbieder == 'Het Leger des Heils GGZ en Verslavingszorg']
  res1[, Hoofddiagnosegroep := 'Alcohol gebonden stoornissen']
  res2 <- mq[Naam_zorgaanbieder == 'Het Leger des Heils GGZ en Verslavingszorg']
  res2[, Hoofddiagnosegroep := 'Overige aan middel gebonden stoornissen']
  
  mq <- mq[Naam_zorgaanbieder != 'Het Leger des Heils GGZ en Verslavingszorg']
  mq <- rbindlist(list(mq, res1, res2))
  
  mq[Diagnosegroep_letterlijk == 'Dag/deeltijdbehandeling; Eetstoornissen t/m 23 jaar', Hoofddiagnosegroep := 'Eetstoornissen']
  mq[Diagnosegroep_letterlijk == 'Eetverslaving', Hoofddiagnosegroep := 'Eetstoornissen']
  mq[Diagnosegroep_letterlijk == 'Pervasieve ontwikkelingsstoornissen', Hoofddiagnosegroep := 'Pervasieve stoornissen']
  
  # Max: check hoofddiagnoses o.b.v. aanbieders en diagnosegroepen
  mq[Hoofddiagnosegroep == 'Geen onderscheid naar hoofddiagnosegroepen' & Diagnosegroep_letterlijk != '' & Type_zorg == 'SGGZ', 
     .N, .(Naam_zorgaanbieder, Diagnosegroep_letterlijk)][201:300]
  mq[grepl('eet|boul|anorex', tolower(Diagnosegroep_letterlijk)) & Hoofddiagnosegroep != 'Eetstoornissen', .N, .(Naam_zorgaanbieder, Diagnosegroep_letterlijk, Hoofddiagnosegroep)]
  mq[grepl('versl', tolower(Diagnosegroep_letterlijk)) & !Hoofddiagnosegroep %in% c('Alcohol gebonden stoornissen', 'Overige aan middel gebonden stoornissen'), .N, .(Naam_zorgaanbieder, Diagnosegroep_letterlijk, Hoofddiagnosegroep)]
  
  # Specifieke patienten
  mq[grepl('diagnost', tolower(Diagnosegroep_letterlijk)) & !grepl('behand', tolower(Diagnosegroep_letterlijk)), .N, .(Naam_zorgaanbieder, Diagnosegroep_letterlijk, Hoofddiagnosegroep)]
  mq[grepl('diagnost', tolower(Diagnosegroep_letterlijk)) & !grepl('behand', tolower(Diagnosegroep_letterlijk)), 
     Type_zorg := 'Diagnostiek']
  
  mq[grepl('crisis', tolower(Diagnosegroep_letterlijk)) | grepl('acuut', tolower(Diagnosegroep_letterlijk)) | 
         grepl('acute', tolower(Diagnosegroep_letterlijk)) | grepl('spoed', tolower(Diagnosegroep_letterlijk)), 
       Type_zorg := 'Acute psychiatrie']
  mq[grepl('acute', tolower(Vestigingslocatie)), Type_zorg := 'Acute psychiatrie']
  mq[grepl('crisis', tolower(Vestigingslocatie)), Type_zorg := 'Acute psychiatrie']
  
  mq[grepl('verstandel', tolower(Diagnosegroep_letterlijk)) | grepl('lvb', tolower(Diagnosegroep_letterlijk)) | 
         grepl('lvg', tolower(Diagnosegroep_letterlijk)), Type_zorg := 'Verstandelijke beperking']
  
  mq[grepl('dov', tolower(Diagnosegroep_letterlijk)), Type_zorg := 'Doven en slechthorenden']
  mq[grepl('dov', tolower(Naam_zorgaanbieder)), Type_zorg := 'Doven en slechthorenden']
  
  mq[, LOCATIE_ID := as.integer(as.factor(paste(Groep, Naam_zorgaanbieder, Vestigingslocatie, sep = '_')))]
  
  maxDatum <- max(mq$Datum_actualisatie_website, na.rm = T)
  
  mq[, ACTUEEL := ifelse(Datum_actualisatie_website >= as.Date(paste0('01-', ifelse(month(maxDatum) == 1, '11', ifelse(month(maxDatum) == 2, '12', sprintf("%02d", month(maxDatum) - 2))), '-', ifelse(month(maxDatum) < 3, year(maxDatum) - 1, year(maxDatum))), format = '%d-%m-%Y'), 1, 0)]
  mq[, ACTUEEL := ifelse(Datum_actualisatie_website >= as.Date('01-01-2018', format = '%d-%m-%Y'), 1, 0)]
  mq[is.na(Datum_actualisatie_website), ACTUEEL := 1]
  
  mq[Naam_zorgaanbieder == 'at. zorg' & Peildatum == as.Date('07-03-2018', format = '%d-%m-%Y') & Opmerking == 'Stop bij ADHD/ASS tot februari 2018',
     ACTUEEL := 0]
  
  # Handmatige aanpassingen mbt patientenstop of onderscheid in verzekeraars
  mq[Zorgverzekeraar_toelichting %in% 
         c('Het budget dat ons voor 2017 is toegekend, is niet toereikend om u een behandeling te kunnen aanbieden.',
           'Het budget dat CZ heeft toegekend is niet toereikend en er is geen financiele ruimte om u uit te nodigenom een behandeling te starten'),
       PAT_STOP := 1]
  mq[Zorgverzekeraar_toelichting == 'EDT Maastricht heeft zich de afgelopen maanden mogen verheugen in een snel groeiend aantal aanmeldingen. Het slechte nieuws is dat voor mensen die bij CZ (ook Ohra en Delta Lloyd) verzekerd zijn, thans helaas een intake-stop geldt tot in ieder geval 1 september 2017 en zeer waarschijnlijk voor de rest van het jaar.' & Zorgverzekeraar == 'CZ',
       PAT_STOP := 1]
  mq[Opmerking == 'Volw en chr op dit moment niet in Barendrecht mogelijk' & Circuit == 'Volwassenen',
       PAT_STOP := 1]
  
  res1 <- mq[Opmerking == 'Clientstop ZV onder koepel VGZ, Menzis en CZ']
  res1[, Zorgverzekeraar := 'VGZ']
  res1[, PAT_STOP := 1]
  res1[, HEEFT_AANMELD := 0]
  res1[, HEEFT_BEHANDEL := 0]
  res1[, Aanmeldwachttijd := NA]
  res1[, Behandelwachttijd := NA]
  
  res2 <- mq[Opmerking == 'Clientstop ZV onder koepel VGZ, Menzis en CZ']
  res2[, Zorgverzekeraar := 'Menzis']
  res2[, PAT_STOP := 1]
  res2[, HEEFT_AANMELD := 0]
  res2[, HEEFT_BEHANDEL := 0]
  res2[, Aanmeldwachttijd := NA]
  res2[, Behandelwachttijd := NA]
  
  res3 <- mq[Opmerking == 'Clientstop ZV onder koepel VGZ, Menzis en CZ']
  res3[, Zorgverzekeraar := 'CZ']
  res3[, PAT_STOP := 1]
  res3[, HEEFT_AANMELD := 0]
  res3[, HEEFT_BEHANDEL := 0]
  res3[, Aanmeldwachttijd := NA]
  res3[, Behandelwachttijd := NA]
  
  mq <- rbindlist(list(mq, res1, res2, res3))
  
  res <- mq[Zorgverzekeraar_toelichting == 'Contracten: CZ, VGZ, Multizorg voor Zilveren Kruis (Achmea) geldt een tijdelijke opnamestop. Geen contracten: Menzis, DSW, Stadholland en Friesland.']
  res[, Zorgverzekeraar := 'Zilveren Kruis']
  res[, PAT_STOP := 1]
  res[, HEEFT_AANMELD := 0]
  res[, HEEFT_BEHANDEL := 0]
  res[, Aanmeldwachttijd := NA]
  res[, Behandelwachttijd := NA]
  
  mq <- rbind(mq, res)
  
  res <- mq[Zorgverzekeraar_toelichting == 'Voor de zorgverzekeraar CZ is dit plafond voor het jaar 2017 bereikt en dat betekent dat u zich als verxekerde bij CZ niet meer kunt aanmelden, ook al heeft u een verwijsbrief gekregen. Pas met ingang van 2018 kan PION weer clienten van CZ in behandeling nemen.']
  res[, Zorgverzekeraar := '']
  res[, PAT_STOP := 0]
  res[, HEEFT_AANMELD := 1]
  res[, HEEFT_BEHANDEL := 1]
  res[, Aanmeldwachttijd := 0]
  res[, Behandelwachttijd := 0]
  
  mq <- rbind(mq, res)
  
  res <- mq[Zorgverzekeraar_toelichting == 'Behandelt geen Menzis verzekerden']
  res[, Zorgverzekeraar := 'Menzis']
  res[, PAT_STOP := 1]
  res[, HEEFT_AANMELD := 0]
  res[, HEEFT_BEHANDEL := 0]
  res[, Aanmeldwachttijd := NA]
  res[, Behandelwachttijd := NA]
  
  mq <- rbind(mq, res)
  
  mq[Zorgverzekeraar_toelichting == 'Voor Achmea SGGZ kunnen wij helaas geen nieuwe clienten aannemen, omdat het budget op is. Indien u verzekerd bent bij Menzis of CZ verdient het, gezien de krappe budgetten, aanbeveling telefonisch contact met ons op te nemen om te bezien of uw wachttijd afwijkt' & PAT_STOP == 1,
       Zorgverzekeraar := 'Achmea']
  
  mq[, MAAKT_ONDERSCHEID := ifelse(LOCATIE_ID %in% mq[, .N, .(LOCATIE_ID, Zorgverzekeraar)][, .N, LOCATIE_ID][N > 1]$LOCATIE_ID,
                                     1, 0)]
  mq[Zorgverzekeraar_toelichting %in% 
        c('Afhankelijk van uw zorgverzekeraar', 
          'Wanneer clienten verzekerd zijn bij VGZ voor hun zorgverzekering kunnen de hierboven genoemde wachttijden afwijken wanneer we het prestatieplafond hebben bereikt. In dit geval worden clienten hiervan op de hoogte gesteld.',
          'Indien u verzekerd bent bij Menzis of CZ verdient het, gezien de krappe budgetten, aanbeveling telefonisch contact met ons op te nemen om te bezien of uw wachttijd afwijkt',
          'Indien u verzekerd bent bij VGZ, CZ, Menzis, Achmea of Friesland verdient hetm gezien de krappe budgetten, aanbeveling telefonisch contact met ons op te nemen om te bezien of u nog op de wachtlijst geplaats kunt worden.',
          'Door het beperkte budget van CZ, OHRA en Delta Lloyd zijn er alleen nog behandelmogelijkheden indien u een kortdurende klachtgerichte behandelvraag heeft (b-ggz), ?f als u een restitutiepolis heeft.',
          'Voor CZ verzekerden geldt dat het toegekende budget voor 2017 beperkt is. Het is echt nog wel mogelijk voor CZ verzekerden om bij onze praktijk in zorg te komen.',
          'Kan afhankelijk zijn van zorgverzekeraar ',
          'De wachttijden zijn soms afhankelijk van de zorgverzekeraar waar u verzekerd bent. Alleen voor verzekerden van Zilveren Kruis (en de daaraan gerelateerde labels) die niet woonachtig zijn in de provincies Friesland, Groningen of Drenthe is geen klinische opname mogelijk.',
          'De wachttijden zijn het kortst voor verzekerden van: Zorg en Zekerheid, PNO, ENO, ONVZ, ASR, DSW, en alle labels van VGZ en Zilveren Kruis.',
          'Voor enkele verzekeraars geldt een beperking van het aantal te behandelen clienten.',
          'Wachtlijst onder andere afhankelijk van de zorgverzekering en periode van aanmelding',
          'Cliënten van verzekeraar Menzis geldt een wachttijd tot januari 2018 voor intake. Voor cliënten van verzekeraars VGZ en Achmea gelden geen wachttijden.',
          'Vanwege de verplichte spreiding van het budget over het jaar kan het zijn dat de wachttijd per verzekering verschilt.',
          'De wachttijd per verzekeraar kan verschillen',
          'Wanneer cliënten verzekerd zijn bij de VGZ voor hun zorgverzekering kunnen de hierboven genoemde wachttijden afwijken wanneer we het prestatieplafond hebben bereikt.',
          'De aanmeldingswachttijd is bij Prisma Praktijk ook afhankelijk van waar de cliënt is verzekerd.',
          'Er is een wachttijd voor de Specialistische GGZ voor personen die bij de VGZ-groep verzekerd zijn.',
          'De precieze wachttijden zijn echter ook afhankelijk van de zorgverzekeraar.'),
       MAAKT_ONDERSCHEID := 1]
  
  mq[grepl('aantal behandelingen dat uw zorgverzekeraar bij ons heeft ingekocht bereikt.', Zorgverzekeraar_toelichting) |
       grepl('aantalHeeft u een verzekering binnen de koepelorganisatie CZ', Zorgverzekeraar_toelichting) | 
       grepl('gezien de krappe budgetten', Zorgverzekeraar_toelichting) | 
         grepl('CZ groep (CZ, Delta Lloyd en Ohra) langere wachttijd', Zorgverzekeraar_toelichting, fixed = T),
       MAAKT_ONDERSCHEID := 1]  
  
  mq[Zorgverzekeraar %in% c('Achmea', 'Zilveren Kruis Achmea', 'Zilveren kruis'), Zorgverzekeraar := 'Zilveren Kruis']
  mq[Zorgverzekeraar %in% c('CZ '), Zorgverzekeraar := 'CZ']
  mq[Zorgverzekeraar_overig %in% c('Cooperatie VGZ'), Zorgverzekeraar := 'VGZ']
  mq[!Zorgverzekeraar %in% c('Zilveren Kruis', 'CZ', 'VGZ', 'Menzis', 'Overig'), Zorgverzekeraar_overig := as.character(Zorgverzekeraar)]
  mq[Zorgverzekeraar != '' | Zorgverzekeraar_overig != '', Zorgverzekeraar := ifelse(Zorgverzekeraar %in% c('Zilveren Kruis', 'CZ', 'VGZ', 'Menzis'), as.character(Zorgverzekeraar), 'Overig')]
  
  # Klinisch of ambulant
  mq[, AMBULANT := 0]
  mq[Type_aanbieder == 'Instelling' & grepl('polik', tolower(Diagnosegroep_letterlijk)), AMBULANT := 1]
  mq[Type_aanbieder == 'Instelling' & grepl('ambul', tolower(Diagnosegroep_letterlijk)), AMBULANT := 1]
  mq[Type_aanbieder == 'Instelling' & grepl('ambul', tolower(Naam_zorgaanbieder)), AMBULANT := 1]
  mq[Type_aanbieder == 'Instelling' & grepl('ambul', tolower(Vestigingslocatie)), AMBULANT := 1]
  mq[Type_aanbieder == 'Instelling' & grepl('polik', tolower(URL_naar_wachttijden)), AMBULANT := 1]
  mq[Naam_zorgaanbieder == 'Sanctuary Kliniek', AMBULANT := 1]
  
  mq[, KLINISCH := 0]
  mq[AMBULANT == 0 & Type_aanbieder == 'Instelling' & grepl('klin', tolower(Diagnosegroep_letterlijk)), KLINISCH := 1]
  mq[AMBULANT == 0 & Type_aanbieder == 'Instelling' & grepl('verb', tolower(Diagnosegroep_letterlijk)), KLINISCH := 1]
  mq[AMBULANT == 0 & Type_aanbieder == 'Instelling' & grepl('opn', tolower(Diagnosegroep_letterlijk)), KLINISCH := 1]
  
  mq[HEEFT_AANMELD == 1 & HEEFT_BEHANDEL == 1, TotaleWachttijd := Aanmeldwachttijd + Behandelwachttijd]
  
  mq
}

voorbereidenWachttijdenGBGGZ <- function(mq) {
  
  res <- mq[HEEFT_AANMELD == 1 & ACTUEEL == 1 & Type_zorg == 'BGGZ', 
            .(Aanmeldtijd = mean(Aanmeldwachttijd),
              Behandeltijd = mean(Behandelwachttijd, na.rm = T),
              Totaletijd = mean(TotaleWachttijd, na.rm = T)), 
            .(aanbieder, Vestigingslocatie, LOCATIE_ID, Circuit, Type_aanbieder)]

  res2 <- res[,.(Aanmeldtijd = mean(Aanmeldtijd, na.rm = T),
                 Behandeltijd = mean(Behandeltijd, na.rm = T),
                 Totaletijd = mean(Totaletijd, na.rm = T),
                 AANT_OBS_AAN = .N,
                 AANT_OBS_TOT = sum(ifelse(is.na(Behandeltijd), 0, 1))), 
              .(aanbieder, Circuit, Type_aanbieder)]
  
  res3 <- res2[, .(AANT_OBS_AAN = sum(AANT_OBS_AAN),
                   AANT_OBS_TOT = sum(AANT_OBS_TOT),
                   Aanmeldtijd = round(mean(Aanmeldtijd)),
                   Behandeltijd = round(mean(Behandeltijd, na.rm = T)),
                   Totaletijd = round(mean(Totaletijd, na.rm = T))), Circuit]
  
  res3[, Aanmeldtijd := ifelse(AANT_OBS_AAN < 25, '< 25 locaties', as.character(format(Aanmeldtijd, decimal.mark = ',')))]
  res3[, Totaletijd := ifelse(AANT_OBS_TOT < 25, '< 25 locaties', as.character(format(Totaletijd, decimal.mark = ',')))]
  
  res4 <- res[Circuit %in% res3[AANT_OBS_AAN >= 25 & AANT_OBS_TOT >= 25]$Circuit, 
              .(AANT_LOC_BOVEN_NORM = nrow(.SD[Aanmeldtijd > 4,]),
                AANT_LOC = .N), Circuit]
  
  res4 <- res4[, PERC_BOVEN_NORM := round(AANT_LOC_BOVEN_NORM * 100 / AANT_LOC, 1)]
  
  list(locaties = res, aanbieders = res2, subgroepen = res3, treeknorm = res4)
}

voorbereidenWachttijdenGespecialiseerdeGGZAlgemeen <- function(mq) {
  
  res <- mq[HEEFT_AANMELD == 1 & ACTUEEL == 1 & Type_zorg == 'SGGZ', 
            .(Aanmeldtijd = mean(Aanmeldwachttijd),
              Behandeltijd = mean(Behandelwachttijd, na.rm = T),
              Totaletijd = mean(TotaleWachttijd, na.rm = T)), 
            .(aanbieder, Vestigingslocatie, LOCATIE_ID, Circuit, Type_aanbieder)]
  
  res2 <- res[,.(Aanmeldtijd = mean(Aanmeldtijd, na.rm = T),
                 Behandeltijd = mean(Behandeltijd, na.rm = T),
                 Totaletijd = mean(Totaletijd, na.rm = T),
                 AANT_OBS_AAN = .N,
                 AANT_OBS_TOT = sum(ifelse(is.na(Behandeltijd), 0, 1))), 
              .(aanbieder, Circuit, Type_aanbieder)]
  
  res3 <- res2[, .(AANT_OBS_AAN = sum(AANT_OBS_AAN),
                   AANT_OBS_TOT = sum(AANT_OBS_TOT),
                   Aanmeldtijd = round(mean(Aanmeldtijd)),
                   Behandeltijd = round(mean(Behandeltijd, na.rm = T)),
                   Totaletijd = round(mean(Totaletijd, na.rm = T))), 
               .(Circuit, Type_aanbieder)]
  
  res3[, Aanmeldtijd := ifelse(AANT_OBS_AAN < 25, '< 25 locaties', as.character(format(Aanmeldtijd, decimal.mark = ',')))]
  res3[, Totaletijd := ifelse(AANT_OBS_TOT < 25, '< 25 locaties', as.character(format(Totaletijd, decimal.mark = ',')))]
  
  res4 <- res[Circuit %in% res3[AANT_OBS_AAN >= 25 & AANT_OBS_TOT >= 25]$Circuit, 
              .(AANT_LOC_BOVEN_NORM = nrow(.SD[Aanmeldtijd > 4,]),
                AANT_LOC = .N), 
              .(Circuit, Type_aanbieder)]
  
  res4 <- res4[, PERC_BOVEN_NORM := round(AANT_LOC_BOVEN_NORM * 100 / AANT_LOC, 1)]
  
  list(locaties = res, aanbieders = res2, subgroepen = res3, treeknorm = res4)
}

voorbereidenWachttijdenInstellingenDiagnoses <- function(mq) {
  
  res <- mq[HEEFT_AANMELD == 1 & ACTUEEL == 1 & Type_zorg == 'SGGZ' & Type_aanbieder == 'Instelling', 
            .(Aanmeldtijd = mean(Aanmeldwachttijd),
              Behandeltijd = mean(Behandelwachttijd, na.rm = T),
              Totaletijd = mean(TotaleWachttijd, na.rm = T)), 
            .(aanbieder, Vestigingslocatie, LOCATIE_ID, Circuit, Type_aanbieder, Hoofddiagnosegroep)]
  
  res2 <- res[,.(Aanmeldtijd = mean(Aanmeldtijd),
                 Behandeltijd = mean(Behandeltijd, na.rm = T),
                 Totaletijd = mean(Totaletijd, na.rm = T),
                 AANT_OBS_AAN = .N,
                 AANT_OBS_TOT = sum(ifelse(is.na(Behandeltijd), 0, 1))), 
              .(aanbieder, Hoofddiagnosegroep)]
  
  res3 <- res2[, .(AANT_OBS_AAN = sum(AANT_OBS_AAN),
                   AANT_OBS_TOT = sum(AANT_OBS_TOT),
                   Aanmeldtijd = round(mean(Aanmeldtijd)),
                   Behandeltijd = round(mean(Behandeltijd, na.rm = T)),
                   Totaletijd = round(mean(Totaletijd, na.rm = T))),
               .(Hoofddiagnosegroep)]
  
  res3[, Aanmeldtijd := ifelse(AANT_OBS_AAN < 25, '< 25 locaties', as.character(format(Aanmeldtijd, decimal.mark = ',')))]
  res3[, Totaletijd := ifelse(AANT_OBS_TOT < 25, '< 25 locaties', as.character(format(Totaletijd, decimal.mark = ',')))]
  
  list(instellingen = res, aanbieders = res2, subgroepen = res3)
}

regio <- function(mq) {
  
  mq[, Gemeentecodes := as.integer(as.character(Gemeentecodes))]
  gemeente <- data.table(read.csv2('input\\Gemeenten alfabetisch 2017.csv'))
  zorgkantoor <- data.table(read.csv2('input\\awbzregiotabel.csv'))
  zorgkantoor <- zorgkantoor[, .N, .(Gemeentecode, Gemeente, Provincie, Zorgkantoorcode, Zorgkantoor, Rechtspersoon)]
  zorgkantoor[, Gemeente := trimws(Gemeente)]
  
  setkey(mq, Gemeentecodes)
  setkey(gemeente, Gemeentecode)
  setkey(zorgkantoor, Gemeentecode)
  
  mq <- gemeente[mq]
  mq <- mq[!is.na(Gemeentecode)]
  
  setkey(mq, Gemeentecode)
  mq <- zorgkantoor[mq]
  
  zorgkantoor[Gemeente == 'BERGEN LB', Gemeente := 'BERGEN (L.)']
  zorgkantoor[Gemeente == 'BERGEN NH', Gemeente := 'BERGEN (NH.)']
  zorgkantoor[Gemeente == 'S GRAVENHAGE', Gemeente := "'S-GRAVENHAGE"]
  zorgkantoor[Gemeente == 'S HERTOGENBOSCH', Gemeente := "'S-HERTOGENBOSCH"]
  zorgkantoor[Gemeente == 'HAARLEMMERLIEDE CA', Gemeente := "HAARLEMMERLIEDE EN SPAARNWOUDE"]
  zorgkantoor[Gemeente == 'KOLLUMERLAND CA', Gemeente := "KOLLUMERLAND EN NIEUWKRUISLAND"]
  zorgkantoor[Gemeente == 'NOORD BEVELAND', Gemeente := "NOORD-BEVELAND"]
  zorgkantoor[Gemeente == 'NUENEN CA', Gemeente := "NUENEN, GERWEN EN NEDERWETTEN"]
  zorgkantoor[Gemeente == 'SUDWEST-FRYSLAN', Gemeente := "SÚDWEST-FRYSLÂN"]
  
  mq[Gemeentenaam == 'Nissewaard', Zorgkantoor := 'ZUID-HOLLANDSE EILANDEN']
  mq[Gemeentenaam == 'Krimpenerwaard', Zorgkantoor := 'MIDDEN-HOLLAND']
  mq[Gemeentenaam == 'De Fryske Marren', Zorgkantoor := 'FRIESLAND']
  mq[Gemeentenaam == 'Gooise Meren', Zorgkantoor := "'T GOOI"]
  mq[Gemeentenaam == 'Berg en Dal', Zorgkantoor := 'NIJMEGEN']
  
  res.gb <- mq[HEEFT_BEHANDEL == 1 & ACTUEEL == 1 & Type_zorg == 'BGGZ', 
               .(Totaletijd = mean(TotaleWachttijd, na.rm = T)), 
               .(Naam_zorgaanbieder, Vestigingslocatie, LOCATIE_ID, Circuit, Type_zorg, Type_aanbieder,
                 Zorgkantoor)]
  
  res.gb2 <- res.gb[, .(Totaletijd = round(mean(Totaletijd, na.rm = T))), Zorgkantoor]
  
  res.g <- mq[HEEFT_BEHANDEL == 1 & ACTUEEL == 1 & Type_zorg == 'SGGZ', 
               .(Totaletijd = mean(TotaleWachttijd, na.rm = T)), 
               .(Naam_zorgaanbieder, Vestigingslocatie, LOCATIE_ID, Circuit, Type_zorg, Type_aanbieder,
                 Zorgkantoor)]
  
  res.g2 <- res.g[, .(Totaletijd = round(mean(Totaletijd, na.rm = T))), Zorgkantoor]
  
  list(tbl.g = res.g2, tbl.gb = res.gb2)
  
}