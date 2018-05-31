library(readr)

read_input_file <- function(infile) {
  return(read_csv(infile))
}

#****************************************************************************************************************************************************
#****************************************************************************************************************************************************
#*********************************STEP 1 - Data Cleanup**********************************************************************************************
#****************************************************************************************************************************************************
#****************************************************************************************************************************************************
#Store Mapping for data cleaning

clean_data <- function(Reed, from = NULL, to = NULL) {
  if(is.null(from)) {
    from = c(' AC ', ' ACP ', ' ADMIN ', ' AFB ', ' ALT ', ' ALTERATION ', ' ALTS ', ' AUTO ZONE ', ' BESTBUY ', ' BI LO ', 'BLVD', ' BP ', ' BSMT ', ' CADOT ', ' CARWASH ', ' CHP ', ' CNTR ', ' COMM ', ' COURT HOUSE ', ' COURT YARD ', ' CTR ', ' CTRL ', ' DEPT ', ' DIST ', ' DMV ', ' DOT ', ' ED ', ' ELEM ', ' ES ', ' EXP ', ' EXPAN ', ' FACIL ', ' FLDOT ', ' FY ', ' GAME STOP ', ' GRND ', ' GYM ', ' HMA ', ' HS ', ' HWY ', ' ILDOT ', ' IMPROV ', ' IMPROVEMENT ', ' IMPRV ', ' IMPRVMNT ', ' IMPRVMNTS ', ' IMPRVMT ', ' IMPRVS ', ' IMPV ', ' IMPVT ', ' INSTALL ', ' JR ', ' MAINT ', ' MKGS ', ' MKT ', ' MOD ', ' MS ', ' NYDOT ', ' PET SMART ', ' PH ', ' PHS ', ' PRO ', ' PROG ', ' PROJ ', ' PVMT ', ' QUALIFICATIONS ', ' RECONSTRUCT ', ' REHAB ', ' REHABILIATION ', ' REHABILITAION ', ' REHABILITAITON ', ' REHABILITAT ', ' REHABILITATE ', ' REHABILITATIONPHASE ', ' REHABILITATIONS ', ' REHABILITIATION ', ' REHABILTION ', ' REHABLITATION ', ' RELO ', ' RELOC ', ' RELOCATABLE ', ' RELOCATE ', ' RELOCATIONS ', ' REM ', ' REMEDIATION ', ' REMIDEL ', ' REMODELING ', ' REMODELS ', ' RENOV ', ' RENOVATE ', ' REPLACEMEN ', ' REPLCMNT ', ' RESURFACE ', ' RHMA ', ' RITE AID ', ' RM ', ' ROADBED ', ' SCH ', ' SCHL ', ' SCHLS ', ' SCHOO ', ' SEWERLINE ', ' SR ', ' STN ', ' STR ', ' SYS ', ' SYST ', ' SYSTM ', ' TI ', ' TXDOT ', ' USCG ', ' USD ', ' VAR ', ' WAL GREEN ', ' WALMART ', ' WASTEWATER ', ' WHSE ', ' WTP ', ' WWTP ', ' NWC ', ' SWC ', ' NEC ', ' SEC ', ' ALLEE ', ' ALLY ', ' ALY ', ' ANNEX ', ' ANNX ', ' ANX ', ' ARC ', ' AV ', ' AVE ', ' AVEN ', ' AVENU ', ' AVN ', ' AVNUE ', ' Apt ', ' BAYOO ', ' BCH ', ' BND ', ' BLF ', ' BLUF ', ' BOT ', ' BTM ', ' BOTTM ', ' BOUL ', ' BOULV ', ' BR ', ' BRNCH ', ' BRDGE ', ' BRG ', ' BRK ', ' BYP ', ' BYPA ', ' BYPAS ', ' BYPS ', ' Bldg ', ' CP ', ' CMP ', ' CANYN ', ' CNYN ', ' CPE ', ' CAUSWA ', ' CSWY ', ' CEN ', ' CENT ', ' CENTR ', ' CENTRE ', ' CNTER ', ' CIR ', ' CIRC ', ' CIRCL ', ' CRCL ', ' CRCLE ', ' CLF ', ' CLFS ', ' CLB ', ' COR ', ' CORS ', ' CRSE ', ' CT ', ' CTS ', ' CV ', ' CRK ', ' CRES ', ' CRSENT ', ' CRSNT ', ' CRSSNG ', ' XING ', ' DL ', ' DM ', ' DIV ', ' DV ', ' DVD ', ' DR ', ' DRIV ', ' DRV ', ' EST ', ' ESTS ', ' EXP ', ' EXPR ', ' EXPRESS ', ' EXPW ', ' EXPY ', ' EXT ', ' EXTN ', ' EXTNSN ', ' EXTS ', ' E ', ' FLS ', ' FRRY ', ' FRY ', ' FLD ', ' FLDS ', ' FLT ', ' FLTS ', ' FRD ', ' FORESTS ', ' FRST ', ' FORG ', ' FRG ', ' FRK ', ' FRKS ', ' FRT ', ' FT ', ' FREEWY ', ' FRWAY ', ' FRWY ', ' FWY ', ' GARDN ', ' GRDEN ', ' GRDN ', ' GDNS ', ' GRDNS ', ' GATEWY ', ' GATWAY ', ' GTWAY ', ' GTWY ', ' GLN ', ' GRN ', ' GROV ', ' GRV ', ' HARB ', ' HARBOR ', ' HARBR ', ' HBR ', ' HRBOR ', ' HARBORS ', ' HAVEN ', ' HVN ', ' HIGHWY ', ' HIWAY ', ' HIWY ', ' HWAY ', ' HL ', ' HLS ', ' HLLW ', ' HOLLOWS ', ' HOLW ', ' HOLWS ', ' INLT ', ' IS ', ' ISLND ', ' ISLNDS ', ' ISS ', ' ISLES ', ' JCT ', ' JCTION ', ' JCTN ', ' JUNCTN ', ' JUNCTON ', ' JCTNS ', ' JCTS ', ' KY ', ' KYS ', ' KNL ', ' KNOL ', ' KNLS ', ' LK ', ' LKS ', ' LNDG ', ' LNDNG ', ' LN ', ' LGT ', ' LF ', ' LCK ', ' LCKS ', ' LDG ', ' LDGE ', ' LODG ', ' LOOPS ', ' MNR ', ' MNRS ', ' MDW ', ' MDWS ', ' MEDOWS ', ' MISSN ', ' MSSN ', ' MNT ', ' MT ', ' MNTAIN ', ' MNTN ', ' MOUNTIN ', ' MTIN ', ' MTN ', ' MNTNS ', ' NCK ', ' N ', ' NE ', ' NW ', ' ORCH ', ' ORCHRD ', ' OVL ', ' PRK ', ' PARKWY ', ' PKWAY ', ' PKWY ', ' PKY ', ' PKWYS ', ' PATHS ', ' PIKES ', ' PNES ', ' PL ', ' PLN ', ' PLNS ', ' PLZ ', ' PLZA ', ' PT ', ' PTS ', ' PRT ', ' PRTS ', ' PR ', ' PRR ', ' PO ', ' RAD ', ' RADIEL ', ' RADL ', ' RANCHES ', ' RNCH ', ' RNCHS ', ' RPD ', ' RPDS ', ' RST ', ' RDG ', ' RDGE ', ' RIV ', ' RVR ', ' RIVR ', ' RD ', ' RDS ', ' SHL ', ' SHLS ', ' SHOAR ', ' SHR ', ' SHOARS ', ' SHRS ', ' SPG ', ' SPNG ', ' SPRNG ', ' SPGS ', ' SPNGS ', ' SPRNGS ', ' SQ ', ' SQR ', ' SQRE ', ' SQU ', ' SQRS ', ' STA ', ' STATN ', ' STRA ', ' STRAV ', ' STRAVEN ', ' STRAVN ', ' STRVN ', ' STRVNUE ', ' STREME ', ' STRM ', ' STRT ', ' ST ', ' STR ', ' SMT ', ' SUMIT ', ' SUMITT ', ' S ', ' SE ', ' SW ', ' STE ', ' TER ', ' TERR ', ' TRACES ', ' TRCE ', ' TRACKS ', ' TRAK ', ' TRK ', ' TRKS ', ' TRAFFICWAY ', ' TRAILS ', ' TRL ', ' TRLS ', ' TRLR ', ' TRLRS ', ' TUNEL ', ' TUNL ', ' TUNLS ', ' TUNNELS ', ' TUNNL ', ' TRNPK ', ' TURNPK ', ' UN ', ' VALLY ', ' VLLY ', ' VLY ', ' VLYS ', ' VDCT ', ' VIA ', ' VIADCT ', ' VW ', ' VWS ', ' VILL ', ' VILLAG ', ' VILLG ', ' VILLIAGE ', ' VLG ', ' VLGS ', ' VL ', ' VIS ', ' VIST ', ' VST ', ' VSTA ', ' WY ', ' WLS ', ' W ', ' st,', ' st.', ' av,', ' Bldg', 'Alts', ' N.', ' S.', ' E.', ' W.', ' rd.', ' ave.', ' N,', ' S,', ' E,', ' W,', ' Ave,', ' dr.', ' & ', '!', '"', '#', '$', '%', '&', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', '^', '_', '{', '|', '}', '~', ' reet ', ' nue ', 'and')
    to =   c(' ASPHALT CONCRETE ',  ' ASPHALT CONCRETE PAVEMENT ',  ' ADMINISTRATION ',  ' AIR FORCE BASE ',  ' ALTERATIONS ',  ' ALTERATIONS ',  ' ALTERATIONS ',  ' AUTOZONE ',  ' BEST BUY ',  ' BILO ',  ' BOULEVARD ',  ' Budget Project ',  ' BASEMENT ',  ' CA DEPARTMENT OF TRANSPORTATION ',  ' CAR WASH ',  ' CALIFORNIA HIGHWAY PATROL ',  ' CENTER ',  ' COMMUNICATION ',  ' COURTHOUSE ',  ' COURTYARD ',  ' CENTER ',  ' CENTRAL ',  ' DEPARTMENT ',  ' DISTRICT ',  ' DEPARTMENT OF MOTOR VEHICLES ',  ' DEPARTMENT OF TRANSPORTATION ',  ' EDUCATION ',  ' ELEMENTARY ',  ' ELEMENTARY SCHOOL ',  ' EXPANSION ',  ' EXPANSION ',  ' FACILITY ',  ' FL DEPARTMENT OF TRANSPORTATION ',  ' FISCAL YEAR ',  ' GAMESTOP ',  ' GROUND ',  ' GYMNASIUM ',  ' HOT MIX ASPHALT ',  ' HIGH SCHOOL ',  ' HIGHWAY ',  ' IL DEPARTMENT OF TRANSPORTATION ',  ' IMPROVEMENT ',  ' IMPROVEMENT ',  ' IMPROVEMENT ',  ' IMPROVEMENT ',  ' IMPROVEMENT ',  ' IMPROVEMENT ',  ' IMPROVEMENT ',  ' IMPROVEMENT ',  ' IMPROVEMENT ',  ' INSTALLATION ',  ' JUNIOR ',  ' MAINTENANCE ',  ' MARKINGS ',  ' MARKET ',  ' MODIFICATION ',  ' MIDDLE SCHOOL ',  ' NY DEPARTMENT OF TRANSPORTATION ',  ' PETSMART ',  ' PHASE ',  ' PHASE ',  ' PROFESSIONAL ',  ' PROGRAM ',  ' PROJECT ',  ' PAVEMENT ',  ' REMOVE ENTIRELY ',  ' RECONSTRUCTION ',  ' REHABILITATION ',  ' REHABILITATION ',  ' REHABILITATION ',  ' REHABILITATION ',  ' REHABILITATION ',  ' REHABILITATION ',  ' REHABILITATION ',  ' REHABILITATION ',  ' REHABILITATION ',  ' REHABILITATION ',  ' REHABILITATION ',  ' RELOCATION ',  ' RELOCATION ',  ' RELOCATION ',  ' RELOCATION ',  ' RELOCATION ',  ' REMODEL ',  ' REMODEL ',  ' REMODEL ',  ' REMODEL ',  ' REMODEL ',  ' RENOVATION ',  ' RENOVATION ',  ' REPLACEMENT ',  ' REPLACEMENT ',  ' RESURFACING ',  ' RUBBERIZED HOT MIX ASPHALT ',  ' RITEAID ',  ' ROOM ',  ' ROAD BED ',  ' SCHOOL ',  ' SCHOOL ',  ' SCHOOL ',  ' SCHOOL ',  ' SEWER LINE ',  ' STATE ROUTE ',  ' STATION ',  ' STRIPE ',  ' SYSTEM ',  ' SYSTEM ',  ' SYSTEM ',  ' TENANT IMPROVEMENT ',  ' TX DEPARTMENT OF TRANSPORTATION ',  ' UNITED STATES COAST GUARD ',  ' UNIFIED SCHOOL DISTRICT ',  ' VARIOUS ',  ' WALGREEN ',  ' WAL MART ',  ' WASTE WATER ',  ' WAREHOUSE ',  ' WATER TREATMENT PLANT ',  ' WASTE WATER TREATMENT PLANT ',  ' NORTHWEST CORNER ',  ' SOUTHWEST CORNER ',  ' NORTHEAST CORNER ',  ' SOUTHEAST CONER ',  ' Alley ',  ' Alley ',  ' Alley ',  ' Anex ',  ' Anex ',  ' Anex ',  ' Arcade ',  ' Avenue ',  ' Avenue ',  ' Avenue ',  ' Avenue ',  ' Avenue ',  ' Avenue ',  ' Apartment ',  ' Bayou ',  ' Beach ',  ' Bend ',  ' Bluff ',  ' Bluff ',  ' Bottom ',  ' Bottom ',  ' Bottom ',  ' Boulevard ',  ' Boulevard ',  ' Branch ',  ' Branch ',  ' Bridge ',  ' Bridge ',  ' Brook ',  ' Bypass ',  ' Bypass ',  ' Bypass ',  ' Bypass ',  ' Building ',  ' Camp ',  ' Camp ',  ' Canyon ',  ' Canyon ',  ' Cape ',  ' Causeway ',  ' Causeway ',  ' Center ',  ' Center ',  ' Center ',  ' Center ',  ' Center ',  ' Circle ',  ' Circle ',  ' Circle ',  ' Circle ',  ' Circle ',  ' Cliff ',  ' Cliffs ',  ' Club ',  ' Corner ',  ' Corners ',  ' Course ',  ' Court ',  ' Courts ',  ' Cove ',  ' Creek ',  ' Crescent ',  ' Crescent ',  ' Crescent ',  ' Crossing ',  ' Crossing ',  ' Dale ',  ' Dam ',  ' Divide ',  ' Divide ',  ' Divide ',  ' Drive ',  ' Drive ',  ' Drive ',  ' Estate ',  ' Estates ',  ' Expressway ',  ' Expressway ',  ' Expressway ',  ' Expressway ',  ' Expressway ',  ' Extension ',  ' Extension ',  ' Extension ',  ' Extensions ',  ' East ',  ' Falls ',  ' Ferry ',  ' Ferry ',  ' Field ',  ' Fields ',  ' Flat ',  ' Flats ',  ' Ford ',  ' Forest ',  ' Forest ',  ' Forge ',  ' Forge ',  ' Fork ',  ' Forks ',  ' Fort ',  ' Fort ',  ' Freeway ',  ' Freeway ',  ' Freeway ',  ' Freeway ',  ' Garden ',  ' Garden ',  ' Garden ',  ' Gardens ',  ' Gardens ',  ' Gateway ',  ' Gateway ',  ' Gateway ',  ' Gateway ',  ' Glen ',  ' Green ',  ' Grove ',  ' Grove ',  ' Harbour ',  ' Harbour ',  ' Harbour ',  ' Harbour ',  ' Harbour ',  ' Harbours ',  ' Heights ',  ' Heights ',  ' Highway ',  ' Highway ',  ' Highway ',  ' Highway ',  ' Hill ',  ' Hills ',  ' Hollow ',  ' Hollow ',  ' Hollow ',  ' Hollow ',  ' Inlet ',  ' Island ',  ' Island ',  ' Islands ',  ' Islands ',  ' Isle ',  ' Junction ',  ' Junction ',  ' Junction ',  ' Junction ',  ' Junction ',  ' Junctions ',  ' Junctions ',  ' Key ',  ' Keys ',  ' Knoll ',  ' Knoll ',  ' Knolls ',  ' Lake ',  ' Lakes ',  ' Landing ',  ' Landing ',  ' Lane ',  ' Light ',  ' Loaf ',  ' Lock ',  ' Locks ',  ' Lodge ',  ' Lodge ',  ' Lodge ',  ' Loop ',  ' Manor ',  ' Manors ',  ' Meadows ',  ' Meadows ',  ' Meadows ',  ' Mission ',  ' Mission ',  ' Mount ',  ' Mount ',  ' Mountain ',  ' Mountain ',  ' Mountain ',  ' Mountain ',  ' Mountain ',  ' Mountains ',  ' Neck ',  ' North ',  ' Northeast ',  ' Northwest ',  ' Orchard ',  ' Orchard ',  ' Oval ',  ' Park ',  ' Parkway ',  ' Parkway ',  ' Parkway ',  ' Parkway ',  ' Parkways ',  ' Path ',  ' Pike ',  ' Pines ',  ' Place ',  ' Plain ',  ' Plains ',  ' Plaza ',  ' Plaza ',  ' Point ',  ' Points ',  ' Port ',  ' Ports ',  ' Prairie ',  ' Prairie ',  ' Post Office ',  ' Radial ',  ' Radial ',  ' Radial ',  ' Ranch ',  ' Ranch ',  ' Ranch ',  ' Rapid ',  ' Rapids ',  ' Rest ',  ' Ridge ',  ' Ridge ',  ' River ',  ' River ',  ' River ',  ' Road ',  ' Roads ',  ' Shoal ',  ' Shoals ',  ' Shore ',  ' Shore ',  ' Shores ',  ' Shores ',  ' Spring ',  ' Spring ',  ' Spring ',  ' Springs ',  ' Springs ',  ' Springs ',  ' Square ',  ' Square ',  ' Square ',  ' Square ',  ' Squares ',  ' Station ',  ' Station ',  ' Stravenue ',  ' Stravenue ',  ' Stravenue ',  ' Stravenue ',  ' Stravenue ',  ' Stravenue ',  ' Stream ',  ' Stream ',  ' Street ',  ' Street ',  ' Street ',  ' Summit ',  ' Summit ',  ' Summit ',  ' South ',  ' Southeast ',  ' Southwest ',  ' Suite ',  ' Terrace ',  ' Terrace ',  ' Trace ',  ' Trace ',  ' Track ',  ' Track ',  ' Track ',  ' Track ',  ' Trafficeway ',  ' Trail ',  ' Trail ',  ' Trail ',  ' Trailer ',  ' Trailer ',  ' Tunnel ',  ' Tunnel ',  ' Tunnel ',  ' Tunnel ',  ' Tunnel ',  ' Turnpike ',  ' Turnpike ',  ' Union ',  ' Valley ',  ' Valley ',  ' Valley ',  ' Valleys ',  ' Viaduct ',  ' Viaduct ',  ' Viaduct ',  ' View ',  ' Views ',  ' Village ',  ' Village ',  ' Village ',  ' Village ',  ' Village ',  ' Villages ',  ' Ville ',  ' Vista ',  ' Vista ',  ' Vista ',  ' Vista ',  ' Way ',  ' Wells ',  ' West ',  ' street ',  ' street ',  ' avenue ',  ' Building ',  ' Alteration ',  ' North ',  ' South ',  ' East ',  ' West ',  ' road ',  ' avenue ',  ' North ',  ' South ',  ' East ',  ' West ',  ' avenue ',  ' drive ',  ' and ',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '',   '')
    to = tolower(to)
    from = tolower(from)
  }
  
  #Reed Clean Up
  Reed = data_complete_State[data_complete_State$RECORDTYPENAME__C == "Reed Project", ]
  Reed$MHC_PUBLISH_DATE_TXT__C_Formated <- as.Date(Reed$MHC_PUBLISH_DATE_TXT__C_Formated, "%m/%d/%Y")
  Reed = Reed[Reed$MHC_PUBLISH_DATE_TXT__C_Formated >= "2015-01-01", ]
  Reed$Cleaned_Name = as.character(Reed$NAME,stringAsFactors=FALSE)
  Reed$Cleaned_Address = as.character(Reed$ADDRESS_FINAL__C,stringAsFactors=FALSE)
  
  for(i in 1:nrow(Reed))
  {
    for(j in 1:length(to))
    {
      Reed[i,"Cleaned_Name"]=  gsub(as.character(from[j]), as.character(to[j]), as.character(tolower(Reed$Cleaned_Name[i])), fixed = TRUE)
      Reed[i,"Cleaned_Address"] =  gsub(as.character(from[j]), as.character(to[j]), as.character(tolower(Reed$Cleaned_Address[i])), fixed = TRUE)
    }
  }
  
  for(i in 1:nrow(Reed))
  {
    if(startsWith(as.character(tolower(Reed$Cleaned_Address[i])),'st '))
    {
      Reed[i,"Cleaned_Address"] = gsub("st "," street ",as.character(tolower(Reed$Cleaned_Address[i])))
    }
    if(startsWith(as.character(tolower(Reed$Cleaned_Name[i])),'st '))
    {
      Reed[i,"Cleaned_Name"] = gsub("st "," street ",as.character(tolower(Reed$Cleaned_Name[i])))
    }
    
    if(endsWith(as.character(tolower(Reed$Cleaned_Address[i])),' st'))
    {
      Reed[i,"Cleaned_Address"] = gsub(" st"," street ",as.character(tolower(Reed$Cleaned_Address[i])))
    }
    if(endsWith(as.character(tolower(Reed$Cleaned_Address[i])),'st)'))
    {
      Reed[i,"Cleaned_Address"] = gsub("st)"," street ",as.character(tolower(Reed$Cleaned_Address[i])))
    }
    
    if(endsWith(as.character(tolower(Reed$Cleaned_Name[i])),' st'))
    {
      Reed[i,"Cleaned_Name"] = gsub(" st"," street ",as.character(tolower(Reed$Cleaned_Name[i])))
    }
    
    if(endsWith(as.character(tolower(Reed$Cleaned_Address[i])),' w'))
    {
      Reed[i,"Cleaned_Address"] = gsub(" w"," west ",as.character(tolower(Reed$Cleaned_Address[i])))
    }
    if(endsWith(as.character(tolower(Reed$Cleaned_Name[i])),' w'))
    {
      Reed[i,"Cleaned_Name"] = gsub(" w"," west ",as.character(tolower(Reed$Cleaned_Name[i])))
    }
    if(endsWith(as.character(tolower(Reed$Cleaned_Address[i])),' n'))
    {
      Reed[i,"Cleaned_Address"] = gsub(" n"," north ",as.character(tolower(Reed$Cleaned_Address[i])))
    }
    if(endsWith(as.character(tolower(Reed$Cleaned_Name[i])),' n'))
    {
      Reed[i,"Cleaned_Name"] = gsub(" n"," north ",as.character(tolower(Reed$Cleaned_Name[i])))
    }
    if(endsWith(as.character(tolower(Reed$Cleaned_Address[i])),' s'))
    {
      Reed[i,"Cleaned_Address"] = gsub(" s"," south ",as.character(tolower(Reed$Cleaned_Address[i])))
    }
    if(endsWith(as.character(tolower(Reed$Cleaned_Name[i])),' s'))
    {
      Reed[i,"Cleaned_Name"] = gsub(" s"," south ",as.character(tolower(Reed$Cleaned_Name[i])))
    }
    if(endsWith(as.character(tolower(Reed$Cleaned_Address[i])),' e'))
    {
      Reed[i,"Cleaned_Address"] = gsub(" e"," east ",as.character(tolower(Reed$Cleaned_Address[i])))
    }
    if(endsWith(as.character(tolower(Reed$Cleaned_Name[i])),' e'))
    {
      Reed[i,"Cleaned_Name"] = gsub(" e"," east ",as.character(tolower(Reed$Cleaned_Name[i])))
    }
    if(endsWith(as.character(tolower(Reed$Cleaned_Address[i])),' ave'))
    {
      Reed[i,"Cleaned_Address"] = gsub(" ave"," avenue ",as.character(tolower(Reed$Cleaned_Address[i])))
    }
    if(endsWith(as.character(tolower(Reed$Cleaned_Name[i])),' ave'))
    {
      Reed[i,"Cleaned_Name"] = gsub(" ave"," avenue ",as.character(tolower(Reed$Cleaned_Name[i])))
    }
    
    if(endsWith(as.character(tolower(Reed$Cleaned_Address[i])),' rd'))
    {
      Reed[i,"Cleaned_Address"] = gsub(" rd"," road ",as.character(tolower(Reed$Cleaned_Address[i])))
    }
    
    if(endsWith(as.character(tolower(Reed$Cleaned_Name[i])),' rd'))
    {
      Reed[i,"Cleaned_Name"] = gsub(" rd"," road ",as.character(tolower(Reed$Cleaned_Name[i])))
    }
    if(endsWith(as.character(tolower(Reed$Cleaned_Address[i])),' dr'))
    {
      Reed[i,"Cleaned_Address"] = gsub(" dr"," drive ",as.character(tolower(Reed$Cleaned_Address[i])))
    }
    
    if(endsWith(as.character(tolower(Reed$Cleaned_Name[i])),' dr'))
    {
      Reed[i,"Cleaned_Name"] = gsub(" dr"," drive ",as.character(tolower(Reed$Cleaned_Name[i])))
    }
    
    if(grepl("multiple",as.character(tolower(Reed$Cleaned_Address[i]))))
    {
      Reed[i,"Cleaned_Address"] = "various locations"
    }
    
    if(grepl("various",as.character(tolower(Reed$Cleaned_Address[i]))))
    {
      Reed[i,"Cleaned_Address"] = "various locations"
    }
    
    if(grepl("Citywide",as.character(tolower(Reed$Cleaned_Address[i]))))
    {
      Reed[i,"Cleaned_Address"] = "various locations"
    }
    if(grepl("Countywide",as.character(tolower(Reed$Cleaned_Address[i]))))
    {
      Reed[i,"Cleaned_Address"] = "various locations"
    }
    if(grepl("Statewide",as.character(tolower(Reed$Cleaned_Address[i]))))
    {
      Reed[i,"Cleaned_Address"] = "various locations"
    }
    if(grepl("county wide",as.character(tolower(Reed$Cleaned_Address[i]))))
    {
      Reed[i,"Cleaned_Address"] = "various locations"
    }
    
    if(grepl("state wide",as.character(tolower(Reed$Cleaned_Address[i]))))
    {
      Reed[i,"Cleaned_Address"] = "various locations"
    } 
    
    if(grepl("unspecified",as.character(tolower(Reed$Cleaned_Address[i]))))
    {
      Reed[i,"Cleaned_Address"] = "various locations"
    } 
  }
  
  
  #Dodge Cleanup
  Dodge = data_complete_State[data_complete_State$RECORDTYPENAME__C == "Dodge Project", ]
  Dodge$MHC_PUBLISH_DATE_TXT__C_Formated <- as.Date(Dodge$MHC_PUBLISH_DATE_TXT__C_Formated, "%m/%d/%Y")
  Dodge = Dodge[Dodge$MHC_PUBLISH_DATE_TXT__C_Formated >= "2015-01-01", ]
  Dodge = Dodge[!is.na(Dodge$MHC_PUBLISH_DATE_TXT__C_Formated), ]
  Dodge$Cleaned_Name = as.character(Dodge$NAME,stringAsFactors=FALSE)
  Dodge$Cleaned_Address = as.character(Dodge$ADDRESS_FINAL__C,stringAsFactors=FALSE)
  
  for(i in 1:nrow(Dodge))
  {
    for(j in 1:length(to))
    {
      Dodge[i,"Cleaned_Name"]=  gsub(as.character(from[j]), as.character(to[j]), as.character(tolower(Dodge$Cleaned_Name[i])), fixed = TRUE)
      Dodge[i,"Cleaned_Address"] =  gsub(as.character(from[j]), as.character(to[j]), as.character(tolower(Dodge$Cleaned_Address[i])), fixed = TRUE)
    }
  }
  
  for(i in 1:nrow(Dodge))
  {
    if(startsWith(as.character(tolower(Dodge$Cleaned_Address[i])),'st '))
    {
      Dodge[i,"Cleaned_Address"] = gsub("st "," street ",as.character(tolower(Dodge$Cleaned_Address[i])))
    }
    if(startsWith(as.character(tolower(Dodge$Cleaned_Name[i])),'st '))
    {
      Dodge[i,"Cleaned_Name"] = gsub("st "," street ",as.character(tolower(Dodge$Cleaned_Name[i])))
    }
    
    if(endsWith(as.character(tolower(Dodge$Cleaned_Address[i])),' st'))
    {
      Dodge[i,"Cleaned_Address"] = gsub(" st"," street ",as.character(tolower(Dodge$Cleaned_Address[i])))
    }
    
    if(endsWith(as.character(tolower(Dodge$Cleaned_Address[i])),'st)'))
    {
      Dodge[i,"Cleaned_Address"] = gsub("st)"," street ",as.character(tolower(Dodge$Cleaned_Address[i])))
    }
    if(endsWith(as.character(tolower(Dodge$Cleaned_Name[i])),' st'))
    {
      Dodge[i,"Cleaned_Name"] = gsub(" st"," street ",as.character(tolower(Dodge$Cleaned_Name[i])))
    }
    
    if(endsWith(as.character(tolower(Dodge$Cleaned_Address[i])),' w'))
    {
      Dodge[i,"Cleaned_Address"] = gsub(" w"," west ",as.character(tolower(Dodge$Cleaned_Address[i])))
    }
    if(endsWith(as.character(tolower(Dodge$Cleaned_Name[i])),' w'))
    {
      Dodge[i,"Cleaned_Name"] = gsub(" w"," west ",as.character(tolower(Dodge$Cleaned_Name[i])))
    }
    if(endsWith(as.character(tolower(Dodge$Cleaned_Address[i])),' n'))
    {
      Dodge[i,"Cleaned_Address"] = gsub(" n"," north ",as.character(tolower(Dodge$Cleaned_Address[i])))
    }
    if(endsWith(as.character(tolower(Dodge$Cleaned_Name[i])),' n'))
    {
      Dodge[i,"Cleaned_Name"] = gsub(" n"," north ",as.character(tolower(Dodge$Cleaned_Name[i])))
    }
    if(endsWith(as.character(tolower(Dodge$Cleaned_Address[i])),' s'))
    {
      Dodge[i,"Cleaned_Address"] = gsub(" s"," south ",as.character(tolower(Dodge$Cleaned_Address[i])))
    }
    if(endsWith(as.character(tolower(Dodge$Cleaned_Name[i])),' s'))
    {
      Dodge[i,"Cleaned_Name"] = gsub(" s"," south ",as.character(tolower(Dodge$Cleaned_Name[i])))
    }
    if(endsWith(as.character(tolower(Dodge$Cleaned_Address[i])),' e'))
    {
      Dodge[i,"Cleaned_Address"] = gsub(" e"," east ",as.character(tolower(Dodge$Cleaned_Address[i])))
    }
    if(endsWith(as.character(tolower(Dodge$Cleaned_Name[i])),' e'))
    {
      Dodge[i,"Cleaned_Name"] = gsub(" e"," east ",as.character(tolower(Dodge$Cleaned_Name[i])))
    }
    if(endsWith(as.character(tolower(Dodge$Cleaned_Address[i])),' ave'))
    {
      Dodge[i,"Cleaned_Address"] = gsub(" ave"," avenue ",as.character(tolower(Dodge$Cleaned_Address[i])))
    }
    if(endsWith(as.character(tolower(Dodge$Cleaned_Name[i])),' ave'))
    {
      Dodge[i,"Cleaned_Name"] = gsub(" ave"," avenue ",as.character(tolower(Dodge$Cleaned_Name[i])))
    }
    
    if(endsWith(as.character(tolower(Dodge$Cleaned_Address[i])),' rd'))
    {
      Dodge[i,"Cleaned_Address"] = gsub(" rd"," road ",as.character(tolower(Dodge$Cleaned_Address[i])))
    }
    
    if(endsWith(as.character(tolower(Dodge$Cleaned_Name[i])),' rd'))
    {
      Dodge[i,"Cleaned_Name"] = gsub(" rd"," road ",as.character(tolower(Dodge$Cleaned_Name[i])))
    }
    if(endsWith(as.character(tolower(Dodge$Cleaned_Address[i])),' dr'))
    {
      Dodge[i,"Cleaned_Address"] = gsub(" dr"," drive ",as.character(tolower(Dodge$Cleaned_Address[i])))
    }
    
    if(endsWith(as.character(tolower(Dodge$Cleaned_Name[i])),' dr'))
    {
      Dodge[i,"Cleaned_Name"] = gsub(" dr"," drive ",as.character(tolower(Dodge$Cleaned_Name[i])))
    }
    
    if(grepl("multiple",as.character(tolower(Dodge$Cleaned_Address[i]))))
    {
      Dodge[i,"Cleaned_Address"] = "various locations"
    }
    
    if(grepl("various",as.character(tolower(Dodge$Cleaned_Address[i]))))
    {
      Dodge[i,"Cleaned_Address"] = "various locations"
    }
    
    if(grepl("Citywide",as.character(tolower(Dodge$Cleaned_Address[i]))))
    {
      Dodge[i,"Cleaned_Address"] = "various locations"
    }
    if(grepl("Countywide",as.character(tolower(Dodge$Cleaned_Address[i]))))
    {
      Dodge[i,"Cleaned_Address"] = "various locations"
    }
    if(grepl("Statewide",as.character(tolower(Dodge$Cleaned_Address[i]))))
    {
      Dodge[i,"Cleaned_Address"] = "various locations"
    }
    if(grepl("County wide",as.character(tolower(Dodge$Cleaned_Address[i]))))
    {
      Dodge[i,"Cleaned_Address"] = "various locations"
    }
    
    if(grepl("State wide",as.character(tolower(Dodge$Cleaned_Address[i]))))
    {
      Dodge[i,"Cleaned_Address"] = "various locations"
    }
    if(grepl("unspecified",as.character(tolower(Dodge$Cleaned_Address[i]))))
    {
      Dodge[i,"Cleaned_Address"] = "various locations"
    }
    if(grepl("request",as.character(tolower(Dodge$Cleaned_Address[i]))))
    {
      Dodge[i,"Cleaned_Name"] = "Not Valid"
    }
    if(grepl("proposal",as.character(tolower(Dodge$Cleaned_Address[i]))))
    {
      Dodge[i,"Cleaned_Name"] = "Not Valid"
    }
    if(grepl("rfq",as.character(tolower(Dodge$Cleaned_Address[i]))))
    {
      Dodge[i,"Cleaned_Name"] = "Not Valid"
    }
    if(grepl("joc",as.character(tolower(Dodge$Cleaned_Address[i]))))
    {
      Dodge[i,"Cleaned_Name"] = "Not Valid"
    }
    if(grepl("qualif",as.character(tolower(Dodge$Cleaned_Address[i]))))
    {
      Dodge[i,"Cleaned_Name"] = "Not Valid"
    }
    if(grepl("encoded",as.character(tolower(Dodge$Cleaned_Address[i]))))
    {
      Dodge[i,"Cleaned_Name"] = "Not Valid"
    }
    
  }
  return(list(Reed = Reed, Dodge = Dodge))
}