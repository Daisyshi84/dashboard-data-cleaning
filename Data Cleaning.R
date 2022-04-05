library(readxl)
library(stringr)
library(tidyverse)
library(writexl)
data<-read_excel("\\\\cdc.gov\\locker\\CPR_RMOI\\Response Activities\\2019 nCoV\\Evaluation Team\\Dashboard\\Data Output\\main_data_dashboard_Mar22.xlsx")
#source('dashboard_OverTheTimeVariables.R')
 

# 'Rules' = words that Daisy will add into the code to be automatically replaced in the qualitative data
#Original text	Replacement text
# she	[they]
# he	[they]
# her	[their]
# his	[their]
 
list<- data%>%
  select(source_other,whyvolunteer_other,pack_other	,		erf01	,		erf02	,		eq_received	,		eq_needed	,
         eq_requested,			ppe_issues,			equip_open,			predepinfo_eoc,			predepinfo_field,		
         hrs_home_other	,		jobdescr02_other	,		unsafe_open	,		incidentreport_other,			hlthprep_missing,			
         breaks_why_other	,		tl_open	,		deploycdc_whynot_other,			deploycdc_suggest	,		adminleave_other,
         adminreject_why	,		barrier_other		,	posaspect	,		negaspect	,		suggestion	,		grrt_overall,	
         grrt_leader	,		grrt_suggest,			grrt_collab		,	stltres_open		,	stltres_add		,	tracing_other	,		
         stlt_leadership,			stlt_open	,		vtf_expopen	,		vtf_res_nothelpful	,		vtf_resourcesopen	,		vtf_depagain_open,
         gmtfnewsletter_rec	,		gmtf_suggestion	,
		pack_other,			erf01	,		erf02	,		eq_received	,		eq_needed	,		eq_requested	,		ppe_issues,
		equip_open	,		predepinfo_eoc	,		predepinfo_field	,		hrs_home_other	,		jobdescr02_other,	
		unsafe_open	,		incidentreport_other,			hlthprep_missing	,		breaks_why_other	,		tl_open	,
		deploycdc_whynot_other	,		deploycdc_suggest	,		adminleave_other,			adminreject_why	,		barrier_other,			posaspect,
		negaspect	,		suggestion	,		grrt_overall,			grrt_leader	,		grrt_suggest	,		grrt_collab,			stltres_open,			
		stltres_add	,		tracing_other	,		stlt_leadership	,		stlt_open	,		vtf_expopen	,		vtf_res_nothelpful,			vtf_resourcesopen	,
		vtf_depagain_open	,		gmtfnewsletter_rec,			gmtf_suggestion	)

 
# for (q in names(list)){
# 	  data[[q]]<- str_replace_all(data[[q]],c("She ","He ","Her ","His "),c("[They ]","[They]","[Their]","[Their]"))
# 	  data[[q]]<- str_replace_all(data[[q]],c("she ","he ","her ","his "),c("[They ]","[They]","[Their]","[Their]"))
# }
#  

for( q in names(list)){
  # gsub("old","new",data,ignore.case=TRUE)
  data[[q]] <- gsub("\\<he\\>", "[They]",data[[q]],ignore.case = TRUE)
  data[[q]] <- gsub("\\<she\\>", "[They]",data[[q]],ignore.case = TRUE)
  data[[q]] <- gsub("\\<her\\>", "[Their]",data[[q]],ignore.case = TRUE)
  data[[q]] <- gsub("\\<his\\>", "[Their]",data[[q]],ignore.case = TRUE)
  #eliminating the numeric values
  #data[[q]] <- gsub('\\<[0-9]*\\>','[Date]',data[[q]])
  # data[[q]] <- gsub("Divia Forbes","[name]",data[[q]])
  # data[[q]] <- gsub("FDA","[non-CDC agency]",data[[q]],ignore.case = TRUE)
  # data[[q]] <- gsub("\\<LNO\\>","[name]",data[[q]],ignore.case = TRUE)
   data[[q]] <- gsub("\\<April\\>","[spring]",data[[q]],ignore.case = TRUE)
  # data[[q]] <- gsub("\\<Loyal Source\\>","[company]",data[[q]],ignore.case = TRUE)
}
# substitute the initial space with 'Mr/Mrs.' expression
# gsub('.*^','Mr/Mrs.',df$Speaker)

#Mar 11 
data$source_other[data$source_other == "Our whole branch was deployed at the beginning of this pandemic -- Immunization Information Systems Support Branch at ISD/NCIRD"] = "Our whole branch was deployed at the beginning of this pandemic -- [home office] NCIRD"
data$source_other[data$source_other == "I tried to apply for OAW, but they did not have room to me. So, Hilary referred me to  GMTF " ] = "I tried to apply for OAW, but they did not have room to me. So, [name] referred me to  GMTF"
data$source_other[data$source_other == "Our entire branch was deployed, IISSB"]="Our entire branch was deployed, [home office]"
data$source_other[data$source_other == "Term FTE to support response since October 2020"] = "Term FTE to support response since [Fall] 2020"
data$source_other[data$source_other == "ERCB employee"] = "[home office] employee"
data$source_other[data$source_other == "I am part of IISSB and I have been deployed since October 2020"] = "I am part of [home office] and I have been deployed since [Fall] 2020"
data$source_other[data$source_other == "I am a contractor and was assigned to the HSWS TF Comms Team (not Epi TF)"] = "I am a contractor and was assigned to the HSWS TF [team] (not Epi TF)"
data$source_other[data$source_other == "I tried to apply for OAW, but they did not have room to me. So, Hilary referred me to  GMTF"] = "I tried to apply for OAW, but they did not have room to me. So, [name] referred me to  GMTF"
data$whyvolunteer_other[data$whyvolunteer_other == "Again, our whole branch was deployed -- IISSB at ISD/NCIRD"] = "Again, our whole branch was deployed -- [home office] NCIRD"
data$pack_other[data$pack_other == "I had been to Ft Bliss before in the military."] = "I had been to [site name] before in the military."
data$predepinfo_eoc[data$predepinfo_eoc == "that my deployment, that started on 1/7/2020, would last over 2 years with no breaks."] = "that my deployment, that started on [winter 2020], would last over 2 years with no breaks."
data$predepinfo_eoc[data$predepinfo_eoc == "I think deployers could benefit from an explanation of the Emergency Operations Command structure and their role in the response. I am in an Emergency Management graduate program and it wasn't until I was enrolled and simultaneously on a response that I more fully understood the deployment to an actual Emergency Response." ] = "I think deployers could benefit from an explanation of the Emergency Operations Command structure and their role in the response. I am in an [early career/educational] program and it wasn't until I was enrolled and simultaneously on a response that I more fully understood the deployment to an actual Emergency Response."
data$predepinfo_eoc[data$predepinfo_eoc == "My supervisor Amanda Carnes provided more than enough information!"] = "My supervisor [name] provided more than enough information!"
data$predepinfo_eoc[data$predepinfo_eoc == "The Commercial Partner Projects team in DMR did an excellent job onboarding. I wouldn't change anything."] = "The [team name] in DMR did an excellent job onboarding. I wouldn't change anything."
data$predepinfo_eoc[data$predepinfo_eoc == "A simple job description. Also, the org charts for TFs are rarely updated. I am not on the EDSTF Comms/Policy Team, and the org chart is outdated, and it's not clear who is supposed to update it. This makes one's job harder when you are new and trying to learn who to ask about what."]= "A simple job description. Also, the org charts for TFs are rarely updated. I am not on the EDSTF [team name], and the org chart is outdated, and it's not clear who is supposed to update it. This makes one's job harder when you are new and trying to learn who to ask about what."
data$predepinfo_field[data$predepinfo_field == "Hmm, I was originally advised that I was not qualified for this deployment by a recruiter. He said that my skill sets were not of any use on this deployment and then he suggested that I apply for something at Roybal.  I advised him I thought that was odd as I had just done a 30 day assignment there two months ago. After speaking with someone else, I was then signed up to assist.   "] = "Hmm, I was originally advised that I was not qualified for this deployment by a recruiter. [They] said that my skill sets were not of any use on this deployment and then [they]suggested that I apply for something at Roybal.  I advised [them] I thought that was odd as I had just done a 30 day assignment there two months ago. After speaking with someone else, I was then signed up to assist.   "
data$predepinfo_field[data$predepinfo_field == "I went to Ft Bliss when I was new to CDC.  I did not know the usual process so just went after my boss approved.  "] = "I went to [site name] when I was new to CDC.  I did not know the usual process so just went after my boss approved.  "
data$hrs_home_other[data$hrs_home_other == "I am an Acting Division Director and needed to keep up with some things"]="I am an [leadership role] and needed to keep up with some things"
data$hrs_home_other[data$hrs_home_other == "Preparedness work in ERCB"]="Preparedness work in [home office]"
data$hrs_home_other[data$hrs_home_other == "I could check in on my colleagues at Humphrey HHS building."] = "I could check in on my colleagues at [in-office location] building."
data$jobdescr02_other[data$jobdescr02_other == "I didn't have a job description, however, I was 1 of 2 LNOs. Since the other LNO had been around for a while, they directed my work. Since I don't report to anyone, this person often got more involved, even after I was assigned to manage projects by the Comms Lead. It's a personality type of thing, but it was known by the Comms Leads and addressed as needed. I think a more direct approach by a leader would have been helpful."] = "I didn't have a job description, however, I was 1 of 2 [role]. Since the other [team member] had been around for a while, they directed my work. Since I don't report to anyone, this person often got more involved, even after I was assigned to manage projects by the [team lead]. It's a personality type of thing, but it was known by the [team leads] and addressed as needed. I think a more direct approach by a leader would have been helpful."
data$unsafe_open[data$unsafe_open == "Hotel staff and other guests were not taking precautions against the spread of COVID-19. Very few other people wore masks inside the hotels I stayed in in NC, TX, and MT."] = "Hotel staff and other guests were not taking precautions against the spread of COVID-19. Very few other people wore masks inside the hotels I stayed in in [southeastern state], [southwestern state], and [western state]."
data$hlthprep_missing[data$hlthprep_missing == "As I had worked in Pecos prior I was very comfortable and Ft. Bliss was fine also"] = "As I had worked in [city] prior I was very comfortable and [location near city] was fine also"
data$breaks_why_other[data$breaks_why_other ==  "Location in California required being online to meet deadlines daily at 5 a.m. PT"] = "Location in [western state] required being online to meet deadlines daily at 5 a.m. PT"
data$breaks_why_other[data$breaks_why_other == "The only times when I was impacted or felt more stress was when a leader told me I had to work into the evening to finish a certain task. This was typically when I was managing a COCA call and we needed to meet deadlines. I performed successfully, but the pressure the Comms Leads and SMEs felt impacted me"] = "The only times when I was impacted or felt more stress was when a leader told me I had to work into the evening to finish a certain task. This was typically when I was managing a COCA call and we needed to meet deadlines. I performed successfully, but the pressure the [team leads] and SMEs felt impacted me"
data$deploycdc_suggest[data$deploycdc_suggest	== "It was impossible for me to find out how to extend, I had to have the someone in Chief of Staff's team do it for me (Florence is amazing).  "] = "It was impossible for me to find out how to extend, I had to have the someone in Chief of Staff's team do it for me ([name] is amazing). "
data$barrier_other[data$barrier_other	== "Will be retiring in August with terminal leave beginning in May."] = "Will be retiring in [this year] with terminal leave beginning in [upcoming month]."
data$barrier_other[data$barrier_other	== "I am returning to retirement 4/30 when the contract ends."] = "I am returning to retirement [date] when the contract ends."
data$barrier_other[data$barrier_other	== "I would love to deploy again as long as I never have to work with the Team Lead that was in FT Bliss again. "] = "I would love to deploy again as long as I never have to work with the Team Lead that was in [location] again. "
data$barrier_other[data$barrier_other	== "I am a contractor (with Tanaq) hired to work on the response, so I will continue until my contract ends."] = "I am a contractor (with [company]) hired to work on the response, so I will continue until my contract ends."
data$posaspect[data$posaspect	== "I loved my Team lead. She was awesome to work with (Chandre' Chaney DAVTF)"] = "I loved my Team lead. She was awesome to work with ([name] DAVTF)"
data$posaspect[data$posaspect	== "The site leader, Arnold Vang was the best leader I have ever worked under in a deployment setting"] = "The site leader, [name] was the best leader I have ever worked under in a deployment setting"
data$posaspect[data$posaspect	== "The team was incredible. I served on the Commercial Partner Projects team within DMR. The leadership (Nkenge, Roua, Lori, Lynn) genuinely cared about the team and everyone on it. The team atmosphere was collaborative, fun and caring. I was always encouraged to take PTO and maintain a strong work life balance. The team's day-to-day operations were also extremely well-run and effective, with a perfect balance of structure and encouraged creativity. "] = "The team was incredible. I served on the [team name] within DMR. The leadership ([names]) genuinely cared about the team and everyone on it. The team atmosphere was collaborative, fun and caring. I was always encouraged to take PTO and maintain a strong work life balance. The team's day-to-day operations were also extremely well-run and effective, with a perfect balance of structure and encouraged creativity. "
data$posaspect[data$posaspect	== "My team is the best (STLT ADS); work was critical to the response. "] = "My team is the best (STLT [team name]); work was critical to the response. "
data$posaspect[data$posaspect	== "Being in LA"] = "Being in [western state]"
data$posaspect[data$posaspect	== "Awesome team at the Philadelphia International Airport Quarentine Station. Great group of deployers. "] = "Awesome team at the [northeastern state] International Airport Quarentine Station. Great group of deployers. "
data$posaspect[data$posaspect	== "I loved working with the OIC and the QPHOs. All the FTEs were extremely hardworking, impressive, and kind, and it was a pleasure to work with them. I appreciated that they trusted me and my public health skills. I learned so much from shadowing and working with the FTEs and some of the contractors.   I really appreciated the excellent training that I received. I learned so much from shadowing the FTEs and contractors. The OIC sent me a list of trainings from DGMQ that were very helpful, and he told me which ones to take first, and then to take the others just one per day, which was the perfect pace.    Part of the reason why I chose to deploy to a field assignment was that I hadn't really done any work 'in the field' since I was a Peace Corps Volunteer in West Africa. Working at a quarantine station gave me the opportunity to work with the public and with partners (airlines, CPB, etc.), and I really appreciated this wonderful opportunity.   The other primary reason for my desire to volunteer to deploy was that I wanted to develop new skills. This deployment entailed a wide range of skills, including the application of communication skills, understanding of logistical processes and internal CDC system to protect public health. I was grateful for the opportunity to use these skills. It was a very enriching experience that connected a lot of aspects of public health.   Overall, deployment was a rewarding, valuable experience, and I am so glad that I got to do it.  "] = "I loved working with the OIC and the QPHOs. All the FTEs were extremely hardworking, impressive, and kind, and it was a pleasure to work with them. I appreciated that they trusted me and my public health skills. I learned so much from shadowing and working with the FTEs and some of the contractors.   I really appreciated the excellent training that I received. I learned so much from shadowing the FTEs and contractors. The OIC sent me a list of trainings from DGMQ that were very helpful, and he told me which ones to take first, and then to take the others just one per day, which was the perfect pace.    Part of the reason why I chose to deploy to a field assignment was that I hadn't really done any work 'in the field' since I was a Peace Corps Volunteer in [international location]. Working at a quarantine station gave me the opportunity to work with the public and with partners (airlines, CPB, etc.), and I really appreciated this wonderful opportunity.   The other primary reason for my desire to volunteer to deploy was that I wanted to develop new skills. This deployment entailed a wide range of skills, including the application of communication skills, understanding of logistical processes and internal CDC system to protect public health. I was grateful for the opportunity to use these skills. It was a very enriching experience that connected a lot of aspects of public health.   Overall, deployment was a rewarding, valuable experience, and I am so glad that I got to do it.  "
data$negaspect <- gsub("Divia Forbes","[name]",data$negaspect)
data$negaspect <- gsub("\\<FDA\\>","[non-CDC agency]",data$negaspect,ignore.case = TRUE)
data$negaspect <- gsub("\\<LNO\\>","[name]",data$negaspect,ignore.case = TRUE)
data$negaspect <- gsub("\\<April\\>","[spring]",data$negaspect,ignore.case = TRUE)
data$negaspect <- gsub("\\<Loyal Source\\>","[company]",data$negaspect,ignore.case = TRUE)
data$negaspect <- gsub("\\<OLSS acting director\\>","[agency leadership position]",data$negaspect,ignore.case = TRUE)
data$suggestion <- gsub("\\<1/9/2020\\>","[winter 2020]",data$suggestion,ignore.case = TRUE)
data$suggestion <- gsub("\\<01/07\\>","[date]",data$suggestion,ignore.case = TRUE)
data$suggestion <- gsub("\\<DFW\\>","[location]",data$suggestion,ignore.case = TRUE)
data$stlt_open <- gsub("\\<Dr Tomi\\>","[name]",data$stlt_open,ignore.case = TRUE)
data$stlt_open <- gsub("\\<Anna\\>","[name]",data$stlt_open,ignore.case = TRUE)
ata$stlt_open <- gsub("\\<IIS SME\\>","[specific role]",data$stlt_open,ignore.case = TRUE)

#Mar22
data$source_other[data$source_other == "Our whole Quarantine and Border Health Services Branch was involved before even the GMTF was established or CDC. We were the first."]= "Our whole [home office] was involved before even the GMTF was established or CDC. We were the first."
data$source_other[data$source_other == "I am a contractor and was assigned to the HSWS TF Comms Team (not Epi TF)"] = "I am a contractor and was assigned to the HSWS TF [team] (not Epi TF)"
data$whyvolunteer_other[data$whyvolunteer_other =="Our  Quarantine and Border Health Services Branch was the first to respond before CDC or GMTF. We supported the response 24/7 for 2 years (730 days), and continue to do so despite the closure of GMTF."] = "Our [home office] was the first to respond before CDC or GMTF. We supported the response 24/7 for 2 years (730 days), and continue to do so despite the closure of GMTF."
data$erf01[data$erf01=="Nothing - the equipment team is always on point, courteous, professional and takes care of business (thanks Rafele)" ] = "Nothing - the equipment team is always on point, courteous, professional and takes care of business (thanks [RSS team member name])"


# Add each weeks code below:




writexl::write_xlsx(data,"\\\\cdc.gov\\locker\\CPR_RMOI\\Response Activities\\2019 nCoV\\Evaluation Team\\Dashboard\\Data Output\\main_data_dashboard_today_TextCleaned.xlsx")





 



