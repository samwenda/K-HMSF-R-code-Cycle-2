rm(list = ls())
library(dplyr)
library(vctrs)
library(tidyr)
library(stringr)
library(tidyverse)
library(haven)
library(leaflet)
library(leaflet.extras)
library(sf)
library(gargle)
library(googledrive)
library(readr)
#drive_auth(email = "smwenda@knbs.or.ke")
#token_fetch()
#drive_find(n_max = 30)
#(files <- drive_find(q = c("starred = true", "visibility = 'anyoneWithLink'")))
setwd('C:/Users/user/Desktop/HMSF2_rev')
#https://drive.google.com/drive/folders/1VlsxH7jjsa05m4uFYyYtF6XUPSVvDWO4?usp=sharing
ls_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1VlsxH7jjsa05m4uFYyYtF6XUPSVvDWO4")
for (file_id in ls_tibble$id) {
  googledrive::drive_download(as_id(file_id),overwrite = TRUE)
}

#lister
unzip("LIST_1_Tabular_All.zip",exdir="raw/lister")
#mapper
unzip("mapform_1_Tabular_All.zip",exdir="raw/mapper")

###cluster data


na4<-read.delim("raw/lister/LIST.tab")  %>% filter(clu00>100000)

#View(na4)
###structures
sa4<-read.delim("raw/lister/structure.tab") 

###households
ha4<-read.delim("raw/lister/household.tab")

#####--------------------------------------------------------------------------------------------------------data 4
sm4<-merge(na4,sa4, by="interview__id")%>% select(clu01,clu00,s02,s01,clusterpart__id, structure__id,
                                                  s03_gps__Latitude, s03_gps__Longitude, s03_gps__Accuracy ,
                                                  s03_gps__Altitude,  s03_gps__Timestamp, s03_gps_1,
                                                  s04, s05, s06,s06b)
nh4<-merge(na4,ha4, by="interview__id")%>% select(clu01,clu00,clusterpart__id,structure__id,household__id,
                                                  h07,h08,h09,h10,h12,h13,h14,h15,h16,h17,h18,h19,
                                                  h20,h21,h23,h24,h24b,h25,h26b,h28)

sh4<-sm4 %>% left_join(nh4, by=c("clu00","clusterpart__id","structure__id"))

#View(sh4)

##filter households which have missing serial numbers and non numeric in the serial number
sm_fin<-sm4
repStr<-dplyr::filter(sm_fin, !grepl('^\\d+$', s02))

sh_fin<-sh4
sh_fin_1 <-sh_fin %>% filter(s05==1)
str_dup_num<-dplyr::filter(sh_fin_1, !grepl('^\\d+$', h07))
repHUser<-dplyr::filter(sh_fin_1, !grepl('^\\d+$', s02))


##structure .TAB
fin_str<-sm_fin%>% 
  rename(County=clu01,cluster_number=clu00,homestead_structure_name=s01,
         structure_number=s02,
         latitude=s03_gps__Latitude, longitude=s03_gps__Longitude,
         gps_str_Accuracy=s03_gps__Accuracy,gps_str_Altitude=s03_gps__Altitude,
         gps_str_Time=s03_gps__Timestamp,PointGPS=s03_gps_1,
         comments_str=s04,str_residential=s05, comment_purpose=s06,
         Feauture_type=s06b)
#point GPS for mapping
p1<-fin_str %>% select(PointGPS)
p1_1<-p1$PointGPS
p2_1<-strsplit(p1_1, split = ";")
p3_1 <- strsplit(unlist(p2_1), split = ",")
p4_1 <- as.data.frame(do.call(rbind, p3_1))
#View(p4_1)

parse_line <- function(line){
  coord_pairs <- strsplit(line, split = ";")
  # Separate the latitude-longitude components
  coords <- strsplit(unlist(coord_pairs), split = ",") # We have to unlist coord_pairs because strsplit() expects a character vector
  
  # coords is a list of two-element vectors (lat and long)
  # Combine the elements of coords into a matrix, then coerce to a dataframe
  
  df <- as.data.frame(do.call(rbind, coords)) 
}

parse_lines <- function(cluster_ids, lines){
  parsed_dfs <- Map(function(x, y) cbind(x, parse_line(y)), cluster_ids, lines) 
  # Iterates over all the pairs of cluster_ids and lines
  # and adds the cluster_id as a column to the dataframe produced by calling 
  # parse_line() on the corresponding line
  combined_df <- do.call(rbind, parsed_dfs) # Combines the list of dataframes into a single dataframe
  colnames(combined_df) <- c("cluster_number", "Longitude.tap", "Latitude.tap") # Adds appropriate column names
  return(combined_df)
}

tap.1<-fin_str %>% select(cluster_number, PointGPS)%>%filter(PointGPS!='##N/A##')
tap.2<-parse_lines(tap.1$cluster_number, tap.1$PointGPS)
#View(tap.2)
#fin_str_1<-fin_str %>% left_join(tap.2, by="cluster_number")%>%select(-PointGPS)
#View(fin_str_1)



#View(fin_str)
#-------------------------------------------------------------------------------------------GPS structures
#str_pick<-fin_str %>% select(County,cluster_number, homestead_structure_name,structure_number,latitude,longitude)
#str_tap<-fin_str %>% select(County,cluster_number, homestead_structure_name,structure_number,latitude.tap,longitude.tap)

#---------------------------------------------------------------------------GPS missing
mis_pick_str<-fin_str %>% select(County,cluster_number,homestead_structure_name,structure_number,latitude,longitude)%>% filter(-50>latitude)
mis_tap_str<-fin_str %>% select(County,cluster_number,homestead_structure_name,structure_number,PointGPS)%>% filter(PointGPS=="##N/A##")

#-------------------------------------------------------------------------------end GPS


#View(sh_fin)

##household .TAB
sh_fin1<-sh_fin %>% filter(s05==1) %>% select(clu01.x,clu00,clusterpart__id,structure__id,s01,s02,household__id,
                                              h07,h08,h09,h10,h12,h13,h14,h15,h16,h17,h18,h19,
                                              h20,h21,h23,h24,h24b,h25,h26b,h28)%>% 
  arrange(clu00,clusterpart__id,structure__id,s02)%>%
  rename(County=clu01.x,cluster_number=clu00,Homestead_structure_Name=s01,structure_number=s02,serial_num_HU=h07,
         House_number=h08,HU_occupied=h09,reason_unoccupied=h10,
         Name_HH_Head=h12,sex_head=h13,occupation=h14,Total_people=h15,
         below_4_male=h16,below_4_female=h17,Five_seventeen_male=h18,
         Five_seventeen_female=h19,eighteen_Above_male=h20,
         eighteen_Above_female=h21,operate_business=h23,
         Telephone_head=h24,Telephone_head_other=h24b,
         Telephone_other_member=h25,relationship=h26b,comments=h28) %>%
  select(-clusterpart__id, -structure__id,-household__id)
#filter occupied and assign a household number
hsfinal_1<-sh_fin1 %>% 
  filter(HU_occupied!=2)%>% 
  group_by(cluster_number) %>% 
  arrange(cluster_number, structure_number, serial_num_HU)%>% 
  mutate(household_number = row_number())
#filter the unoccupied
hsfinal_2<-sh_fin1 %>% 
  filter(HU_occupied==2)

#merge the occupied and unoccupied to form the final data set
hsfinal<-dplyr::bind_rows(hsfinal_1,hsfinal_2)

#View(hsfinal)
##------------------------telephone less than 80%
cl_tt<-hsfinal %>% filter(HU_occupied!=2) %>% group_by(cluster_number) %>% summarise(tot_hh=n())
tel_h<-hsfinal %>% filter(Telephone_head!="##N/A##") %>% filter(Telephone_head!="")
tel_h2<-tel_h  %>%select(County,cluster_number,Telephone_head)
tel_tt<-tel_h2 %>% group_by(County,cluster_number)%>% summarise(tot_tel=n())
tel_m<-cl_tt %>% left_join(tel_tt, by="cluster_number")
tel_m$tel_per<-round(tel_m$tot_tel/tel_m$tot_hh*100,0) 
tel_n<-tel_m %>% filter(tel_per<81)
##end------------------------telephone less than 80%
###----------------------------------------------------------------------------------------------
###-----~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~spss

##codes here exctract the data to be send to catographers for verification mapping and segmenting
s4<-read.delim("raw/mapper/mapform.tab") %>%filter(clu00>10000)

s4$d11<-as.character(s4$d11)
s4$d21<-as.character(s4$d21)
s4$d31<-as.character(s4$d31)
s4$d41<-as.character(s4$d41)
s<-s4

x1<-s %>% select(clu00,clu01,clu07,s11) %>% filter(s11!="##N/A##")
#hom_str1<-parse_lines(x1$clu00, x1$s11) %>% as.data.frame()
#head(hom_str1)
#head(x1)
#View(x1)
f1_1<-x1$s11
j1_1<-strsplit(f1_1, split = ";")
j2_1 <- strsplit(unlist(j1_1), split = ",")
hom_str1 <- as.data.frame(do.call(rbind, j2_1))
#head(hom_str1)
#x2<-s %>% select(clu00,clu01,clu07,s12)%>% filter(s11!="##N/A##")
#f1_2<-x2$s11
#j1_2<-strsplit(f1_2, split = ";")
#j2_2 <- strsplit(unlist(j1_2), split = ",")
#hom_str2 <- as.data.frame(do.call(rbind, j2_2))

#x3<-s %>% select(clu00,clu01,clu07,s13)
#f1_3<-x3$s11
#j1_3<-strsplit(f1_3, split = ";")
#j2_3 <- strsplit(unlist(j1_3), split = ",")
#hom_str3 <- as.data.frame(do.call(rbind, j2_3))

#x4<-s %>% select(clu00,clu01,clu07,s14)
#f1_4<-x4$s11
#j1_4<-strsplit(f1_4, split = ";")
#j2_4 <- strsplit(unlist(j1_4), split = ",")
#hom_str4 <- as.data.frame(do.call(rbind, j2_4))

hom_str<-hom_str1 # %>% filter(cluster_number>1001)

##segmentation points
###----------------------------------------------------------------------------------------------------------start seg points
segz<-c(2,3,4)
y1<-s %>% select(clu00,segselect,p3)%>%filter(clu00>100000) %>% filter(segselect %in% segz)%>%filter(p3!="##N/A##")
#d4
#View(y1)
#seg_point<-parse_lines(y1$clu00, y1$p3)%>% as.data.frame()

dy_1<-y1 %>% filter(!is.na(segselect))
k1_1<-dy_1$p3
h1_1<-strsplit(k1_1, split = ";")
h2_1 <- strsplit(unlist(h1_1), split = ",")
seg_point <- as.data.frame(do.call(rbind, h2_1))
#head(seg_point)


#----------------------------------------------------------------------------------------------------------end seg points
##centroid segment 1
d5<-s %>% select(clu00,d1)  %>%filter(clu00>100000)#%>%filter(d1!="##N/A##")%>%filter(!is.na(d1))
mm1_1<-d5$d1
mm1_2<-strsplit(mm1_1, split = ";")
mm1_3<- strsplit(unlist(mm1_2), split = ",")
seg_1 <- as.data.frame(do.call(rbind, mm1_3))
#View(d5)
##centroid segment 2
d6<-s %>% select(clu00,d2)%>%filter(clu00>100000)
mm2_1<-d6$d2
mm2_2<-strsplit(mm2_1, split = ";")
mm2_3<- strsplit(unlist(mm2_2), split = ",")
seg_2 <- as.data.frame(do.call(rbind, mm2_3))

##centroid segment 3
d7<-s %>% select(clu00,d3)%>%filter(clu00>100000) 
mm3_1<-d7$d3
mm3_2<-strsplit(mm3_1, split = ";")
mm3_3<- strsplit(unlist(mm3_2), split = ",")
seg_3 <- as.data.frame(do.call(rbind, mm3_3))

#----------------------------------------------------------------------------------------------------------end centroids
###codes for structures listed by the lister and the points picked by the mapper and total households in the structure

hm1<- read.delim("raw/lister/hm1.tab")

q22 <- read.delim("raw/lister/hm11.tab")
q22$ï..interview__key<-as.character(q22$ï..interview__key)
q22$interview__id<-as.character(q22$interview__id)
q22$hm11__id<-as.integer(q22$hm11__id)
q22$hom_st1<-as.character(q22$hom_st1)
q22$nh1<-as.integer(q22$nh1)
q22_1<-q22
hm11_1<- q22_1 %>% rename(hom_st=hom_st1,hm1__id=hm11__id, nh=nh1)

q33 <- read.delim("raw/lister/hm12.tab")
q33$ï..interview__key<-as.character(q33$ï..interview__key)
q33$interview__id<-as.character(q33$interview__id)
q33$hm12__id<-as.integer(q33$hm12__id)
q33$hom_st2<-as.character(q33$hom_st2)
q33$nh2<-as.integer(q33$nh2)
q33_1<-q33
hm12_1 <-q33_1 %>% select(ï..interview__key,interview__id,hm12__id,hom_st2, nh2)%>%
  rename(hom_st=hom_st2,hm1__id=hm12__id , nh=nh2)


q44 <- read.delim("raw/lister/hm13.tab")
q44_1<-q44
hm13_1 <-q44_1 %>%  rename(hom_st=hom_st3,hm1__id=hm13__id , nh=nh3)

q55 <- read.delim("raw/lister/hm14.tab")
q55_1<-q55
hm14_1 <-q55_1 %>%  rename(hom_st=hom_st4,hm1__id=hm14__id , nh=nh4)

hm1$hom_st<-as.character(hm1$hom_st)

hm11_1$hom_st<-as.character(hm11_1$hom_st)

hm12_1$interview__id<-as.character(hm12_1$interview__id)
hm12_1$ï..interview__key<-as.character(hm12_1$ï..interview__key)
hm12_1$hm1__id<-as.integer(hm12_1$hm1__id)
hm12_1$hom_st<-as.character(hm12_1$hom_st)
hm12_1$nh<-as.integer(hm12_1$nh)

hm13_1$interview__id<-as.character(hm13_1$interview__id)
hm13_1$ï..interview__key<-as.character(hm13_1$ï..interview__key)
hm13_1$hm1__id<-as.integer(hm13_1$hm1__id)
hm13_1$hom_st<-as.character(hm13_1$hom_st)
hm13_1$nh<-as.integer(hm13_1$nh)

hm14_1$interview__id<-as.character(hm14_1$interview__id)
hm14_1$ï..interview__key<-as.character(hm14_1$ï..interview__key)
hm14_1$hm1__id<-as.integer(hm14_1$hm1__id)
hm14_1$hom_st<-as.character(hm14_1$hom_st)
hm14_1$nh<-as.integer(hm14_1$nh)


a1<-full_join(hm1,hm11_1, by = c("ï..interview__key","interview__id","hm1__id", "hom_st","nh")) 
a2<-full_join(a1,hm12_1, by = c("ï..interview__key","interview__id", "hm1__id", "hom_st","nh")) 
a3<-full_join(a2,hm13_1, by = c("ï..interview__key","interview__id", "hm1__id", "hom_st","nh")) 
a4<-full_join(a3,hm14_1, by = c("ï..interview__key","interview__id","hm1__id", "hom_st","nh"))

nm<-na4

nm$g<-ifelse(nm$GTM>0,nm$GTM,0)
nm$g1<-ifelse(nm$GTM1>0,nm$GTM1,0)
nm$g2<-ifelse(nm$GTM2>0,nm$GTM2,0)
nm$g3<-ifelse(nm$GTM3>0,nm$GTM3,0)
nm$g4<-ifelse(nm$GTM4>0,nm$GTM4,0)
nm$Tot_hom=nm$g+nm$g1+nm$g2+nm$g3+nm$g4

### homestead for part 1 and 2
nm$k<-ifelse(nm$v1>0,nm$v1,0)
nm$k1<-ifelse(nm$v11>0,nm$v11,0)
nm$k2<-ifelse(nm$v12>0,nm$v12,0)
nm$k3<-ifelse(nm$v13>0,nm$v13,0)
nm$k4<-ifelse(nm$v14>0,nm$v14,0)
nm$Tot_hom1<-rowSums(cbind(nm$k,nm$k1,nm$k2,nm$k3,nm$k4), na.rm = T )
##households for the part 1 and 2
nm$h<-ifelse(nm$v2>0,nm$v2,0)
nm$h1<-ifelse(nm$v21>0,nm$v21,0)
nm$h2<-ifelse(nm$v22>0,nm$v22,0)
nm$h3<-ifelse(nm$v23>0,nm$v23,0)
nm$h4<-ifelse(nm$v24>0,nm$v24,0)
#nm$Tot_house=nm$h+nm$h1+nm$h2+nm$h3+nm$h4
nm$Tot_house<-rowSums(cbind(nm$h,nm$h1,nm$h2,nm$h3,nm$h4), na.rm = T )

nm1<-nm %>%
  mutate(tot_home_str = case_when(
    is.na(Tot_hom) ~ Tot_hom1,
    Tot_hom == Tot_hom1 ~ Tot_hom,
    Tot_hom > Tot_hom1 ~ Tot_hom
  ))

nm2<-nm1 %>%filter(clu00>100000) %>% select(clu00,clu01,clu02,clu03,clu04,clu05,clu06,clu07,clu08,
                                            clu09,clu10,gps_clus__Latitude,gps_clus__Longitude,
                                            gps_clus__Accuracy,date_int,tot_home_str,Tot_house, qw,hs1,perhs1,
                                            hs2,pp_1, perhs2,hs3,perhs3, c0_0,c1_1,c2_2,
                                            c3_3,n1,
                                            c1a,c1b,cc2,dist_ch, cc3,time_ch,c4a,
                                            c4b,dist_tow,c5a,c5b__1,c5b__2,c5b__3,
                                            c5b__4,c5b__5,c5b__6, c5b__7, c6,c7,c8,
                                            c9,c10, c11a,per1,c11b,per2,
                                            c11c,per3,c12,c13,c13b,chief,chieftelphone,
                                            asschief,asschieftelphone,villageelder,
                                            villageeldertelephone,date_end)

nm3<-nm2 %>% rename(ClusterNumber=clu00,County=clu01,Subcounty=clu02,Division=clu03,
                    Location=clu04,SubLocation=clu05,GEOCODE=clu06,EAName_1=clu07,EAtype_Residence=clu08,
                    EAStatus=clu09,frame_component=clu10,clu_gps_lat=gps_clus__Latitude,clu_gps_long=gps_clus__Longitude,
                    clu_gps_accuracy=gps_clus__Accuracy,date_start_interview=date_int,
                    Total_Count_homesteads=tot_home_str,Total_households=Tot_house,
                    cluster_req_seg=qw,households_Seg_1=hs1,Percent_seg_1=perhs1,
                    households_Seg_2=hs2,Pop_total_minus_pop_1=pp_1,
                    Percent_seg_2=perhs2,hh_Segmet3=hs3,Percent_seg_3=perhs3,
                    CumPercenths_0=c0_0,CumPerc_1=c1_1,CumPerc_2=c2_2,
                    CumPer_3=c3_3,Last_2_dig_cluster=n1,
                    #Segmentserial_1=segser1_1,Segmentserial_2=segser2_2,Segmentserial_3=segser3_3,
                    #selected_seg_1=ss1_1,selected_seg_2=ss2_2,selected_seg_3=ss3_3,
                    Cluster_Listed=c1a,Why_not_listed=c1b,distance_County_HQ_to_cluster=cc2,dist_Km_Metres=dist_ch,
                    Time_County_HQ_to_cluster=cc3,time_hours_minutes=time_ch,appr_town_spend_night=c4a,
                    Dist_townorfacility_cluster=c4b,distance_in_KM_Metres=dist_tow,clu_req_security=c5a,
                    causes_insecurity_1=c5b__1,causes_insecurity_2=c5b__2,causes_insecurity_3=c5b__3,
                    causes_insecurity_4=c5b__4,causes_insecurity_5=c5b__5,causes_insecurity_6=c5b__6,
                    causes_insecurity_7=c5b__7, mode_transport=c6,terrain_cluster=c7,size_cluster=c8,
                    households_within_cluster=c9,current_settlement_cluster=c10,
                    native_languages_1=c11a,Per_1_lang=per1,native_languages_2=c11b,Per_2_lang=per2,
                    native_languages_3=c11c,Per_3_lang=per3,
                    main_econ_act=c12,EAName=c13,name_EA_asknown=c13b,name_chief=chief,telephone_chief=chieftelphone,
                    name_Assist_Chief=asschief,telephone_Ast_chief=asschieftelphone,Village_Elder=villageelder,
                    telephone_village_elder=villageeldertelephone,Date_End=date_end)

###sneaking in feautures-------------------------------------------------------------------------------


f4<-read.delim("raw/mapper/r4.tab")
fn<-f4
fs<-s %>% group_by(interview__id, clu00,clu01)%>% summarise()
fsn<-fn %>% left_join(fs, by="interview__id") %>% filter(clu00>100000)

f11<-fsn %>% select(clu00,clu01,r4__id,ft,Clu_feat,sp,name_feature,gps_f__Latitude, 
                    gps_f__Longitude,gps_f__Accuracy,gps_f__Timestamp) %>% 
  rename(cluster_number=clu00, feature_number=r4__id,feauture_name=ft,
         feauture_type=Clu_feat,feauture_specify=sp, name_feature2=name_feature,
         latitude=gps_f__Latitude, longitude=gps_f__Longitude)
mis_pick_Feat<-f11 %>% filter(-50>latitude)

#View(mis_pick_Feat)

#q1<-hom_str %>% group_by(cluster_number) %>% summarise(total_structure_MAPPER=n())%>% 
#  filter(cluster_number>10000)%>% as.data.frame()
#Total listed by the LISTER during quick count
#p1<-nm3 %>% select(ClusterNumber, Total_Count_homesteads) %>%
#  rename(cluster_number=ClusterNumber,total_structure_LISTER=Total_Count_homesteads)%>%
#  as.data.frame()
#c <- full_join(p1,q1, by="cluster_number")
#View(c)
#head(hom_str)
hom_strF<-hom_str %>% filter(V1!="NA")%>% filter(V1!="##N/A##")
quick.count <- st_as_sf(hom_strF, coords = c("V1", "V2"), crs = 4326)

seg_point1<-seg_point %>% filter(V1!="NA") %>% filter(V1!="##N/A##")
segmentation <- st_as_sf(seg_point1, coords = c("V1", "V2"), crs = 4326)

###no missing GPS on the picked households

clust3_1<- fin_str %>% 
  filter(latitude!="NA") %>% 
  filter(longitude!="NA")%>%
  filter(latitude>-50)
#head(clust3_1)

gps.pick<- st_as_sf(clust3_1, coords = c("longitude", "latitude"), crs = 4326)

##no missing tap coordinates in the file before extracting
clust3_2<- p4_1 %>% 
  filter(V1!="##N/A##") %>%
  filter(V2!="##N/A##") #%>%
# filter(-50>latitude.tap)
gps.tap<- st_as_sf(clust3_2, coords = c("V1", "V2"), crs = 4326)

##---------------------------------------------------------------------------selected centroid GPS

seg_q<-nm %>% select(clu00,ss1_1_1,ss2_2_2,ss3_3_3,sr)
#head(seg_q)
xdf<-s %>% select(clu00,d1,d2,d3)
#head(xdf)
mxm<-seg_q %>% left_join(xdf, by="clu00")
mxm2<- mxm %>% filter(sr!="NA")
#head(mxm2)

pg1<-mxm2 %>% filter(ss1_1_1!='NA')
c1<-pg1 %>% select(clu00, d1) %>% rename(cent=d1)
p2<-mxm2 %>% filter(ss2_2_2!='NA')
c2<-p2 %>% select(clu00, d2) %>% rename(cent=d2)
p3<-mxm2 %>% filter(ss3_3_3!='NA')
c3<-p3 %>% select(clu00, d3) %>% rename(cent=d3)

d13<-full_join(c1,c2,c3, by = c("clu00", "cent"))


#d13<-mxm2 %>% select(clu00,cent)
pq1<-d13%>% select(cent)
pq1_1<-pq1$cent
pq2_1<-strsplit(pq1_1, split = ";")
pq3_1 <- strsplit(unlist(pq2_1), split = ",")
pq4 <- as.data.frame(do.call(rbind, pq3_1))%>%filter(!is.na(V1))
centroid<- st_as_sf(pq4, coords = c("V1", "V2"), crs = 4326)

###########---------------------------------------------------------------overlay on the shapefiles
##write all the files for reference
write.table(nm3, file="final/samplers_clean/cluster.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
write.table(fin_str, file="final/samplers_clean/structure.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')

write.table(tap.2, file="final/carto/structure_tap.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
#write.table(str_pick, file="final/carto/structure_pick.tab", na = "",
#            row.names = F, col.names = T,   quote = F,   sep = '\t')


write.table(hsfinal, file="final/samplers_clean/household.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')



#write.table(tel_f, file="final/ISSUES/telnumLESSeightyPer/tel_num.tab", na = "",
#            row.names = F, col.names = T,   quote = F,   sep = '\t')


write.table(tel_n, file="final/ISSUES/telnumLESSeightyPer/tel_num_less_80_per_cent.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
write.table(mis_pick_str, file="final/ISSUES/missingGPS/mis_pick_str.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
write.table(mis_tap_str, file="final/ISSUES/missingGPS/mis_tap_str.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
write.table(mis_pick_Feat, file="final/ISSUES/missingGPS/mis_pick_Features.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
#------
write.table(repStr, file="final/ISSUES/repeatedSTRUCTURES/str_issue.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')

write.table(str_dup_num, file="final/ISSUES/repeatedSTRUCTURES/hh_issue.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
write.table(repHUser, file="final/ISSUES/repeatedSTRUCTURES/str_hh_issue.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
#-------
write.table(hom_str, file="final/carto/Quick_Count_Structure.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
write.table(f11, file="final/carto/Features.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
write.table(seg_point, file="final/carto/SEG_points.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
write.table(seg_1, file="final/carto/segment_1_centroids.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
write.table(seg_2, file="final/carto/segment_2_centroids.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
write.table(seg_3, file="final/carto/segment_3_centroids.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')

#write.table(c, file="final/Quality/LIST_MAP_AGGREGATE.tab", na = "",
#           row.names = F, col.names = T,   quote = F,   sep = '\t')
##find the shapefiles


kenya.polys <- st_read("shapefiles/cou/21_Counties_Cycle_2_Phase_1.shp")
kenya.polys1 <- st_read("shapefiles/ea/200_Cycle_2_Phase_1_Search.shp")
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery,options = providerTileOptions(noWrap = FALSE)) %>%
  addCircles(data = quick.count, weight = 2,radius=5,color="#FFFF00", stroke = FALSE, fillOpacity = 1)%>%  
  addCircles(data = segmentation, weight = 2, radius=5,color="#FFFFFF", stroke = TRUE, fillOpacity = 1) %>%
  addCircles(data = gps.tap, weight = 2, radius=5,color="#FF00FF", stroke = TRUE, fillOpacity = 1) %>%
  addCircles(data = gps.pick, weight = 2, radius=5,color="#00FFFF", stroke = TRUE, fillOpacity = 1) %>%
  addCircles(data = centroid,color="#FF0000") %>%
  addPolygons(data = kenya.polys, weight = 2,color="white",stroke = TRUE, fillOpacity = 0.000, popup = ~FIRST_CouN) %>%
  addPolygons(data = kenya.polys1, weight = 2,color="yellow",stroke = TRUE, 
              fillOpacity = 0.000, popup =~CLustNam,label=~CLustNam,group ="kenya.polys1") %>%
  addLegend("bottomright", colors= c("#FFFF00","#FFFFFF", "#FF00FF","#00FFFF", "#FF0000"), 
            labels=c("Quick Count", "Segmentation Line" ,"Actual Listing GPS Tap",
                     "Actual Listing GPS Pick", "Centroid of Selected Segement"),
            title="Cartographic GPS Coordinates")  %>%
  addResetMapButton() %>%
  addSearchFeatures(
    targetGroups  = "kenya.polys1",
    options = searchFeaturesOptions(zoom = 10, openPopup = TRUE,
                                    firstTipSubmit = TRUE, autoCollapse = FALSE, hideMarkerOnCollapse = TRUE))%>%
  addControl("<P><B>Hint!</B> Click on the Search button above.....<br/><ul><li>Type the firts 3 digits EA Code.. e.g 134...</li><li>Or the First 2 letters of the EA, e.g mu..</li></P>",
             position = "topleft")







