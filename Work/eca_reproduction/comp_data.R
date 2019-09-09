# hauls new filter: 306
# hauls old ECA: 365
# hauls wo lengthsampleweight
# catches without lengthsampleweight: 82
# catches without lengthsampleweight, that has catchcount: 24

l<-load("/Users/a5362/hi_sync/bunnfisk/nssk_2019/North_Sea/ECA/Data/Rdata/Projectdata/HYSE2018_ns_v00")
snr_old_data <- unique(caadata$data$SERIENR)

pd<-loadProjectData("ECA_NSSK_hyse_2018")
missingsnr <- snr_old_data[!snr_old_data %in% pd$prepareRECA$StoxExport$biotic$serialnumber]

bl <- getBaseline("ECA_NSSK_hyse_2018")

fs<-bl$outputData$ReadBioticXML$ReadBioticXML_BioticData_fishstation.txt
cs <- bl$outputData$ReadBioticXML$ReadBioticXML_BioticData_catchsample.txt
csh <- cs[cs$catchcategory=='164744',]


rb<-fs[fs$serialnumber %in% snr_old_data,]

missingstations <- fs[fs$serialnumber %in% missingsnr,]
missingcatches  <- csh[csh$serialnumber %in% missingsnr,]
