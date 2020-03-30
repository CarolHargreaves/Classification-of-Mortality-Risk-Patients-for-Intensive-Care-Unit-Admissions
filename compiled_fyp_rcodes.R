library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(caret)
library(ROCR)

##### R Shiny App #####
library(rsconnect)
library(shiny)
library(DT)
library(dplyr)


server = function(input, output, session) {
  values = reactiveValues()
  
  observe({
    values$EyeOpening = as.numeric(input$EyeOpening)
    values$eyeopening = values$EyeOpening * (-0.41461)
    
    values$MotorResponse = as.numeric(input$MotorResponse)
    values$motorresponse = values$MotorResponse * (-0.1855)
    
    values$VerbalResponse = as.numeric(input$VerbalResponse)
    values$verbalresponse = values$VerbalResponse * (-0.10808)
    
    values$PO2 = input$PO2
    values$po2 = case_when(values$PO2 == "<120mmHg"~0.93580,
                           values$PO2 == "120-125mmHg"~4.23804,
                           values$PO2 == "125-155mmHg"~0.58886,
                           T~0)
    
    values$AdmissionAge = input$AdmissionAge
    values$admissionage = case_when(values$AdmissionAge=="<47 years"~0,
                                    values$AdmissionAge=="47-61 years"~0.65465,
                                    values$AdmissionAge=="61-73 years"~0.98743,
                                    T~1.82696)
    
    values$Bilirubin = input$Bilirubin
    values$bilirubin = case_when(values$Bilirubin == "<0.75 mg/dl"~0,
                                 values$Bilirubin == "0.75-1 mg/dl"~2.89520,
                                 values$Bilirubin == "1-7.75 mg/dl"~0.69242,
                                 T~2.03523)
    
    values$Foley = input$Foley
    values$foley = case_when(values$Foley == "<6 mL"~0.86566, 
                             values$Foley == "6-20 mL"~0.28029,
                             values$Foley == "20-25 mL"~0.58092,
                             T~0)
    
    values$SystolicBP = input$SystolicBP
    values$systolicbp = case_when(values$SystolicBP == "<80 mmHg"~0.38861,
                                  values$SystolicBP == "80-90 mmHg"~0.32199,
                                  values$SystolicBP == "90-95 mmHg"~0.63199,
                                  T~0)
    
    values$DiastolicBP = input$DiastolicBP
    values$diastolicbp = case_when(values$DiastolicBP == "<45 mmHg"~1.29462,
                                   values$DiastolicBP == "45-50 mmHg"~0,
                                   values$DiastolicBP == "50-55 mmHg"~0.82218,
                                   T~0.66162)
    
    values$MeanBP = input$MeanBP
    values$meanbp = case_when(values$MeanBP == "<40 mmHg"~-0.55085,
                              values$MeanBP == "40-50 mmHg"~-0.60798,
                              values$MeanBP == "50-55 mmHg"~-0.72236,
                              T~0)
    
    values$WBC = input$WBC
    values$wbc = case_when(values$WBC == "<3.0"~0.60184,
                           values$WBC == "3.0-12.5"~0,
                           values$WBC == "12.5-19.5"~0.41386,
                           T~0.81632)
    
    values$Sodium = input$Sodium
    values$sodium = case_when(values$Sodium == "<129 mEq/L"~-0.72771,
                              values$Sodium == "129=134 mEq/L"~0.14478,
                              T~0)
    
    values$BUN = input$BUN
    values$bun = case_when(values$BUN == "<49 mg/dL"~0,
                           T~0.49198)
    
    values$Hematocrit = input$Hematocrit
    values$hematocrit = case_when(values$Hematocrit == "<49%"~0,
                                  T~-0.42810)
    
    values$Temperature = input$Temperature
    values$temperature = case_when(values$Temperature == "<36.0"~0.37293,
                                   values$Temperature == "36.0-37.1"~0,
                                   T~0.45264)
    
    values$SPO2 = input$SPO2
    values$spo2 = case_when(values$SPO2 == "<66%"~1.28253,
                            values$SPO2 == "66-86%"~0.55164,
                            values$SPO2 == "86-96%"~0,
                            T~0.53805)
    
    values$Potassium = input$Potassium
    values$potassium = case_when(values$Potassium == "<3.4 mEq/L"~0.48884,
                                 values$Potassium == "3.4-4.6 mEq/L"~-0.15013,
                                 values$Potassium == "4.6-5.1 mEq/L"~0,
                                 T~0.40614)
    
    values$Platelets = input$Platelets
    values$platelets = case_when(values$Platelets == "<20"~0.49081,
                                 values$Platelets == "20-28"~0,
                                 values$Platelets == "28-34"~-0.33701,
                                 T~-0.27353)
    
    values$Glucose = input$Glucose
    values$glucose = case_when(values$Glucose == "<204 mg/dL"~0,
                               values$Glucose == "204-241 mg/dL"~0.21752,
                               T~0.17071)
    
    values$HeartRate = input$HeartRate
    values$heartrate = case_when(values$HeartRate == "<105 beats/min"~0,
                                 values$HeartRate == ">=135 beats/min"~0,
                                 values$HeartRate == "105-120 beats/min"~0.11356,
                                 T~0.172)
    
    values$Vasopressors = input$Vasopressors
    values$vaso = ifelse(values$Vasopressors == "No", 0, 0.74001)
    
    values$AdmissionType = input$AdmissionType
    values$admissiontype = ifelse(values$AdmissionType == "Planned", 0, 1.37918)
    
    values$SevereCOPD = input$SevereCOPD
    values$severecopd = ifelse(values$SevereCOPD == "No", 0, 0.85747)
    
    values$CardiacArrest = input$CardiacArrest
    values$cardiacarrest = ifelse(values$CardiacArrest == "No", 0, 2.13783)
    
    values$HematologicMalignancy = input$HematologicMalignancy
    values$hematologicmalignancy = ifelse(values$HematologicMalignancy == "No", 0, 0.79106)
    
    values$MechanicalVentilation = input$MechanicalVentilation
    values$mechanicanventilation = ifelse(values$MechanicalVentilation == "No", 0, 0.54973)
    
    values$AdmissionLocation = input$AdmissionLocation
    values$admissionlocation = ifelse(values$AdmissionLocation == "Outside ICU", 0, 0.61225)
    
    values$MetastaticCancer = input$MetastaticCancer
    values$metastaticcancer = ifelse(values$MetastaticCancer == "No", 0, 0.48827)
    
    values$Cirrhosis = input$Cirrhosis
    values$cirrhosis = ifelse(values$Cirrhosis == "No", 0, 0.76393)
    
    values$logit = sum(-6.2901, values$eyeopening, values$motorresponse, values$verbalresponse, values$po2, values$admissionage,
                       values$bilirubin, values$foley, values$systolicbp, values$diastolicbp, values$meanbp, values$wbc,
                       values$sodium, values$bun, values$hematocrit, values$temperature, values$spo2, values$potassium,
                       values$platelets, values$glucose, values$heartrate, values$vaso, values$admissiontype, values$severecopd,
                       values$cardiacarrest, values$hematologicmalignancy, values$mechanicalventilation, values$admissionlocation,
                       values$metastaticcancer, values$cirrhosis)
    
    values$risk = exp(values$logit)/(1+exp(values$logit))
    values$outcome = case_when(values$risk <0.15~"Low Risk",
                               values$risk >=0.15 & values$risk <0.5~"Moderate Risk",
                               values$risk >=0.5 & values$risk <0.8~"High Risk",
                               values$risk >=0.8~"Extremely High Risk")
    
  })
  
  output$mytable1 = renderText({
    line1= paste("Predicted Mortality Risk: ", round(values$risk,2)*100, "%")
    line2 =paste("Risk Group: ", values$outcome)
    paste(line1, line2, sep="\n")
  })
  
  df = reactiveValues()
  
  observe({
    df$note_po2 = case_when(values$risk>=0.5 & (values$PO2 == "<120mmHg" | values$PO2 == "120-125mmHg") & values$MechanicalVentilation=="Yes"~"On ventilation, but still poor PO2 flow; Indication of severe respiratory failure. ",
                            values$risk>=0.5 & (values$PO2 == "<120mmHg" | values$PO2 == "120-125mmHg") & values$MechanicalVentilation=="No"~"Poor PO2 flow and not on ventilation; Indication of severe respiratory failure. ",
                            values$risk<0.5 & values$PO2 == ">=155mmHg" & values$MechanicalVentilation=="No"~"Good indication of healthy respiratory condition. ",
                            T~"")
    df$note_vaso = case_when(values$risk>=0.5 & values$Vasopressors == "Yes"~"Required vasopressor(s) to treat cardiovascular problems. ",
                             T~"")
    df$note_cardiac = case_when(values$risk>=0.5 & values$CardiacArrest=="Yes"~"Cardiac Arrest upon admission. ",
                                T~"")
    df$note_bp = case_when(values$risk>=0.5 & values$DiastolicBP=="<45 mmHg"~"Diastolic Blood Pressure below healthy range. ",
                           values$risk<0.5 & values$DiastolicBP == "45-50 mmHg" ~"Diastolic Blood Pressure within healthy range. ",
                           values$risk<0.5 & values$SystolicBP==">= 95 mmHg"~"Systolic Blood Pressure within healthy range. ",
                           T~"")
    df$note_copd = case_when(values$risk>=0.5 & values$SevereCOPD=="Yes"~"Severe COPD upon admission. ",
                             T~"")
    df$note_foley = case_when(values$risk>=0.5 & values$Foley=="<6 mL"~"Foley volume below healthy range; Indication of kidney failure. ",
                              T~"")
    df$note_wbc = case_when(values$risk>=0.5 & values$WBC==">=19.5"~"White blood cells above healthy range. ",
                            values$risk>=0.5 & values$WBC=="<3.0"~"White blood cells below healthy range. ",
                            T~"")
    df$note_bun = case_when(values$risk>=0.5 & values$BUN==">=49 mg/dL" ~"Blood Urea Nitrogen levels above healthy range. ",
                            T~"")
    df$note_spo2 = case_when(values$risk>=0.5 & values$SPO2=="<66%"~"SPO2 levels below healthy range; Indication of respiratory failure. ",
                             T~"")
    df$note_bilirubin = case_when(values$risk>=0.5 & values$Bilirubin==">= 7.75 mg/dl"~"Bilirubin levels above healthy range; Indication of kidney failure. ",
                                  values$risk<0.5 & values$Bilirubin=="<0.75 mg/dl"~"Bilirubin levels within healthy range. ",
                                  T~"")
    df$note_gcs = case_when(values$risk >=0.5 & values$EyeOpening==1~"Does not open eye in response to any stimulation (Worst eye opening score). ",
                            values$risk>=0.5 & values$EyeOpening==2~"Opens eyes in response to paintful stimulation (Second worst eye opening score). ",
                            values$risk<0.5 & (values$EyeOpening + values$MotorResponse + values$VerbalResponse== 15)~"Best Glasgow Coma Scale score; very low risk. ",
                            values$risk<0.5 & values$VerbalResponse==5~"Best GCS verbal response score; low risk. ",
                            values$risk<0.5 & values$MotorResponse==6~"Best GCS motor response score; low risk. ",
                            values$risk<0.5 & values$EyeOpening==4~"Best GCS eye opening response score; low risk. ",
                            T~"")
    df$note_age = case_when(values$AdmissionAge==">= 73 years"~"Higher risk due to old age. ",
                            values$AdmissionAge=="47-61 years"~"Moderate risk due to age. ",
                            values$AdmissionAge=="<47 years"~"Low risk due to age. ",
                            T~"")
    df$note_planned = case_when(values$risk>=0.5 & values$PlannedAdmission=="Yes"~"High risk due to unplanned admission. ",
                                T~"")
    df$note_loc = case_when(values$risk>=0.5 & values$AdmissionLocation=="Emergency room/ICU ward"~"High risk due to admission location - Emergency room/ICU. ", 
                            T~"")
    df$note_cirrhosis = case_when(values$risk>=0.5 & values$Cirrhosis=="Yes"~"Patient was diagnosed with cirrhosis upon admission, increased risk. ",
                                  T~"")
    df$note_all = paste(df$note_po2, df$note_vaso, df$note_cardiac, df$note_bp, df$note_copd, df$note_foley, df$note_wbc,
                        df$note_bun, df$note_spo2, df$note_bilirubin, df$note_gcs, df$note_age, 
                        df$note_planned, df$anote_loc, df$note_cirrhosis, sep="\n")
  })
  
  output$mytable2 = renderText({
    paste(df$note_all)
  })
  
  
}

ui = fluidPage(
  headerPanel("ICU Baseline Mortality Risk Calculator"),
  
  sidebarPanel(
    p("For temporal variables taken in ICU, please select the min/max/median value from the first 24h of admission."),
    p("For diagnosed conditions like cardiac arrest, please only select Yes if it is a condition diagnosed upon admission within the first 24 hours of admission."),
    br(),
    selectInput("EyeOpening", "Eye Opening (min)", choices = c(1,2,3,4)),
    selectInput("MotorResponse", "Motor Response (min)", choices = c(1,2,3,4,5,6)),
    selectInput("VerbalResponse", "Verbal Response (min)", choices = c(1,2,3,4,5)),
    selectInput("PO2", "PO2 (median)", choices=c("<120mmHg", "120-125mmHg", "125-155mmHg", ">= 155mmHg")),
    selectInput("AdmissionAge", "Admission Age", choices=c("<47 years", "47-61 years", "61-73 years", ">= 73 years")),
    selectInput("Bilirubin", "Bilirubin (max)", choices=c("<0.75 mg/dl", "0.75-1 mg/dl", "1-7.75 mg/dl", ">= 7.75 mg/dl")),
    selectInput("Foley", "Foley (min)", choices=c("<6 mL", "6-20 mL", "20-25 mL", ">= 25 mL")),
    selectInput("SystolicBP", "SystolicBP (min)", choices= c("<80 mmHg", "80-90 mmHg", "90-95 mmHg", ">= 95 mmHg")),
    selectInput("DiastolicBP", "DiastolicBP (min)", choices=c("<45 mmHg", "45-50 mmHg", ">= 55 mmHg")),
    selectInput("MeanBP", "MeanBP (min)", choices=c("<40 mmHg", "40-50 mmHg", "50-55 mmHg", ">= 55 mmHg")),
    selectInput("WBC", "WBC (min)", choices=c("<3.0", "3.0-12.5", "12.5-19.5", ">=19.5")),
    selectInput("Sodium", "Sodium (min)", choices=c("<129 mEq/L", "129-134 mEq/L", ">=134 mEq/L")),
    selectInput("BUN", "BUN (min)", choices=c("<49 mg/dL", ">=49 mg/dL")),
    selectInput("Hematocrit", "Hematocrit (max)", choices=c("<49%", ">=49%")),
    selectInput("Temperature", "Temperature (min)", choices=c("<36.0", "36.0-37.1", ">=37.1")),
    selectInput("SPO2", "SPO2 (min)", choices=c("<66%", "66-86%", "86-96%", ">=96%")),
    selectInput("Potassium", "Potassium (median)", choices=c("<3.4 mEq/L", "3.4-4.6 mEq/L", "4.6-5.1 mEq/L", ">=5.1 mEq/L")),
    selectInput("Platelets", "Platelets (min)", choices=c("<20", "20-28", "28-34", ">=34")),
    selectInput("Glucose", "Glucose (max)", choices=c("<204 mg/dL", "204-241 mg/dL", ">=241 mg/dL")),
    selectInput("HeartRate", "Heart Rate (max)", choices=c("<105 beats/min", "105-120 beats/min", "120-135 beats/min", ">=135 beats/min")),
    selectInput("RespiratoryRate", "Respiratory Rate (median)", choices=c("<18 inspir/min", "18-20 inspir/min", "21-24 inspir/min", ">=24 inspir/min")),
    selectInput("Vasopressor", "Use of vasopressors", choices=c("Yes", "No")),
    selectInput("AdmissionType", "Admission Type", choices=c("Planned", "Unplanned")),
    selectInput("SevereCOPD", "Severe COPD", choices=c("No", "Yes")),
    selectInput("CardiacArrest", "Cardiac Arrest", choices=c("No", "Yes")), 
    selectInput("HematologicMalignancy", "Hematologic Malignancy", choices=c("Yes", "No")),
    selectInput("MechanicalVentilation", "Mechanical Ventilation", choices=c("Yes", "No")),
    selectInput("AdmissionLocation", "Admission Location", choices=c("Outside ICU", "Emergency Room/ICU ward")),
    selectInput("MetastaticCancer", "Metastatic Cancer", choices=c("Yes", "No")),
    selectInput("Cirrhosis", "Cirrhosis", choices=c("Yes", "No"))
  ),
  
  mainPanel(
    tabsetPanel(type="tabs",
                tabPanel("Predicted Mortality Risk", verbatimTextOutput("mytable1")),
                tabPanel("Notes for Clinicians", verbatimTextOutput("mytable2"))))
)

shinyApp(ui, server)


#### Logistics Regression 5-Fold CV ####
#5-fold logistics model====
logistics_function = function(data, threshold, maxit) { 
  folds = createDataPartition(data$FIRST_ADMISSION_EXPIRE_FLAG, times=5, p=0.7, list=T) #5-fold cv
  perf_df = NULL
  varimpt_model = data.frame(row.names=colnames(data)[1:(ncol(data)-1)])
  counter = 0
  for (k in c("Resample1", "Resample2", "Resample3", "Resample4", "Resample5")) {
    counter = counter + 1
    train_index = folds[[k]]
    train = data[train_index,]
    test = data[-train_index,]
    train = upSample(x=train[,1:(ncol(train)-1)], y=train$FIRST_ADMISSION_EXPIRE_FLAG)
    
    glm_model = glm(Class~., train, family="binomial"(link="logit"), control=list(maxit=maxit))
    glm_predict = predict(glm_model, test, type="response")
    
    varimpt = varImp(glm_model)
    colnames(varimpt) = paste("Var_impt_model_", counter, sep="")
    varimpt_model = cbind(varimpt_model, varimpt)
    
    glm_confusion_mat = confusionMatrix(as.character(as.numeric(glm_predict>threshold)), test$FIRST_ADMISSION_EXPIRE_FLAG, positive="1")
    
    pred_glm = prediction(glm_predict, test$FIRST_ADMISSION_EXPIRE_FLAG)
    perf_glm_auc = as.numeric(performance(pred_glm, measure="auc")@y.values)
    perf_glm_sensitivity = glm_confusion_mat[["byClass"]][["Sensitivity"]]
    perf_glm_specificity = glm_confusion_mat[["byClass"]][["Specificity"]]
    
    perf = data.frame(auc = perf_glm_auc, sensitivity=perf_glm_sensitivity, specificity=perf_glm_specificity)
    cat("Fold ", counter, ": ", "auc=", perf_glm_auc, "; sensitivity=", perf_glm_sensitivity, "; specificity=", perf_glm_specificity, "\n")
    perf_df = rbind(perf_df, perf)
  }
  cat("Average : ", "auc=", mean(perf_df$auc), "; sensitivity=", mean(perf_df$sensitivity), "; specificity=", mean(perf_df$specificity), "\n")
  
  row.names(varimpt_model) = colnames(data)[1:(ncol(data)-1)]
  varimpt_model = cbind(row.names(varimpt_model), varimpt_model)
  colnames(varimpt_model)[1] = "Variable"
  varimpt_overall = varimpt_model %>% group_by(Variable) %>% summarise(Overall = mean(c(Var_impt_model_1, Var_impt_model_2, Var_impt_model_3, Var_impt_model_4, Var_impt_model_5))) %>% ungroup()
  varimpt_overall$Variable=as.character(varimpt_overall$Variable)
  varimpt_model = merge(varimpt_model, varimpt_overall, by=c("Variable"))
  varimpt_model = varimpt_model[,c(1,7,2:6)] %>% arrange(desc(Overall))
  varimpt_model}

#Leave one out cross validation
LOCOV_function = function(data) {
  predict_prob = NULL
  for (id in 1:nrow(data)) {
    train = data[-id,]
    test = data[id,]
    train = upSample(x=train[,1:(ncol(train)-1)], y=train$FIRST_ADMISSION_EXPIRE_FLAG)
    glm_model = glm(Class~., train, family="binomial", control=list(maxit=50))
    glm_predict_prob = as.numeric(predict(glm_model, test, type="response"))
    predict_prob = c(predict_prob, glm_predict_prob)
    cat(nrow(data)-id, " observations left", "\n")
  }
  predict_prob
}

#5-fold Tree Methods====
##1. basic tree method====
library(tree, quietly=T)
library(rpart)
library(rpart.plot)
basic_tree_function = function(data) {
  data$FIRST_ADMISSION_EXPIRE_FLAG = factor(data$FIRST_ADMISSION_EXPIRE_FLAG, labels = c("Survived", "Died"))
  folds = createDataPartition(data$FIRST_ADMISSION_EXPIRE_FLAG, times=5, p=0.7, list=T) #5-fold cv
  perf_df = NULL
  counter = 0
  for (k in c("Resample1", "Resample2", "Resample3", "Resample4", "Resample5")) {
    counter = counter + 1
    train_index = folds[[k]]
    train = data[train_index,]
    test = data[-train_index,]
    train = upSample(x=train[,1:(ncol(train)-1)], y=train$FIRST_ADMISSION_EXPIRE_FLAG)
    
    #basictree_model = tree(Class~., train)
    basictree_model = rpart(Class~., train, method="class")
    title=paste("Decision Tree Algorithm for #", counter, " cross validation", sep="")
    rpart.plot(basictree_model, type=4, extra=1)
    basictree_predict = predict(basictree_model, test, type="class")
    basictree_predict_prob = predict(basictree_model, test)
    basictree_confusion_mat = confusionMatrix(basictree_predict, test$FIRST_ADMISSION_EXPIRE_FLAG, positive="Died")
    
    pred_basictree = prediction(basictree_predict_prob[,2], test$FIRST_ADMISSION_EXPIRE_FLAG)
    
    perf_basictree_auc = unlist(performance(pred_basictree, measure="auc")@y.values)
    perf_basictree_sensitivity = basictree_confusion_mat[["byClass"]][["Sensitivity"]]
    perf_basictree_specificity = basictree_confusion_mat[["byClass"]][["Specificity"]]
    perf = data.frame(auc = perf_basictree_auc, sensitivity=perf_basictree_sensitivity, specificity=perf_basictree_specificity)
    print(summary(basictree_model))
    cat("Fold ", counter, ": ", "auc=", perf_basictree_auc, "; sensitivity=", perf_basictree_sensitivity, "; specificity=", perf_basictree_specificity, "\n")
    perf_df = rbind(perf_df, perf)
    
  }
  cat("Average : ", "auc=", mean(perf_df$auc), "; sensitivity=", mean(perf_df$sensitivity), "; specificity=", mean(perf_df$specificity), "\n")
  perf_df}


#### input data ####
## NOTE: the final processed datasets for analysis and plotting of the AUC are obtained from lines 1011 onwards

data = fread("/Volumes/JULIET/baseline_first24h/atleast25h/8452patients.23numericvariables.csv", stringsAsFactors = F)

data = data[,c(1,3:25,2)] #%>% dplyr::rename(FIRST_ADMISSION_EXPIRE_FLAG=HOSPITAL_EXPIRE_FLAG)
data = data %>% rename(FIRST_ADMISSION_EXPIRE_FLAG=HOSPITAL_EXPIRE_FLAG)
data$FIRST_ADMISSION_EXPIRE_FLAG = as.factor(data$FIRST_ADMISSION_EXPIRE_FLAG)
data=data%>% mutate(GCS_score=eye_opening+verbal_response+motor_response)
data = data[,c(1:24,26,25)]

data_cat = fread("/Volumes/JULIET/baseline_first24h/atleast25h/8452patients.12categoricalvariables.csv", stringsAsFactors = F)
data_cat = data_cat[,c(1,3:14,2)] %>% rename(FIRST_ADMISSION_EXPIRE_FLAG=HOSPITAL_EXPIRE_FLAG)
data_cat$FIRST_ADMISSION_EXPIRE_FLAG = as.factor(data_cat$FIRST_ADMISSION_EXPIRE_FLAG)
data_cat$vassopressor=as.factor(data_cat$vassopressor)
data_cat$is_ventilated=as.factor(data_cat$is_ventilated)
data_cat$is_planned=as.factor(data_cat$is_planned)
data_cat=data_cat%>%mutate(admit_loc=ifelse(admit_loc=="Outside ICU", admit_loc, "Emergency room/ ICU ward"))
data_cat$admit_loc=as.factor(data_cat$admit_loc)
data_cat$Metastatic_cancer=as.factor(data_cat$Metastatic_cancer)
data_cat$Hematologic_malignancy=as.factor(data_cat$Hematologic_malignancy)
data_cat$Cirrhosis=as.factor(data_cat$Cirrhosis)
data_cat$Heart_failure=as.factor(data_cat$Heart_failure)
data_cat$Cardiac_arrest=as.factor(data_cat$Cardiac_arrest)
data_cat$Severe_COPD=as.factor(data_cat$Severe_COPD)
data_cat$Immunocompromised=as.factor(data_cat$Immunocompromised)
data_cat$co_morbidities_grp=as.factor(data_cat$co_morbidities_grp)
data_cat$co_morbidities_grp=factor(data_cat$co_morbidities_grp, levels(data_cat$co_morbidities_grp)[c(4,1:3)])

data_all = merge(data, data_cat,by=c("HADM_ID", "FIRST_ADMISSION_EXPIRE_FLAG"))#[,c(1,3:38,2)]
data_all = data_all[,c(3:25,27:38,2)]

#### Finding optimal thresholds for numeric variables ####
temperature_threshold = seq(36,38.5,by=0.1)
temperature_threshold1 = threshold_function(data[,c(8,26)], temperature_threshold,F)
temperature_threshold1 = filter(temperature_threshold1, x2_stat > 3.84)
temperature_threshold2 = threshold_function2(data[,c(8,26)], temperature_threshold1$threshold[1], temperature_threshold1$threshold[2:nrow(temperature_threshold1)], F)
temperature_threshold2 = filter(temperature_threshold2, x2_stat > 5.99)
temperature_threshold1[1,6] < temperature_threshold2[1,8]
temperature_threshold3 = threshold_function3(data[,c(8,26)], temperature_threshold2$threshold1[1], temperature_threshold2$threshold2[1], temperature_threshold2$threshold2[2:nrow(temperature_threshold2)],F)
temperature_threshold3 = filter(temperature_threshold3, x2_stat > 7.82)
temperature_threshold2[1,8] < temperature_threshold3[1,10]

systolic_threshold = seq(80,200,by=1)
systolic_threshold1 = threshold_function(data[,c(5,26)], systolic_threshold, F)
systolic_threshold1 = filter(systolic_threshold1, x2_stat > 3.84)
systolic_threshold2 = threshold_function2(data[,c(5,26)], systolic_threshold1$threshold[1], systolic_threshold1$threshold[2:nrow(systolic_threshold1)], F)
systolic_threshold2 = filter(systolic_threshold2, x2_stat>5.99)
systolic_threshold1[1,6] < systolic_threshold2[1,8]
systolic_threshold3 = threshold_function3(data[,c(5,26)], systolic_threshold2$threshold1[1], systolic_threshold2$threshold2[1], systolic_threshold2$threshold2[2:nrow(systolic_threshold2)],F)
systolic_threshold3 = filter(systolic_threshold3, x2_stat > 7.82)
systolic_threshold2[1,8] < systolic_threshold3[1,10]

diastolic_threshold = seq(0,120,by=5)
diastolic_threshold1 = threshold_function(data[,c(6,26)], diastolic_threshold, F)
diastolic_threshold1 = filter(diastolic_threshold1, x2_stat > 3.84)
diastolic_threshold2 = threshold_function2(data[,c(6,26)], diastolic_threshold1$threshold[1], diastolic_threshold1$threshold[2:nrow(diastolic_threshold1)], F)
diastolic_threshold2 = filter(diastolic_threshold2, x2_stat>5.99)
diastolic_threshold1[1,6] < diastolic_threshold2[1,8]
diastolic_threshold3 = threshold_function3(data[,c(6,26)], diastolic_threshold2$threshold1[1], diastolic_threshold2$threshold2[1], diastolic_threshold2$threshold2[2:nrow(diastolic_threshold2)],F)
diastolic_threshold3 = filter(diastolic_threshold3, x2_stat > 7.82)
diastolic_threshold2[1,8] < diastolic_threshold3[1,10]

meanBP_threshold = seq(0,145,by=5)
meanBP_threshold1 = threshold_function(data[,c(7,26)], meanBP_threshold, F)
meanBP_threshold1 = filter(meanBP_threshold1, x2_stat > 3.84)
meanBP_threshold2 = threshold_function2(data[,c(7,26)], meanBP_threshold1$threshold[1], meanBP_threshold1$threshold[2:nrow(meanBP_threshold1)], F)
meanBP_threshold2 = filter(meanBP_threshold2, x2_stat>5.99)
meanBP_threshold1[1,6] < meanBP_threshold2[1,8]
meanBP_threshold3 = threshold_function3(data[,c(7,26)], meanBP_threshold2$threshold1[1], meanBP_threshold2$threshold2[1], meanBP_threshold2$threshold2[2:nrow(meanBP_threshold2)],F)
meanBP_threshold3 = filter(meanBP_threshold3, x2_stat > 7.82)
meanBP_threshold2[1,8] < meanBP_threshold3[1,10]

heart_threshold = seq(40,200,by=1)
heart_threshold1 = threshold_function(data[,c(9,26)], heart_threshold, F)
heart_threshold1 = filter(heart_threshold1, x2_stat > 3.84)
heart_threshold2 = threshold_function2(data[,c(9,26)], heart_threshold1$threshold[1], heart_threshold1$threshold[2:nrow(heart_threshold1)], F)
heart_threshold2 = filter(heart_threshold2, x2_stat>5.99)
heart_threshold1[1,6] < heart_threshold2[1,8]
heart_threshold3 = threshold_function3(data[,c(9,26)], heart_threshold2$threshold1[1], heart_threshold2$threshold2[1], heart_threshold2$threshold2[2:nrow(heart_threshold2)],F)
heart_threshold3 = filter(heart_threshold3, x2_stat > 7.82)
heart_threshold2[1,8] < heart_threshold3[1,10]

respir_threshold = seq(0,40,by=2)
respir_threshold1 = threshold_function(data[,c(10,26)], respir_threshold,F)
respir_threshold1 = filter(respir_threshold1, x2_stat > 3.84)
respir_threshold2 = threshold_function2(data[,c(10,26)], respir_threshold1$threshold[1], respir_threshold1$threshold[2:nrow(respir_threshold1)], F)
respir_threshold2 = filter(respir_threshold2, x2_stat>5.99)
respir_threshold1[1,6] < respir_threshold2[1,8]
respir_threshold3 = threshold_function3(data[,c(10,26)], respir_threshold2$threshold1[1], respir_threshold2$threshold2[1], respir_threshold2$threshold2[2:nrow(respir_threshold2)],F)
respir_threshold3 = filter(respir_threshold3, x2_stat > 7.82)
respir_threshold2[1,8] < respir_threshold3[1,10]

po2_threshold = seq(0,600,by=5)
po2_threshold1 = threshold_function(data[,c(11,26)],po2_threshold, T)
po2_threshold1 = filter(po2_threshold1, x2_stat > 3.84)
po2_threshold2 = threshold_function2(data[,c(11,26)], po2_threshold1$threshold[1], po2_threshold1$threshold[2:nrow(po2_threshold1)], F)
po2_threshold2 = filter(po2_threshold2, x2_stat>5.99)
po2_threshold1[1,6] < po2_threshold2[1,8]
po2_threshold3 = threshold_function3(data[,c(11,26)], po2_threshold2$threshold1[1], po2_threshold2$threshold2[1], po2_threshold2$threshold2[2:nrow(po2_threshold2)],F)
po2_threshold3 = filter(po2_threshold3, x2_stat > 7.82)
po2_threshold2[1,8] < po2_threshold3[1,10]

spo2_threshold = seq(0,100,by=2)
spo2_threshold1 = threshold_function(data[,c(12,26)], spo2_threshold,F)
spo2_threshold1 = filter(spo2_threshold1, x2_stat > 3.84)
spo2_threshold2 = threshold_function2(data[,c(12,26)], spo2_threshold1$threshold[1], spo2_threshold1$threshold[2:nrow(spo2_threshold1)], F)
spo2_threshold2 = filter(spo2_threshold2, x2_stat>5.99)
spo2_threshold1[1,6] < spo2_threshold2[1,8]
spo2_threshold3 = threshold_function3(data[,c(12,26)], spo2_threshold2$threshold1[1], spo2_threshold2$threshold2[1], spo2_threshold2$threshold2[2:nrow(spo2_threshold2)],F)
spo2_threshold3 = filter(spo2_threshold3, x2_stat > 7.82)
spo2_threshold2[1,8] < spo2_threshold3[1,10]

bicarbonate_threshold = seq(0,50,by=2)
bicarbonate_threshold1 = threshold_function(data[,c(13,26)], bicarbonate_threshold, F)
bicarbonate_threshold1 = filter(bicarbonate_threshold1, x2_stat > 3.84)
bicarbonate_threshold2 = threshold_function2(data[,c(13,26)], bicarbonate_threshold1$threshold[1], bicarbonate_threshold1$threshold[2:nrow(bicarbonate_threshold1)], F)
bicarbonate_threshold2 = filter(bicarbonate_threshold2, x2_stat>5.99)
bicarbonate_threshold1[1,6] < bicarbonate_threshold2[1,8]
bicarbonate_threshold3 = threshold_function3(data[,c(13,26)], bicarbonate_threshold2$threshold1[1], bicarbonate_threshold2$threshold2[1], bicarbonate_threshold2$threshold2[2:nrow(bicarbonate_threshold2)],F)
bicarbonate_threshold3 = filter(bicarbonate_threshold3, x2_stat > 7.82)
bicarbonate_threshold2[1,8] < bicarbonate_threshold3[1,10]

bilirubin_threshold=seq(0,44,by=0.25)
bilirubin_threshold1 = threshold_function(data[,c(14,26)], bilirubin_threshold,F)
bilirubin_threshold1 = filter(bilirubin_threshold1, x2_stat > 3.84)
bilirubin_threshold2 = threshold_function2(data[,c(14,26)], bilirubin_threshold1$threshold[1], bilirubin_threshold1$threshold[2:nrow(bilirubin_threshold1)], F)
bilirubin_threshold2 = filter(bilirubin_threshold2, x2_stat>5.99)
bilirubin_threshold1[1,6] < bilirubin_threshold2[1,8]
bilirubin_threshold3 = threshold_function3(data[,c(14,26)], bilirubin_threshold2$threshold1[1], bilirubin_threshold2$threshold2[1], bilirubin_threshold2$threshold2[2:nrow(bilirubin_threshold2)],F)
bilirubin_threshold3 = filter(bilirubin_threshold3, x2_stat > 7.82)
bilirubin_threshold2[1,8] < bilirubin_threshold3[1,10]

wbc_threshold=seq(0,100,by=0.5)
wbc_threshold1 = threshold_function(data[,c(15,26)], wbc_threshold,F)
wbc_threshold1 = filter(wbc_threshold1, x2_stat > 3.84)
wbc_threshold2 = threshold_function2(data[,c(15,26)],wbc_threshold1$threshold[1], wbc_threshold1$threshold[2:nrow(wbc_threshold1)], F)
wbc_threshold2 = filter(wbc_threshold2, x2_stat>5.99)
wbc_threshold1[1,6] < wbc_threshold2[1,8]
wbc_threshold3 = threshold_function3(data[,c(15,26)], wbc_threshold2$threshold1[1], wbc_threshold2$threshold2[1], wbc_threshold2$threshold2[2:nrow(wbc_threshold2)],F)
wbc_threshold3 = filter(wbc_threshold3, x2_stat > 7.82)
wbc_threshold2[1,8] < wbc_threshold3[1,10]

creatinine_threshold=seq(0,44,by=0.1)
creatinine_threshold1 = threshold_function(data[,c(16,26)], creatinine_threshold,F)
creatinine_threshold1 = filter(creatinine_threshold1, x2_stat > 3.84)
creatinine_threshold2 = threshold_function2(data[,c(16,26)], creatinine_threshold1$threshold[1], creatinine_threshold1$threshold[2:nrow(creatinine_threshold1)], F)
creatinine_threshold2 = filter(creatinine_threshold2, x2_stat>5.99)
creatinine_threshold1[1,6] < creatinine_threshold2[1,8]
creatinine_threshold3 = threshold_function3(data[,c(16,26)], creatinine_threshold2$threshold1[1], creatinine_threshold2$threshold2[1], creatinine_threshold2$threshold2[2:nrow(creatinine_threshold2)],F)
creatinine_threshold3 = filter(creatinine_threshold3, x2_stat > 7.82)
creatinine_threshold2[1,8] < creatinine_threshold3[1,10]

hematocrit_threshold=seq(18,65,by=1)
hematocrit_threshold1 = threshold_function(data[,c(17,26)], hematocrit_threshold,F)
hematocrit_threshold1 = filter(hematocrit_threshold1, x2_stat > 3.84)
hematocrit_threshold2 = threshold_function2(data[,c(17,26)], hematocrit_threshold1$threshold[1], hematocrit_threshold1$threshold[2:nrow(hematocrit_threshold1)], F)
creatinine_threshold2 = filter(creatinine_threshold2, x2_stat>5.99)
hematocrit_threshold1[1,6] < hematocrit_threshold2[1,8]
hematocrit_threshold3 = threshold_function3(data[,c(17,26)], hematocrit_threshold2$threshold1[1], hematocrit_threshold2$threshold2[1], hematocrit_threshold2$threshold2[2:nrow(hematocrit_threshold2)],F)
hematocrit_threshold3 = filter(hematocrit_threshold3, x2_stat > 7.82)
hematocrit_threshold2[1,8] < hematocrit_threshold3[1,10]

platelets_threshold=seq(16,150,by=1)
platelets_threshold1 = threshold_function(data[,c(18,26)], platelets_threshold,F)
platelets_threshold1 = filter(platelets_threshold1, x2_stat > 3.84)
platelets_threshold2 = threshold_function2(data[,c(18,26)], platelets_threshold1$threshold[1], platelets_threshold1$threshold[2:nrow(platelets_threshold1)], F)
platelets_threshold2 = filter(platelets_threshold2, x2_stat>5.99)
platelets_threshold1[1,6] < platelets_threshold2[1,8]
platelets_threshold3 = threshold_function3(data[,c(18,26)], platelets_threshold2$threshold1[1], platelets_threshold2$threshold2[1], platelets_threshold2$threshold2[2:nrow(platelets_threshold2)],F)
platelets_threshold3 = filter(platelets_threshold3, x2_stat > 7.82)
platelets_threshold2[1,8] < platelets_threshold3[1,10]

sodium_threshold=seq(74,172,by=1)
sodium_threshold1 = threshold_function(data[,c(19,26)], sodium_threshold,F)
sodium_threshold1 = filter(sodium_threshold1, x2_stat > 3.84)
sodium_threshold2 = threshold_function2(data[,c(19,26)], sodium_threshold1$threshold[1], sodium_threshold1$threshold[2:nrow(sodium_threshold1)], F)
sodium_threshold2 = filter(sodium_threshold2, x2_stat>5.99)
sodium_threshold1[1,6] < sodium_threshold2[1,8]
sodium_threshold3 = threshold_function3(data[,c(19,26)], sodium_threshold2$threshold1[1], sodium_threshold2$threshold2[1], sodium_threshold2$threshold2[2:nrow(sodium_threshold2)],F)
sodium_threshold3 = filter(sodium_threshold3, x2_stat > 7.82)
sodium_threshold2[1,8] < sodium_threshold3[1,10]

potassium_threshold=seq(2,7.7,by=0.1)
potassium_threshold1 = threshold_function(data[,c(20,26)], potassium_threshold,F)
potassium_threshold1 = filter(potassium_threshold1, x2_stat > 3.84)
potassium_threshold2 = threshold_function2(data[,c(20,26)], potassium_threshold1$threshold[1], potassium_threshold1$threshold[2:nrow(potassium_threshold1)], F)
potassium_threshold2 = filter(potassium_threshold2, x2_stat>5.99)
potassium_threshold1[1,6] < potassium_threshold2[1,8]
potassium_threshold3 = threshold_function3(data[,c(20,26)], potassium_threshold2$threshold1[1], potassium_threshold2$threshold2[1], potassium_threshold2$threshold2[2:nrow(potassium_threshold2)],F)
potassium_threshold3 = filter(potassium_threshold3, x2_stat > 7.82)
potassium_threshold2[1,8] < potassium_threshold3[1,10]

bun_threshold=seq(1,224,by=1)
bun_threshold1 = threshold_function(data[,c(21,26)], bun_threshold,F)
bun_threshold1 = filter(bun_threshold1, x2_stat > 3.84)
bun_threshold2 = threshold_function2(data[,c(21,26)], bun_threshold1$threshold[1], bun_threshold1$threshold[2:nrow(bun_threshold1)], F)
bun_threshold2 = filter(bun_threshold2, x2_stat>5.99)
bun_threshold1[1,6] < bun_threshold2[1,8]
bun_threshold3 = threshold_function3(data[,c(21,26)], bun_threshold2$threshold1[1], bun_threshold2$threshold2[1], bun_threshold2$threshold2[2:nrow(bun_threshold2)],F)
bun_threshold3 = filter(bun_threshold3, x2_stat > 7.82)
bun_threshold2[1,8] < bun_threshold3[1,10]

foley_threshold=seq(0,1000,by=1)
foley_threshold1 = threshold_function(data[,c(22,26)], foley_threshold, T)
foley_threshold1 = filter(foley_threshold1, x2_stat > 3.84)
foley_threshold2 = threshold_function2(data[,c(22,26)], foley_threshold1$threshold[1], foley_threshold1$threshold[2:nrow(foley_threshold1)], F)
foley_threshold2 = filter(foley_threshold2, x2_stat>5.99)
foley_threshold1[1,6] < foley_threshold2[1,8]
foley_threshold3 = threshold_function3(data[,c(22,26)], foley_threshold2$threshold1[1], foley_threshold2$threshold2[1], foley_threshold2$threshold2[2:nrow(foley_threshold2)],F)
foley_threshold3 = filter(foley_threshold3, x2_stat > 7.82)
foley_threshold2[1,8] < foley_threshold3[1,10]

glucose_threshold = seq(55,2290,by=1)
glucose_threshold1 = threshold_function(data[,c(23,26)], glucose_threshold,F)
glucose_threshold1 = filter(glucose_threshold1, x2_stat > 3.84)
glucose_threshold2 = threshold_function2(data[,c(23,26)], glucose_threshold1$threshold[1], glucose_threshold1$threshold[2:nrow(glucose_threshold1)], F)
glucose_threshold2 = filter(glucose_threshold2, x2_stat>5.99)
glucose_threshold1[1,6] < glucose_threshold2[1,8]
glucose_threshold3 = threshold_function3(data[,c(23,26)], glucose_threshold2$threshold1[1], glucose_threshold2$threshold2[1], glucose_threshold2$threshold2[2:nrow(glucose_threshold2)],F)
glucose_threshold3 = filter(glucose_threshold3, x2_stat > 7.82)
glucose_threshold2[1,8] < glucose_threshold3[1,10]

age_threshold=seq(17,90,by=1)
age_threshold1 = threshold_function(data[,c(24,26)], age_threshold,F)
age_threshold1 = filter(age_threshold1, x2_stat > 3.84)
age_threshold2 = threshold_function2(data[,c(24,26)], age_threshold1$threshold[1], age_threshold1$threshold[2:nrow(age_threshold1)], F)
age_threshold2 = filter(age_threshold2, x2_stat>5.99)
age_threshold1[1,6] < age_threshold2[1,8]
age_threshold3 = threshold_function3(data[,c(24,26)], age_threshold2$threshold1[1], age_threshold2$threshold2[1], age_threshold2$threshold2[2:nrow(age_threshold2)],F)
age_threshold3 = filter(age_threshold3, x2_stat > 7.82)
age_threshold2[1,8] < age_threshold3[1,10]

gcs_threshold = seq(3,15,by=1)
gcs_threshold1 = threshold_function(data[,25:26], gcs_threshold, F)
gcs_threshold1 = filter(gcs_threshold1, x2_stat > 3.84)
gcs_threshold2 = threshold_function2(data[,c(25,26)], gcs_threshold1$threshold[1], gcs_threshold1$threshold[2:nrow(gcs_threshold1)], F)
gcs_threshold2 = filter(gcs_threshold2, x2_stat>5.99)
gcs_threshold1[1,6] < gcs_threshold2[1,8]
gcs_threshold3 = threshold_function3(data[,c(25,26)], gcs_threshold2$threshold1[1], gcs_threshold2$threshold2[1], gcs_threshold2$threshold2[2:nrow(gcs_threshold2)],F)
gcs_threshold3 = filter(gcs_threshold3, x2_stat > 7.82)
gcs_threshold2[1,8] < gcs_threshold3[1,10]

eye_opening_threshold = seq(1,4, by=1)
eye_opening_threshold1 = threshold_function(data[,2,26], eye_opening_threshold, F)
eye_opening_threshold1 = filter(eye_opening_threshold1, x2_stat > 3.84)
gcs_threshold1 = threshold_function2(data[,c(2,26)], eye_opening_threshold1$threshold[1], eye_opening_threshold1$threshold[2:nrow(eye_opening_threshold1)], F)
gcs_threshold2 = filter(gcs_threshold2, x2_stat>5.99)
gcs_threshold1[1,6] < gcs_threshold2[1,8]
gcs_threshold3 = threshold_function3(data[,c(25,26)], gcs_threshold2$threshold1[1], gcs_threshold2$threshold2[1], gcs_threshold2$threshold2[2:nrow(gcs_threshold2)],F)
gcs_threshold3 = filter(gcs_threshold3, x2_stat > 7.82)
gcs_threshold2[1,8] < gcs_threshold3[1,10]

threshold_function = function(data, threshold, quantile_cut) {#covariate shld be univariate with 2 columns - variable obs + hospital expire flag
  variable = colnames(data)[1]
  colnames(data)[1] = "variable"
  if (quantile_cut == T){
    quantile_range = quantile(data$variable, probs = c(0.05,0.95))
    data = data %>% filter(variable>=quantile_range[1], variable <=quantile_range[2])
  }
  df = NULL
  for (thresh in threshold) {
    thresh_covariate = data %>% mutate(thresh=thresh,
                                       threshold_grp = ifelse(variable<=thresh,"below", "above"))
    cm= table(thresh_covariate$threshold_grp, thresh_covariate$FIRST_ADMISSION_EXPIRE_FLAG)
    if (nrow(filter(thresh_covariate,threshold_grp=="below"))>50 & nrow(filter(thresh_covariate,threshold_grp=="above"))>50) {
      #cm[1,1]+cm[1,2]>50 & cm[2,1]+cm[2,2]>50
      chi = chisq.test(cm, correct=F)
      pvalue = as.numeric(chi$p.value)
      x2_stat = as.numeric(chi$statistic)
      percent_death_below = cm[2,2]/sum(cm[2,1],cm[2,2])
      percent_death_above = cm[1,2]/sum(cm[1,1],cm[1,2])
      print(thresh)
      print(cm)
      df.sub = data.frame(Variable=variable, threshold=thresh, percent_death_below=percent_death_below, percent_death_above=percent_death_above,pvalue=pvalue, x2_stat=x2_stat, stringsAsFactors = F)
      df = rbind(df, df.sub)}
  }
  df = df %>% arrange(desc(x2_stat))
  df
}


threshold_function2 = function(data, threshold1,threshold2, quantile_cut){ #threshold1 is the primary threshold (numeric), threshold2 is a vector of candidate secondary thresholds
  variable = colnames(data)[1]
  colnames(data)[1] = "variable"
  if (quantile_cut == T){
    quantile_range = quantile(data$variable, probs = c(0.05,0.95))
    data = data %>% filter(variable>=quantile_range[1], variable <=quantile_range[2])
  }
  df = NULL
  for (thresh in threshold2) {
    thresh_lower = min(thresh, threshold1)
    thresh_upper = max(thresh, threshold1)
    thresh_covariate = data %>% mutate(thresh_lower=thresh_lower, thresh_upper=thresh_upper,
                                       threshold_grp = case_when(variable<thresh_lower~"below",
                                                                 variable>thresh_upper~"above",
                                                                 T~"middle"))
    cm= table(thresh_covariate$threshold_grp, thresh_covariate$FIRST_ADMISSION_EXPIRE_FLAG)
    if (nrow(filter(thresh_covariate,threshold_grp=="below"))>50 & nrow(filter(thresh_covariate,threshold_grp=="above"))>50 & nrow(filter(thresh_covariate,threshold_grp=="middle"))>50) {
      #cm[1,1]+cm[1,2]>50 & cm[2,1]+cm[2,2]>50
      chi = chisq.test(cm, correct=F)
      pvalue = as.numeric(chi$p.value)
      x2_stat = as.numeric(chi$statistic)
      percent_death_below = cm[2,2]/sum(cm[2,1],cm[2,2])
      percent_death_middle = cm[3,2]/sum(cm[3,1], cm[3,2])
      percent_death_above = cm[1,2]/sum(cm[1,1],cm[1,2])
      print(c(thresh_lower,thresh_upper))
      print(cm)
      df.sub = data.frame(Variable=variable, threshold1=threshold1, threshold2=thresh, percent_death_below=percent_death_below,percent_death_middle=percent_death_middle, percent_death_above=percent_death_above,pvalue=pvalue, x2_stat=x2_stat, stringsAsFactors = F)
      df = rbind(df, df.sub)}
  }
  df = df %>% arrange(desc(x2_stat))
  df
}


threshold_function3 = function(data, threshold1,threshold2, threshold3, quantile_cut){ #threshold1 is the primary threshold, threshold2 is the secondary threshold, and threshold3 is a vector of candidate tertiary threshold.
  variable = colnames(data)[1]
  colnames(data)[1] = "variable"
  if (quantile_cut == T){
    quantile_range = quantile(data$variable, probs = c(0.05,0.95))
    data = data %>% filter(variable>=quantile_range[1], variable <=quantile_range[2])
  }
  df = NULL
  for (thresh in threshold3) {
    thresh_lower = min(thresh, threshold1, threshold2)
    thresh_upper = max(thresh, threshold1, threshold2)
    thresh_middle = as.numeric(setdiff(c(thresh, threshold1, threshold2), c(thresh_lower, thresh_upper)))
    thresh_covariate = data %>% mutate(thresh_lower=thresh_lower, thresh_upper=thresh_upper,
                                       threshold_grp = case_when(variable<thresh_lower~"below",
                                                                 variable>thresh_upper~"above",
                                                                 variable<thresh_middle~"middle_lower",
                                                                 T~"middle_upper"))
    cm= table(thresh_covariate$threshold_grp, thresh_covariate$FIRST_ADMISSION_EXPIRE_FLAG)
    if (nrow(filter(thresh_covariate,threshold_grp=="below"))>50 & nrow(filter(thresh_covariate,threshold_grp=="above"))>50 & nrow(filter(thresh_covariate,threshold_grp=="middle_lower"))>50 & nrow(filter(thresh_covariate,threshold_grp=="middle_upper"))>50) {
      #cm[1,1]+cm[1,2]>50 & cm[2,1]+cm[2,2]>50
      chi = chisq.test(cm, correct=F)
      pvalue = as.numeric(chi$p.value)
      x2_stat = as.numeric(chi$statistic)
      percent_death_below = cm[2,2]/sum(cm[2,1],cm[2,2])
      percent_death_middle_lower = cm[3,2]/sum(cm[3,1], cm[3,2])
      percent_death_middle_upper = cm[4,2]/sum(cm[4,1], cm[4,2])
      percent_death_above = cm[1,2]/sum(cm[1,1],cm[1,2])
      print(c(thresh_lower,thresh_middle,thresh_upper))
      print(cm)
      df.sub = data.frame(Variable=variable, threshold1=threshold1, threshold2=threshold2, threshold3=thresh, percent_death_below=percent_death_below,percent_death_middle_lower=percent_death_middle_lower,percent_death_middle_upper=percent_death_middle_upper, percent_death_above=percent_death_above,pvalue=pvalue, x2_stat=x2_stat, stringsAsFactors = F)
      df = rbind(df, df.sub)}
  }
  df = df %>% arrange(desc(x2_stat))
  df
}

#### for illustration purpose only. import the threshold_df4_withcat into R directly. 
# threshold_df4 = data %>% mutate(systolic_thresh=case_when(systolicBP<80~ "<80",
#                                                           systolicBP<90~"80-90",
#                                                           systolicBP<95~"90-95",
#                                                           T~">=95"),
#                                 diastolic_thresh=case_when(diastolicBP<45~"<45",
#                                                            diastolicBP<50~"45-50",
#                                                            diastolicBP<55~"50-55",
#                                                            T~">=55"),
#                                 meanBP_thresh=case_when(meanBP<40~"<40",
#                                                         meanBP<50~"40-50",
#                                                         meanBP<55~"50-55",
#                                                         T~">=55"),
#                                 temperature_thresh=case_when(temperature<36~"<36",
#                                                              temperature<37.1~"36-37.1",
#                                                              T~">=37.1"),
#                                 heart_thresh=case_when(heart<105~"<105",
#                                                        heart<120~"105-120",
#                                                        heart<=135~"120-135",
#                                                        T~">=135"),
#                                 respir_thresh=case_when(respir<18~"<18",
#                                                         respir<20~"18-20",
#                                                         respir<24~"20-24",
#                                                         T~">=24"),
#                                 PO2_thresh=case_when(po2<=120~"<120",
#                                                      po2<125~"120-125",
#                                                      po2<155~"125-155",
#                                                      T~">=155"),
#                                 SPO2_thresh=case_when(spo2<66~"<66",
#                                                       spo2<86~"66-86",
#                                                       spo2<96~"86-96",
#                                                       T~">=96"),
#                                 bicarbonate_thresh=case_when(bicarbonate<16~"<16",
#                                                              bicarbonate<18~"16-18",
#                                                              bicarbonate<20~"18-20",
#                                                              T~">=20"),
#                                 bilirubin_thresh=case_when(bilirubin<0.75~"<0.75",
#                                                            bilirubin<1.00~"0.75-1",
#                                                            bilirubin<7.75~"1-7.75",
#                                                            T~">=7.75"),
#                                 wbc_thresh=case_when(wbc<3~"<3",
#                                                      wbc<12.5~"3-12.5",
#                                                      wbc<19.5~"12.5-19.5",
#                                                      T~">=19.5"),
#                                 creatinine_thresh=case_when(creatinine<1.3~"<1.3",
#                                                             creatinine<2.0~"1.3-2.0",
#                                                             creatinine<8.5~"2.0-8.5",
#                                                             T~">=8.5"),
#                                 hematocrit_thresh=case_when(hematocrit<31~"<31",
#                                                             hematocrit<37~"31-37",
#                                                             hematocrit<49~"37-49",
#                                                             T~">=49"),
#                                 platelets_thresh=case_when(platelets<20~"<20",
#                                                            platelets<28~"20-28",
#                                                            platelets<34~"28-34",
#                                                            T~">=34"),
#                                 sodium_thresh=case_when(sodium<129~"<129",
#                                                         sodium<134~"129-134",
#                                                         sodium<143~"134-143",
#                                                         T~">=143"),
#                                 potassium_thresh=case_when(potassium<3.4~"<3.4",
#                                                            potassium<4.6~"3.4-4.6",
#                                                            potassium<5.1~"4.6-5.1",
#                                                            T~">=5.1"),
#                                 BUN_thresh=case_when(BUN<19~"<19",
#                                                      BUN<23~"19-23",
#                                                      BUN<49~"23-49",
#                                                      T~">=49"),
#                                 foley_thresh=case_when(foley<6~"<6",
#                                                        foley<20~"6-20",
#                                                        foley<25~"20-25",
#                                                        T~">=25"),
#                                 glucose_thresh=case_when(glucose<82~"<82",
#                                                          glucose<204~"82-204",
#                                                          glucose<241~"204-241",
#                                                          T~">=241"),
#                                 age_thresh=case_when(age<47~"<47",
#                                                      age<61~"47-61",
#                                                      age<73~"61-73",
#                                                      T~">=73"))

#threshold partitions ordered in increasing mortality rate
threshold_df4_withcat = fread("/Volumes/JULIET/baseline_first24h/atleast25h/threshold_df4_withcat.csv", stringsAsFactors = F,
                              colClasses = list(character=1, numeric=c(2:25), factor=c(26:58)))
threshold_df4_withcat$systolic_thresh=factor(threshold_df4_withcat$systolic_thresh, levels(threshold_df4_withcat$systolic_thresh)[c(4,2,3,1)])
threshold_df4_withcat$diastolic_thresh=factor(threshold_df4_withcat$diastolic_thresh, levels(threshold_df4_withcat$diastolic_thresh)[c(1,4,2,3)])
threshold_df4_withcat$meanBP_thresh=factor(threshold_df4_withcat$meanBP_thresh, levels(threshold_df4_withcat$meanBP_thresh)[c(4,3,2,1)])
threshold_df4_withcat$heart_thresh=factor(threshold_df4_withcat$heart_thresh, levels(threshold_df4_withcat$heart_thresh)[c(3,1,2,4)])
threshold_df4_withcat$respir_thresh=factor(threshold_df4_withcat$respir_thresh, levels(threshold_df4_withcat$respir_thresh)[c(3,1,2,4)])
threshold_df4_withcat$temperature_thresh=factor(threshold_df4_withcat$temperature_thresh, levels(threshold_df4_withcat$temperature_thresh)[c(1,3,2)])
threshold_df4_withcat$PO2_thresh=factor(threshold_df4_withcat$PO2_thresh, levels(threshold_df4_withcat$PO2_thresh)[c(4,3,1,2)])
threshold_df4_withcat$SPO2_thresh=factor(threshold_df4_withcat$SPO2_thresh, levels(threshold_df4_withcat$SPO2_thresh)[c(2,4,1,3)])
threshold_df4_withcat$bicarbonate_thresh=factor(threshold_df4_withcat$bicarbonate_thresh, levels(threshold_df4_withcat$bicarbonate_thresh)[c(4,3,2,1)])
threshold_df4_withcat$bilirubin_thresh=factor(threshold_df4_withcat$bilirubin_thresh, levels(threshold_df4_withcat$bilirubin_thresh)[c(3,2,4,1)])
threshold_df4_withcat$wbc_thresh=factor(threshold_df4_withcat$wbc_thresh, levels(threshold_df4_withcat$wbc_thresh)[c(2,1,3,4)])
threshold_df4_withcat$creatinine_thresh=factor(threshold_df4_withcat$creatinine_thresh, levels(threshold_df4_withcat$creatinine_thresh)[c(4,1,2,3)])
threshold_df4_withcat$hematocrit_thresh=factor(threshold_df4_withcat$hematocrit_thresh, levels(threshold_df4_withcat$hematocrit_thresh)[c(2,1,3,4)])
threshold_df4_withcat$sodium_thresh=factor(threshold_df4_withcat$sodium_thresh, levels(threshold_df4_withcat$sodium_thresh)[c(2,4,3,1)])
threshold_df4_withcat$potassium_thresh=factor(threshold_df4_withcat$potassium_thresh, levels(threshold_df4_withcat$potassium_thresh)[c(1,2,3,4)])
threshold_df4_withcat$BUN_thresh=factor(threshold_df4_withcat$BUN_thresh, levels(threshold_df4_withcat$BUN_thresh)[c(3,1,2,4)])
threshold_df4_withcat$foley_thresh=factor(threshold_df4_withcat$foley_thresh, levels(threshold_df4_withcat$foley_thresh)[c(4,2,3,1)])
threshold_df4_withcat$glucose_thresh=factor(threshold_df4_withcat$glucose_thresh, levels(threshold_df4_withcat$glucose_thresh)[c(2,3,4,1)])
threshold_df4_withcat$age_thresh=factor(threshold_df4_withcat$age_thresh, levels(threshold_df4_withcat$age_thresh)[c(3,1,2,4)])
threshold_df4_withcat$is_planned=factor(threshold_df4_withcat$is_planned, levels(threshold_df4_withcat$is_planned)[c(2,1)])
threshold_df4_withcat$co_morbidities_grp=factor(threshold_df4_withcat$co_morbidities_grp, levels(threshold_df4_withcat$co_morbidities_grp)[c(4,1,2,3)])
threshold_df4_withcat$admit_loc=factor(threshold_df4_withcat$admit_loc, levels(threshold_df4_withcat$admit_loc)[c(2,1)])

#dummy vars
threshold_df4_withcat_dmy = threshold_df4_withcat[,c(27:58)]
threshold_df4_dummy = dummyVars("~.", data=threshold_df4_withcat_dmy, fullRank=T)
threshold_df4_dummy = data.frame(predict(threshold_df4_dummy, newdata=threshold_df4_withcat_dmy))
threshold_df4_withcat_dmy=cbind(threshold_df4_dummy, threshold_df4_withcat[,c(2:4,26)]) #dummy var version

logit_partition_num_withcat = logistics_function(threshold_df4_withcat_dmy[,c(1:66,68:70,73:77)], 0.5,50)
logit_partition_num_withcat = logistics_function(threshold_df4_withcat_dmy[,c(1:66,68:70,77)], 0.5,50)

logit_partition_num_withcat_full_model4 = upSample(x=threshold_df4_withcat_dmy[,c(1:76)], y=threshold_df4_withcat_dmy$FIRST_ADMISSION_EXPIRE_FLAG)
#logit_partition_num_withcat_full_model2 = glm(Class~., logit_partition_num_withcat_full_model2, family="binomial", control=list(maxit=50))
#summary(logit_partition_num_withcat_full_model2)
library(My.stepwise)
logit_step.model4 = My.stepwise::My.stepwise.glm(Y="Class", variable.list = colnames(logit_partition_num_withcat_full_model4)[c(1:66,68:70,73:76)], data=logit_partition_num_withcat_full_model4, myfamily = "binomial")

threshold_df_stepwise_var = threshold_df4_withcat_dmy %>% dplyr::select(bilirubin_thresh.0.75.1    ,
                                                                        vassopressor.TRUE      ,
                                                                        Severe_COPD.TRUE         ,
                                                                        PO2_thresh.120.125        ,
                                                                        eye_opening          ,
                                                                        is_planned.FALSE        ,
                                                                        Cardiac_arrest.TRUE          ,
                                                                        PO2_thresh..120         ,
                                                                        age_thresh...73          ,
                                                                        bilirubin_thresh...7.75    ,
                                                                        bilirubin_thresh.1.7.75      ,
                                                                        foley_thresh..6            ,
                                                                        Hematologic_malignancy.TRUE    ,
                                                                        diastolic_thresh..45       ,
                                                                        respir_thresh...24         ,
                                                                        motor_response         ,
                                                                        BUN_thresh...49        ,
                                                                        admit_loc.Emergency.room..ICU.ward  ,
                                                                        foley_thresh.20.25        ,
                                                                        diastolic_thresh.50.55    ,
                                                                        diastolic_thresh...55     ,
                                                                        PO2_thresh.125.155    ,
                                                                        age_thresh.61.73    ,
                                                                        wbc_thresh...19.5        ,
                                                                        wbc_thresh.12.5.19.5       ,
                                                                        systolic_thresh.90.95     ,
                                                                        sodium_thresh..129     ,
                                                                        SPO2_thresh...96    ,
                                                                        age_thresh.47.61   ,
                                                                        meanBP_thresh.50.55  ,
                                                                        temperature_thresh..36  ,
                                                                        platelets_thresh.28.34  ,
                                                                        is_ventilated.TRUE,
                                                                        respir_thresh.18.20  ,
                                                                        respir_thresh.20.24 ,
                                                                        Cirrhosis.TRUE  ,
                                                                        potassium_thresh...5.1 ,
                                                                        wbc_thresh..3   ,
                                                                        foley_thresh.6.20 ,
                                                                        platelets_thresh...34 ,
                                                                        verbal_response   , 
                                                                        platelets_thresh..20  ,
                                                                        meanBP_thresh.40.50 ,
                                                                        temperature_thresh...37.1  ,
                                                                        SPO2_thresh.66.86  ,
                                                                        heart_thresh.120.135 ,
                                                                        systolic_thresh.80.90 ,
                                                                        systolic_thresh..80 ,
                                                                        meanBP_thresh..40 ,
                                                                        SPO2_thresh..66 ,
                                                                        hematocrit_thresh...49 ,  
                                                                        Metastatic_cancer.TRUE , 
                                                                        glucose_thresh...241 , 
                                                                        glucose_thresh.204.241,  
                                                                        sodium_thresh.129.134 , 
                                                                        potassium_thresh..3.4 ,  
                                                                        heart_thresh.105.120 ,
                                                                        FIRST_ADMISSION_EXPIRE_FLAG) 

logit_partition_num_withcat_reduced_model4 = upSample(x=threshold_df_stepwise_var[,1:(ncol(threshold_df_stepwise_var)-1)] , y=threshold_df_stepwise_var$FIRST_ADMISSION_EXPIRE_FLAG)
logit_partition_num_withcat_reduced_model4 = glm(Class~., logit_partition_num_withcat_reduced_model4, family="binomial", control=list(maxit=50))
summary(logit_partition_num_withcat_reduced_model4)
View(varImp(logit_partition_num_withcat_reduced_model4))
logistic.display(logit_partition_num_withcat_reduced_model4)

threshold_df_stepwise_logit4 = threshold_df4_withcat_dmy  %>% mutate(intercept = -6.29014,
                                                                     eye_opening_thresh=eye_opening*(-0.41461),
                                                                     bilirubin_thresh=case_when(bilirubin_thresh.0.75.1==1~2.89520,
                                                                                                bilirubin_thresh...7.75==1~2.03523,
                                                                                                bilirubin_thresh.1.7.75==1~ 0.69242,
                                                                                                T~0),
                                                                     vasopressor_thresh=0.74001*vassopressor.TRUE,
                                                                     PO2_thresh=case_when(PO2_thresh.120.125==1~4.23804,
                                                                                          PO2_thresh..120==1~0.93580,
                                                                                          PO2_thresh.125.155==1~0.58886,
                                                                                          T~0),
                                                                     Severe_COPD_thresh=0.85747*Severe_COPD.TRUE,
                                                                     is_planned_thresh=1.37918*is_planned.FALSE,
                                                                     Cardiac_arrest_thresh=2.13783*Cardiac_arrest.TRUE,
                                                                     age_thresh=case_when(age_thresh...73==1~1.82696,
                                                                                          age_thresh.47.61==1~0.65465,
                                                                                          age_thresh.61.73==1~0.98743,
                                                                                          T~0),
                                                                     diastolic_thresh=case_when(diastolic_thresh...55==1~0.66162,
                                                                                                diastolic_thresh..45==1~1.29462,
                                                                                                diastolic_thresh.50.55==1~0.82218,
                                                                                                T~0),
                                                                     Hematologic_malignancy_thresh=0.79106*Hematologic_malignancy.TRUE,
                                                                     foley_thresh=case_when(foley_thresh..6==1~ 0.86566,
                                                                                            foley_thresh.20.25==1~0.58092,
                                                                                            foley_thresh.6.20==1~0.28029,
                                                                                            T~0),
                                                                     BUN_thresh=case_when(BUN_thresh...49==1~0.49198,
                                                                                          T~0),
                                                                     respir_thresh=case_when(respir_thresh.18.20==1~0.53167,
                                                                                             respir_thresh...24==1~0.89472,
                                                                                             respir_thresh.20.24==1~ 0.45007,
                                                                                             T~0),
                                                                     SPO2_thresh=case_when(SPO2_thresh...96 ==1~0.49267,
                                                                                           SPO2_thresh.66.86==1~ 0.32137,
                                                                                           SPO2_thresh..66==1~0.70491,
                                                                                           T~0),
                                                                     systolic_thresh=case_when(systolic_thresh.90.95==1~0.63199,
                                                                                               systolic_thresh..80==1~ 0.38861,
                                                                                               systolic_thresh.80.90==1~0.32199,
                                                                                               T~0),
                                                                     motor_response_thresh=-0.1855*motor_response,
                                                                     wbc_thresh=case_when(wbc_thresh...19.5==1~0.81632,
                                                                                          wbc_thresh.12.5.19.5==1~0.41386,
                                                                                          wbc_thresh..3==1~0.60184,
                                                                                          T~0),
                                                                     temperature_thresh=case_when(temperature_thresh..36==1~0.30356,
                                                                                                  temperature_thresh...37.1 ==1~0.34310,
                                                                                                  T~0),
                                                                     admit_loc_thresh=admit_loc.Emergency.room..ICU.ward*0.61225,
                                                                     potassium_thresh=case_when(potassium_thresh...5.1==1~0.43052,
                                                                                                potassium_thresh..3.4==1~0.19955,
                                                                                                T~0),
                                                                     sodium_thresh=case_when(sodium_thresh..129==1~-0.72771,
                                                                                             sodium_thresh.129.134  ==1~0.14478,
                                                                                             T~0),
                                                                     platelets_thresh=case_when(platelets_thresh..20==1~0.49081,
                                                                                                platelets_thresh.28.34==1~-0.33701,
                                                                                                platelets_thresh...34==1~-0.27353,
                                                                                                T~0),
                                                                     glucose_thresh=case_when(glucose_thresh...241==1~0.17071,
                                                                                              glucose_thresh.204.241==1~0.21752 ,
                                                                                              T~0),
                                                                     Metastatic_cancer_thresh=Metastatic_cancer.TRUE*0.48827,
                                                                     Cirrhosis_thresh=Cirrhosis.TRUE*0.76393,
                                                                     meanBP_thresh=case_when(meanBP_thresh.50.55==1~-0.72236,
                                                                                             meanBP_thresh.40.50==1~-0.60798,
                                                                                             meanBP_thresh..40==1~-0.55085,
                                                                                             T~0),
                                                                     is_ventilated_thresh=is_ventilated.TRUE*0.54973,
                                                                     verbal_response_thresh=verbal_response* (-0.10808),
                                                                     hematocrit_thresh=case_when(hematocrit_thresh...49==1~-0.42810,
                                                                                                 T~0),
                                                                     heart_thresh=case_when(heart_thresh.120.135==1~0.1727,
                                                                                            heart_thresh.105.120==1~ 0.11356,
                                                                                            T~0))

logit_score4 = rowSums(threshold_df_stepwise_logit4[,78:108]) 
logit_score4 = cbind(threshold_df_stepwise_logit4[,1:77], logit_score4) %>% mutate(predicted_mortality_risk = exp(logit_score4)/(1+exp(logit_score4)),
                                                                                   predicted_class = ifelse(predicted_mortality_risk>=0.5, 1,0))

glm_confusion_mat = confusionMatrix(logit_score4$predicted_class, logit_score4$FIRST_ADMISSION_EXPIRE_FLAG,
                                    positive="1") #positive: dead

pred_glm = prediction(predictions=logit_score4$predicted_mortality_risk, labels=logit_score4$FIRST_ADMISSION_EXPIRE_FLAG)
perf_glm = performance(pred_glm,measure="tpr", x.measure="fpr")
perf_glm_auc = performance(pred_glm, measure="auc")
perf_glm_auc = unlist(perf_glm_auc@y.values)
perf_glm_sensitivity = glm_confusion_mat[["byClass"]][["Sensitivity"]]
perf_glm_specificity = glm_confusion_mat[["byClass"]][["Specificity"]]                                                                    
plot(perf_glm, main="ROC CURVE (+ve class: In-hospital death)", col="blue", lwd=2)
abline(a=0,b=1,lty=2)
text(x=0.8, y=0.2, labels=paste("AUC = ", round(perf_glm_auc,3)), col="blue")
text(x=0.8, y=0.15, labels=paste("Sensitivity = ", round(perf_glm_sensitivity,3)), col="darkgreen")
text(x=0.8, y=0.1, labels=paste("Specificity = ", round(perf_glm_specificity,3)), col="red")                                                                                          


#### Plotting the SAPS II, APACHE II, SOFA and new method AUC on the same AUC graph ####
library(ggplot2)

SAPSII_results = fread("/Volumes/JULIET/baseline_first24h/atleast25h/baseline_SAPII_circulatory.disease_8310patients.csv")

SAPSII_results = SAPSII_results %>% mutate(logit=-7.7631 + 0.0737*Total_score + 0.9971 * log(Total_score+1),
                                           risk=exp(logit)/(1+exp(logit)))

ggplot(SAPSII_results, aes(x=as.factor(FIRST_ADMISSION_EXPIRE_FLAG), y=Total_score, fill=FIRST_ADMISSION_EXPIRE_FLAG)) + geom_boxplot() +
  ggtitle("SAPS II Total Score by First Admission Mortality Status") + xlab("Mortality status") + ylab("SAPS II Total score")

summary(filter(SAPSII_results, FIRST_ADMISSION_EXPIRE_FLAG==0)$Total_score)
summary(filter(SAPSII_results, FIRST_ADMISSION_EXPIRE_FLAG==1)$Total_score)

APACHEII_results = fread("/Volumes/JULIET/baseline_first24h/atleast25h/baseline_APACHEII_circulatory.disease_8282patients.csv")
APACHEII_diagnostic_cat_weight = fread("/Volumes/JULIET/baseline_first24h/atleast25h/APACHEII_diagnostic_category_weight.csv", stringsAsFactors = F)
APACHEII_is.emergency.admit.surgery = fread("/Volumes/JULIET/baseline_first24h/atleast25h/APACHEII_is.emergency.admit.surgery.csv", stringsAsFactors = F)
APACHEII_results=merge(APACHEII_results, APACHEII_diagnostic_cat_weight[,c(1,9)], by="HADM_ID")
APACHEII_results=merge(APACHEII_results, APACHEII_is.emergency.admit.surgery, by="HADM_ID")
APACHEII_results=APACHEII_results %>% mutate(logit=-3.517+0.146*Total_score+0.603*is.emergency.surgery+diagnostic_category_weight,
                                             risk=exp(logit)/(1+exp(logit)))

ggplot(APACHEII_results, aes(x=as.factor(FIRST_ADMISSION_EXPIRE_FLAG), y=Total_score, fill=FIRST_ADMISSION_EXPIRE_FLAG)) + geom_boxplot() +
  ggtitle("APACHE II Total Score by First Admission Mortality Status") + xlab("Mortality status") + ylab("APACHE II Total score")

summary(filter(APACHEII_results, FIRST_ADMISSION_EXPIRE_FLAG==0)$Total_score)
summary(filter(APACHEII_results, FIRST_ADMISSION_EXPIRE_FLAG==1)$Total_score)

SOFA_results = fread("/Volumes/JULIET/baseline_first24h/atleast25h/baseline_SOFA_circulatory.disease_10047patients.csv")
SOFA_results = SOFA_results %>% mutate(risk = case_when(Total_score <= 6~"<10%",
                                                        Total_score <= 9~"15-20%",
                                                        Total_score <= 12~"40-50%",
                                                        Total_score <=14~"50-60%",
                                                        Total_score == 15~">=80%",
                                                        T~">90%"))

ggplot(SOFA_results, aes(x=as.factor(FIRST_ADMISSION_EXPIRE_FLAG), y=Total_score, fill=FIRST_ADMISSION_EXPIRE_FLAG)) + geom_boxplot() +
  +   ggtitle("SOFA Total Score by First Admission Mortality Status") + xlab("Mortality status") + ylab("SOFA Total score")

summary(filter(SOFA_results, FIRST_ADMISSION_EXPIRE_FLAG==0)$Total_score)
summary(filter(SOFA_results, FIRST_ADMISSION_EXPIRE_FLAG==1)$Total_score)


logit_results = fread("/Volumes/JULIET/slides and draft report/stepwise_logitscore.csv")

library(pROC)
SAPSII_results$FIRST_ADMISSION_EXPIRE_FLAG = as.factor(SAPSII_results$FIRST_ADMISSION_EXPIRE_FLAG)
SAPS_roc = pROC::roc(SAPSII_results$FIRST_ADMISSION_EXPIRE_FLAG~SAPSII_results$risk)
APACHE_roc = pROC::roc(APACHEII_results$FIRST_ADMISSION_EXPIRE_FLAG~APACHEII_results$risk)
SOFA_roc = pROC::roc(SOFA_results$FIRST_ADMISSION_EXPIRE_FLAG~SOFA_results$Total_score)
logit_roc = pROC::roc(logit_results$FIRST_ADMISSION_EXPIRE_FLAG~logit_results$predicted_mortality_risk)
multiple_roc_plots = plot.roc(SAPS_roc, print.auc=T, col="red", main="ROC Curve for SAPSII, APACHEII and New Method on sample")
multiple_roc_plots = plot(APACHE_roc, print.auc=T, col="blue", print.auc.y=0.4, add=T)
multiple_roc_plots = plot(SOFA_roc, print.auc=T, col="black", print.auc.y=0.3, add=T)
multiple_roc_plots = plot(logit_roc, print.auc=T, col="purple", print.auc.y=0.2, add=T)

coords(SAPS_roc, x="best", ret=c("specificity", "sensitivity", "accuracy", "threshold", "tn", "tp", "fn", "fp", best.method="c"))
coords(APACHE_roc, x="best", ret=c("specificity", "sensitivity", "accuracy", "threshold", "tn", "tp", "fn", "fp", best.method="c"))
coords(SOFA_roc, x="best", ret=c("specificity", "sensitivity", "accuracy", "threshold", "tn", "tp", "fn", "fp", best.method="c"))
coords(logit_roc, x="best", ret=c("specificity", "sensitivity", "accuracy", "threshold", "tn", "tp", "fn", "fp", best.method="c"))
