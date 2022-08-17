last_patient_time=0 #initially setting the time of the last patient as 0
patient_count=0 #initializing the patient count to 0
closing_time=420 # the closing time of office i.e(7*60)
total_wait=0 #total waiting time
waited=0 #number of patients who waited 
doc_gets_free=c(0,0,0) #vector to store the time at what time the doctor gets free
#loop until the patient enters after 420 minutes from the opening i.e before 4PM
while(TRUE){
  rn_exp <- rexp(1, 1/10) #generating a random exponential number to decide a patients arrival time
  last_patient_time<-last_patient_time+rn_exp #adjusting the last patient entry time by adding the exponetial value
  #checking if the arrival time of last patient is after 4pm
  if(closing_time<last_patient_time){ 
    break #breaking out of the while loop
  }
  patient_count=patient_count+1 #increment the patient_count for the particular simulation
  next_available_doc=min(doc_gets_free) #choosing the doctor with the minimum remaining time to get free
  #time for patient waits is either 0, in case of no wait
  #or the difference between the next available doctors time and patients arrival time 
  waitTime=max(0,next_available_doc-last_patient_time) 
  total_wait=total_wait+waitTime #adding the wait time to total wait for the simulation
  #looping for each of the three doctors 
  for(doc in 1:length(doc_gets_free)){
    # checking for the next available doctor
    if(doc_gets_free[doc]==next_available_doc){
      consultTime=runif(1, min = 5, max = 20)  #generating random uniform number as the consultation time
      #checking if the patient had to wait 
      if(waitTime>0){
        waited=waited+1 #increment the waited counter by 1 for the simulation 
        doc_gets_free[doc]=next_available_doc+consultTime #updating the doc_gets_free vector by adding the time in which doc will get free
      }
      #if patient didn't had to wait
      else{
        doc_gets_free[doc]=last_patient_time+consultTime #updating the doc_gets_free vector by adding the consult time to last patients time
        break #breaking out of the for loop
      } 
    }
    
  }
  
}
patient_count #total number of patients
waited #total number of patients who had to wait
total_wait/patient_count #average waiting time for patients
total_wait/waited #average waiting time for patients who waited
max(doc_gets_free,closing_time) #time at which the office closes



##Question 1(b)


patients_per_simulation=c() #stores the total number of patients for each simulation.
patients_Waited=c() #vector to store the number of patients who waited. 
average_waiting=c() #vector to store the average waiting time per patient.
average_waiting_waited=c() #vector to store the average waiting time per patient.
office_close=c() #vector to store the time at which the office closed
#looping 1000 times for 1000 simulations
for(i in 1:1000){
  #the value of these variables are reinitialized for each simulation
  last_patient_time=0 #initially setting the time of the last patient as 0
  patient_count=0 #initializing the patient count to 0
  closing_time=420 # the closing time of office i.e(7*60)
  total_wait=0 #total waiting time
  waited=0 #number of patients who waited 
  doc_gets_free=c(0,0,0) #vector to store the time at what time the doctor gets free
  #loop until the patient enters after 420 minutes from the opening i.e before 4PM
  while(TRUE){
    rn_exp <- rexp(1, 1/10) #generating a random exponential number to decide a patients arrival time
    last_patient_time<-last_patient_time+rn_exp #adjusting the last patient entry time by adding the exponetial value
    #checking if the arrival time of last patient is after 4pm
    if(closing_time<last_patient_time){ 
      break #breaking out of the while loop
    }
    patient_count=patient_count+1 #increment the patient_count for the particular simulation
    next_available_doc=min(doc_gets_free) #choosing the doctor with the minimum remaining time to get free
    #time for patient waits is either 0, in case of no wait
    #or the difference between the next available doctors time and patients arrival time 
    waitTime=max(0,next_available_doc-last_patient_time) 
    total_wait=total_wait+waitTime #adding the wait time to total wait for the simulation
    #looping for each of the three doctors 
    for(doc in 1:length(doc_gets_free)){
      # checking for the next available doctor
      if(doc_gets_free[doc]==next_available_doc){
        consultTime=runif(1, min = 5, max = 20)  #generating random uniform number as the consultation time
        #checking if the patient had to wait 
        if(waitTime>0){
          waited=waited+1 #increment the waited counter by 1 for the simulation 
          doc_gets_free[doc]=next_available_doc+consultTime #updating the doc_gets_free vector by adding the time in which doc will get free
        }
        #if patient didn't had to wait
        else{
          doc_gets_free[doc]=last_patient_time+consultTime #updating the doc_gets_free vector by adding the consult time to last patients time
          break #breaking out of the for loop
        } 
      }
      
    }
    
  }
  
  patients_per_simulation[i]=patient_count #storing the total number of patients for the simulation in vector
  average_waiting[i]=total_wait/patient_count #storing the average waiting time for patients for the simulation in vector
  average_waiting_waited[i]=total_wait/waited #storing the average waiting time for patients for the simulation in vector
  patients_Waited[i]=waited #storing the total number of patients who had to wait for the simulation in vector
  office_close[i]=max(doc_gets_free,closing_time) #storing the time at which the office closes for the simulation in vector
}
median=1000/2 #to get the median values for the 
#sorting the vectors from lowest to highest
patients_per_simulation=sort(patients_per_simulation) 
average_waiting=sort(average_waiting)
average_waiting_waited=sort(average_waiting_waited)
patients_Waited=sort(patients_Waited)
office_close=sort(office_close)
#getting the median values
patients_per_simulation[median] #median value for patient count
patients_Waited[median] #median value for number of patients who waited
average_waiting[median] # median value for average waiting time per patient
average_waiting_waited[median] # median value for average waiting time per patient who waited
office_close[median] #median value for office closing time
#plotting the histograms for the sorted vectors
hist(patients_per_simulation,breaks = 100)
hist(patients_Waited,breaks = 100)
hist(average_waiting,breaks = 100)
hist(average_waiting_waited,breaks = 100)
hist(office_close,breaks = 100)


