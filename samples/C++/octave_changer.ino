const int buttons[4] = {2,3,4,5};
const int octaves[2] = {6,7};

void setup() {                
  // initialize the digital pin as an output.
  // Pin 13 has an LED connected on most Arduino boards:

  pinMode(13,OUTPUT);

 for(int i =0;i<sizeof(buttons)/sizeof(int);i++){
   pinMode(buttons[i],INPUT );
 }
 
 for(int i =0;i<sizeof(octaves)/sizeof(int);i++){
   pinMode(octaves[i],INPUT );
 }
 
  Serial.begin(9600);
}


void loop() {
  delay(1);              // wait
  int output = -1;
  
 // Serial.print(digitalRead(buttons[0]));
  
 for(int i =0;i<sizeof(buttons)/sizeof(int);i++){
   if(digitalRead(buttons[i])==LOW
   ){
     if(output<=0){
       output=1;
     }
     output+=i+1;
   }
 }
 
 for(int i =0;i<sizeof(octaves)/sizeof(int);i++){
    if(output<=0){
       break;
     }
   if(digitalRead(octaves[i])==LOW
   ){
     output*=7*(i==1 ? -1 : 1);
   }
 }
  if(output>=0){
  Serial.print(output);
  Serial.println(";");
  digitalWrite(13,HIGH);
  }else{
  digitalWrite(13,LOW);
  }
  
}
