;*****************************************************************
;* This stationery serves as the framework for a                 *
;* user application (single file, absolute assembly application) *
;* For a more comprehensive program that                         *
;* demonstrates the more advanced functionality of this          *
;* processor, please see the demonstration applications          *
;* located in the examples subdirectory of the                   *
;* Freescale CodeWarrior for the HC12 Program directory          *
;*****************************************************************

; export symbols
            XDEF Entry, _Startup            ; export 'Entry' symbol
            ABSENTRY Entry        ; for absolute assembly: mark this as application entry point



; Include derivative-specific definitions 
		INCLUDE 'derivative.inc' 

ROMStart    EQU  $4000  ; absolute address to place my code/constant data

; variable/data section

            ORG RAMStart
 ; Insert here your data definition.
Counter     DS.B 1
Dont_Run    DS.B 1
Turn_Var    DS.B 1

; code section
            ORG   ROMStart


Entry:
_Startup:
            LDS   #RAMEnd+1       ; initialize the stack pointer
            CLR    Counter
            CLR    Dont_Run
            CLRB
            CLR    Turn_Var
            TSTB   
            BNE    WAIT   
            SEI
            BCLR   DDRA,%11
           
            BSET   PWME,%00111111      ;Enables pins PP0,PP1,PP4,PP5 for PWM for motors, and Enables pins PP2 and PP3 for PWM timer
            BSET   PWMCTL,%01010000    ;Concatenates pins PP0 and PP1 and PP4 and PP5 to create 16 bit PWM channel
            BSET   PWMCLK,%110011      ;Makes Pins PP0,PP1,PP4,PP5 use Clock SA
            BCLR   PWMCAE,%110011      ;Left aligns PWM output
            BSET   PWMSCLA,%00001010   ;Makes the scale for Clock SA to be 10 so that (bus clock)/(2*Clock SA scale) equal to 100 kHz
            BSET   PWMPOL,%100010      ;Gives Polarity of PP1 and PP5 to be high at beginning of period and low when duty cycle reached
            MOVW   #132,PWMDTY0        ;Sets PWM Duty as 140   LEFT WHEEL
            MOVW   #1800,PWMPER0       ;Sets PWM Period as 1800
            MOVW   #159,PWMDTY4        ;Sets PWM Duty as 140   RIGHT WHEEL
            MOVW   #1800,PWMPER4       ;Sets PWM Period as 1800

            ;BSET   PWME,%1100          ;Enables pins PP2 and PP3 for PWM
            BSET   PWMCLK,%1100        ;Makes Pins PP2 and PP3 use Clock SA
            BCLR   PWMCAE,%1100        ;Left aligns PWM output
            BSET   PWMPRCLK,%01110000  ;scales Clock B
            BSET   PWMSCLB,%00000001   ;Makes the scale for Clock SB to be 30 so that (bus clock)/(2*Clock SA scale) equal to 100 kHz
            BSET   PWMPOL,%1000        ;Gives Polarity of PP1 to be high at beginning of period and low when duty cycle reached
            
            LDAB  #1
            
            CLI
WAIT        BRSET PORTA,%1,TR
            BRCLR PORTA,%10,TL         ;PA1 = front , PA0 = right
            BRA   WAIT
TR          BSR   TRN_RGHT                                     ;LEFT WHEEL STOP : 9A, lower for straight   , RIGHT WHEEL STOP  :  96, Higher for straight
            BRA   WAIT      
TL          BSR   TRN_LFT
            BRA   WAIT
            
            
TRN_RGHT    LDAA  Dont_Run
            TSTA  
            BNE   ENDSR1              ;                                             TEST 'A' CODE
             
            LDAB   Turn_Var
            INCB   
            STAB   Turn_Var
            ;;;;MOVW   #$9A,PWMDTY0      ;Sets PWM Duty as 140
            ;;;;MOVW   #1800,PWMPER0     ;Sets PWM Period as 1800                      TURN RIGHT CODE
            ;MOVW   #143,PWMDTY4      ;Sets PWM Duty as 140
            ;MOVW   #1800,PWMPER4     ;Sets PWM Period as 1800

            ;MOVB   #127,PWMDTY2      
            ;MOVB   #255,PWMPER2       ;                                             PWM TIMER PULSE
            MOVB   #127,PWMDTY3      
            MOVB   #255,PWMPER3     
            
            MOVB  #$80,TSCR1         ;turns on the timer and enables fast flag clearing
            MOVB  #$1,TSCR2          ;prescales clock by 2
            BCLR  TIOS,%1           ;sets channel 0 for input compare
            BCLR  TCTL4,%01        ;channel 0 captures on falling edges
            BSET  TCTL4,%10        
            LDAA  #1
            STAA  Dont_Run
            BSET  TIE,%1            ;Enables interrupt for channel 0 
                                                                                                                                                           ;
ENDSR1      RTS



TRN_LFT     LDAA  Dont_Run
            TSTA  
            BNE   ENDSR2
            
            ;MOVW   #138,PWMDTY0      ;Sets PWM Duty as 140    LEFT WHEEL
            ;MOVW   #1800,PWMPER0     ;Sets PWM Period as 1800
            MOVW   #$96,PWMDTY4      ;Sets PWM Duty as 140    RIGHT WHEEL
            MOVW   #1800,PWMPER4     ;Sets PWM Period as 1800
            
            ;MOVB   #127,PWMDTY2      
            ;MOVB   #255,PWMPER2       ;                                             PWM TIMER PULSE
            MOVB   #127,PWMDTY3      
            MOVB   #255,PWMPER3     
            
            MOVB  #$80,TSCR1         ;turns on the timer and enables fast flag clearing
            MOVB  #$1,TSCR2          ;prescales clock by 2
            BCLR  TIOS,%1           ;sets channel 0 for input compare
            BCLR  TCTL4,%01        ;channel 0 captures on falling edges
            BSET  TCTL4,%10        
            LDAA  #1
            STAA  Dont_Run            
            BSET  TIE,%1           ;Enables interrupt for channel 0        
            
ENDSR2      RTS


ISR1        BSET   TFLG1,%1
            
            INC    Counter
            LDAB   Counter
            
            CMPB    #27
            BEQ    TRN_NOW
            CMPB    #77
            BEQ    GO_STRGHT
            CMPB    #115
            BEQ    CLR_CNTR
            
            BRA    ENDISR

TRN_NOW     LDAB   Turn_Var
            TSTB   
            BEQ    TR_LT
            MOVW   #$9A,PWMDTY0      ;Sets PWM Duty as 140
            MOVW   #1800,PWMPER0     ;Sets PWM Period as 1800 
            BRA    ENDISR
TR_LT       INC    Counter
            LDAB   Counter
            CMPB   #59
            BLT    TR_LT           
                                
            BRA    ENDISR
              
GO_STRGHT   MOVW   #132,PWMDTY0      ;Sets PWM Duty as 140   RIGHT WHEEL
            MOVW   #1800,PWMPER0     ;Sets PWM Period as 1800
            MOVW   #159,PWMDTY4      ;Sets PWM Duty as 140   LEFT WHEEL
            MOVW   #1800,PWMPER4     ;Sets PWM Period as 1800
            BRA    ENDISR

CLR_CNTR    CLR    Counter    
            CLRA
            CLR    Dont_Run
            CLR    Turn_Var
            BCLR  TIE,%1            ;Enables interrupt for channel 0 and 1
            
            
ENDISR      RTI










            ORG   $FFEE               ;installs interrupt for timer channel 0
            DC.W  ISR1                ;names interrupt
            
;**************************************************************
;*                 Interrupt Vectors                          *
;**************************************************************
            ORG   $FFFE
            DC.W  Entry           ; Reset Vector








;ISR1        BSET   TFLG1,%11
 ;           BRSET  PTP,%11,TRN_LFT     ;
 ;           BRSET  PTP,%01,TRN_RGHT  ;Front Wall Sensor
 ;           BRSET  PTP,%10,GO_STRGHT ;Right Wall Sensor

;TRN_LEFT    MOVW   #138,PWMDTY0      ;Sets PWM Duty as 140    LEFT WHEEL
;            MOVW   #1800,PWMPER0     ;Sets PWM Period as 1800
;            MOVW   #$96,PWMDTY2      ;Sets PWM Duty as 140    RIGHT WHEEL
;            MOVW   #1800,PWMPER2     ;Sets PWM Period as 1800

            
;            BRA    ENDISR


;TRN_RGHT    MOVW   #$9A,PWMDTY0      ;Sets PWM Duty as 140
;            MOVW   #1800,PWMPER0     ;Sets PWM Period as 1800
;            MOVW   #143,PWMDTY2      ;Sets PWM Duty as 140
;            MOVW   #1800,PWMPER2     ;Sets PWM Period as 1800

            
;            BRA    ENDISR
            
;GO_STRGHT   MOVW   #138,PWMDTY0      ;Sets PWM Duty as 140
;            MOVW   #1800,PWMPER0     ;Sets PWM Period as 1800
 ;           MOVW   #143,PWMDTY2      ;Sets PWM Duty as 140
  ;          MOVW   #1800,PWMPER2     ;Sets PWM Period as 1800


   ;         BRA    ENDISR            
            
;ENDISR      RTI
