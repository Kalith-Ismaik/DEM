!NORDSIECK INTEGRATOR (FIFTH-ORDER PREDICTOR-CORRECTOR ALGORITHM).
!KINETIC ENERGY IS CALCULATED HERE AS WELL.
!THIS SUBROUTINE IS WRITTEN BY KALITH M ISMAIL, NOVEMBER-2022, NUS SINGAPORE.

    SUBROUTINE Nord5()

        INCLUDE 'common.h'

        trans_loop: DO I=1,NAN3 

            P1=Q2(I)                                                  
            P2=Q3(I)                                                
            P3=P2+P2+P2                                     
            P4=Q4(I)                                      
            P5=P4+P4                                    
            P6=P5+P5                                  
            P7=Q5(I)                                
            P8=(P7+P7+P7)+(P7+P7)                 
            P9=P8+P8                            
            X(I)=P7+P4+P2+P1+Q1(I)+X(I)       
            Q1(I)=P8+P6+P3+P1+P1+Q1(I)      
            Q2(I)=P9+P6+P5+P3+P1          
            Q3(I)=P9+P6+P2        
            Q4(I)=P8+P4         
        END DO trans_loop
        
        CALL Forces()
        
        QIN(1:NAN) = 0.0d0
        FO(1:NAN3) = F(1:NAN3)

        trans2_loop: DO I=1,NAN3
           
            GG=G1(I)
            P=GG*F(I)-Q2(I)                                
            X(I)=X(I)+C1*P                               
            Q1(I)=Q1(I)+C2*P                           
            Q2(I)=Q2(I)+P                            
            Q3(I)=Q3(I)+C3*P                       
            Q4(I)=Q4(I)+C4*P 
            Q5(I)=Q5(I)+C5*P     
            QIN(I3)=QIN(I3)+Q1(I)*Q1(I)*0.25/GG
        END DO trans2_loop
        
        RETURN 

    END SUBROUTINE Nord5