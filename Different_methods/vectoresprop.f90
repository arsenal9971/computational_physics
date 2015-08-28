
!este programa encuentra los valores y vectores propios de una matriz
      implicit double precision(a-h,o,q-z)
      DIMENSION A(20,21),B(20,21), C(20,21),D(20,21)
      PRINT *
     data n/3/
!Definimos ls matrices de los 
     DATA (A(1,J),J=1,4)/3,2,1,-3/
      DATA (A(2,J),J=1,4)/2,2,1,-3/
      DATA (A(3,J),J=1,4)/2,1,1,-2/
      
      DATA (B(1,J),J=1,4)/3,2,1,-3.71688163/
      DATA (B(2,J),J=1,4)/2.7168816364,2,1,-3/
      DATA (B(3,J),J=1,4)/2,1.7168816364,1,-2/    

      DATA (C(1,J),J=1,4)/3,2,1,-3.71688163/
      DATA (C(2,J),J=1,4)/2.7168816364,2,1,-3/
      DATA (C(3,J),J=1,4)/2,1.7168816364,1,-2/
    
      DATA (D(1,J),J=1,4)/3,2,1,-3.71688163/
      DATA (D(2,J),J=1,4)/2.7168816364,2,1,-3/
      DATA (D(3,J),J=1,4)/2,1.7168816364,1,-2/

      PRINT *
      PRINT *,'MATRIZ AUMENTADA'
      PRINT *
      DO I=1,N
          PRINT 61,(A(I,J),J=1,n+1)
61        FORMAT(1X,1P6E12.4)
      END DO
      PRINT *
      CALL GAUSS(N,A)
65    PRINT *
68    PRINT *,'SOLUCION'
69    PRINT *,'------------------------------------------------'
      PRINT *,'         I            X(I) '
70    PRINT *,'------------------------------------------------'
100   FORMAT(5X,A,1PE16.6)
      PRINT 100,'    1' , 1/ Sqrt(1+A(1,1)**2+A(2,2)**2+A(3,3)**2) 
      DO I=1,N
72         FORMAT(5X,I5,1PE16.6)
           PRINT 72,I+1,A(I,N+1)/Sqrt(1+A(1,1)**2+A(2,2)**2+A(3,3)**2)
        END DO
75    PRINT *,'------------------------------------------------'
80    PRINT *
      !Ahora con la matriz B
PRINT *
      PRINT *,'MATRIZ AUMENTADA'
      PRINT *
      DO I=1,N
          PRINT 61,(B(I,J),J=1,n+1)

      END DO
      PRINT *
      CALL GAUSS(N,B)
     PRINT *
      PRINT *,'SOLUCION'
      PRINT *,'------------------------------------------------'
      PRINT *,'         I            X(I) '
      PRINT *,'------------------------------------------------'

      PRINT 100,'    1' , 1/ Sqrt(1+B(1,1)**2+B(2,2)**2+B(3,3)**2) 
      DO I=1,N

           PRINT 72,I+1,B(I,N+1)/Sqrt(1+B(1,1)**2+B(2,2)**2+B(3,3)**2)
        END DO
   PRINT *,'------------------------------------------------'
   PRINT *
PRINT *
      PRINT *,'MATRIZ AUMENTADA'
      PRINT *
      DO I=1,N
          PRINT 61,(C(I,J),J=1,n+1)

      END DO
      PRINT *
      CALL GAUSS(N,C)
     PRINT *
      PRINT *,'SOLUCION'
      PRINT *,'------------------------------------------------'
      PRINT *,'         I            X(I) '
      PRINT *,'------------------------------------------------'

      PRINT 100,'    1' , 1/ Sqrt(1+C(1,1)**2+C(2,2)**2+C(3,3)**2) 
      DO I=1,N

           PRINT 72,I+1,C(I,N+1)/Sqrt(1+C(1,1)**2+C(2,2)**2+C(3,3)**2)
        END DO
   PRINT *,'------------------------------------------------'
   PRINT *
PRINT *
      PRINT *,'MATRIZ AUMENTADA'
      PRINT *
      DO I=1,N
          PRINT 61,(D(I,J),J=1,n+1)

      END DO
      PRINT *
      CALL GAUSS(N,D)
     PRINT *
      PRINT *,'SOLUCION'
      PRINT *,'------------------------------------------------'
      PRINT *,'         I            X(I) '
      PRINT *,'------------------------------------------------'

      PRINT 100,'    1' , 1/ Sqrt(1+D(1,1)**2+D(2,2)**2+D(3,3)**2) 
      DO I=1,N

           PRINT 72,I+1,D(I,N+1)/Sqrt(1+D(1,1)**2+D(2,2)**2+D(3,3)**2)
        END DO
   PRINT *,'------------------------------------------------'
   PRINT *


      STOP
      END
      

      SUBROUTINE GAUSS(N,A)
      implicit double precision (a-h,o,q-z)
      INTEGER PV
      DIMENSION A(20,21)
      EPS=1.0
10    IF(1.0+EPS.GT.1.0) THEN
           EPS=EPS/2
           GOTO 10
      END IF
      EPS=EPS*2.0
      PRINT *,'EPSILON DE LA MAQUINA: ', EPS
      EPS2=EPS*2.
1005  DET=1.
      DO 1010 I=1,N-1
        PV=I
        DO J=I+1,N
           IF(ABS(A(PV,I)).LT.ABS(A(J,I))) PV=J
        END DO
        IF(PV.EQ.I) GOTO 1050
        
        DO JC=1,N+1
           TM=A(I,JC)
           A(I,JC)=A(PV,JC)
           A(PV,JC)=TM
        END DO
1045    DET=-1*DET
1050    IF(A(I,I).EQ.0) GOTO 1200
      
        DO JR=I+1,N
            IF(A(JR,I).NE.0) THEN
               R=A(JR,I)/A(I,I)
               DO KC=I+1,N+1
                  TEMP=A(JR,KC)
                  A(JR,KC)=A(JR,KC)-R*A(I,KC)
                  IF(ABS(A(JR,KC)).LT.EPS2*TEMP) A(JR,KC)=0.0
               END DO
             END IF
1060    END DO
1010    CONTINUE

        DO I=1,N
           DET=DET*A(I,I)
        END DO

        PRINT *
        PRINT *,'DETERMINANTE = ', DET
        PRINT *
        IF(A(N,N).EQ.0) GOTO 1200
        A(N,N+1)=A(N,N+1)/A(N,N)
       
        DO NV=N-1,1,-1
           VA=A(NV,N+1)
           DO K=NV+1,N
              VA=VA-A(NV,K)*A(K,N+1)
           END DO
           A(NV,N+1)=VA/A(NV,NV)
        END DO
        RETURN
1200    PRINT *,'LA MATRIZ ES SINGULAR'
        STOP
        END



