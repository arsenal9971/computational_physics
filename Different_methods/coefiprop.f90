! METODO DE INTERPOLACION
	DIMENSION A(20,20),FF(0:20),RA(0:20),DF(0:20,0:20),B(20,20)
	DIMENSION CC(0:20)
	REAL MV(0:20)
	PRINT *
	PRINT *,'  METODO DE INTERPOLACION'
	PRINT *
! DEFINICION DE LA MATRIZ
        DATA N/4/           !DEFINE EL ORDEN DE LA MATRIZ
        DATA (B(1,J),J=1,4)/4,3,2,1/
        DATA (B(2,J),J=1,4)/3,3,2,1/
        DATA (B(3,J),J=1,4)/2,2,2,1/
        DATA (B(4,J),J=1,4)/1,1,1,1/
        PRINT *,'ORDEN DE LA MATRIZ : ', N
        PRINT *,'MATRIZ '
                 DO I=1,N
                    PRINT 56,(B(I,J),J=1,N)
                 END DO
                 
 1       PRINT *
         PRINT *, 'DELTA LAMBDA ?'
         READ *, DE      !INCREMENTO DE LAMBDA PARA LA TABLA DE DIFERENCIAS
         DO JJ=0,N
            RA(JJ)=JJ*DE
            PRINT *
            PRINT *, 'LAMBDA= ', RA(JJ)
                 DO J=1,N
                    DO I=1,N
                       A(I,J)=B(I,J)
                    END DO
                    A(J,J)=A(J,J)-RA(JJ)
                  END DO
               CALL DETMNT(N,A,S)
               FF(JJ)=S
          END DO
56        FORMAT(1X,1P6E12.5)
! --LA SIGUIENTE PARTE CALCULA LA TABLA DE DIFERENCIAS HACIA ADELANTE
       DO I=0,N 
          DF(I,0)=FF(I)    !INICIALIZACION DE LA TABLA DE DIFERENCIAS
       END DO
       M=N
       DO J=1,N
          M=M-1
          DO I=0,M
             DF(I,J)=DF(I+1,J-1)-DF(I,J-1)   !TABLA DE DIFERENCIAS
          END DO
       END DO
       PRINT *
       PRINT *,'TABLA DE DIFERENCIAS PARA LOS DETERMINANTES'
       DO I=0,N
          WRITE(*,'(F8.4,1P6E11.3)') RA(I),(DF(I,J),J=0,N-I)
       END DO
! LA SIGUIENTE RUTINA ES PARA CALCULAR LOS COEFICIENTES DE LAS POTENCIAS
! POR MEDIO DE LOS COEFICIENTES DE MARKOV
        PRINT *
205     PRINT *,'COEFICIENTES DE MARKOV '
        DO I=0,N
           CC(I)=0
           MV(I)=0
        END DO
        MV(1)=1
        CC(0)=DF(0,0)
        CC(1)=DF(0,1)
        DO K=2,N
           DO L=K,1,-1
              MV(L)=(MV(L-1)-(K-1)*MV(L))/K
              CC(L)=CC(L)+MV(L)*DF(0,K)
           END DO
           PRINT 223,(MV(L),L=1,K)
223        FORMAT(1X,6F12.7)
        END DO
        PRINT *
        PRINT *,'RESULTADO FINAL '
        PRINT *,'POTENCIA      COEFICIENTES '
        DO I=0,N
           CC(I)=CC(I)/DE**I
           WRITE(6,'(2X,I3,5X,F10.4)') I,CC(I)
        END DO  
        PRINT *
460     PRINT *,'------------------------------------------'
500     PRINT *
        PRINT *
        PRINT *,'OPRIMA 1 PARA CONTINUAR O 0 PARA TERMINAR'
        READ *,K
        IF(K.EQ.1) GOTO 1
        PRINT *
        END
        
! *************************************************
        SUBROUTINE DETMNT(N,A,DET)
        DIMENSION A(20,20)
        INTEGER PV,PC
        PC=0           !INCIALIZA EL CONTADOR DE PIVOTEO
        DO I=1,N-1      !INICIA ELIMINACION HACIA ATRAS
           PV=I
           DO J=I+1,N
              IF(ABS(A(PV,I)).LT.ABS(A(J,I))) PV=J
           END DO
           IF(PV.NE.I) THEN
              DO JC=1,N
                TM=A(I,JC)
                A(I,JC)=A(PV,JC)
                A(PV,JC)=TM
              END DO
              PC=PC+1
            END IF
            DO JR=I+1,N
               IF(A(JR,I).NE.0) THEN
                 R=A(JR,I)/A(I,I)
                 DO KC=I+1,N
                    A(JR,KC)=A(JR,KC)-R*A(I,KC)
                 END DO 
               END IF
             END DO
          END DO        !FIN DE LA ELIMINACION HACIA ATRAS
          IF(A(N,N).EQ.0) GOTO 1200
          DET=1         !INCIALIZACION DEL DETERMINANTE
            DO I=1,N
               DET=DET*A(I,I)
            END DO
          IF(PC.NE.INT(PC/2)*2) DET=-DET
          PRINT *,'DETERMINANTE   =',DET
          PRINT *,'NUMERO DE PIVOTEOS= ',PC
          RETURN
1200      PRINT *,'LA MATRIZ ES SINGULAR'
          RETURN
          END                
                                       
                                      
                                                      	 
