C SPLINES.F INTERPOLACION CON SPLINES CUBICOS
      DIMENSION X(0:100),F(0:100),A(0:100),B(0:100),C(0:100)
     %,S(0:100),H(0:100),DD(0:100),XA(99),FA(99)
C  X(I) VALORES X DE LOS DATOS
C  F(I) VALORES F(X(I))
C  NI NUMERO DE PUNTOS MENOS 1
C  JAN NUMERO DE VALORES X A LOS QUE SE APLICA LA INTERPOLACION
C  XA(I)  VALORES DE X EN DONDE SE INTERPOLA
      DATA NI/10/
      DATA (X(I),I=0,10)
     % /0.0, 0.1, 0.78539, 1.57079, 2.35619, 3.14159, 3.92699,
     % 4.71239, 5.4978, 6.18318, 6.28318/
      DATA (F(I), I=0,10)
     % /0.0, 0.099833, 0.707106, 1.0, 0.707106,0.0
     %  , -0.707106, -1.0, -0.707106, -0.099833, 0.0/
      DATA JAN/10/
      DATA (XA(I),I=1,10)
     % /0.0, 0.698131, 1.396262, 2.094393, 2.792525, 3.490656,
     % 4.188787, 4.866918, 5.585049, 6.28318 /

      open(1,file='resultado.dat',status='replace')
      PRINT *,'PROGRAMA DE INTERPOLACION CON SPLINES CUBICOS'
      PRINT *

      CALL SPL1(NI,X,F,JAN,XA,FA,DD)
      PRINT *,'   I    X(I)          F(I)            FDER2(I) '
      
      DO I=0,NI
           write(1,34)X(I),F(I),DD(I)
34         FORMAT(2X,3F14.7)
      END DO
      PRINT *,'        X          F(   )   '
      DO 44 I=1,JAN
      write(1,50) XA(I),FA(I)
44    CONTINUE
55    FORMAT(5F12.6)
50    FORMAT(4F12.6)
      close(1)
      END
      
      
      SUBROUTINE SPL1(NI,X,F,JAN,XA,FA,DD)
      DIMENSION X(0:100), F(0:100), A(0:100), B(0:100), C(0:100)
     % , S(0:100), H(0:100), DD(0:100), XA(30), FA(30)
     
!      PRINT*,'OPRIMA 0 PARA ESPECIFICAR LAS SEGUNDAS DERIVADAS'
!      PRINT *,'       1 PARA EXTRAPOLAR, 0'
!      PRINT *,'       2 PARA CONDICIONES DE FRONTERA CICLICA'
       KBC=0
      IF(KBC.EQ.0) THEN
          ! PRINT *,'PROPORCIONE LA SEGUNDA DERIVADA IZQUIERDA'
           DD(0)=
          ! PRINT *,'PROPORCIONE LA SEGUNDA DERIVADA DERECHA'
           DD(NI)=
       END IF
C ---------------------------------DETERMINACION DEL SPLINE
      IM=NI-1
      DO 202 I=0,NI-1
      H(I)=X(I+1)-X(I)
202   CONTINUE  
      DO 305 I=1,NI-1
           A(I)=H(I-1)
           C(I)=H(I)
           B(I)=2*(A(I)+C(I))
           S(I)=6*((F(I-1)-F(I))/H(I-1) + (F(I+1)-F(I))/H(I) )
305   CONTINUE
       IF (KBC.LT.2) THEN
            IF (KBC.EQ.0) then
               S(1)=S(1)-A(1)*DD(0)
               S(NI-1)=S(NI-1)-C(NI-1)*DD(NI)
            END IF
            B(1)=B(1)+2*A(1)
            C(1)=C(1)-A(1)
            B(IM)=B(IM)+2*C(IM)
            A(IM)=A(IM)-C(IM)
            CALL TRID(A,B,C,S,DD,NI-1)
            IF (KBC.EQ.1) THEN
                DD(0)=2*DD(1)-DD(2)
                DD(NI)=2*DD(NI-1)-DD(NI-2)
                END IF
       ELSE
          A(NI)=H(NI-1)
          C(NI)=H(0)
          B(NI)=2*(A(NI)+C(NI))
          S(NI)=6*((F(NI-1)-F(NI))/H(NI-1) +(F(1)-F(0))/H(0) )
          CALL TRIDCY(A,B,C,S,DD,NI)
          DD(0)=DD(NI)
       END IF
       
C -----------------------INTERPOLACION

444   J=0
      DO 400 K=1,JAN
      IF (XA(K).LT.X(0) .OR. XA(K).GT.X(NI)) GOTO 550
360   IF (XA(K).GE.X(J) .AND. XA(K).LE.X(J+1)) GOTO 380
      IF (J.GT.NI) GOTO 560
      J=J+1
      GOTO 360
380   Z=XA(K)-X(J)
      FA(K)=F(J)+(-(2*DD(J)+DD(J+1))/6*H(J) + (F(J+1)-F(J))/H(J))*Z
     % + (DD(J+1)-DD(J))/6/H(J)*Z**3 + DD(J)*Z**2/2
400   CONTINUE
      RETURN
550   PRINT *,'XA(K) = ',XA(K),' : FUERA DE RANGO', K
      RETURN
560   PRINT *,'J = ',J,' : FUERA DE RANGO '
      END
      
      SUBROUTINE TRID(A,B,C,S,DD,IM)
      DIMENSION A(0:1),B(0:1),C(0:1),S(0:1),DD(0:1)
        DO 410 I=2,IM
           R=A(I)/B(I-1)
           B(I)=B(I)-R*C(I-1)
           S(I)=S(I)-R*S(I-1)
410   CONTINUE
      DD(IM)=S(IM)/B(IM)
      DO 540 I=IM-1,1,-1
          DD(I)=(S(I)-C(I)*DD(I+1))/B(I)
540   CONTINUE
      RETURN
      END
      
      SUBROUTINE TRIDCY(A,B,C,S,DD,N)
      DIMENSION A(0:1),B(0:1),C(0:1),S(0:1),DD(0:1),H(0:100),V(0:100)
      V(1)=A(1)
      H(1)=C(N)
      H(N-1)=A(N)
      H(N)=B(N)
      V(N-1)=C(N-1)
      IM=N-1
      DO I=2,IM
         R=A(I)/B(I-1)
         B(I)=B(I)-R*C(I-1)
         S(I)=S(I)-R*S(I-1)
         V(I)=V(I)-R*V(I-1)
         P=H(I-1)/B(I-1)
         H(I)=H(I)-P*C(I-1)
         H(N)=H(N)-P*V(I-1)
         S(N)=S(N)-P*S(I-1)
      END DO
      T=H(N-1)/B(N-1)
      H(N)=H(N)-T*V(N-1)
      DD(N)=(S(N)-T*S(N-1))/H(N)
      DD(N-1)=(S(N-1)-V(N-1)*DD(N))/B(N-1)
      DO I=N-2,1,-1
          DD(I)=(S(I)-V(I)*DD(N)-C(I)*DD(I+1))/B(I)
      END DO
      RETURN
      END                                                                               
                               
                                                   
