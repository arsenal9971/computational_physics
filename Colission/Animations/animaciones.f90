real l, m1,m2

vi1=5.
vi2=5.

tk=0.
tp=0.
tl=0.
ts=0.
tr=0.
tx=0.
txx=0.


xi1=1.
xi2=9.

l=10.

m1=1.
m2=1.

t=0.001

do i=0, 1000
   tx=i*t
   x1=xi1+vi1*tx
   x2=xi2-1.*vi2*tx
  

   if((x2-x1).lt.0.) goto 5
   print *, tx,x2!,x2
   end do


5      do i=0, 1000
      vf1=(((m1-m2)/(m1+m2))*vi1-2.*(m2/(m1+m2))*vi2)
      vf2=2.*(m1/(m1+m2))*vi1-((m2-m1)/(m1+m2))*vi2
         tr=i*t
         x1r=x1+vf1*tr
         x2r=x2+vf2*tr
      if(((l-x1r).lt.0 .or. x1r.lt.0)) goto 6
      if(((l-x2r).lt.0 .or. x2r.lt.0)) goto 7
         print*,ts+tk+tp+tl+tr+tx+txx,x2r!,x2r

      end do

6     do i=0, 1000        
         tk=i*t
         x1k=x1r-vf1*tk
         x2k=x2r+vf2*tk
        if((l-x2k).lt.0 .or. x2k.lt.0) goto 8
         x1x=x1l
         x2x=x2l
         if((x2k-x1k).lt.0.) goto 10
         print *,ts+tk+tp+tl+tr+tx+txx, x2k !,x2k
      end do

7    do i=0, 1000
         tl=i*t
         x1l=x1r+vf1*tl
         x2l=x2r-vf2*tl
       if(((l-x1l).lt.0 .or. x1l.lt.0)) goto 9
         x1x=x1l
         x2x=x2l
      if((x2l-x1l).lt.0.) goto 10
         print *,ts+tk+tp+tl+tr+tx+txx, x2l!,x2l
         end do

8    do i=0, 1000
         ts=i*t
         x1s=x1k-vf1*ts
         x2s=x2k-vf2*ts
         x1x=x1s
         x2x=x2s
       if((x2s-x1s).lt.0.) goto 10
         print *,ts+tk+tp+tl+tr+tx+txx, x2s!,x2s
         end do

9   do i=0, 1000
         tp=i*t
         x1p=x1l-vf1*tp
         x2p=x2l-vf2*tp
         x1x=x1p
         x2x=x2p
        if((x2p-x1p).lt.0.) goto 10
         print *,ts+tk+tp+tl+tr+tx+txx, x2p!,x2p
        end do

10   do i=0, 100
      vff1=-(((m1-m2)/(m1+m2))*vf1+2.*(m2/(m1+m2))*vf2)
      vff2=-2.*(m1/(m1+m2))*vf1+((m2-m1)/(m1+m2))*vf2
         txx=i*t
         x1r=x1x+vff1*txx
         x2r=x2x+vff2*txx
      if(((l-x1r).lt.0 .or. x1r.lt.0)) goto 6
      if(((l-x2r).lt.0 .or. x2r.lt.0)) goto 7
         print*,ts+tk+tp+tl+tr+tx+txx,x2r!,x2r

      end do

end 
