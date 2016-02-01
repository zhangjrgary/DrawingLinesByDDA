      program dda_main
      integer i,n
      integer x1,y1,x2,y2
      character*1 mark(0:78,0:22)
      
      !initial 
      call dda_init(mark)
      
      i=1
      !open the input file
      open(1,file='input.txt',status='old',err=902)
      read(1,*,err=903)n
      read(1,*,err=903)x2,y2
501   i=i+1
      y1=y2
      x1=x2
      read(1,*,err=903)x2,y2
      call dda_calc(x1,y1,x2,y2,mark)   !calculation
      if(i.lt.n)goto 501
      close(1)
      
      !plot
      call dda_plot(mark)
      
      
      goto 901    
902   stop 'input file openning error'
903   stop 'reading error'
901   continue      
      end
      
      
      !!!!------------ initialize the plot matrix
      subroutine dda_init(mark)
      character*1 mark(0:78,0:22)
      integer i,j
      mark(0,0)='+'
      i=0
502   i=i+1
      mark(i,0)='-'
      if(i.lt.78)goto 502
      
      i=0
503   i=i+1
      mark(0,i)='|'
      if(i.lt.22)goto 503
      
      i=0
505   i=i+1
       j=0
506    j=j+1
         mark(j,i)=' '
       if(j.lt.78)goto 506
      if(i.lt.22)goto 505
      
      end
      
      !!!!-------------calc by DDA algorithm to fill the mark matrix 
      subroutine dda_calc(x1,y1,x2,y2,mark)
      integer j,k,stepx,stepy
      integer x1,y1,x2,y2
      character*1 mark(0:78,0:22)
      real m
      

      stepx=1
      stepy=1
      m=real(y2-y1)/real(x2-x1)
      if(x2.lt.x1)stepx=-1
      if(y2.lt.y1)stepy=-1 
      if(abs(m).gt.1)goto 601
         j=x1-stepx
         k=-1
701      j=j+stepx
         k=k+1
         mark(j,nint(y1+k*abs(m)*stepy))='*' 
         if(j.ne.x2)goto 701   
      goto 602
      
601   continue
         j=y1-stepy
         k=-1
702      j=j+stepy
         k=k+1
         mark(nint(x1+k*stepx/abs(m)),y1+k*stepy)='*' 
         if(j.ne.y2)goto 702    
602   continue
           
      end
      
      !!!!!--------plot in console
      subroutine dda_plot(mark)
      character*1 mark(0:78,0:22)
      integer i,j
      
      i=23
504   i=i-1   
      write(*,801) (mark(j,i),j=0,78)
801   format(79a1)
      if(i.gt.0)goto 504
      
      end