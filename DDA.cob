       *注意文件名
       identification division.
       program-id. dda_main.

       *环境
       environment division.
       input-output section.
       file-control.

       select inputfile assign to "input1.txt"
           organization is line sequential
           access is sequential.
       select outputfile assign to "output1.txt"
           organization is line sequential
           access is sequential.

       *数据
       data division.
       file section.

       fd inputfile.
       01 input1.
           05 data1     pic 99.
           05 data2     pic 99.
       fd outputfile.
       01 output1.
           05 data11     pic x(1).

       working-storage section.
       01  data-table.
            03  data-1 occurs 78 times.
            05  data-2 occurs 22 times.
              10  row-col  pic x(1).
       77  r  pic 99.
       77  c  pic 99.
       77  i  pic 99.
       77  j  pic 99.
       77  n pic  99.
       77  x1  pic 99.
       77  x2  pic 99.
       77  y1  pic 99.
       77  y2 pic  99.
       77 stepx  pic  99.
       77 stepy  pic  99.
       77 m      pic  99.
       77 k   pic 99.
       77 t   pic 99.
       77 z   pic 99.

       *程序
       procedure division.
           perform dda_init.
           perform readfile.
           perform dda_plot.

       dda_init section.
           move "+" to row-col(1,1)
           perform  varying i from 2 by 1 until i > 78
               move "-"  to row-col(i,1)
           end-perform
           perform varying i from 2 by 1 until i > 22
               move "|"  to  row-col(1,i)
           end-perform
           perform varying i from 2 by 1 until i > 78
               perform varying j from 2 by 1 until j > 22
                   move " "  to row-col(i,j)
               end-perform
           end-perform.

       readfile section.
           move 1 to i
           open input inputfile.
           read inputfile next   record
           not at end
             move data1 to n
           end-read
           read inputfile next   record
           not at end
             move data1 to x2
             move data2 to y2
           end-read
           perform 501.
       501 section.
           add 1 to i
           move y2 to y1
           move x2 to y2
           read inputfile next   record
           not at end
             move data1 to x2
             move data2 to y2
           end-read
           read inputfile next   record
           not at end
               perform dda_calc
               if i = n then
               go to 501
               end-if
           end-read
           close inputfile.

       dda_calc section.
           move 1 to  stepx
           move  1 to  stepy
           compute m  = (y2 - y1) / (x2 - x1)
           if x2 =  x1 then
               add -1 to stepx
           end-if
           if y2 =  y1 then
               add -1 to stepy
           end-if
           if M = 1 then
           go to 601
           end-if
           compute j = x1 - stepx
           compute k = k - 1
           perform 701.
       701 section.
           compute   j = j + stepx
           compute k = k + 1
           compute t = FUNCTION INTEGER(y1 + k * m * stepy )
           move "*" to row-col(j,t)
           if j = x2 then
            go to 701
           end-if
           go to 602.
       601 section.
           continue
           compute j = y1 - stepy
           compute k = k - 1
           perform 702.
       702 section.
           compute j = j + stepy
           compute k  = k + 1
           compute r = x1 + k * stepx / m
           compute c = y1 + k * stepy
           move "*" to row-col(r,c)
           if j = y2 then
           go to 702
           end-if.
       602 section.
           continue.

       dda_plot section.
           open output outputfile
           perform varying i from 23 by -1 until i < 1
               perform varying j from 1 by 1 until j > 78
                   move row-col(i,j) to data11
                   write output1
               end-perform
           end-perform
           
           close outputfile.
