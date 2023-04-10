!- compiler.1
   10 print"{down}piler.com{down}"
   20 dim w$(10),ml$(10),pm$(10)
   30 nw=0
   40 read w$:if w$="{arrow left}end" then 90
   50 w$(nw)=w$
   60 read v$:if v$<>"{arrow left}" then ml$(nw)=ml$(nw)+chr$(dec(v$)):goto 60
   70 nw=nw+1:goto 40
   75 :
   90 nw=nw-1
   99 :
  100 data open,a9,00,20,bd,ff,a9,04,a2
  110 data 04,a0,00,20,ba,ff,20,c0,ff,{arrow left}
  120 :
  130 data start,a2,04,20,c9,ff,{arrow left}
  140 :
  150 data hello,a9,48,20,d2,ff,a9,45,20
  160 data d2,ff,a9,4c,20,d2,ff,a9
  170 data 4c,20,d2,ff,a9,4f,20,d2
  180 data ff,a9,0d,20,d2,ff,{arrow left}
  190 :
  200 data stop,20,cc,ff,{arrow left}
  210 :
  220 data close,a9,04,20,c3,ff,{arrow left}
  230 :
  290 data {arrow left}end
  998 :
  999 :
 1000 input "filename (.fig is added)? test.fig{left}{left}{left}{left}{left}{left}{left}{left}{left}{left}";f$
 1010 if right$(f$,4)=".fig" then f$=left$(f$,len(f$)-4)
 1015 scratch (f$+".ml")
 1020 open 1,8,2,f$+".fig,s,r"
 1030 open 2,8,3,f$+".ml,p,w"
 1040 print#2,chr$(0)chr$(2*16);
 1099 :
 1100 input#1,w$:s1=st
 1110 gosub 10000:rem find word
 1120 printw$;w
 1130 print#2,ml$(w);
 1140 if s1=0 then 1100
 8999 :
 9000 print#2,chr$(96);
 9010 close 2
 9020 close 1
 9030 end
 10000 rem search for w$ and return its
 10010 rem position in w
 10020 for w=0 to nw
 10030 if w$=w$(w) then return
 10040 next
 10050 w=-1:return
