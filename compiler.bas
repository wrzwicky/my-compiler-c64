!- compiler.2
   10 print"{down}piler.com{down}"
   20 dim w$(10),ml$(10),pt$(10)
   30 nw=0
   40 read w$:if w$="{arrow left}end" then 90
   50 w$(nw)=w$
   55 read pt$(nw)
   60 read v$:if v$<>"{arrow left}" then ml$(nw)=ml$(nw)+chr$(dec(v$)):goto 60
   70 nw=nw+1:goto 40
   75 :
   90 nw=nw-1
   99 :
  100 data open,"i1 @ 1024, i1 @ 1025, i1 @ 1026"
  101 data a9,00,20,bd,ff,a9,04,a2,04
  102 data a0,00,20,ba,ff,20,c0,ff,{arrow left}
  119 :
  120 data start,"i1 @ 1024"
  121 data a2,04,20,c9,ff,{arrow left}
  139 :
  140 data hello,"i2 @ 1024"
  141 data a9,48,20,d2,ff,a9,45,20,d2,ff
  142 data a9,4c,20,d2,ff,a9,4c,20,d2,ff
  143 data a9,4f,20,d2,ff,a9,0d,20,d2,ff
  144 data {arrow left}
  159 :
  160 data stop,"i1 @ 1024"
  161 data 20,cc,ff,{arrow left}
  179 :
  180 data close,"i1 @ 1024"
  181 data a9,04,20,c3,ff,{arrow left}
  199 :
  200 data {arrow left}end
  490 of=3:open of,3
  500 input "word number";w
  510 print "template=";pt$(w)
  520 input "parameters";p$
  530 gosub 11000
  540 goto 500
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
 10999 :
 11000 rem compile parameters
 11010 rem  w  = index of parameter template (word number)
 11020 rem  p$ = string to be compiled
 11030 it=1:ip=1
 11040 print " (what var type?) ";
 11050 : s$=""
 11060 : v$=mid$(pt$(w), it, 1):it=it+1
 11070 : c$=mid$(pt$(w), it, 1):it=it+1
 11080 : if c$>="0" and c$<="9" then s$=s$+c$:goto 11070
 11090 : print "  var type: ";v$;" - ";
 11100 :
 11110 : if v$="i" then gosub 12000:goto 11200
 11120 : if v$="s" then gosub 13000:goto 11200
 11130 : print "unknown": goto 9000
 11140 :
 11200 if it<len(pt$(w)) and ip<len(p$) then 11040
 11210 :
 11220 return
 11999 :
 12000 if s$="" then print"missing int size": goto 12900
 12010 s=val(s$):if s<1 or s>2 then print"unsupported int size":goto 12900
 12020 :
 12030 n$="" : print " (fetch the number) ";
 12040 c$=mid$(p$, ip, 1):ip=ip+1
 12050 if c$>="0" and c$<="9" then n$=n$+c$:goto 12040
 12060 n=val(n$):n$=""
 12070 :
 12080 print " (convert number to int) ";
 12090 for i=1 to s
 12100 : n=n/256:n$=n$+chr$( (n - int(n)) * 256 + .2 ):n=int(n)
 12110 : next i
 12120 :
 12130 it=it+1:if mid$(pt$(w), it, 1) <> "@" then print"unknown template command":goto 12900
 12140 it=it+2
 12150 :
 12160 print " (fetch storage location) ";
 12170 l$="":lt=0 :rem lt=loc type: 0=dec ad, 1=hex ad, 2=reg
 12180 c$=mid$(pt$(w), it, 1):it=it+1
 12190 if instr("0123456789", c$) > 0  then   l$=l$+c$:goto 12180
 12200 if instr("$abcdef", c$) > 0  then lt=1:l$=l$+c$:goto 12180
 12210 if instr("axy", c$) > 0  then lt=2    :l$=l$+c$:goto 12180
 12220 if lt=2 then 12500: rem handle register storage
 12230 if lt=1 then l=dec(right$(l$,len(l$)-1))
 12240 if lt=0 then l=val(l$)
 12250 :
 12260 print " (generate code to store the bytes) ";
 12270 for i=0 to s-1
 12280 : print#of,chr$()mid$(n$,i+1,1);
 12290 : lh=int(l/256):ll=l-lh*256
 12300 : print#of,chr$()chr$(l)chr$(h);
 12310 : next
 12320 :
 12330 goto 12900
 12490 :
 12500 if len(l$) <> len(n$) then print "number of regs doesn't match specified int size":goto 12900
 12510 :
 12520 rem set up ld? instructions
 12530 ld(1)=...:ld(2)=...:ld(3)=...
 12540 :
 12550 print " (load reg(s)) ";
 12560 for i=1 to len(l$)
 12570 : print#of,chr$(ld(instr("axy",mid$(l$,i,1))));mid$(n$,i,1);
 12580 : next
 12590 :
 12600 goto 12900
 12899 :
 12900 print " (advance indices past next ',') ";
 12910 do until mid$(pt$(w),it,1)=",":it=it+1:loop
 12920 it=it+1
 12930 :
 12940 do until mid$(p$,ip,1)=",":ip=ip+1:loop
 12950 ip=ip+1
 12960 :
 12970 return
 12999 :
