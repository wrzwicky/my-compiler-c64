!- compiler.4
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
  100 data ints,"i1 @ 1024,i2 @ 1025,i3 @ 1027"
  101 data a9,00,20,bd,ff,a9,04,a2,04
  102 data a0,00,20,ba,ff,20,c0,ff,{arrow left}
  119 :
  120 data reg1,"i1 @ a"
  121 data a2,04,20,c9,ff,{arrow left}
  139 :
  140 data reg2,"i2 @ yx"
  141 data 20,d2,ff,{arrow left}
  159 :
  160 data regs,"i1 @ a,i2 @ xy
  161 data 20,cc,ff,{arrow left}
  179 :
  180 data close,"i1 @ a"
  181 data a9,04,20,c3,ff,{arrow left}
  199 :
  200 data {arrow left}end
  480 :
  490 of=4:open of,4,3
  500 input "{down}word number";w
  505 if w<0 then close of:end
  510 print "template=";pt$(w)
  515 print"separate parameters with a '.'"
  520 input "parameters";p$
  530 gosub 11000
  540 goto 500
  998 :
  999 :
 1000 input "filename (.fig is added)? test.fig{left}{left}{left}{left}{left}{left}{left}{left}{left}{left}";f$
 1010 if right$(f$,4)=".fig" then f$=left$(f$,len(f$)-4)
 1015 scratch (f$+".ml")
 1020 open 1,8,2,f$+".fig,s,r"
 1030 of=2:open of,8,3,f$+".ml,p,w"
 1040 print#of,chr$(0)chr$(2*16);
 1099 :
 1100 input#1,w$:s1=st
 1110 gosub 10000:rem find word
 1120 printw$;w
 1130 print#of,ml$(w);
 1140 if s1=0 then 1100
 8999 :
 9000 print#of,chr$(96)
 9010 close of
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
 11030 it=0:ip=0
 11040 print " (what var type?) ";
 11050 : s$=""
 11060 : it=it+1:v$=mid$(pt$(w), it, 1)
 11070 : it=it+1:c$=mid$(pt$(w), it, 1)
 11080 : if c$>="0" and c$<="9" then s$=s$+c$:goto 11070
 11090 : print" var type: ";v$;" - ";
 11100 :
 11110 : if v$="i" then gosub 12000:goto 11200
 11120 : if v$="s" then gosub 13000:goto 11200
 11130 : print "unknown": goto 9000
 11140 :
 11200 if it>1 and ip>1 then 11040
 11210 :
 11220 return
 11999 :
 12000 if s$="" then print"missing int siye": goto 12900
 12010 s=val(s$):if s<1 or s>4 then print"unsupported int size":goto 12900
 12020 :
 12030 n$="" : print " (fetch the number) ";
 12040 ip=ip+1:c$=mid$(p$, ip, 1)
 12050 if c$>="0" and c$<="9" then n$=n$+c$:goto 12040
 12060 n=val(n$):n$=""
 12070 :
 12080 print " (convert number to int) ";
 12090 for i=1 to s
 12100 : n=n/256:n$=n$+chr$( (n - int(n)) * 256 + .2 ):n=int(n)
 12110 : next i
 12120 :
 12130 it=it+1:if mid$(pt$(w), it, 1) <> "@" then print"unknown template command":goto 12900
 12140 it=it+1
 12150 :
 12160 print " (fetch storage location) ";
 12170 l$="":lt=0 :rem lt=loc type: 0=dec ad, 1=hex ad, 2=reg
 12180 it=it+1:c$=mid$(pt$(w), it, 1)
 12190 : if instr("0123456789", c$) > 0  then   l$=l$+c$:goto 12180
 12195 : if c$="$" then                         lt=1:goto 12180
 12200 : if lt=1 and instr("abcdef", c$) > 0  then l$=l$+c$:goto 12180
 12210 : if instr("axy", c$) > 0  then lt=2    :l$=l$+c$:goto 12180
 12220 if lt=2 then 12500: rem handle register storage
 12230 if lt=1 then l=dec(right$(l$,len(l$)-1))
 12240 if lt=0 then l=val(l$)
 12250 :
 12260 print " (generate code to store the bytes) ";
 12270 for i=0 to s-1
 12280 : print#of,chr$(169)mid$(n$,i+1,1);
 12290 : lh=int(l/256):ll=l-lh*256
 12300 : print#of,chr$(141)chr$(ll)chr$(lh);
 12305 : l=l+1
 12310 : next
 12320 :
 12330 goto 12900
 12490 :
 12500 if len(l$) <> len(n$) then print "number of regs doesn't match specified int size":goto 12900
 12510 :
 12520 rem set up ld? instructions
 12530 ld(1)=169:ld(2)=162:ld(3)=160
 12540 :
 12550 print " (load reg(s)) ";
 12560 for i=1 to len(l$)
 12570 : print#of,chr$(ld(instr("axy",mid$(l$,i,1))));mid$(n$,i,1);
 12580 : next
 12590 :
 12600 goto 12900
 12899 :
 12900 print " (advance indices to next ',') ";
 12910 it=instr(pt$(w), ",", it)
 12920 ip=instr(p$, ".", ip)
 12930 :
 12932 print#of
 12934 print
 12940 return
 12999 :
