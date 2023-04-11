!- compiler.7.3
    1 rem 10   : compiler
    2 rem 50000: linker
    3 rem 62000: file viewers
    9 :
   10 print"{down}piler.com, v7.3{down}"
   20 dim w$(10),ml$(10),pt$(10)
   25 goto 1000
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
  160 data regs,"i1 @ a,i2 @ xy"
  161 data 20,cc,ff,{arrow left}
  179 :
  180 data close,"i1 @ a"
  181 data a9,04,20,c3,ff,{arrow left}
  199 :
  200 data {arrow left}end
  480 :
  490 of=4:open of,4,3
  500 input "{down}word number (0+)";w
  505 if w<0 then close of:end
  510 print "command=";w$(w);", template=";pt$(w)
  515 print"separate parameters with a '.'"
  520 input "parameters";p$
  530 gosub 11000
  540 goto 500
  998 :
  999 :
 1000 gosub 5000
 1005 input "filename (.fig is added)? test.fig{left}{left}{left}{left}{left}{left}{left}{left}{left}{left}";f$
 1010 if right$(f$,4)=".fig" then f$=left$(f$,len(f$)-4)
 1015 scratch (f$+".obj")
 1020 open 1,8,2,f$+".fig,s,r"
 1030 of=2:open of,8,3,f$+".obj,u,w"
 1040 print#of,chr$(64)chr$(196);"code";chr$(0); :rem select and name code seg
 1050 :
 1060 cs = -1 :rem current segment
 1099 :
 1100 input#1,l$ :print l$ :s1=st :if l$="" then begin :if s1=0 then 1100               :else 1900 :bend
 1101 p=1 : l=len(l$)
 1102 if p <= l then  if mid$(l$,p,1) <> " " then p=p+1 :goto 1102
 1104 w$ = left$(l$,p-1) : if p < l then p$=right$(l$,l-p) :else p$ = ""
 1110 gosub 10000 :rem find word
 1115 print "  word #";w
 1120 if p$ <> "" then gosub 11000 :rem compile params
 1130 s=0 : m$=ml$(w) : gosub 15000 :rem write data
 1140 if s1=0 then 1100
 1190 :
 1200 s=0 : m$ = chr$(96)
 1210 gosub 15000
 1899 :
 1900 close of:close 1
 1910 end
 1998 :
 1999 :
 5000 restore 6000
 5010 nw=0
 5020 read w$:if w$="{arrow left}end" then 5080
 5030 w$(nw)=w$
 5040 read pt$(nw)
 5050 read v$:if v$<>"{arrow left}" then ml$(nw)=ml$(nw)+chr$(dec(v$)):goto 5050
 5060 nw=nw+1:goto 5020
 5070 nw=nw-1
 5080 return
 6000 data open,"i1 @ a,i1 @ x,i1 @ y"
 6005 data 20,ba,ff, a9,00,20,bd,ff
 6010 data 20,c0,ff,{arrow left}
 6020 :
 6030 data start,"i1 @ x"
 6035 data 20,c9,ff,{arrow left}
 6040 :
 6050 data hello,""
 6055 data a9,48,20,d2,ff,a9,45,20
 6060 data d2,ff,a9,4c,20,d2,ff,a9
 6070 data 4c,20,d2,ff,a9,4f,20,d2
 6080 data ff,a9,0d,20,d2,ff,{arrow left}
 6090 :
 6100 data stop,""
 6105 data 20,cc,ff,{arrow left}
 6110 :
 6120 data close,"i1 @ a"
 6125 data 20,c3,ff,{arrow left}
 6130 :
 6140 data {arrow left}end
 8999 :
 9000 close of
 9010 close 1
 9090 end
 9997 :
 9998 :
 9999 :
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
 11030 if pt$(w) = "" then return
 11035 it=0 : ip=0 : m$=""
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
 11220 s=0 : gosub 15000 :rem select data, insert it
 11230 return
 11999 :
 12000 if s$="" then print"missing int size": goto 12900
 12010 s=val(s$):if s<1 or s>4 then print"unsupported int size":goto 12900
 12020 :
 12030 n$="" : print " (fetch the number) ";
 12035 ip=ip+1:c$=mid$(p$,ip,1):if c$=" " then 12035   :rem strip leading spcs
 12040 if c$<"0" or c$>"9" then 12060
 12050 n$=n$+c$ : ip=ip+1 : c$=mid$(p$, ip, 1) : goto 12040
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
 12280 : m$ = m$ + chr$(169)+mid$(n$,i+1,1)
 12290 : lh=int(l/256) :ll=l-lh*256
 12300 : m$ = m$ + chr$(141)chr$(ll)chr$(lh)
 12305 : l = l+1
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
 12570 : m$ = m$ + chr$(ld(instr("axy",mid$(l$,i,1)))) + mid$(n$,i,1)
 12580 : next
 12590 :
 12600 goto 12900
 12899 :
 12900 print " (advance indices to next ',') ";
 12910 it=instr(pt$(w), ",", it)
 12920 ip=instr(p$, ".", ip)
 12930 :
 12934 print
 12940 return
 12999 :
 13000 return :rem strings not implemented yet
 14994 :
 14995 :
 14996 rem generate 'write data' linker command
 14997 rem   s  = segment to insert data into
 14998 rem   m$ = data to write
 14999 :
 15000 if s <> cs then cs = s :print#of,chr$(64+s);
 15010 rem if s>63, use 'quick select'
 15020 rem if len(m$) > 63 then <use long form of write, if it exists>
 15030 print#of,chr$(len(m$));m$; :rem insert byte count & data
 15040 return
 49990 :
 49991 :
 49992 :
 49993 :
 49994 rem fig linker
 49995 :
 50000 open 5,8,15,"i":close 5:if ds<>0 then printds$:goto 59100
 50002 f$="linktest"
 50005 print ".obj file to link [";f$;"]";:input f$
 50010 if right$(f$,4) <> ".obj" then i$=f$+".obj": else i$=f$:                       f$=left$(i$,len(i$)-4)
 50012 open 2,8,2,i$:close 2:if ds<>0 then print ds$:goto 50005
 50020 o$=f$+".ml"
 50021 print "executable name [";o$;"]";:input o$
 50023 open 2,8,2,o$:close 2:if ds=0 then print "-scratching file":scratch (o$)
 50025 print "link: in ";i$;" and out ";o$ : print
 50030 :
 50040 rem --- general inits ---
 50050 z$=chr$(0)
 50060 dim ss(100), sv(100) : rem symbol segments, values
 50070 dim gt$(63),ga(63),gp(63) : rem segment titles, addresses, pointers
 50075 : tg=0 : ts=0 :rem highest (top) seg#, sym#
 50080 :
 50090 :
 50100 for ps = 1 to 2
 50110 :
 50120 rem --- pass 1 inits ---
 50125 if ps=1 then begin
 50130 : zl = 1 : rem # times to do pass 1
 50140 : bend
 50145 :
 50150 rem --- pass 2 inits ---
 50155 if ps=2 then begin
 50158 : gosub 61000  :rem resolve segs and syms
 50160 : open 3,8,3,o$+",p,w":if ds <> 0 then printds$:goto 59100
 50161 : b=int(ga(0)/256):print#3,chr$(ga(0)-256*b);chr$(b);      :rem load address
 50162 : zl = tg+1 : rem # times to do pass 2
 50163 : ad = ga(0)
 50165 : bend
 50170 :
 50172 for zi=1 to zl
 50174 : if ps = 2  and  ga(zi-1) > ad then for i=1 to ad-ga(zi-1) : print#3,z$;         : next : ad=ga(zi-1)
 50178 : open 2,8,2,i$+",r":if ds <> 0 then printds$:goto 59100
 50179 :
 50180 get#2,c$ :h=asc(c$+z$) :s2=st :rem chunk header
 50190 if (h and 192) = 0   then gosub 51000 : goto 50260 : rem fetch data
 50200 if (h and 192) = 64  then gosub 52000 : goto 50260 : rem set segment
 50210 if (h and 192) = 128 then gosub 53000 : goto 50260 : rem unused
 50220 :
 50230 rem --- command handler ---
 50240 h = h and 63
 50245 if (h and 32) = 32 then gosub 55000 : goto 50260
 50250 on h gosub 54000, 54100, 54200, 54300
 50260 if s2 = 0 then 50180
 50270 :
 50280 goto 59000
 50998 :
 50999 :
 51000 rem handle just data
 51010 print "data: segment"; sg; ";";
 51020 b = h and 63
 51025 print b;" bytes"
 51030 gp(sg) = gp(sg) + b
 51040 if ps = 2  and  sg = zi - 1  then 51500
 51050 for i=1 to b  : rem skip the data
 51060 : get#2,c$:next
 51070 return
 51498 :
 51499 rem tranfer data to executable (pass 2)
 51500 : for i=1 to b  :rem transfer the data
 51510 :   get#2,c$:print#3,left$(c$+z$,1);:next
 51520 : bend
 51530 return
 51998 :
 51999 :
 52000 rem set current segment (pass 1 and 2)
 52010 sg = h and 63
 52020 print "current segment is now"; sg
 52030 if sg>tg then tg=sg
 52040 return
 52399 :
 52998 :
 52999 :
 53000 rem unused command range
 53010 print "nonsense command (";h;") in file!"
 53020 goto 59000
 53998 :
 53999 :
 54000 rem define symbol
 54010 gosub 60100 : s=b
 54020 gosub 60100
 54025 print "put symbol"; s; "at address"; b
 54026 if s>ts then ts=s
 54028 if ps=1 then begin
 54030 : ss(s) = -1 : sv(s) = b :rem segnum=-1 means no segment
 54035 : bend
 54040 return
 54099 :
 54100 rem force segment address
 54110 gosub 60100
 54115 print "put segment"; sg; " at address"; b
 54118 if ps=1 then begin
 54120 : ga(sg) = b
 54125 : bend
 54130 return
 54199 :
 54200 rem put symbol in segment
 54210 gosub 60100 : s=b    :rem symbol #
 54220 gosub 60000          :rem # bytes
 54225 print "put symbol"; s; " in segment"; sg; ", offset"; gp(sg); ",";
 54227 : print b; " byte";:if b>1 then print"s": else print
 54228 if s>ts then ts=s
 54230 if ps=1 then begin
 54232 : ss(s) = sg : sv(s) = gp(sg) : gp(sg) = gp(sg) + b
 54234 : bend
 54240 return
 54299 :
 54300 rem name segment
 54310 print "title of segment";sg;" is: ";
 54320 t$=""
 54330 get#2,c$:if c$<>"" then t$=t$+c$:print c$;:goto 54330
 54340 print
 54350 if ps = 1 then begin
 54360 : st$(s) = t$
 54365 : bend
 54370 return
 54399 :
 55000 rem insert symbol into executable
 55010 s = h and 15 : d = h and 16
 55020 gosub 60100
 55030 print "insert symbol"; b; "here as"; s; "bytes, ";
 55040 if d=0 then print "low"; : else print "high";
 55050 print " byte first"
 55055 if ss(b)=0 then print "{ct o}   *** symbol is not defined ***":return                :rem count error
 55060 if ps=2 and sg=zi-1 then begin    :rem do the insertion if at right seg
 55062 : v=sv(b)                         :rem  (get val to ins)
 55065 : if d=1 then begin               :rem  high byte first
 55070 :   s=s-1 : d=256^int(log(v)/log(2)/8+.99)    :rem min # bytes
 55080 :   for i=s to 0 step -1
 55085 :     c=int(v/d) : v=v-b*d : d=d/256
 55090 :     print#3,chr$(c);
 55100 :     next
 55110 :  bend : else begin                      :rem low byte first
 55120 :   s=s-1
 55130 :   for i=0 to s
 55140 :     v=v/256 : c=(v-int(v))*256 : v=int(v)
 55150 :     print#3,chr$(c);
 55160 :     next
 55170 :   bend
 55180 : bend
 55190 return
 55998 :
 55999 :
 59000 print "{down}-one run of pass";ps;"done-{down}"
 59010 close 2                 :rem close input file
 59015 if ps=2 then ad = ad+gp(zi-1)
 59020 next zi                 :rem repeat this pass
 59025 print "{reverse on}W  pass";ps;"{left} done W"
 59030 if ps=2 then close 3    :rem close output file (pass 2 only)
 59040 next ps                 :rem do next pass
 59050 end
 59099 :
 59100 rem emergency stop
 59110 print "emergency abort!"
 59120 close 3:close 2
 59130 end
 59999 :
 60000 rem get next byte into c1$, b = value
 60010 get#2,c1$ : c1$=left$(c1$+z$,1)
 60020 b = asc(c1$)
 60030 return
 60099 :
 60100 rem get next 2 bytes into c1$,c2$ (in that order) b = lo/hi value
 60110 get#2,c1$,c2$
 60120 c1$=left$(c1$+z$,1) : c2$=left$(c2$+z$,1)
 60130 b=asc(c2$) * 256 + asc(c1$)
 60140 return
 60199 :
 60997 :
 60998 rem resolver - locate all segments and symbols
 60999 :
 61000 print:print
 61010 print "resolver"
 61020 ad=ga(0)
 61030 if ad=0 then ad=49152
 61040 print "base address of program [";ad;"]";:input ad
 61090 :
 61100 print "{down}resolving segments ... "
 61110 for i=0 to tg
 61120 : if ga(i)=0 then ga(i)=ad : ad=ad+gp(i)
 61125 : print "seg";i;"@";ga(i)
 61130 : next
 61140 rem print "done"
 61190 :
 61200 print "resolving symbols ... "
 61210 for i=0 to ts
 61220 : if ss(i) > 0 then begin   :rem -1=predef'd, 0=code only
 61230 :   sv(i)=sv(i)+ga(ss(i))
 61235 : print "sym";i;"@";sv(i)
 61240 :   bend
 61250 : next
 61260 rem print "done"
 61270 :
 61280 print "resolver is done{down}{down}"
 61290 return
 61999 :
 62000 open 1,8,2,"test.fig"
 62010 get#1,a$:printa$;:if st=0 then 62010
 62020 close 1
 62030 end
 62200 open 1,8,2,"test.obj":z$=chr$(0)
 62210 get#1,a$:printright$(hex$(asc(a$+z$)),2)", ";:if st=0 then 62210
 62220 close 1
 62230 end
