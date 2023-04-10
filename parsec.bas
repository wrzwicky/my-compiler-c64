!- parsec2.10
!- rearrange code, verbose parse
!- parens and = works!
    0 rem parsec2.10, 21 jan 1991
    1 rem  by bill zwicky
    9 :
   10 print "{white}{clear}{down}{down}welcome to zparser compiler, v2.10{down}"
   15 do=0:po=0:rem no device output
   20 gosub 40000:rem init arrays
   30 rem (init tokens)
   39 :
   40 read ne:dim er$(ne):for i=1 to ne
   50 read er$(i):next
   55 rem * data is at ln 63500
   59 :
   80 pe=1:rem print all errors
   82 :
   89 rem open bank for exp. eval. stack
   90 ty$="math.stack":gosub 36000
   92 if er<>0 then print "can't open system stack!" :goto 9000
   94 mb=b :rem record bank for math stack
   96 :
   98 print "{down}ready for formulae."
  100 print "{down}ok."
  110 f$=""
  120 input f$
  130 if f$="" then 120
  137 f$=f$+"{arrow left}"
  140 print ":{left}";
  150 if left$(f$,4)<>"dump" then 200
  155 for i=1 to nw
  160 : b$=bt$(b(i))
  165 : if left$(b$,1)="*" then print w$(i)      tab(20)right$(b$,len(b$)-1)
  170 : next i
  175 goto 100
  180 :
  200 if left$(f$,4)="quit" then close 8:end
  210 if left$(f$,4)<>"help" then 300
  220 print "available commands:"
  225 print "  decl - declare a variable"
  230 print "  dump - list all variables"
  235 print "  file - open an object file"
  240 print "  prin - send program to printer"
  250 print "  help - display this"
  260 print "  quit - end program"
  270 goto 100
  290 :
  300 if left$(f$,4)<>"decl" then 390
  305 : l=len(f$)
  310 : for p1=4 to l
  320 :   if mid$(f$,p1,1)<>" " then next
  330 : if p1>l then er=1:l=330:ptr=5:            gosub 45010:goto 100
  332 : for p2=p1+1 to l
  334 :   if mid$(f$,p2,1)<>" " then next
  336 : if p2>l then er=13:l=336:ptr=p2:          gosub 45010:goto 100
  340 : w$ =mid$(f$,p1+1,p2-p1-1):              ty$="*"+mid$(f$,p2+1,l-p2-1)
  350 : gosub 36000:if er<>0 then 100
  355 : gosub 37000:if er<>0 then 100
  360 : print "variable '";w$;"' created as ";ty$
  370 : goto 100
  380 :
  390 if left$(f$,4)<>"file" then 490
  400 : for i=4 to len(f$)
  410 :   if mid$(f$,i,1)<>" " then next
  420 : if i>len(f$) then er=1:l=420:ptr=i       :gosub 45010:goto 100
  430 : n$=mid$(f$,i+1,len(f$)-i-1)
  440 : print "open 8,8,2,"chr$(34);n$",p,w"chr$(34):do=1:open 8,8,2
  450 : pl=0
  460 : l$="dim var("+str$(bs)+",100)":gosub 33000
  470 : goto 100
  480 :
  490 if left$(f$,4)<>"prin" then 900
  500 : if po=0 then po=1:open 4,4               :print "printer on":goto 520
  510 : if po=1 then po=0:close 4                :print "printer off"
  520 : goto 100
  898 :
  899 :
  900 gosub 20000
  910 goto 100
 9970 :
 9980 :
 9990 :
 20000 ptr=0:part=1:sp=1:er=0:pe=1             :ms=0
 20010 :
 20020 rem get next char
 20030 ptr=ptr+1
 20040 c$=mid$(f$,ptr,1)
 20050 if c$=" " then 20020
 20060 if c$="{arrow left}" then op$=c$:                   gosub 31000:return
 20070 if c$<"0" or c$>"9" then 20110
 20080 : if part=1 then part=2:goto 30000        :rem extract #
 20090 : if part=2 then op$="*":gosub 31000      :goto 30000:rem implied *
 20100 :
 20110 if c$<"a" or c$>"z" then 20170
 20120 : if part=2 then op$="*":gosub 31000      :rem implied mult
 20130 : part=2
 20140 : gosub 30200 :if er<>0 then return       :rem extract a var
 20150 : goto 20020
 20160 :
 20170 if c$<>"+"and c$<>"*"and c$<>"/"and c$<>"^"and c$<>"="andc$<>","then 20210
 20180 : if part=1 then er=1:l=20180:gosub        45010:return:rem var needed
 20190 : if part=2 then op$=c$:gosub 31000       :part=1:goto 20020
 20200 :
 20210 if c$<>"-" then 20250
 20220 : if part=2 then op$="-":gosub 31000       :part=1:goto 20020
 20230 : op$="{sh asterisk}":gosub 31000:goto 20020
 20240 :
 20250 if c$<>"(" then 20290
 20260 : if part=1 then pr=pr+10:goto 20020
 20270 : if part=2 then op$="*":gosub 31000       :pr=pr+10:part=1:goto 20020
 20280 :
 20290 if c$<>")" then 20350
 20300 : if part=1 then er=1:l=20300:gosub        45010:return
 20310 : rem else ...
 20320 :  if pr>10 then pr=pr-10
 20330 :  goto 20020
 20340 :
 20350 rem anything else to process?
 20360 :
 20370 rem what's this char?
 20380 er=3:l=20380:gosub 45010:return
 20390 :
 20400 :
 20410 :
 22000 l$="t="+v1$+"+"+v2$:gosub 33000
 22001 goto 31210
 22002 :
 22003 :
 23000 l$="t="+v1$+"-"+v2$:gosub 33000
 23001 goto 31210
 23002 :
 23003 :
 24000 l$="t="+v1$+"*"+v2$:gosub 33000
 24001 goto 31210
 24002 :
 24003 :
 25000 l$="t="+v1$+"/"+v2$:gosub 33000
 25001 goto 31210
 25002 :
 25003 :
 26000 l$="t="+v1$+"^"+v2$:gosub 33000
 26001 goto 31210
 26002 :
 26003 :
 27000 if left$(n1$,1)<>"*" then er=17:l= 27000:goto 45010 :rem assign to const
 27001 l$=v1$+"="+v2$:gosub 33000
 27002 goto 31210
 27003 :
 27004 :
 30000 rem extract constant
 30010 vr$=c$:pd=0:ee=0:nt$="i"
 30020 ptr=ptr+1:c$=mid$(f$,ptr,1)
 30030 if c$="{arrow left}" then 30120
 30040 if c$=" " then 30120
 30050 if c$>="0" and c$<="9" then              vr$=vr$+c$:goto 30020
 30060 if c$<>"." then 30090
 30070 : if pd=1 then 30120
 30080 : pd=1:vr$=vr$+c$:goto 30020
 30090 if c$<>"e" then 30120
 30100 : if ee=1 then 30120
 30110 : ee=1:vr$=vr$+c$:goto 30020
 30120 ptr=ptr-1:goto 20020
 30130 :
 30140 :
 30200 rem extract variable
 30210 :
 30220 er=0 : w$=c$
 30230 ptr=ptr+1 : c$=mid$(f$,ptr,1)
 30240 if c$<"0" or (c$>"9" and c$<"a") or c$>"z" and c$<>"." then 30270
 30250 w$=w$+c$ : goto 30230
 30260 :
 30270 ptr=ptr-1
 30280 gosub 34000
 30290 if d%<>0 then er=11:l=30290              :goto 45010 :rem undef'd var
 30300 vr$="var(" + str$(b(wl%)) + "," +  str$(n(wl%)) + ")"
 30310 nt$=bt$(b(wl%))
 30320 return
 30330 :
 30340 :
 31000 rem push var,op,priority
 31005 print "push & eval:"
 31010 v2$=vr$:n2$=nt$
 31020 op=5    :rem if op$ is a func
 31030 if op$="{arrow left}" then op=0:pr=0                :rem force end of expression
 31040 if op$="=" then op=1
 31050 if op$="+" or op$="-" then op=2
 31060 if op$="*" or op$="/" then op=3
 31070 if op$="^" then op=4
 31080 op=op+pr
 31090 if op>ps(sp-1) then pv$=vr$:             goto 31250
 31100 tp$=op$:tr=op
 31110 :
 31120 print "-eval"
 31125 gosub 32000:v1$=vr$:n1$=nt$
 31130 if v1$="t+" then ms=ms-1:v1$="var(" + str$(mb) + "," + str$(ms) + ")"
 31140 if op$="+" then 22000
 31150 if op$="-" then 23000
 31160 if op$="*" then 24000
 31170 if op$="/" then 25000
 31180 if op$="^" then 26000
 31190 if op$="=" then 27000
 31200 l=31200:er=3:gosub 45010:return
 31210 v2$="t"
 31220 if sp=1 then 31240
 31230 if tr<=ps(sp-1) then 31120
 31240 op$=tp$:op=tr
 31250 if vs$(sp-1)<>"t" then 31290
 31260 vs$(sp-1)="t+"
 31270 l$="var(" + str$(mb) + "," + str$(ms) + ")=t" :gosub 33000
 31280 ms=ms+1 :if ms>mm then mm=ms
 31290 print "-push into"sp
 31295 vs$(sp)=v2$
 31300 os$(sp)=op$
 31310 ps(sp)=op
 31320 ts$(sp)=n2$
 31330 sp=sp+1
 31340 if sp>ss then er=10:l=31340:gosub        45010:return
 31350 return
 31360 :
 32000 rem pop stuff
 32010 sp=sp-1
 32015 print "pop from"sp
 32020 vr$=vs$(sp)
 32030 op$=os$(sp)
 32040 op =pr(sp)
 32050 nt$=ts$(sp)
 32060 return
 32070 :
 32080 :
 33000 rem generate next line number
 33010 rem  and send line to devices
 33020 pl=pl+10
 33030 print pl;l$;tab(25)n1$;tab(32)n2$
 33040 if do=1 then print#8,pl;l$
 33050 if po=1 then print#4,pl;l$
 33060 return
 33070 :
 33080 :
 34000 rem find the location for a word
 34010 rem  w$=word to find
 34020 rem  wl% is loc; d%=-1,0,1 -> which
 34030 rem  side of wl% to go to
 34040 rem p=1024 to 1030:poke p,32:next:p=1023
 34050 wl%=nw/2:lo%=0:hi%=nw
 34060 p=p+1:rem  p,46
 34070 if w$<w$(wl%) then hi%=wl%:wl%=(wl%+lo%)/2:goto 34100
 34080 if w$>w$(wl%) then lo%=wl%:wl%=(wl%+hi%)/2+.5:goto 34100
 34090 goto 34110
 34100 if hi%-lo%>1 then 34060
 34110 d%=0
 34120 if w$>w$(wl%) then d%=1
 34130 if w$<w$(wl%) then d%=-1
 34140 print "("wl%;d%")"
 34150 return
 34160 :
 35000 rem insert a space in word list
 35010 if nw=50 then print"word overflow!":return
 35020 nw=nw+1:for ii=nw-1 to wl% step -1
 35030 w$(ii+1)=w$(ii)
 35040 b(ii+1)=b(ii):n(ii+1)=n(ii):next
 35050 return
 35060 :
 36000 rem open a bank
 36010 er=0
 36020 if nb=0 then b=0:goto 36070
 36030 for b=0 to nb-1
 36040 if ty$=bt$(b) then 36080
 36050 next
 36060 if b>bs then er=15:l=36060:              gosub 45010:goto 37080
 36070 bt$(b)=ty$:nb=nb+1:goto 36090
 36080 ty$=bt$(b)
 36090 return
 36100 :
 37000 rem insertion sort one word (w$)         into bank b
 37010 er=0
 37020 print w$;" ";
 37030 gosub 34000:rem find pos
 37040 if d%=0 then er=14:l=37040:gosub         45010:goto 37080
 37050 if d%=1 then wl%=wl%+1
 37060 gosub 35000:w$(wl%)=w$:b(wl%)=b:n(wl%)=nn(b):nn(b)=nn(b)+1
 37070 printtab(16)"inserted as ";ty$
 37080 return
 37090 :
 39980 :
 39990 :
 40000 rem dimension arrays used
 40010 ss=100:dim vs$(ss),os$(ss),ps(ss),       ts$(ss)
 40020 rem stack: var,op,priority,type
 40030 ps(0)=0:rem force finish a line
 40040 :
 40050 dim an$(20),ad(20),at$(20),al(20):       rem ary name,dim,type,loc
 40060 ts=20:dim w$(ts),b(ts),n(ts):            rem tokens: words,bank #,elem #
 40070 nw=0:rem total number of tokens
 40080 bs=5:dim bt$(bs),nn(bs):                 rem banks: type, # of elem
 40090 nb=0:rem number of banks
 40100 dim param$(bs,ts,5):rem params for       everything
 40110 rem  vars:mem loc
 40120 cm$="":dim cm(20):rem tokenized          command line
 40130 rem er$(ne):rem errors
 40140 return
 40150 :
 40160 :
 40170 :
 40180 :
 42000 for i=1 to 10
 42001 printvs$(i),os$(i),ps(i):next:end
 42002 rem all data used in this program
 42003 rem  because of no restore # cmd
 42004 :
 42005 rem errors-for ln 40
 42006 data 17,missing var,early end of line,syntax,too many '='
 42007 data illegal self operator,equation left of '=',illegal op this side of '='
 42008 data illegal variable name,divide by 0,stack overflow,undefined variable
 42009 data too many close parenthesis,unknown var type,var already exists
 42010 data too many banks,too many tokens in this bank,assignment to a constant
 45000 rem report errors
 45010 if pe=0 then return
 45020 if er<1 or er>ne then print"bad error # ("er") in"l:return
 45030 if ptr>0 then print " {left}"tab(ptr+1);"^ ("c$")";
 45040 print
 45050 print "error in line"l"{left}:":printer$(er)
 45060 return
 45070 :
 45080 :
