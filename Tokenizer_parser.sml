val code = CommandLine.arguments();
datatype TOKEN1 = ADDOP of int*string 
				| MULOP of int*string
                | NEG of int
                | NOT of int 
                | AND of int 
                | OR of int
                | ASSIGN of int 
                | EQUAL of int
                | RELOP of int*string
                | LB of int
                | RB of int
                | LP of int
                | RP of int
                | SC of int
                | COLON of int
                | DCOLON of int
                | COMMA of int
                | PROGRAM of int
                | VAR of int
                | INT of int
                | BOOL of int
                | IF of int
                | THEN of int
                | ELSE of int
                | ENDIF of int
                | WHILE of int
                | DO of int
                | ENDWH of int
                | READ of int
                | WRITE of int
                | VBOOL of int*bool
                | IDENTF of int*string
                | VINT of int*int
                | ERROR of int
datatype State = One | Two | Three | Four 

fun toString TOKEN1 = case TOKEN1 of ADDOP(a,b) => "ADDOP(" ^ Int.toString a ^ "," ^ b ^ ")\n"
								| MULOP(a,b) => "MULOP(" ^ Int.toString a ^ "," ^ b ^ ")\n" 
                                | NEG(a) => "NEG(" ^ Int.toString a ^ ")\n"
								| NOT(a) => "NOT(" ^ Int.toString a ^ ")\n"
                                | AND(a) => "AND(" ^ Int.toString a ^ ")\n"
                                | OR(a) => "OR(" ^ Int.toString a ^ ")\n"
                                | ASSIGN(a) => "ASSIGN(" ^ Int.toString a ^ ")\n"
                                | EQUAL(a) => "EQUAL(" ^ Int.toString a ^ ")\n" 
                                | RELOP(a,b) => "RELOP(" ^ Int.toString a ^ "," ^ b ^ ")\n"
                                | LB(a) => "LB(" ^ Int.toString a ^ ")\n"
                                | RB(a) => "RB(" ^ Int.toString a ^ ")\n"
                                | LP(a) => "LP(" ^ Int.toString a ^ ")\n"
                                | RP(a) => "RP(" ^ Int.toString a ^ ")\n"
                                | SC(a) => "SC(" ^ Int.toString a ^ ")\n"
                                | COLON(a) => "COLON(" ^ Int.toString a ^ ")\n"
                                | DCOLON(a) => "DCOLON(" ^ Int.toString a ^ ")\n"
                                | COMMA(a) => "COMMA(" ^ Int.toString a ^ ")\n"
                                | PROGRAM(a) => "PROGRAM(" ^ Int.toString a ^ ")\n"
                                | VAR(a) => "VAR(" ^ Int.toString a ^ ")\n"
                                | INT(a) => "INT(" ^ Int.toString a ^ ")\n"
                                | BOOL(a) => "BOOL(" ^ Int.toString a ^ ")\n"
                                | IF(a) => "IF(" ^ Int.toString a ^ ")\n"
                                | THEN(a) => "THEN(" ^ Int.toString a ^ ")\n"
                                | ELSE(a) => "ELSE(" ^ Int.toString a ^ ")\n"
                                | ENDIF(a) => "ENDIF(" ^ Int.toString a ^ ")\n"
                                | WHILE(a) => "WHILE(" ^ Int.toString a ^ ")\n"
                                | DO(a) => "DO(" ^ Int.toString a ^ ")\n"
                                | ENDWH(a) => "ENDWH(" ^ Int.toString a ^ ")\n"
                                | READ(a) => "READ(" ^ Int.toString a ^ ")\n"
                                | WRITE(a) => "WRITE(" ^ Int.toString a ^ ")\n"
                                | VBOOL(a,b)=>"VBOOL("^ Int.toString a ^ "," ^ Bool.toString b ^")\n"
                                | IDENTF(a,b) => "IDENTF(" ^ Int.toString a ^ "," ^ b ^ ")\n"
                                | VINT(a,b) => "VINT(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
                                | ERROR(a)=>"ERROR(" ^ Int.toString a ^ ")\n" 

fun printError(line:int) = toString(ERROR(line))
fun printVInt (vint:string,line:int) = toString(VINT(line,trunc(Option.getOpt(Real.fromString(vint),0.0))))

fun printkeyword (keyword:string,line:int) = 
	case keyword of
    	 	"+" => toString(ADDOP(line,"+"))
        |   "~" => toString(NEG(line)) 
		| 	"-" => toString(ADDOP(line,"-"))
		| 	"/" => toString(MULOP(line,"/"))
		| 	"*" => toString(MULOP(line,"*"))
		| 	"%" => toString(MULOP(line,"%"))
		| 	"!" => toString(NOT(line))
		| 	"&&" => toString(AND(line))
        | 	":" => toString(COLON(line))
        | 	"::" => toString(DCOLON(line))
		| 	"||" => toString(OR(line))
		| 	":=" => toString(ASSIGN(line))
		| 	"=" => toString(EQUAL(line))
		| 	"<>" => toString(RELOP(line,"<>"))
		| 	"<" => toString(RELOP(line,"<"))
		| 	"<=" => toString(RELOP(line,"<="))
		| 	">" => toString(RELOP(line,">"))
		| 	">=" => toString(RELOP(line,">="))
		| 	"(" => toString(LB(line))
		| 	")" => toString(RB(line))
		| 	"{" => toString(LP(line))
		| 	"}" => toString(RP(line))
		| 	";" => toString(SC(line))
		|	"," => toString(COMMA(line))
		|	"int" => toString(INT(line))
		| 	"bool" => toString(BOOL(line))
        | 	"program" => toString(PROGRAM(line))
        | 	"var" => toString(VAR(line))
		| 	"if" => toString(IF(line))
		| 	"then" => toString(THEN(line))
		| 	"else" => toString(ELSE(line))
        |   "endif" => toString(ENDIF(line))
		| 	"while" => toString(WHILE(line))
		| 	"do" => toString(DO(line))
        | 	"endwh" => toString(ENDWH(line))
		| 	"write" => toString(WRITE(line))
		| 	"read" => toString(READ(line))
		| 	"tt" => toString(VBOOL(line,true))
		| 	"ff" => toString(VBOOL(line,false))
        |   ""  =>  ""
		|	_   => (
            let
              val first = String.sub(keyword,0);
              val first1 = Char.toString(first);
            in
              if((first1>="a" andalso first1<="z") orelse (first1>="A" andalso first1<="Z")) then toString(IDENTF(line,keyword))
              else if((first1>="0" andalso first1<="9") orelse first1="~" orelse first1="+") then printVInt(keyword,line)
              else printError(line)
            end
        )

val outfile1 = hd (tl code)

fun printStringtoFile (str:string, file:string) = 
    let 
      	val f =  TextIO.openAppend file
    in
    	(TextIO.output (f, str); TextIO.closeOut f) 
    end	

val infile = (hd code);
fun readInput (filename:string) = 
    let
    	val f = TextIO.getInstream(TextIO.openIn filename);
		fun loop (clist, f) = case TextIO.StreamIO.input1 f of
			SOME (c, f') => loop (c::clist, f')
		  | NONE		 => (TextIO.StreamIO.closeIn; #" "::clist)
	in
		rev(loop ([], f))
    end
fun isIn (s:string,[]) = false
	| isIn (s,(h::t):string list) = if (s=h) then true else isIn(s,t);
fun scanner([], symac :string, State, line :int) = printStringtoFile("",outfile1)
    | scanner(h::t,symac :string, State,line :int) =
        case h of
            #"\n" => (printStringtoFile(printkeyword(symac,line),outfile1);scanner(t,"",One,line+1))
        |   #"\t" => (printStringtoFile(printkeyword(symac,line),outfile1);scanner(t,"",One,line))
        |   #" " => (printStringtoFile(printkeyword(symac,line),outfile1);scanner(t,"",One,line))
        |	_	=>(
				let
					val x = Char.toString(h);
		 		in
                    case State of
                        One => (if(x>="0" andalso x<="9") then scanner(t,symac^x,Three,line)
                                else if((x>="a" andalso x<="z") orelse (x>="A" andalso x<="Z")) then scanner(t,symac^x,Four,line)
                                else if(x="~") then (printStringtoFile(printkeyword(symac,line),outfile1);scanner(t,"~",Two,line))
                                else if(x=":") then (printStringtoFile(printkeyword(symac,line),outfile1);scanner(t,":",Two,line))
                                else if(x="&") then (printStringtoFile(printkeyword(symac,line),outfile1);scanner(t,"&",Two,line))
                                else if(x="|") then (printStringtoFile(printkeyword(symac,line),outfile1);scanner(t,"|",Two,line))
                                else if(x="<") then (printStringtoFile(printkeyword(symac,line),outfile1);scanner(t,"<",Two,line))
                                else if(x=">") then (printStringtoFile(printkeyword(symac,line),outfile1);scanner(t,">",Two,line))
                                else if(x="+") then (printStringtoFile(printkeyword(symac,line),outfile1);scanner(t,"+",Two,line))
                                else if(isIn(x,["-","/","*","%",")","{","}","!","=",";",","])) then (printStringtoFile(printkeyword(symac,line),outfile1); printStringtoFile(printkeyword(x,line),outfile1); scanner(t,"",One,line))
                                else (printStringtoFile(printError(line),outfile1); scanner(t,"",One,line))
                        )
        
                    | Two => (if(symac="+" andalso x>="0" andalso x<="9") then scanner(t,symac^x,Three,line)
                                else if( x>="0" andalso x<="9" andalso symac="~") then scanner(t,symac^x,Three,line)
                                else if(x="~" andalso symac="~" ) then (printStringtoFile(printkeyword("~",line),outfile1);scanner(t,"~",Two,line))
                                else if(x="&" andalso symac="&")then (printStringtoFile(printkeyword("&&",line),outfile1);scanner(t,"",One,line))
                                else if(x=":" andalso symac=":") then (printStringtoFile(printkeyword("::",line),outfile1);scanner(t,"",One,line))
                                else if(x="|" andalso symac="|") then (printStringtoFile(printkeyword("||",line),outfile1);scanner(t,"",One,line))
                                else if(x="=" andalso symac=":") then (printStringtoFile(printkeyword(":=",line),outfile1);scanner(t,"",One,line))
                                else if(x=">" andalso symac="<") then (printStringtoFile(printkeyword("<>",line),outfile1);scanner(t,"",One,line))
                                else if(x="=" andalso symac="<") then (printStringtoFile(printkeyword("<=",line),outfile1);scanner(t,"",One,line))
                                else if(symac="<") then (printStringtoFile(printkeyword("<",line),outfile1);scanner((h::t),"",One,line))
                                else if(x="=" andalso symac=">") then (printStringtoFile(printkeyword(">=",line),outfile1);scanner(t,"",One,line))
                                else if(symac=">") then (printStringtoFile(printkeyword(">",line),outfile1);scanner((h::t),"",One,line))
                                else (printStringtoFile(printError(line),outfile1); scanner(t,"",One,line))
                        )
                        
                    |  Three => (if(x>="0" andalso x<="9") then scanner(t,symac^x,Three,line)
                                else if(isIn(x,[":","&","|","<",">","+","~","-","/","*","%",")","{","}","!","=",";"])) then (printStringtoFile(printVInt(symac,line),outfile1); scanner((h::t),"",One,line))
                                else (printStringtoFile(printError(line),outfile1); scanner(t,"",One,line))
                        )
                    
                    |  Four => (if(x>="0" andalso x<="9") then scanner(t,symac^x, Four,line)
                                else if((x>="a" andalso x<="z") orelse (x>="A" andalso x<="Z")) then scanner(t,symac^x,Four,line)
                                else if(isIn(x,[":","&","|","<",">","+","~","-","/","*","%",")","{","}","!","=",";",","])) then (printStringtoFile(printkeyword(symac,line),outfile1); scanner((h::t),"",One,line))
                                else (printStringtoFile(printError(line),outfile1); scanner(t,"",One,line))
                        )
		 		end 
                );


val inputfile1 = readInput(infile);

scanner(inputfile1,"",One,1);

datatype TOKEN = ADDOP of string 
				| MULOP of string
                | NEG 
                | NOT  
                | AND 
                | OR 
                | ASSIGN 
                | EQUAL
                | RELOP of string
                | LB 
                | RB 
                | LP 
                | RP 
                | SC 
                | COLON 
                | DCOLON 
                | COMMA 
                | PROGRAM 
                | VAR 
                | INT 
                | BOOL 
                | IF 
                | THEN 
                | ELSE 
                | ENDIF 
                | WHILE 
                | DO 
                | ENDWH 
                | READ 
                | WRITE 
                | VBOOL of string
                | IDENTF of string
                | VINT of string
                | ERROR of int
                | NULL of string



fun printsymbolTable([]) = print("******\n") 
  | printsymbolTable(h::t) = (print(h^"\n"); printsymbolTable(t))

fun lookup(identifier,h::t,index) = if(h=identifier) then "t"^Int.toString(index)
    else lookup(identifier,t,index+1)
fun newtemp([],index) = "t"^Int.toString(index)
    | newtemp(h::t,index) = newtemp(t, index+1)
fun extractSymbol(s: string, acc: string,flag: bool) = if(s="") then acc 
    else if(String.substring(s,0,1)=",") then extractSymbol(String.extract(s,1,NONE),acc,true)
    else if(String.substring(s,0,1)=")") then extractSymbol(String.extract(s,1,NONE),acc,false) 
    else if (flag=true) then extractSymbol(String.extract(s,1,NONE),acc^String.substring(s,0,1),true)
    else extractSymbol(String.extract(s,1,NONE),acc,false)
fun newlabel(index:int) = "label"^Int.toString(index) 

val outfile = hd(tl (tl code)) ;

fun printStringtoFile (str:string, file:string) = 
    let 
      	val f =  TextIO.openAppend file
    in
    	(TextIO.output (f, str); TextIO.closeOut f) 
    end	

fun errorMessage(tkn) = (print("\n\n\n********** Expected "^tkn^" Got some other token, Code Aborted!! *************\n\n\n") ; OS.Process.exit(OS.Process.success))

fun totok(s: string) =case String.substring(s,0,3) of
			"OR(" => OR
        |   "ADD" => ADDOP(extractSymbol(s,"",false))
        |   "MUL" => MULOP(extractSymbol(s,"",false))
		| 	"EQ(" => EQUAL
		| 	"NOT" => NOT
		| 	"LP(" => LP
		| 	"RP(" => RP
		| 	"LB(" => LB
		| 	"RB(" => RB
		| 	"IF(" => IF
		| 	"SC(" => SC
		| 	"NEG" => NEG
		| 	"AND" => AND
		| 	"INT" => INT 
		| 	"BOO" => BOOL 
		| 	"THE" => THEN
		| 	"ELS" => ELSE
        |   "END" => (if(String.substring(s,0,5)="ENDWH") then ENDWH else ENDIF)
		| 	"PRO" => PROGRAM
		| 	"REA" => READ
		| 	"WRI" => WRITE
        |   "COL" => COLON
        |   "DCO" => DCOLON
		| 	"COM" => COMMA
		| 	"WHI" => WHILE
		| 	"DO(" => DO
		| 	"ERR" => ERROR(1)
		| 	"IDE" => IDENTF(extractSymbol(s,"",false))
		| 	"ASS" => ASSIGN
        |   "VBO" => VBOOL(extractSymbol(s,"",false))
        |   "VIN" => VINT(extractSymbol(s,"",false))
        |   "VAR" =>  VAR
        |   "REL" => RELOP(extractSymbol(s,"",false))
        | _  => NULL(s)

exception NotParsableERROR 

fun readfile (infile:string,outfile: string) = 
	let
		val ins = TextIO.openIn infile
        fun consume (token:TOKEN) = ( 
        							(case (TextIO.inputLine ins) of 
        								SOME (chunk) => totok(chunk)
									|	NONE => (TextIO.closeIn ins; totok("NULL")))
        							)

		and program(token:TOKEN) = ( 
									case token of 
                                        PROGRAM => let
                                           val p = consume(token);
                                           in
                                           case p of
                                                IDENTF(a) => (
                                                    let
                                                      val p1 = consume(p);
                                                    in
                                                      case p1 of 
                                                            DCOLON => (
                                                                let
                                                                  val p2 = consume(p1);
                                                                in
                                                                  let
                                                                    val t1 = block(p2);
                                                                  in
                                                                    t1
                                                                  end
                                                                end
                                                            )
                                                            | _ => errorMessage("::")
                                                    end
                                                )
                                        | _ => errorMessage("IDENTIFIER")
                                          
                                        end
                                      | _ => errorMessage("PROGRAM")
		        					)
                      

        and block(token:TOKEN) = (
        						let 
                      val t_symTab = declarationSeq(token);
	        						val t = #1(t_symTab);
                      val symTab = #2(t_symTab);
	        					in
	        						(let 
	        							val p = commandseq(t,symTab)
	        						in
	        							(printsymbolTable(symTab);p)	
	        						end)
	        					end)
        and declarationSeq(token:TOKEN) = (
                                let
                                  val t_symTab = declaratiion(token)
                                in
                                  t_symTab
                                end
        )
        and declaratiion(token:TOKEN) = (
                                let
                                  val t_symTab = decl1(token)
                                in
                                  t_symTab
                                end
        )
        and decl1(token:TOKEN) = (
                                let
                                  val t_symTab = decl(token)
                                  val t = #1 t_symTab
                                in
                                  case t of 
                                        VAR => (
                                            let
                                              val t1_symTab = decl(t)
                                            in
                                              t1_symTab
                                            end
                                        )
                                        | LP => t_symTab
                                        | _ => errorMessage("VAR")
                                end
        )
        and decl(token:TOKEN) = case token of
                                VAR => (
                                    let
                                      val p = consume(token);
                                    in
                                      let
                                        val t_symTab = varlist(p,["start"])
                                        val t = #1 t_symTab
                                        val symTab = #2 t_symTab
                                      in
                                        case t of   
                                            COLON => (
                                                let
                                                  val p1 = consume(t);
                                                in
                                                  let
                                                    val t2 = type1(p1);
                                                  in
                                                    case t2 of 
                                                        SC =>(
                                                            let
                                                              val p2 = consume(t2);
                                                            in
                                                              (p2,symTab)
                                                            end
                                                        )
                                                        | _ => errorMessage(";")
                                                  end
                                                end
                                            )
                                            | _ => errorMessage(":")
                                      end

                                    end
                                
                                )
                                | _ => errorMessage("VAR")
    and varlist(token:TOKEN,symTable) = 
                    case token of 
                        IDENTF(a) => 
                                let
                                    val p1 = consume(token);
                                in
                                    case p1 of
                                        COMMA => (
                                            let
                                              val t1 = consume(p1);
            
                                            in
                                              let
                                                val p2_symTable = varlist(t1,symTable@[a])
                                                val symTable = #2 p2_symTable
                                              in
                                                p2_symTable
                                              end
                                            end
                                        )
                                    |    COLON =>  (p1,symTable@[a])
                                    | _ => errorMessage(": or ,")
                                end
                                | _ => errorMessage("Identifier")
                            
    and  type1(token:TOKEN) =
                case token of 
                    INT =>
                        let
                          val p1 = consume(token);
                        in
                          p1
                        end
                |   BOOL=>
                        let
                          val p2 = consume(token);
                        in
                          p2
                        end
                | _ => errorMessage("int or bool")
    and commandseq(token:TOKEN,symTab) =
        case token of 
            LP => (
                    let
                      val p1 = consume(token)
                    in
                      let
                        val t1 = commandseq1(p1,symTab);
                      in
                        case t1 of 
                            RP => 
                                let
                                    val p3 = consume(t1);
                                in
                                    p3
                                end
                              | _ => errorMessage(";")
                      end
                    end
            )
          | _ => errorMessage("{")
    and commandseq1(token:TOKEN,symTab) =
        case token of 
            RP => token 
            | _ => 
                let
                  val p1 = command(token,symTab) ;
                in
                  case p1 of
                    SC => 
                        let
                          val t1 = consume(p1);
                        in
                          case t1 of 
                            RP => t1 
                            | _ => 
                                let
                                  val p2 = commandseq1(t1,symTab);
                                in
                                    p2 
                                end
                        end
                end
    and command(token:TOKEN,symTab) = 
        case token of
            IDENTF(a) => 
                let
                  val p1 = consume(token);
                  val tempVar = lookup(a,symTab,0);
                in
                  case p1 of 
                    ASSIGN =>
                        let
                          val p2 = consume(p1);
                        in
                          let
                            val t2_newvar = intexp(p2,symTab);
                            val t2 = #1 t2_newvar ;
                            val newvar = #2 t2_newvar ;
                             val print1 = printStringtoFile(tempVar,outfile);
                             val print2 = printStringtoFile(":= ",outfile);
                             val print3= printStringtoFile(newvar^"\n",outfile);
                          in
                            t2
                          end
                        end
                      | _ => errorMessage(":=")
                end
        |    READ => 
                let
                  val p1 = consume(token);
                in
                  case p1 of 
                    IDENTF(a) => 
                        let
                          val p2 = consume(p1);
                          val print1 = printStringtoFile("read ",outfile);
                          val print2 = printStringtoFile(lookup(a,symTab,0)^"\n",outfile);
                        in
                          p2
                        end
                    | _ => errorMessage("Identifier")
                end
        |    WRITE =>
                let
                  val p1 = consume(token);
                in
                  let
                    val t1_newvar =  intexp(p1,symTab);
                    val t1 = #1 t1_newvar ;
                    val newvar = #2 t1_newvar ;
                    val print1 = printStringtoFile("write ",outfile) ;
                    val print2=printStringtoFile(newvar^"\n",outfile);
                  in
                    t1
                  end
                end
        |    IF =>
                let
                  val p1 = consume(token);
                in
                  let
                    val t1_newvar = boolexp(p1, symTab);
                    val t1 = #1 t1_newvar ;
                    val newvar = #2 t1_newvar ;
                    val lab1 = newlabel(4);
                    val lab2 = newlabel(5);
                    val exitlab = newlabel(6);
                    val print1 = printStringtoFile("if "^newvar^" goto "^lab1^"\n",outfile);
                  in
                    case t1 of
                        THEN =>
                            let
                              val p2 = consume(t1);
                            in
                              let
                                val print2 = printStringtoFile("goto "^lab2^"\n"^lab1^"\n",outfile)
                                val t2 = commandseq(p2,symTab);
                                val print4 = printStringtoFile("goto "^exitlab^"\n",outfile) ;
                              in
                                case t2 of
                                    ELSE =>
                                        let
                                          val print3 = printStringtoFile(lab2^"\n",outfile)
                                          val p3 = consume(t2);
                                        in
                                          let
                                            val t3 = commandseq(p3,symTab);
                                          in
                                            case t3 of 
                                                ENDIF =>
                                                    let
                                                      val p4 = consume(t3);
                                                      val print5 = printStringtoFile(exitlab^"\n",outfile)
                                                    in
                                                      p4
                                                    end
                                                  | _ => errorMessage("ENDIF")
                                          end
                                        end
                                        | _ => errorMessage("ELSE")
                              end
                            
                            end
                          | _ => errorMessage("THEN")
                  end
                end
        |   WHILE => 
                let
                  val p1 = consume(token);
                in
                  let
                    val t1_newvar = boolexp(p1, symTab);
                    val t1 = #1 t1_newvar ;
                    val newvar = #2 t1_newvar ;
                    val exitlab = newlabel(1);
                    val looplab = newlabel(2);
                    val dolab = newlabel(3);
                    val print0 = printStringtoFile(looplab^"\n", outfile) ;
                    val print1 = printStringtoFile("if "^newvar^" goto "^dolab^"\n", outfile)
                  in
                    case t1 of
                        DO =>
                            let
                              val p2 = consume(t1);
                            in
                              let
                                val print2 = printStringtoFile("goto "^exitlab^"\n"^dolab^"\n", outfile);
                                val t2 = commandseq(p2,symTab);
                              in
                                case t2 of
                                    ENDWH =>
                                        let
                                          val print4 = printStringtoFile("goto "^looplab^"\n", outfile);
                                          val print3 = printStringtoFile(exitlab^"\n", outfile);
                                          val p3 = consume(t2);
                                        in
                                          p3
                                        end
                                    | _ => errorMessage("ENDWH")
                              end
                            end
                            | _ => errorMessage("DO")
                  end
                end
          | _ => errorMessage("Identifier or if or read or write or while")
    and intexp(token:TOKEN,symTab) = 
						let
							val t_newvar = intT(token,symTab);
                            val t = #1 t_newvar
                            val newvar = #2 t_newvar
						in
							case t of
                                    SC => (t,newvar)
                                    | _ =>			
									    let 
				        			        val p_newvar1 = intE(t,symTab,newvar);
                                            val p = #1 p_newvar1 ;
                                            val newvar1 = #2 p_newvar1 ;
				        			    in
				        				    (p,newvar1)		
				        			    end	
						end
		
	and intE(token:TOKEN, symTab, newvar) = 
			case token of
				ADDOP(a) => 
                    let
                        val newvar1 = newtemp(symTab@["#"],0);
						val t = consume(token);
					in
						let 
				        	val p_newvar2 = intexp(t,symTab);
                            val p = #1 p_newvar2 ;
                            val newvar2 = #2 p_newvar2 ;
                            val print1 = printStringtoFile(newvar1^" := "^newvar^a,outfile)
                            val print2 = printStringtoFile(newvar2^"\n",outfile)
				        in
				        	(p,newvar1)		
				        end
				    end
        | ERROR(a)=> errorMessage("Operator or Numeral")
        | _ => (token, newvar)
			
    and intT(token:TOKEN, symTab) = 
							let
								val t_newvar =  intF(token,symTab)
                                val t = #1 t_newvar ;
                                val newvar = #2 t_newvar ;
							in
                                case t of 
                                SC => (t,newvar)
                                | _ =>
								    let 
	        						    val p_newvar1 = intT1(t,symTab, newvar) ;
	        					    in
	        					        p_newvar1
	        				        end	
							end
          

	and intT1 (token:TOKEN, symTab ,newvar) = 
		case token of
				MULOP(a) => 
                    let
                        val newvar1 = newtemp(symTab@["#"],1)
						val t = consume(token); 
					in
						let 
				        	val p_newvar2 = intT(t,symTab);
                            val p = #1 p_newvar2;
                            val newvar2 = #2 p_newvar2 ;
                            val print1 = printStringtoFile(newvar1^" := "^newvar^a,outfile)
                            val print2 = printStringtoFile(newvar2^"\n",outfile);
				        in
				        	(p,newvar1)		
				        end
					end
            | ERROR(a) => errorMessage("Operator or Numeral")
			| _ => (token, newvar)
    

	and intF (token:TOKEN,symTab) = 
			case token of
				NEG => 
                    let
						val t = consume(token); 
                        val newvar = newtemp(symTab@["#"],2);
					in
													
						let 
					        val p_newvar1 = intF1(t,symTab) ;
                            val p = #1 p_newvar1 ;
                            val newvar1 = #2 p_newvar1 ;
                            val print1 = printStringtoFile(newvar^" := neg ",outfile);
                            val print2=printStringtoFile(newvar1^"\n",outfile);
					    in
					        (p,newvar)		
					    end
					end
          | ERROR(a) => errorMessage("Operator or Numeral")
            | _ 		=> 
                    let 
				        val p_newvar = intF1(token,symTab);
				    in
				        p_newvar		
					end
								

	and intF1 (token:TOKEN,symTab) = 
			case token of
				IDENTF(a) => 
                    let 
				        val p = consume(token);
                        val newvar = lookup(a,symTab,0);
				    in
				        (p,newvar)		
					end
			|	VINT(a) => 
                    let 
				        val p = consume(token);
				    in
				        (p,a)
					end
			|	LB => 
                    let 
                        val t = consume(token);
                    in 
						let 
                            val t1_newvar = intexp(t,symTab);
                            val t1 = #1 t1_newvar ;
                            val newvar = #2 t1_newvar ;
                        in
							case t1 of 
								RB => 
                                    let 
							        	val p = consume(t1)
							        in
							            (p,newvar)		
								    end
                | _ => errorMessage(")")
												
						end
					end
      | _ => errorMessage("Numeral or '(' or Identifier")
    and boolexp (token:TOKEN,symTab) = 
						let
							val t_newvar = boolF(token, symTab);
                            val t = #1 t_newvar ;
                            val newvar = #2 t_newvar ;
						in
							let 
				        		val p_newvar1 = boolE(t, symTab, newvar);
                                val p = #1 p_newvar1 ;
                                val newvar1 = #2 p_newvar1 ;
				        	in
				        	    (p, newvar1)
					        end	
						end

	and boolE (token:TOKEN, symTab, newvar) = 
			case token of
				OR => 
                    let
                        val newvar1 = newtemp(symTab@["#"],6) ;
					    val t = consume(token);
					in
						let 
				        	val p_newvar2 = boolexp(t, symTab);
                            val p = #1 p_newvar2 ;
                            val newvar2 = #2 p_newvar2 ;
                            val print1 = printStringtoFile(newvar1^" := "^newvar^" or "^newvar2^"\n",outfile);
				        in
				        	(p, newvar1)
					    end
					end
    | ERROR(a) => errorMessage("Operator or Bool")
	| _ => (token, newvar)

	and boolF (token:TOKEN, symTab) = 
							    let
									val t_newvar = boolG(token, symTab);
                                    val t = #1 t_newvar ;
                                    val newvar = #2 t_newvar ;
								in
								    let 
		        						val p_newvar1 = boolF1(t, symTab, newvar);
                                        val p = #1 p_newvar1 ;
                                        val newvar1 = #2 p_newvar1;
		        					in
		        					    (p, newvar1)
			        				end	
								end

	and boolF1 (token:TOKEN, symTab, newvar) = 
				case token of
					AND => 
                        let
                            val newvar1 = newtemp(symTab@["#"],6);
							val t = consume(token);
						in
							let 
				        		val p_newvar2 = boolF(t, symTab);
                                val p = #1 p_newvar2;
                                val newvar2 = #2 p_newvar2;
                                val print1 = printStringtoFile(newvar1^" := "^newvar^" and "^newvar2^"\n",outfile)
				        	in
				        		(p,newvar1)
					        end
						end
        | ERROR(a) => errorMessage("Operator or Bool")
				| _ => (token, newvar)			

	and boolG (token:TOKEN, symTab) = 
					let
						val t_newvar = boolH(token, symTab);
                        val t = #1 t_newvar;
                        val newvar = #2 t_newvar;
					in
						let 
		        		    val p_newvar1 =  boolG1(t, symTab, newvar);
                            val p = #1 p_newvar1 ;
                            val newvar1 = #2 p_newvar1;
		        		in
		        			(p,newvar1)
			            end
					end

	and boolG1 (token:TOKEN, symTab, newvar) = 
				case token of
					EQUAL => 	
                        let
                            val newvar1 = newtemp(symTab@["#"],5);
							val t = consume(token);
						in
							let 
				        		val p_newvar2 = boolG(t, symTab);
                                val p = #1 p_newvar2 ;
                                val newvar2 = #2 p_newvar2 ;
                                val print1 = printStringtoFile(newvar1^" := "^newvar^" = "^newvar2^"\n", outfile)
				        	in
				        		(p,newvar1)
					        end
						end
				|	RELOP(a) => 	
                        let 
                            val newvar1 = newtemp(symTab@["#"],5);
							val t = consume(token);
						in
							let 
				        	    val p_newvar2 = boolG(t, symTab);
                                val p = #1 p_newvar2 ;
                                val newvar2 = #2 p_newvar2 ;
                                val print1 = printStringtoFile(newvar1^" := "^newvar^a^newvar2^"\n", outfile);
				        	in
				        		(p,newvar1)
					        end
						end
        | ERROR(a) => errorMessage("Operator or Bool")
		| _ => (token, newvar)		

	and boolH (token:TOKEN, symTab) = 
			                let
								val t_newvar = (boolI(token, symTab));
                                val t = #1 t_newvar;
                                val newvar = #2 t_newvar;
							in
								let 
		        					val p_newvar1 = boolH1(t, symTab, newvar);
                                    val p = #1 p_newvar1;
                                    val newvar1 = #2 p_newvar1;
		        			    in
		        					(p,newvar1)
			        			end
							end

	and boolH1 (token:TOKEN, symTab, newvar) = 
				case token of
						RELOP(a) => 	
                            let
                                val newvar1 = newtemp(symTab@["#"],4);
								val t = consume(token);
							in
                  
							    let 
				        			val p_newvar2 = boolH(t, symTab);
                                    val p = #1 p_newvar2;
                                    val newvar2 = #2 p_newvar2;
                                    val print1 = printStringtoFile(newvar1^" := "^newvar^a^newvar2^"\n", outfile)
				        		in
				        			(p,newvar1)
					        	end
							end
            | ERROR(a) => errorMessage("Operator or Numeral")
			| _ => (token,newvar)		

	and boolI (token:TOKEN, symTab) = 
						case token of
							NOT => let
                                        val newvar = newtemp(symTab@["#"],3);
                                        val t = consume(token);
									in 
                                        
										    let 
				        						val p_newvar1 =  (boolJ(token,symTab));
                                                val p = #1 p_newvar1 ;
                                                val newvar1 = #2 p_newvar1 ;
                                                val print1 = printStringtoFile(newvar^" := not"^newvar1^"\n", outfile);
                                            in
				        							(p, newvar)
					        				end
									end
								| ERROR(a) => errorMessage("Operator or bool")
								|	_ 	=> 
                                        let 
			        						val p_newvar = (boolJ(token, symTab));
                                            val p = #1 p_newvar;
                                            val newvar = #2 p_newvar ;
			        					in
			        						(p,newvar)
				        				end
								

	and boolJ (token:TOKEN, symTab) = 
			case token of
					VBOOL(a) => 
                            let 
					        	val p = consume(token);
					        in
					        	    (p,a)
						    end
            | ERROR(a) => errorMessage("Bool")
									
			|	_ => 
                let 
			        val p_newvar =  intexp(token, symTab);
                    val p  = #1 p_newvar ;
                    val newvar = #2 p_newvar ;
			    in
			        (p, newvar)
				end
								
	


    in
		case TextIO.inputLine ins of
			SOME(v) => program(totok(v))
		|	NONE => (TextIO.closeIn ins; totok("NULL"))
	end ;


readfile(hd (tl code),outfile);
print("\n\n\n**********    Parsable Code!! SUCCESS!!      ***********\n\n\n") ;
OS.Process.exit(OS.Process.success);



