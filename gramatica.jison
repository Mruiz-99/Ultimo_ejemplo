
%{ 
    const Declaracion => (identifcar, valor)
    
    
    %}

%lex 

%options case-insensitive

%%

\"((\\\\")|[^\n\"])*\"      {yytext= yytext.substr(1,yyleng-2); return 'CADENA';}

\s+                 // se ignoran espacios en blanco

"//".*                  // comentario simple
[/][*][^*]*[*]+([^/*][^*]*[*]+)*[/]     //comentario multiple

"print"         return 'PRINT';
"toLower"       return 'LOWER';
"toUpper"       return 'UPPER';
"length"        return 'LENGTH';
"truncate"      return 'TRUNCATE';
"round"         return 'ROUND';
"typeof"        return 'TYPEOF';
"toString"      return 'TOSTRING';
"toCharArray"   return 'CHARARRAY';
"exec"          return 'EXEC';
"new"           return 'NEW';

"int"           return 'RENTERO';
"double"        return 'RDOUBLE';
"boolean"       return 'RBOOLEANO';
"char"          return 'RCARACTER';
"string"        return 'RCADENA';

\\\n            return 'SALTOLINEA';
\\\\            return 'BARINVERSA';
\\\'            return 'COMILLASSIM';
\\\"            return 'COMILLASDOB';
\\\t            return 'TABULAR';

"while"         return 'WHILE';
"do"            return 'DO';
"if"            return 'IF';
"else"          return 'ELSE';
"for"           return 'FOR';
"switch"        return 'SWITCH';
"case"          return 'CASE';
"default"       return 'DEFAULT';

"break"         return 'BREAK';
"continue"      return 'CONTINUE';
"return"        return 'RETURN';

"void"          return 'VOID';

":"             return 'DOSPTS';
","             return 'COMA';
";"             return 'PTCOMA';
"{"             return 'LLAVIZQ';
"}"             return 'LLAVDER';
"("             return 'PARIZQ';
")"             return 'PARDER';
"["             return 'CORIZQ';
"]"             return 'CORDER';


"+="             return 'O_MAS';
"-="             return 'O_MENOS';
"*="             return 'O_POR';
"/="             return 'O_DIVIDIDO';

"++"            return 'INCREMENTO';
"--"            return 'DECREMENTO';
"+"             return 'MAS';
"-"             return 'MENOS';
"*"             return 'POR';
"/"             return 'DIVIDIDO';
"^"             return 'POTENCIA';
"%"             return 'MOD';




"<="            return 'MENIGQUE';
">="            return 'MAYIGQUE';
"=="            return 'DOBLEIGUAL';
"!="            return 'NOIGUAL';
"<"             return 'MENQUE';
">"             return 'MAYQUE';


"!"             return 'NOT';
"&&"            return 'AND';
"||"            return 'OR';


"true"          return 'TRUE';
"false"         return 'FALSE';

"="             return 'IGUAL';
"?"             return 'OPTERNARIO';
"&"             return 'CONCAT';



\'((\\\\')|[^\n\'])\'      {yytext= yytext.substr(1,yyleng-2); return 'CARACTER';}

[0-9]+("."[0-9]+)\b         return 'DECIMAL';
[0-9]+\b                    return 'ENTERO';
([a-zA-Z])[a-zA-Z0-9_]*     return 'IDENTIFICADOR';

<<EOF>>                     return 'EOF';

.                           {console.log('Este es un error lexico'+yytext+yylloc.first_line+yylloc.first_column)}

/lex


%left 'OR'
%left 'AND'
%right 'NOT'
%left  'DOBLEIGUAL' 'NOIGUAL' 'MENQUE' 'MAYQUE' 'MENIGQUE' 'MAYIGQUE'
%left   'MAS' 'MENOS'
%left   'POR' 'DIVIDIDO' 'MOD'
%nonassoc   'POTENCIA'
%right UMENOS

%start IDENTIFICADOR

%%

inicio:     
        instrucciones EOF   {console.log(JSON.strinify($1,null,2));
                            let sal = Salida;
                            Salida = []
                            return {salida: sal, ast: $1}
                            }
;

instrucciones:
          instrucciones  instruccion {$$=$1; $$.push($2)}
        | instruccion                {$$=[]; $$.push($1);}
;

instruccion 
    : PRINT PARIZQ expresion PARDER PTCOMA	                { $$ = print("print",$3); }
    | DECLARACION                                           { $$ = $1 }
    | ASIGNACION                                            { $$ = $1 }
    | condIF                                                { $$ = $1 }
    | switchCASE                                            { $$ = $1 }
    | condWHILE                                             { $$ = $1 }
    | condDOWHILE                                           { $$ = $1 }
    | condFOR                                               { $$ = $1 }
    | FUNCION                                               { $$ = $1 }
    | LLAMADA                                               { $$ = $1 }
    | IDENTIFICADOR INCREMENTO PTCOMA		                { $$ = Incremento($1);}
    | IDENTIFICADOR DECREMENTO PTCOMA		                { $$ = Decremento($1);}
    | BREAK PTCOMA                                          { $$ = insBreak();}
    | CONTINUE PTCOMA                                      {$$ = insContinue();}
    | RETORNO
    | error  {Salida.push('Este es un error Sintactico: ' + yytext + ', en la linea: ' + this.$.first_line + ', en la columna: ' + this.$.first_line.first_column);
                    errores.push(new Error_(this.$.first_line,this.$.first_column,"Sintactico","No se esperaba la expresion: " + yytext));
                    console.log('Este es un error Sintactico: ' + yytext + ', en la linea: ' + yylineno.first_line + ', en la columna: ' + yylineno.first_column);}
;

RETORNO   
    : RETURN expresion  PTCOMA   { $$ = Retorno($2); }
    | RETURN PTCOMA       { $$ = Retorno(setSimbolos("@Vacio@","void")); }
;

LLAMADA
    : IDENTIFICADOR PARIZQ PARDER  PTCOMA                         { $$ = Llamada($1,[]);}
    | IDENTIFICADOR PARIZQ L_exp PARDER  PTCOMA                   { $$ = Llamada($1,$3);}
    | EXEC IDENTIFICADOR PARIZQ PARDER  PTCOMA                    { $$ = LlamadaEXEC($2,[]);}
    | EXEC IDENTIFICADOR PARIZQ L_exp PARDER  PTCOMA              { $$ = LlamadaEXEC($2,$4);}
;

FUNCION
    : TIPO IDENTIFICADOR PARIZQ PARDER BLOQUE                       { $$ = Funcion($2,[],$1,$5); }
    | VOID IDENTIFICADOR PARIZQ PARDER BLOQUE                       { $$ = Funcion($2,[],"void",$5); }
    | TIPO IDENTIFICADOR PARIZQ PARAMETROS PARDER BLOQUE            { $$ = Funcion($2, $4, $1, $6); }
    | VOID IDENTIFICADOR PARIZQ PARAMETROS PARDER BLOQUE            { $$ = Funcion($2, $4, "void", $6); }
;

PARAMETROS
    : PARAMETROS COMA TIPO IDENTIFICADOR                        { $$ = $1; $$.push(Declaracion($4, $3, null));}
    | TIPO IDENTIFICADOR                                        { $$ = []; $$.push(Declaracion($2, $1, null));}
;

condFOR
    : FOR PARIZQ DECLARACION expresion PTCOMA refeshFOR PARDER  BLOQUE       { $$ = condFOR($3, $4, $6, $8);}
    | FOR PARIZQ ASIGNACION  expresion PTCOMA refeshFOR PARDER  BLOQUE       { $$ = condFOR($3, $4, $6, $8);}
;

refeshFOR
    : IDENTIFICADOR INCREMENTO				            { $$ = Incremento($1);}
    | IDENTIFICADOR DECREMENTO				            { $$ = Decremento($1);}
    | ASIGNACION                                        {$$ = $1;}
;

condWHILE
    : WHILE expresion BLOQUE                { $$ = condWHILE($2, $3);}
;

condDOWHILE
    : DO BLOQUE WHILE expresion PTCOMA               { $$ = condDOWHILE($4, $2);}
;

switchCASE
    : SWITCH expresion LLAVIZQ ListCase LLAVDER                    { $$ = Seleccionar($2, $4); }
    | SWITCH expresion LLAVIZQ ListCase DEFAULT BLOQUECASE LLAVDER { $$ = Seleccionar($2, $4, $6); } 
;

ListCase
    : ListCase CASE expresion  BLOQUECASE   { $$ = $1; $$.push(Caso($3, $4)); }
    | CASE expresion BLOQUECASE             { $$ = []; $$.push(Caso($2, $3)); }
;

BLOQUECASE
    : DOSPTS                          { $$ = []; }
    | DOSPTS instrucciones            { $$ = $2; }
; 

condIF
    : IF expresion BLOQUE                   { $$ = condIF($2, $3, null); }
    | IF expresion BLOQUE ELSE condIF   { $$ = condIF($2, $3, Array ($5) );}
    | IF expresion BLOQUE ELSE BLOQUE       { $$ = condIF($2, $3, $5 ); }
;  
   

BLOQUE
    : LLAVIZQ LLAVDER                       { $$ = []; }
    | LLAVIZQ instrucciones LLAVDER         { $$ = $2; }
; 

ASIGNACION 
    :IDENTIFICADOR IGUAL expresion  PTCOMA	        { $$ = Asignar($1,$3); }
;

DECLARACION 
    : TIPO IDENTIFICADOR IGUAL expresion  PTCOMA	        { $$ = Declaracion($2,$1,$4); }
    | TIPO IDENTIFICADOR PTCOMA	                            { $$ = Declaracion($2,$1,null); }
;

TIPO 
    : RENTERO               { $$ = "entero" }
    | Rbooleano             { $$ = "booleano" }   
    | RCADENA               { $$ = "cadena" }   
    | RCARACTER             { $$ = "caracter" }   
    | RDOUBLE               { $$ = "doble" }   
;

expresion
	: expresion MAS expresion				{ $$ = setOperacion($1,$3,"+");}
	| expresion MENOS expresion				{ $$ = setOperacion($1,$3,"-");}
	| expresion POR expresion				{ $$ = setOperacion($1,$3,"*");}
	| expresion DIVIDIDO expresion			{ $$ = setOperacion($1,$3,"/");}
	| expresion POTENCIA expresion			{ $$ = setOperacion($1,$3,"^");}
	| expresion MOD expresion				{ $$ = setOperacion($1,$3,"%");}
	| expresion DOBLEIGUAL expresion		{ $$ = setOperacion($1,$3,"==");}
	| expresion NOIGUAL expresion			{ $$ = setOperacion($1,$3,"!=");}
	| expresion MENQUE expresion			{ $$ = setOperacion($1,$3,"<");}
	| expresion MAYQUE expresion			{ $$ = setOperacion($1,$3,">");}
	| expresion MENIGQUE expresion			{ $$ = setOperacion($1,$3,"<=");}
	| expresion MAYIGQUE expresion			{ $$ = setOperacion($1,$3,">=");}
	| expresion AND expresion				{ $$ = setOperacion($1,$3,"and");}
	| expresion NOT expresion				{ $$ = setOperacion($1,$3,"not");}
	| expresion OR expresion				{ $$ = setOperacion($1,$3,"or");}
    | PARIZQ expresion PARDER     			{ $$ = $2}
    | LOWER PARIZQ expresion PARDER         { $$ = setOperacionUnario($3,"lower"); }
    | UPPER PARIZQ expresion PARDER         { $$ = setOperacionUnario($3,"upper"); }
    | LENGTH PARIZQ expresion PARDER         { $$ = setOperacionUnario($3,"length"); }
    | TRUNCATE PARIZQ expresion PARDER         { $$ = setOperacionUnario($3,"truncate"); }
    | ROUND PARIZQ expresion PARDER         { $$ = setOperacionUnario($3,"round"); }
    | TYPEOF PARIZQ expresion PARDER         { $$ = setOperacionUnario($3,"typeof"); }
    | TOSTRING PARIZQ expresion PARDER         { $$ = setOperacionUnario($3,"tostring"); }
	| NOT expresion							{ $$ = setOperacionUnario($2,"not");}
	| MENOS expresion %prec UMENOS			{ $$ = setOperacionUnario($2,"umenos");}
	| TRUE				     				{ $$ = setSimbolos(true,"booleano");}
	| FALSE				     				{ $$ = setSimbolos(false,"booleano");}
	| CADENA				     			{ $$ = setSimbolos($1,"cadena");}
	| CARACTER				     			{ $$ = setSimbolos($1,"caracter");}
	| DECIMAL				     			{ $$ = setSimbolos(parseFloat($1),"doble");}
	| ENTERO				     			{ $$ = setSimbolos($1,"entero");}
	| IDENTIFICADOR							{ $$ = setSimbolos($1,"identificador");}
    | IDENTIFICADOR PARIZQ PARDER           { $$ = setSimbolos({Id: $1, Params: []}, "funcion");}
    | IDENTIFICADOR PARIZQ L_exp PARDER     { $$ = setSimbolos({Id: $1, Params:$3}, "funcion");}
    	
;
L_exp
    : L_exp COMA expresion              { $$ = $1; $$.push($3);}
    | expresion                         { $$ = []; $$.push($1);}
;