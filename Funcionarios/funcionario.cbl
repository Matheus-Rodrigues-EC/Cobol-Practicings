       IDENTIFICATION DIVISION.
       PROGRAM-ID. FUNCIONARIO.
       ENVIRONMENT DIVISION. 
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
      *    DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT FUNC-ARQ ASSIGN TO "FUNCIONARIOS-DATA.dat"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS FILE-OUTPUT-STATUS.
           

       DATA DIVISION.
       FILE SECTION.
       FD FUNC-ARQ.
       01 FUNCIONARIO.
           05 MATRICULA      PIC 9(06).
           05 FILLER         PIC X(04) VALUE SPACES.
           05 NOME           PIC X(60).
           05 FILLER         PIC X(02) VALUE SPACES.
           05 CARGO          PIC X(20).
           05 FILLER         PIC X(02) VALUE SPACES.
           05 SALARIO        PIC 9(06)V99.
           05 FILLER         PIC X(02) VALUE SPACES.
           05 DATA-ADMISSAO  PIC X(08).

       WORKING-STORAGE SECTION.
       01 FILE-OUTPUT-STATUS       PIC X(2).
       01 EOF-FLAG                 PIC X    VALUE 'N'.
           88 END-OF-FILE          VALUE 'Y'.
       01 RECORD-COUNT             PIC 9(5) VALUE ZEROS.
       01 WS-HEADER-FILE.
           05 HEADER-PAGE.        
              10 FILLER             PIC X(5) VALUE "PAGE ".
              10 HEADER-PAGE-NMBR   PIC 9(2) VALUE ZEROS.
           05 HEADER-MAT         PIC X(10) VALUE SPACES.
           05 HEADER-NOME        PIC X(30) VALUE SPACES.
           05 HEADER-SOBRE       PIC X(30) VALUE SPACES.
           05 HEADER-SALARIO     PIC X(08) VALUE SPACES.
           05 HEADER-DATA        PIC X(10) VALUE SPACES.
       01 WS-VARIAVEIS-ARQ.
           05 WS-MATRICULA       PIC 9(06).
           05 FILLER             PIC X(04) VALUE SPACES.
           05 WS-NOME-VAR.
              10 WS-NOME         PIC X(26) VALUE "FULANO".
              10 WS-SEQ-1        PIC 9(04) VALUE ZEROS.
              10 WS-SOBRENOME    PIC X(26) VALUE "DA SILVA".
              10 WS-SEQ-2        PIC 9(04) VALUE ZEROS.
           05 FILLER             PIC X(02) VALUE SPACES.
           05 WS-CARGO           PIC X(20) VALUE SPACES.
           05 FILLER             PIC X(02) VALUE SPACES.
           05 WS-SALARIO         PIC Z(06)V99 VALUE ZEROS.
           05 FILLER             PIC X(02) VALUE SPACES.
           05 WS-DATA-ADMISSAO   PIC X(08) VALUE ZEROS.
       01 TAB-CARGO.
           05 FILLER             PIC X(20) VALUE "AAAAAAAAAAAAAAAAAAAA".
           05 FILLER             PIC X(20) VALUE "BBBBBBBBBBBBBBBBBBBB".
           05 FILLER             PIC X(20) VALUE "CCCCCCCCCCCCCCCCCCCC".
           05 FILLER             PIC X(20) VALUE "DDDDDDDDDDDDDDDDDDDD".
           05 FILLER             PIC X(20) VALUE "EEEEEEEEEEEEEEEEEEEE".
       01 TAB-CARGO-RED REDEFINES TAB-CARGO.
           05 DESC-CARGO PIC X(20) OCCURS 5 TIMES.
       01 CAMPO-AUX.
           05 I-CARGO              PIC 9(02) VALUE 1.
           05 SEQ-REG              PIC 9(20) VALUE 1.
           05 WS-RECS-GRAVADOS     PIC 9(20) VALUE ZEROS.
           05 I-PAGE               PIC 9(02) VALUE 1.
           01 TOTAL-FILE-BUDGET    PIC 9(12) VALUE ZEROS.
           01 SEQ-PAGE             PIC 9(10) VALUE ZEROS.
       01  WS-CURRENT-DATE-FIELDS.
            05  WS-CURRENT-DATE.
                10  WS-CURRENT-YEAR    PIC  9(4).
                10  FILLER             PIC  X(1) VALUE "/".
                10  WS-CURRENT-MONTH   PIC  9(2).
                10  FILLER             PIC  X(1) VALUE "/".
                10  WS-CURRENT-DAY     PIC  9(2).
            05  WS-CURRENT-TIME.
                10  WS-CURRENT-HOUR    PIC  9(2).
                10  FILLER             PIC  X(1) VALUE ":".
                10  WS-CURRENT-MINUTE  PIC  9(2).
                10  FILLER             PIC  X(1) VALUE ":".
                10  WS-CURRENT-SECOND  PIC  9(2).
                10  FILLER             PIC  X(1) VALUE ":".
                10  WS-CURRENT-MS      PIC  9(2).
            05  WS-DIFF-FROM-GMT       PIC S9(4).
       01 CONFIG-FLAGS.    
           05 WS-MAX-REGISTROS            PIC 9(10) VALUE ZEROS.
           05 WS-MAX-REGISTROS-PER-PAGES  PIC 9(2) VALUE 1.
       01 WS-PAGE-REGISTER         .
           05 ACTUAL-PAGE-HEADER      PIC X(100) VALUE SPACES.
           05 ACTUAL-PAGE-NUMBER      PIC 9(10) VALUE 0.
           05 ACTUAL-PAGE-BUDGET      PIC 9(15) VALUE ZEROS. 
           05 WS-FOOTER-PAGE          PIC X(90) VALUE SPACES.

       01 WS-FILE-REGISTER.
           05 TOTAL-SALARY-BUDGET      PIC 9(15) VALUE ZEROS.

       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM 000-CONFIG-FUNCTION
           PERFORM 001-WRITE-AUTO-DATA-FILE.
           STOP RUN.

       000-CONFIG-FUNCTION.
           MOVE 1000 TO WS-MAX-REGISTROS.
           MOVE 40 TO WS-MAX-REGISTROS-PER-PAGES.
       
       001-WRITE-AUTO-DATA-FILE.
           PERFORM 400-INITIALIZATION-OUTPUT.
           PERFORM GET-CURRENT-DATE.
           IF FILE-OUTPUT-STATUS NOT = "00"
              DISPLAY "Error writing record: " FILE-OUTPUT-STATUS
              PERFORM ERROR-OUTPUT-ROUTINE
           END-IF.
           PERFORM 002-WRITE-PAGE VARYING SEQ-REG FROM 1 BY 1
              UNTIL SEQ-REG > WS-MAX-REGISTROS OR END-OF-FILE.
           PERFORM 400-TERMINATION-OUTPUT.

       002-WRITE-PAGE.
           PERFORM 003-FUNC-AUTO-RECORD
           PERFORM MODULE-OPERATION-PAGE.
       
       003-FUNC-AUTO-CREATION.
           MOVE SEQ-REG TO WS-MATRICULA.
           MOVE SEQ-REG TO WS-SEQ-1.
           MOVE SEQ-REG TO WS-SEQ-2.
           MOVE DESC-CARGO(I-CARGO) TO WS-CARGO.
           PERFORM MODULE-OPERATION-CARGO.
           COMPUTE WS-SALARIO = 1800 + (FUNCTION RANDOM * (3600 - 1800))
           STRING
              WS-CURRENT-YEAR
              WS-CURRENT-MONTH
              WS-CURRENT-DAY
              DELIMITED BY SIZE
              INTO WS-DATA-ADMISSAO
           END-STRING.
       
       003-FUNC-AUTO-RECORD.
           PERFORM 003-FUNC-AUTO-CREATION.
           WRITE FUNCIONARIO FROM WS-VARIAVEIS-ARQ.
           IF FILE-OUTPUT-STATUS = "00"
              ADD 1 TO WS-RECS-GRAVADOS
           ELSE
              DISPLAY "ERROR WRITING RECORD: " FILE-OUTPUT-STATUS
              PERFORM ERROR-OUTPUT-ROUTINE
           END-IF.
       
       WRITE-HEADER-FILE.
           MOVE "MATRICULA" TO HEADER-MAT.
           MOVE "NOME" TO HEADER-NOME.
           MOVE "SOBRENOME" TO HEADER-SOBRE.
           MOVE "SALARIO" TO HEADER-SALARIO.
           MOVE "DATA" TO HEADER-DATA.
           WRITE FUNCIONARIO FROM WS-HEADER-FILE.
       
       WRITE-FOOTER-PAGE.
           STRING " NOVA PAGINA. TOTAL DE REGISTROS: "
           WS-MAX-REGISTROS " ,TOTAL FILE BUDGET: " ACTUAL-PAGE-BUDGET 
           DELIMITED BY SIZE
           INTO WS-FOOTER-PAGE.
       
       WRITE-FOOTER-FILE.
           STRING " NOVA PAGINA. TOTAL DE PAGINAS REGISTROS: "
              ACTUAL-PAGE-NUMBER
              " ,TOTAL FILE BUDGET: "
              TOTAL-SALARY-BUDGET
           DELIMITED BY SIZE
           INTO WS-FOOTER-PAGE.
       
       MODULE-OPERATION-CARGO.
           ADD 1 TO I-CARGO.
           IF I-CARGO > 5
              MOVE 1 TO I-CARGO
           END-IF.

       MODULE-OPERATION-PAGE.
           ADD 1 TO I-PAGE.
           IF I-PAGE > WS-MAX-REGISTROS-PER-PAGES
              MOVE 1 TO I-PAGE
           END-IF.

       GET-CURRENT-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS. 
       
       400-INITIALIZATION-OUTPUT.
           OPEN OUTPUT FUNC-ARQ.
           IF FILE-OUTPUT-STATUS NOT = "00"
              DISPLAY "ERROR OPENING OUTPUT FILE: " FILE-OUTPUT-STATUS 
              MOVE "Y" TO EOF-FLAG
           END-IF.

       400-TERMINATION-OUTPUT.
           CLOSE FUNC-ARQ 
           DISPLAY "TOTAL DE REGISTROS GRAVADOS: " WS-RECS-GRAVADOS.
           DISPLAY "File processing complete.".

       ERROR-OUTPUT-ROUTINE.
           DISPLAY "File operation failed with status: " 
           FILE-OUTPUT-STATUS 
           DISPLAY "Processing terminated."
           MOVE "Y" TO EOF-FLAG.
