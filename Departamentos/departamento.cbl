       IDENTIFICATION VIVISION.
       PROGRAM-ID. DEPARTAMENTO.
       ENVIRONMENT DIVISION. 
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
      *    DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT DEPT-ARQ ASSIGN TO "DEPARTAMENTOS-DATA.dat"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS FILE-OUTPUT-STATUS.
           

       DATA DIVISION.
       FILE SECTION.
       FD DEPT-ARQ.
       01 DEPARTAMENTO.
           05 CONTA                           PIC 9(02).
              10 CENTRO-CUSTO                 PIC 9(02).
              10 COD-DEPTO                    PIC 9(03).
           05 NOME-DEPTO.
              10 NOME-DEPTO-DET               PIC X(28).
              10 SEQ-DEPTO                    PIC 9(02).
           05 LOCAL-DEPTO.
              10 NOME-LOCAL-DEPTO             PIC X(27).
              10 SEQ-LOCAL-DEPTO              PIC 9(03).
           05 TOTAL-DESP                      PIC 9(08)V99.

       WORKING-STORAGE SECTION.
       01 FILE-OUTPUT-STATUS                  PIC X(2).
       01 EOF-FLAG                            PIC X    VALUE 'N'.
           88 END-OF-FILE                     VALUE 'Y'. 
       01 RECORD-COUNT                        PIC 9(5) VALUE ZEROS.
       01 WS-HEADER-FILE.
           05 HEADER-PAGE.        
              10 FILLER                       PIC X(5) VALUE "PAGE ".
              10 HEADER-PAGE-NMBR             PIC 9(2) VALUE ZEROS.
           05 HEADER-CENTRO-CUSTO             PIC X(10) VALUE SPACES.
           05 HEADER-COD-DEPTO                PIC X(10) VALUE SPACES.
           05 HEADER-NOME-DEPTO-DET           PIC X(30) VALUE SPACES.
           05 HEADER-NOME-LOCAL-DEPTO         PIC X(30) VALUE SPACES.
           05 HEADER-TOTAL-DESP               PIC X(08) VALUE SPACES.
       01 WS-VARIAVEIS-ARQ.
           05 WS-CENTRO-CUSTO                 PIC 9(02).
           05 WS-COD-DEPTO                    PIC 9(03).
           05 WS-NOME-DEPTO-DET               PIC X(28) VALUE SPACES.
           05 WS-SEQ-DEPTO                    PIC 9(02).
           05 WS-NOME-LOCAL-DEPTO             PIC X(27) VALUE SPACES.
           05 WS-SEQ-LOCAL-DEPTO              PIC 9(03).
           05 WS-TOTAL-DESP                   PIC Z(08)V99 VALUE ZEROS.
       01 CAMPO-AUX.
           