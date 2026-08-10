       IDENTIFICATION DIVISION. 
       PROGRAM-ID. RELATORIO-FUNCIONARIOS.

       ENVIRONMENT DIVISION. 
       CONFIGURATION SECTION. 
       SPECIAL-NAMES. 
           DECIMAL-POINT IS COMMA.
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 

           SELECT FUNC-ARQ
              ASSIGN TO "FUNCIONARIOS-DATA.dat"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS FUNC-FILE-STATUS.

           SELECT RELATORIO-ARQ
              ASSIGN TO "RELATORIO-FUNCIONARIOS.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS RELAT-FILE-STATUS.

       DATA DIVISION.

       FILE SECTION. 

       FD FUNC-ARQ.

       01 FUNCIONARIO-REGISTRO.
           05 MATRICULA-ARQ                 PIC 9(06).
           05 FILLER                        PIC X(04).
           05 NOME-ARQ                      PIC X(26).
           05 SEQUENCIA-ARQ                 PIC 9(04).
           05 SOBRENOME-ARQ                 PIC X(26).
           05 SEQUENCIA-SOBRE               PIC 9(04).
           05 FILLER                        PIC X(02).
           05 CARGO-ARQ                     PIC X(20).
           05 FILLER                        PIC X(02).
           05 SALARIO-ARQ-TEXTO             PIC X(08).
           05 FILLER                        PIC X(02).
           05 DATA-ADMISSAO-ARQ             PIC X(08).

       FD RELATORIO-ARQ.

       01 RELATORIO-LINHA                   PIC X(120).

       WORKING-STORAGE SECTION. 

       01 WS-STRING-POINTER                 PIC 9(03) VALUE 1.

       01 WS-FILE-STATUS.
           05 FUNC-FILE-STATUS              PIC X(02).
           05 RELAT-FILE-STATUS             PIC X(02).

       01 WS-FIM-ARQUIVO                    PIC X(01) VALUE "N".
           88 FIM-ARQUIVO                   VALUE "S".
           88 NAO-FIM-ARQUIVO               VALUE "N".

       01 WS-CONTROLE.
           05 WS-REGISTROS-PAGINA           PIC 9(02) VALUE ZEROS.
           05 WS-TOTAL-FUNCIONARIOS         PIC 9(10) VALUE ZEROS.
           05 WS-NUMERO-PAGINA              PIC 9(04) VALUE 1.

       01 WS-TOTAIS.
           05 WS-SALARIOS-PAGINA            PIC 9(12)V99 VALUES ZEROS.
           05 WS-TOTAL-SALARIOS             PIC 9(12)V99 VALUES ZEROS.
           05 WS-SALARIO-ARQ                 PIC 9(06)V99 VALUE ZEROS.

       01 WS-CAMPOS-EDITADOS.
           05 WS-SALARIOS-EDITADO           PIC Z.ZZ9.99.
           05 WS-SALARIO-PAG-EDITADO        PIC ZZZ.ZZZ.ZZ9,99.
           05 WS-TOTAL-SALARIO-EDITADO      PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
           05 WS-TOTAL-FUNC-EDITADO         PIC ZZZ.ZZZ.ZZ9.
           05 WS-PAGINA-EDITADA             PIC ZZ9.
           05 WS-QTD-PAGINA-EDITADO         PIC ZZ9.

       01 WS-DATA-SISTEMA.
           05 WS-DATA-BRUTA                 PIC X(21).
           05 WS-DIA                        PIC X(02).
           05 WS-MES                        PIC X(02).
           05 WS-ANO                        PIC X(04).
           05 WS-DATA-FORMATADA             PIC X(10).

       01 WS-DATA-ADMISSAO.
           05 WS-ADMISSAO-DIA               PIC X(02).
           05 WS-SEPARADOR-DATA-1           PIC X VALUE "/".
           05 WS-ADMISSAO-MES               PIC X(02).
           05 WS-SEPARADOR-DATA-2           PIC X VALUE "/".
           05 WS-ADMISSAO-ANO               PIC X(04).

       01 WS-DATA-RELATORIO.

           05 WS-LINHA-TITULO.
              10 FILLER                     PIC X(05) VALUE SPACES.
              10 FILLER                     PIC X(32) 
                 VALUE "GOT - RELACAO DE FUNCIONARIOS - ".
              10 FILLER                     PIC X(07) VALUE "DATA: ".
              10 WS-TITULO-DATA             PIC X(10).
              10 FILLER                     PIC X(70) VALUE SPACES.

       01 WS-LINHA-PAGINA.
           05 FILLER                 PIC X(05) VALUE SPACES.
           05 FILLER                 PIC X(08) VALUE "PAGINA: ".
           05 WS-TITULO-PAGINA       PIC ZZ9.
           05 FILLER                 PIC X(104) VALUE SPACES.

       05 WS-LINHA-COLUNAS.
              10 FILLER             PIC X(12)
                 VALUE "MATRICULA | ".
              10 FILLER             PIC X(29)
                 VALUE "NOME                       | ".
              10 FILLER             PIC X(29)
                 VALUE "SOBRENOME                  | ".
              10 FILLER             PIC X(13)
                 VALUE "ADMISSAO   | ".
              10 FILLER             PIC X(07)
                 VALUE "SALARIO".
              10 FILLER             PIC X(30) VALUE SPACES.

           05 WS-LINHA-DETALHE.
               10 WS-DET-MATRICULA          PIC 9(06).
               10 FILLER                    PIC X(03) VALUE " | ".
               10 WS-DET-NOME               PIC X(26).
               10 FILLER                    PIC X(03) VALUE " | ".
               10 WS-DET-SOBRENOME          PIC X(26).
               10 FILLER                    PIC X(03) VALUE " | ".
               10 WS-DET-ADMISSAO           PIC X(10).
               10 FILLER                    PIC X(06) VALUE " | R$ ".
               10 WS-DET-SALARIO            PIC Z.ZZ9,99.
               10 FILLER                    PIC X(29) VALUE SPACES.

           05 WS-LINHA-SEPARADOR.
               10 FILLER                    PIC X(120) VALUE ALL "-".

           05 WS-LINHA-SEPARADOR-DUPLO.
               10 FILLER                    PIC X(120) VALUE ALL "=".

           05 WS-LINHA-QTD-PAGINA.
               10 FILLER                    PIC X(30)
                   VALUE "FUNCIONARIOS NESTA PAGINA: ".
               10 WS-LINHA-QTD-PAG-EDIT     PIC ZZ9.
               10 FILLER                    PIC X(87) VALUE SPACES.

           05 WS-LINHA-SALARIO-PAGINA.
               10 FILLER                    PIC X(37)
                   VALUE "SOMA DOS SALARIOS DESTA PAGINA: R$ ".
               10 WS-LINHA-SAL-PAG-EDIT     PIC ZZZ.ZZZ.ZZ9,99.
               10 FILLER                    PIC X(69) VALUE SPACES.

           05 WS-LINHA-QTD-ACUMULADA.
               10 FILLER                    PIC X(38)
                   VALUE "SUBTOTAL DE FUNCIONARIOS JA LISTADOS: ".
               10 WS-LINHA-QTD-ACUM-EDIT    PIC ZZZ.ZZZ.ZZ9.
               10 FILLER                    PIC X(71) VALUE SPACES.

           05 WS-LINHA-SALARIO-ACUMULADO.
               10 FILLER                    PIC X(38)
                   VALUE "SOMA DOS SALARIOS JA LISTADOS: R$ ".
               10 WS-LINHA-SAL-ACUM-EDIT
                                            PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
               10 FILLER                    PIC X(65) VALUE SPACES.

           05 WS-LINHA-TOTAL-FUNC.
               10 FILLER                    PIC X(23)
                   VALUE "TOTAL DE FUNCIONARIOS: ".
               10 WS-LINHA-TOTAL-FUNC-EDIT  PIC ZZZ.ZZZ.ZZ9.
               10 FILLER                    PIC X(86) VALUE SPACES.

           05 WS-LINHA-TOTAL-SAL.
               10 FILLER                    PIC X(28)
                   VALUE "SOMA TOTAL DOS SALARIOS: R$ ".
               10 WS-LINHA-TOTAL-SAL-EDIT
                                            PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
               10 FILLER                    PIC X(75) VALUE SPACES.
       
       
       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR

           IF FUNC-FILE-STATUS = "00"
               IF RELAT-FILE-STATUS = "00"
                   PERFORM 2000-GERAR-RELATORIO
                       UNTIL FIM-ARQUIVO

                   PERFORM 5000-GRAVAR-TOTAL-GERAL
               END-IF
           END-IF

           PERFORM 6000-FINALIZAR

           STOP RUN.

       1000-INICIALIZAR.
           PERFORM 1100-OBTER-DATA-ATUAL

           OPEN INPUT FUNC-ARQ 

           IF FUNC-FILE-STATUS NOT = "00"
              DISPLAY "ERRO AO ABRIR FUNCIONARIOS-DATA.DAT"
              DISPLAY "FILE STATUS: " FUNC-FILE-STATUS
              SET FIM-ARQUIVO TO TRUE 
           END-IF 

           IF FUNC-FILE-STATUS = "00"
              OPEN OUTPUT RELATORIO-ARQ

              IF RELAT-FILE-STATUS NOT ="00"
                 DISPLAY "ERRO AO CRIAR O RELATORIO"
                 DISPLAY "FILE STATUS: " RELAT-FILE-STATUS 
                 SET FIM-ARQUIVO TO TRUE 
              END-IF
           END-IF.

       1100-OBTER-DATA-ATUAL.
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-BRUTA 
           
           MOVE WS-DATA-BRUTA(1:4) TO WS-ANO
           MOVE WS-DATA-BRUTA(5:2) TO WS-MES 
           MOVE WS-DATA-BRUTA(7:2) TO WS-DIA

           STRING 
              WS-DIA "/" WS-MES "/" WS-ANO DELIMITED BY SIZE 
              INTO WS-DATA-FORMATADA 
           END-STRING.

       2000-GERAR-RELATORIO.
           MOVE ZEROS TO WS-REGISTROS-PAGINA
           MOVE ZEROS TO WS-SALARIOS-PAGINA

           PERFORM 2100-GRAVAR-CABECALHO

           PERFORM 2200-LER-FUNCIONARIO
               UNTIL WS-REGISTROS-PAGINA = 40
                  OR WS-FIM-ARQUIVO = "S"

           IF WS-REGISTROS-PAGINA > 0
               PERFORM 3000-GRAVAR-SUBTOTAIS
           END-IF

           IF NAO-FIM-ARQUIVO
               PERFORM 4000-GRAVAR-RODAPE
               ADD 1 TO WS-NUMERO-PAGINA
           END-IF.

       2100-GRAVAR-CABECALHO.
           MOVE WS-DATA-FORMATADA TO WS-TITULO-DATA
           MOVE WS-NUMERO-PAGINA TO WS-TITULO-PAGINA

           MOVE WS-LINHA-SEPARADOR-DUPLO
               TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA

           MOVE WS-LINHA-TITULO
               TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA

           MOVE WS-LINHA-PAGINA
               TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA

           MOVE WS-LINHA-SEPARADOR-DUPLO
               TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA

           MOVE WS-LINHA-COLUNAS
               TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA

           MOVE WS-LINHA-SEPARADOR
               TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA.

       2200-LER-FUNCIONARIO.
           READ FUNC-ARQ
               AT END
                   SET FIM-ARQUIVO TO TRUE

               NOT AT END
                   PERFORM 2300-PROCESSAR-FUNCIONARIO
           END-READ.

       2300-PROCESSAR-FUNCIONARIO.
           ADD 1 TO WS-REGISTROS-PAGINA
           ADD 1 TO WS-TOTAL-FUNCIONARIOS

           COMPUTE WS-SALARIO-ARQ =
              FUNCTION NUMVAL(SALARIO-ARQ-TEXTO) / 100

           ADD WS-SALARIO-ARQ TO WS-SALARIOS-PAGINA
           ADD WS-SALARIO-ARQ TO WS-TOTAL-SALARIOS

           PERFORM 2400-FORMATAR-DATA-ADMISSAO

           MOVE MATRICULA-ARQ
               TO WS-DET-MATRICULA

           MOVE NOME-ARQ
               TO WS-DET-NOME

           MOVE SOBRENOME-ARQ
               TO WS-DET-SOBRENOME

           MOVE WS-DATA-ADMISSAO
               TO WS-DET-ADMISSAO

           MOVE WS-SALARIO-ARQ
               TO WS-DET-SALARIO

           MOVE WS-LINHA-DETALHE
               TO RELATORIO-LINHA

           WRITE RELATORIO-LINHA.

       2400-FORMATAR-DATA-ADMISSAO.
           MOVE "/" TO WS-SEPARADOR-DATA-1
                       WS-SEPARADOR-DATA-2

           IF DATA-ADMISSAO-ARQ IS NUMERIC
               MOVE DATA-ADMISSAO-ARQ(1:4)
                   TO WS-ADMISSAO-ANO

               MOVE DATA-ADMISSAO-ARQ(5:2)
                   TO WS-ADMISSAO-MES

               MOVE DATA-ADMISSAO-ARQ(7:2)
                   TO WS-ADMISSAO-DIA
           ELSE
               MOVE "--" TO WS-ADMISSAO-DIA
               MOVE "--" TO WS-ADMISSAO-MES
               MOVE "----" TO WS-ADMISSAO-ANO
           END-IF.

       3000-GRAVAR-SUBTOTAIS.
           MOVE WS-REGISTROS-PAGINA
               TO WS-LINHA-QTD-PAG-EDIT

           MOVE WS-SALARIOS-PAGINA
               TO WS-LINHA-SAL-PAG-EDIT

           MOVE WS-TOTAL-FUNCIONARIOS
               TO WS-LINHA-QTD-ACUM-EDIT

           MOVE WS-TOTAL-SALARIOS
               TO WS-LINHA-SAL-ACUM-EDIT

           MOVE WS-LINHA-SEPARADOR
               TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA

           MOVE WS-LINHA-QTD-PAGINA
               TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA

           MOVE WS-LINHA-SALARIO-PAGINA
               TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA

           MOVE WS-LINHA-QTD-ACUMULADA
               TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA

           MOVE WS-LINHA-SALARIO-ACUMULADO
               TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA.

       4000-GRAVAR-RODAPE.
           MOVE SPACES TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA

           MOVE WS-LINHA-SEPARADOR-DUPLO
               TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA

           MOVE SPACES TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA.

       5000-GRAVAR-TOTAL-GERAL.
           MOVE WS-TOTAL-FUNCIONARIOS
               TO WS-LINHA-TOTAL-FUNC-EDIT

           MOVE WS-TOTAL-SALARIOS
               TO WS-LINHA-TOTAL-SAL-EDIT

           MOVE SPACES TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA

           MOVE WS-LINHA-SEPARADOR-DUPLO
               TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA

           MOVE SPACES TO RELATORIO-LINHA
           MOVE 53 TO WS-STRING-POINTER

           STRING
               "FIM DA LISTAGEM"
                   DELIMITED BY SIZE
               INTO RELATORIO-LINHA
               WITH POINTER WS-STRING-POINTER
           END-STRING

           WRITE RELATORIO-LINHA

           MOVE WS-LINHA-TOTAL-FUNC
               TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA

           MOVE WS-LINHA-TOTAL-SAL
               TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA

           MOVE WS-LINHA-SEPARADOR-DUPLO
               TO RELATORIO-LINHA
           WRITE RELATORIO-LINHA.

       6000-FINALIZAR.
           IF FUNC-FILE-STATUS = "00"
               CLOSE FUNC-ARQ
           END-IF

           IF RELAT-FILE-STATUS = "00"
               CLOSE RELATORIO-ARQ
           END-IF

           DISPLAY "RELATORIO GERADO: "
                   "RELATORIO-FUNCIONARIOS.txt"

           DISPLAY "TOTAL DE FUNCIONARIOS: "
                   WS-TOTAL-FUNCIONARIOS.

       
