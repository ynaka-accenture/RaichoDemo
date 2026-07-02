       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RXDB2TB.
      * Db2 スタブ: INSERT を履歴ファイルへ追記し SQLCODE=0
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GTFF ASSIGN TO 'spool/gtf/DB2.trc'
               ORGANIZATION LINE SEQUENTIAL FILE STATUS IS ST-GT.
           SELECT LOGF ASSIGN TO 'app/data/portable/DB2LOG.dat'
               ORGANIZATION LINE SEQUENTIAL FILE STATUS IS ST-LG.
       DATA DIVISION.
       FILE SECTION.
       FD  GTFF RECORD CONTAINS 132 CHARACTERS.
       01  GT-REC              PIC X(132).
       FD  LOGF RECORD CONTAINS 80 CHARACTERS.
       01  LG-REC              PIC X(80).
       WORKING-STORAGE SECTION.
       77  ST-LG               PIC XX VALUE '00'.
       77  ST-GT               PIC XX VALUE '00'.
       77  GTF-MODE            PIC X  VALUE ' '.
       77  GTF-OPEN            PIC X  VALUE 'N'.
       01  GTF-SEQ             PIC 9(9) EXTERNAL.
       77  GTF-I               PIC 9(3) VALUE ZERO.
       77  GTF-RESP-ED         PIC 9(4) VALUE ZERO.
       77  OPEN-FLG            PIC X  VALUE 'N'.
       LINKAGE SECTION.
       01  SQLCA.
           05  SQLCAID         PIC X(8).
           05  SQLCABC         PIC S9(9) COMP.
           05  SQLCODE         PIC S9(9) COMP.
           05  FILLER          PIC X(120).
       01  SQ-PARM.
           05  SQ-STMT         PIC X(18).
           05  SQ-HV1          PIC X(22).
           05  SQ-HV2          PIC X(8).
           05  SQ-HV3          PIC X(8).
           05  SQ-HV4          PIC X(8).
           05  SQ-HV5          PIC X(8).
           05  SQ-HV6          PIC X(8).
       PROCEDURE DIVISION USING SQLCA SQ-PARM.
       MAIN-RTN.
           IF OPEN-FLG = 'N'
               ACCEPT GTF-MODE FROM ENVIRONMENT 'RACS_GTF'
               OPEN OUTPUT LOGF
               MOVE 'Y' TO OPEN-FLG.
           MOVE SPACES TO LG-REC.
           STRING SQ-STMT '|' SQ-HV1 '|' SQ-HV2 '|' SQ-HV3
               '|' SQ-HV4 '|' SQ-HV5
               DELIMITED BY SIZE INTO LG-REC.
           WRITE LG-REC.
           MOVE ZERO TO SQLCODE.
           IF GTF-MODE = '1'
               PERFORM GTF-KAKI-R THRU GTF-KAKI-EX.
           GOBACK.
       GTF-KAKI-R.
           IF GTF-OPEN = 'N'
               OPEN EXTEND GTFF
               IF ST-GT = '35'
                   OPEN OUTPUT GTFF
               END-IF
               IF GTF-SEQ NOT NUMERIC
                   MOVE ZERO TO GTF-SEQ
               END-IF
               MOVE 'Y' TO GTF-OPEN.
           ADD 1 TO GTF-SEQ.
           MOVE SPACES TO GT-REC.
           STRING 'GTF ' GTF-SEQ ' DB2  EXEC-SQL ' SQ-STMT
               ' HV1=' SQ-HV1 ' SQLCODE=+000000000'
               DELIMITED BY SIZE INTO GT-REC.
           PERFORM VARYING GTF-I FROM 1 BY 1
                   UNTIL GTF-I GREATER 132
               IF GT-REC (GTF-I:1) LESS SPACE
                   MOVE '.' TO GT-REC (GTF-I:1)
               END-IF
           END-PERFORM.
           WRITE GT-REC.
       GTF-KAKI-EX.
           EXIT.
