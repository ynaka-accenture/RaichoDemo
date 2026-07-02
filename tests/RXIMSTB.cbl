       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBLTDLI.
      * DL/I スタブ: GU/GN/GNP/ISRT/REPL を模擬 (計器設備DB)
      * unload 形式 MTRDB.dat を在庫表に展開し階層順で応答する
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GTFF ASSIGN TO 'spool/gtf/DLI.trc'
               ORGANIZATION LINE SEQUENTIAL FILE STATUS IS ST-GT.
           SELECT DBIN ASSIGN TO 'app/data/portable/MTRDB.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-DB.
           SELECT UPDJ ASSIGN TO 'app/data/portable/MTRUPD.dat'
               ORGANIZATION LINE SEQUENTIAL FILE STATUS IS ST-UP.
       DATA DIVISION.
       FILE SECTION.
       FD  GTFF RECORD CONTAINS 132 CHARACTERS.
       01  GT-REC              PIC X(132).
       FD  DBIN RECORD CONTAINS 80 CHARACTERS.
       01  DB-REC              PIC X(80).
       FD  UPDJ RECORD CONTAINS 80 CHARACTERS.
       01  UP-REC              PIC X(80).
       WORKING-STORAGE SECTION.
       77  ST-DB               PIC XX VALUE '00'.
       77  ST-GT               PIC XX VALUE '00'.
       77  GTF-MODE            PIC X  VALUE ' '.
       77  GTF-OPEN            PIC X  VALUE 'N'.
       01  GTF-SEQ             PIC 9(9) EXTERNAL.
       77  GTF-I               PIC 9(3) VALUE ZERO.
       77  GTF-RESP-ED         PIC 9(4) VALUE ZERO.
       77  ST-UP               PIC XX VALUE '00'.
       77  DB-LOADED           PIC X  VALUE 'N'.
       77  UPD-OPEN            PIC X  VALUE 'N'.
       77  WS-I                PIC 9(7) VALUE ZERO.
       77  WS-J                PIC 9(7) VALUE ZERO.
       77  CUR-POS             PIC 9(7) VALUE ZERO.
       77  CUR-ROOT            PIC 9(7) VALUE ZERO.
       77  W-KEY               PIC X(10) VALUE SPACE.
       01  SEG-AREA.
           05  SEG-CNT         PIC S9(7) COMP-3 VALUE ZERO.
           05  SEG-E OCCURS 20000.
               10  SEG-TYPE    PIC X(1).
               10  SEG-DATA    PIC X(48).
               10  SEG-ROOT-IX PIC 9(7).
       LINKAGE SECTION.
       01  DLI-FUNC            PIC X(4).
       01  DLI-PCB.
       COPY RCIMSPCB.
       01  DLI-IOAREA          PIC X(48).
       01  DLI-SSA             PIC X(40).
       PROCEDURE DIVISION USING DLI-FUNC DLI-PCB DLI-IOAREA DLI-SSA.
       MAIN-RTN.
           IF DB-LOADED = 'N'
               ACCEPT GTF-MODE FROM ENVIRONMENT 'RACS_GTF'
               PERFORM DB-LOAD-R
               MOVE 'Y' TO DB-LOADED.
           MOVE SPACES TO PCB-STATUS.
           EVALUATE DLI-FUNC
               WHEN 'GU  ' PERFORM GU-R   THRU GU-EX
               WHEN 'GN  ' PERFORM GN-R   THRU GN-EX
               WHEN 'GNP ' PERFORM GNP-R  THRU GNP-EX
               WHEN 'ISRT' PERFORM ISRT-R THRU ISRT-EX
               WHEN 'REPL' PERFORM REPL-R THRU REPL-EX
               WHEN OTHER  MOVE 'AD' TO PCB-STATUS
           END-EVALUATE.
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
           STRING 'GTF ' GTF-SEQ ' DLI  ' DLI-FUNC
               ' SEG=' PCB-SEG-NAME ' STAT=<' PCB-STATUS '>'
               ' KEY=' PCB-KEY-FB (1:10)
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
      *----------------------------------------------------------------
       DB-LOAD-R.
           OPEN INPUT DBIN.
           MOVE ZERO TO CUR-ROOT.
           PERFORM UNTIL ST-DB NOT = '00'
                        OR SEG-CNT NOT LESS 20000
               READ DBIN
                   AT END CONTINUE
                   NOT AT END
                       ADD 1 TO SEG-CNT
                       MOVE DB-REC (1:1)  TO SEG-TYPE (SEG-CNT)
                       MOVE DB-REC (2:48) TO SEG-DATA (SEG-CNT)
                       IF DB-REC (1:1) = 'R'
                           MOVE SEG-CNT TO CUR-ROOT
                       END-IF
                       MOVE CUR-ROOT TO SEG-ROOT-IX (SEG-CNT)
               END-READ
           END-PERFORM.
           CLOSE DBIN.
           MOVE ZERO TO CUR-POS CUR-ROOT.
      *----------------------------------------------------------------
      * GU: 修飾 SSA 'KEIKISEG (KEIKINO =xxxxxxxxxx)' か無修飾先頭
      *----------------------------------------------------------------
       GU-R.
           IF DLI-SSA (1:8) = 'KEIKISEG' AND DLI-SSA (10:1) = '('
               MOVE DLI-SSA (20:10) TO W-KEY
               PERFORM VARYING WS-I FROM 1 BY 1
                       UNTIL WS-I GREATER SEG-CNT
                   IF SEG-TYPE (WS-I) = 'R'
                       IF SEG-DATA (WS-I) (1:10) = W-KEY
                           PERFORM HIT-ROOT-R
                           GO TO GU-EX
                       END-IF
                   END-IF
               END-PERFORM
               MOVE 'GE' TO PCB-STATUS
               GO TO GU-EX.
           MOVE ZERO TO CUR-POS.
           PERFORM GN-R.
       GU-EX.
           EXIT.
       HIT-ROOT-R.
           MOVE WS-I TO CUR-POS CUR-ROOT.
           MOVE SEG-DATA (WS-I) (1:32) TO DLI-IOAREA (1:32).
           MOVE 'KEIKISEG' TO PCB-SEG-NAME.
           MOVE '01' TO PCB-SEG-LEVEL.
           MOVE SEG-DATA (WS-I) (1:10) TO PCB-KEY-FB.
      *----------------------------------------------------------------
       GN-R.
           ADD 1 TO CUR-POS.
           IF CUR-POS GREATER SEG-CNT
               MOVE 'GB' TO PCB-STATUS
               GO TO GN-EX.
           IF SEG-TYPE (CUR-POS) = 'R'
               MOVE CUR-POS TO CUR-ROOT
               MOVE SEG-DATA (CUR-POS) (1:32) TO DLI-IOAREA (1:32)
               MOVE 'KEIKISEG' TO PCB-SEG-NAME
               MOVE '01' TO PCB-SEG-LEVEL
           ELSE
               MOVE SEG-DATA (CUR-POS) (1:48) TO DLI-IOAREA (1:48)
               MOVE 'TRTUSEG ' TO PCB-SEG-NAME
               MOVE '02' TO PCB-SEG-LEVEL
           END-IF.
           MOVE SEG-DATA (CUR-ROOT) (1:10) TO PCB-KEY-FB.
       GN-EX.
           EXIT.
      *----------------------------------------------------------------
      * GNP: 現ルート配下の次ツイン. 尽きたら GE
      *----------------------------------------------------------------
       GNP-R.
           IF CUR-ROOT = ZERO
               MOVE 'GP' TO PCB-STATUS
               GO TO GNP-EX.
           ADD 1 TO CUR-POS.
           IF CUR-POS GREATER SEG-CNT
               MOVE 'GE' TO PCB-STATUS
               GO TO GNP-EX.
           IF SEG-TYPE (CUR-POS) = 'R'
               SUBTRACT 1 FROM CUR-POS
               MOVE 'GE' TO PCB-STATUS
               GO TO GNP-EX.
           MOVE SEG-DATA (CUR-POS) (1:48) TO DLI-IOAREA (1:48).
           MOVE 'TRTUSEG ' TO PCB-SEG-NAME.
           MOVE '02' TO PCB-SEG-LEVEL.
       GNP-EX.
           EXIT.
      *----------------------------------------------------------------
      * ISRT: 現ルート配下へツイン追加 (在庫表末尾でなく論理追加は
      *       簡易化: ジャーナルに記録し在庫表は親直後群の末尾挿入)
      *----------------------------------------------------------------
       ISRT-R.
           PERFORM UPD-OPEN-R.
           MOVE SPACES TO UP-REC.
           STRING 'ISRT|' DLI-IOAREA (1:48)
               DELIMITED BY SIZE INTO UP-REC.
           WRITE UP-REC.
           MOVE SPACES TO PCB-STATUS.
       ISRT-EX.
           EXIT.
       REPL-R.
           IF CUR-POS = ZERO
               MOVE 'DJ' TO PCB-STATUS
               GO TO REPL-EX.
           PERFORM UPD-OPEN-R.
           MOVE SPACES TO UP-REC.
           STRING 'REPL|' DLI-IOAREA (1:48)
               DELIMITED BY SIZE INTO UP-REC.
           WRITE UP-REC.
           MOVE DLI-IOAREA (1:48) TO SEG-DATA (CUR-POS).
           MOVE SPACES TO PCB-STATUS.
       REPL-EX.
           EXIT.
       UPD-OPEN-R.
           IF UPD-OPEN = 'N'
               OPEN OUTPUT UPDJ
               MOVE 'Y' TO UPD-OPEN.
