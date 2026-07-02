       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RXCICSTB.
      * ミニCICSスタブ: 擬似会話・MAP送受信・READ・XCTL を模擬
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GTFF ASSIGN TO 'spool/gtf/CICS.trc'
               ORGANIZATION LINE SEQUENTIAL FILE STATUS IS ST-GT.
           SELECT SCRIN  ASSIGN TO 'app/data/portable/CICSIN.txt'
               ORGANIZATION LINE SEQUENTIAL FILE STATUS IS ST-SC.
           SELECT SCROUT ASSIGN TO 'app/data/portable/CICSOUT.txt'
               ORGANIZATION LINE SEQUENTIAL FILE STATUS IS ST-SO.
           SELECT TSCR   ASSIGN TO 'spool/term/SCREEN.dat'
               ORGANIZATION LINE SEQUENTIAL FILE STATUS IS ST-TS.
           SELECT TRDY   ASSIGN TO 'spool/term/READY.flg'
               ORGANIZATION LINE SEQUENTIAL FILE STATUS IS ST-TS.
           SELECT TIN    ASSIGN TO 'spool/term/INPUT.dat'
               ORGANIZATION LINE SEQUENTIAL FILE STATUS IS ST-TI.
           SELECT KYKF   ASSIGN TO 'app/data/portable/KYKMAST.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-KY.
           SELECT UPDJ   ASSIGN TO 'app/data/portable/CICSUPD.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-UP.
           SELECT KENFF  ASSIGN TO 'app/data/portable/KENFILE.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-KE.
           SELECT RYOFF  ASSIGN TO 'app/data/portable/RYOFILE.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-RY.
       DATA DIVISION.
       FILE SECTION.
       FD  GTFF RECORD CONTAINS 132 CHARACTERS.
       01  GT-REC              PIC X(132).
       FD  SCRIN RECORD CONTAINS 80 CHARACTERS.
       01  SC-REC              PIC X(80).
       FD  SCROUT RECORD CONTAINS 120 CHARACTERS.
       01  SO-REC              PIC X(120).
       FD  TSCR RECORD CONTAINS 100 CHARACTERS.
       01  TS-REC              PIC X(100).
       FD  TRDY RECORD CONTAINS 8 CHARACTERS.
       01  TR-REC              PIC X(8).
       FD  TIN RECORD CONTAINS 80 CHARACTERS.
       01  TI-REC              PIC X(80).
       FD  KYKF RECORD CONTAINS 320 CHARACTERS.
       01  KY-REC              PIC X(320).
       FD  UPDJ RECORD CONTAINS 320 CHARACTERS.
       01  UP-REC              PIC X(320).
       FD  KENFF RECORD CONTAINS 128 CHARACTERS.
       01  KE-REC              PIC X(128).
       FD  RYOFF RECORD CONTAINS 256 CHARACTERS.
       01  RY-REC              PIC X(256).
       WORKING-STORAGE SECTION.
       77  ST-SC               PIC XX VALUE '00'.
       77  ST-GT               PIC XX VALUE '00'.
       77  GTF-MODE            PIC X  VALUE ' '.
       77  GTF-OPEN            PIC X  VALUE 'N'.
       01  GTF-SEQ             PIC 9(9) EXTERNAL.
       77  GTF-I               PIC 9(3) VALUE ZERO.
       77  GTF-RESP-ED         PIC 9(4) VALUE ZERO.
       77  ST-SO               PIC XX VALUE '00'.
       77  ST-KY               PIC XX VALUE '00'.
       77  ST-UP               PIC XX VALUE '00'.
       77  ST-TS               PIC XX VALUE '00'.
       77  ST-TI               PIC XX VALUE '00'.
       77  OPEN-FLG            PIC X  VALUE 'N'.
       77  KYK-LOADED          PIC X  VALUE 'N'.
       77  UPD-OPEN            PIC X  VALUE 'N'.
       77  TERM-MODE           PIC X  VALUE ' '.
       77  TERM-SEQ            PIC 9(5) VALUE ZERO.
       77  WT-I                PIC 9(5) VALUE ZERO.
       77  NS-WAIT             PIC 9(9) COMP-5 VALUE 150000000.
       77  ST-KE               PIC XX VALUE '00'.
       77  ST-RY               PIC XX VALUE '00'.
       77  KEN-LOADED          PIC X  VALUE 'N'.
       77  RYO-LOADED          PIC X  VALUE 'N'.
       77  W-MAX-YM            PIC 9(6) VALUE ZERO.
       77  W-HIT-IX            PIC 9(6) VALUE ZERO.
       77  WS-I                PIC 9(5) VALUE ZERO.
       77  NEXT-TRAN           PIC X(4) VALUE SPACE.
       77  NEXT-PGM            PIC X(8) VALUE SPACE.
       77  END-FLG             PIC X  VALUE 'N'.
       77  SAVE-LEN            PIC S9(4) COMP VALUE ZERO.
       77  WS-C                PIC 9(3) VALUE ZERO.
       01  SAVE-COMM           PIC X(200) VALUE SPACE.
       01  KYT-AREA.
           05  KYT-CNT         PIC S9(5) COMP-3 VALUE ZERO.
           05  KYT-REC         PIC X(320) OCCURS 6000.
       01  KET-AREA.
           05  KET-CNT         PIC S9(7) COMP-3 VALUE ZERO.
           05  KET-REC         PIC X(128) OCCURS 72000.
       01  RYT-AREA.
           05  RYT-CNT         PIC S9(5) COMP-3 VALUE ZERO.
           05  RYT-REC         PIC X(256) OCCURS 6000.
       LINKAGE SECTION.
       01  DFHEIBLK.
       COPY RCEIBLK.
       01  CICS-PARM.
       COPY RCCICSPM.
       01  DATA-AREA           PIC X(512).
       PROCEDURE DIVISION USING DFHEIBLK CICS-PARM DATA-AREA.
       MAIN-RTN.
           IF OPEN-FLG = 'N'
               ACCEPT GTF-MODE  FROM ENVIRONMENT 'RACS_GTF'
               ACCEPT TERM-MODE FROM ENVIRONMENT 'RACS_TERM'
               IF TERM-MODE NOT = '1'
                   OPEN INPUT SCRIN
               END-IF
               OPEN OUTPUT SCROUT
               MOVE 'Y' TO OPEN-FLG.
           MOVE 0 TO CP-RESP.
           EVALUATE CP-CMD
               WHEN 'SEND-MAP   ' PERFORM SEND-MAP-R
               WHEN 'RECEIVE-MAP' PERFORM RECV-MAP-R THRU RECV-EX
               WHEN 'RETURN-TRN ' PERFORM RET-TRN-R
               WHEN 'RETURN-END ' PERFORM RET-END-R
               WHEN 'XCTL       ' PERFORM XCTL-R
               WHEN 'READ-DS    ' PERFORM READ-DS-R
               WHEN 'REWRITE-DS ' PERFORM REWRITE-DS-R
               WHEN 'ASKTIME    ' PERFORM ASKTIME-R
               WHEN 'ABEND      ' PERFORM ABEND-R
               WHEN 'GET-NEXT   ' PERFORM GET-NEXT-R
               WHEN 'PUT-COMM   ' PERFORM PUT-COMM-R
               WHEN OTHER MOVE 98 TO CP-RESP
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
           MOVE CP-RESP TO GTF-RESP-ED.
           MOVE SPACES TO GT-REC.
           STRING 'GTF ' GTF-SEQ ' CICS ' CP-CMD
               ' MAP=' CP-MAPSET '/' CP-MAP
               ' DS=' CP-DATASET ' TRN=' EIBTRNID
               ' RESP=' GTF-RESP-ED
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
       SEND-MAP-R.
           IF TERM-MODE = '1'
               PERFORM TERM-SEND-R THRU TERM-SEND-EX.
           MOVE SPACES TO SO-REC.
           STRING 'SND ' CP-MAPSET '/' CP-MAP ' | '
               DATA-AREA (1:80) DELIMITED BY SIZE INTO SO-REC.
           PERFORM SANITIZE-R.
           WRITE SO-REC.
       RECV-MAP-R.
           IF TERM-MODE = '1'
               PERFORM TERM-RECV-R THRU TERM-RECV-EX
               IF CP-RESP = 99
                   GO TO RECV-EX
               END-IF
               MOVE TI-REC TO DATA-AREA (1:80)
                              SC-REC
               GO TO RECV-LOG.
           READ SCRIN
               AT END MOVE 'Y' TO END-FLG
                      MOVE 99 TO CP-RESP
                      GO TO RECV-EX.
           MOVE SC-REC TO DATA-AREA (1:80).
       RECV-LOG.
           MOVE SPACES TO SO-REC.
           STRING 'RCV ' CP-MAPSET '/' CP-MAP ' | ' SC-REC (1:60)
               DELIMITED BY SIZE INTO SO-REC.
           PERFORM SANITIZE-R.
           WRITE SO-REC.
       RECV-EX.
           EXIT.
      *    端末モード: 画面スナップショット書出し (都度 CLOSE=確実)
       TERM-SEND-R.
           ADD 1 TO TERM-SEQ.
           OPEN OUTPUT TSCR.
           MOVE SPACES TO TS-REC.
           STRING 'MAP=' CP-MAPSET '/' CP-MAP ' SEQ=' TERM-SEQ
               DELIMITED BY SIZE INTO TS-REC.
           WRITE TS-REC.
           MOVE DATA-AREA (1:80) TO TS-REC.
           INSPECT TS-REC REPLACING ALL LOW-VALUE BY SPACE.
           WRITE TS-REC.
           CLOSE TSCR.
           OPEN OUTPUT TRDY.
           MOVE 'READY' TO TR-REC.
           WRITE TR-REC.
           CLOSE TRDY.
       TERM-SEND-EX.
           EXIT.
      *    端末モード: 入力到着をポーリング待機
       TERM-RECV-R.
           MOVE ZERO TO WT-I.
       TRC-010.
           OPEN INPUT TIN.
           IF ST-TI = '00'
               READ TIN
                   AT END
                       CLOSE TIN
                       GO TO TRC-020
               END-READ
               CLOSE TIN
               CALL 'CBL_DELETE_FILE'
                   USING 'spool/term/INPUT.dat'
               IF TI-REC (1:4) = '/END'
                   MOVE 'Y' TO END-FLG
                   MOVE 99 TO CP-RESP
               END-IF
               GO TO TERM-RECV-EX.
           CLOSE TIN.
       TRC-020.
           ADD 1 TO WT-I.
           IF WT-I GREATER 2400
               MOVE 'Y' TO END-FLG
               MOVE 99 TO CP-RESP
               GO TO TERM-RECV-EX.
           CALL 'CBL_GC_NANOSLEEP' USING NS-WAIT.
           GO TO TRC-010.
       TERM-RECV-EX.
           EXIT.
      *    非印字バイトは '.' に置換 (端末ダンプ風・行落ち防止)
       SANITIZE-R.
           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C GREATER 120
               IF SO-REC (WS-C:1) LESS SPACE
                   MOVE '.' TO SO-REC (WS-C:1)
               END-IF
           END-PERFORM.
       RET-TRN-R.
           MOVE CP-TRANSID TO NEXT-TRAN.
           MOVE SPACES     TO NEXT-PGM.
           MOVE DATA-AREA (1:200) TO SAVE-COMM.
           MOVE CP-DATA-LEN TO SAVE-LEN.
       RET-END-R.
           IF CP-CMD = 'RETURN-END '
               MOVE SPACES TO NEXT-TRAN NEXT-PGM
               MOVE 'Y' TO END-FLG.
       XCTL-R.
           MOVE CP-PROGRAM TO NEXT-PGM.
           MOVE SPACES     TO NEXT-TRAN.
           MOVE DATA-AREA (1:200) TO SAVE-COMM.
           MOVE CP-DATA-LEN TO SAVE-LEN.
           MOVE SPACES TO SO-REC.
           STRING 'XCT -> ' CP-PROGRAM
               DELIMITED BY SIZE INTO SO-REC.
           WRITE SO-REC.
       READ-DS-R.
           EVALUATE CP-DATASET
               WHEN 'KYKMAST ' PERFORM READ-KYK-R
               WHEN 'KENFILE ' PERFORM READ-KEN-R
               WHEN 'RYOFILE ' PERFORM READ-RYO-R
               WHEN OTHER MOVE 97 TO CP-RESP
           END-EVALUATE.
       READ-KYK-R.
           IF KYK-LOADED = 'N'
               PERFORM KYK-LOAD-R
               MOVE 'Y' TO KYK-LOADED.
           MOVE 13 TO CP-RESP.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I GREATER KYT-CNT
               IF KYT-REC (WS-I) (1:22) = CP-RIDFLD
                   MOVE KYT-REC (WS-I) TO DATA-AREA (1:320)
                   MOVE 0 TO CP-RESP
                   MOVE KYT-CNT TO WS-I
               END-IF
           END-PERFORM.
      *    検針/料金: 地点キーで最新月レコードを返す (簡易VSAMパス)
       READ-KEN-R.
           IF KEN-LOADED = 'N'
               OPEN INPUT KENFF
               PERFORM UNTIL ST-KE NOT = '00'
                            OR KET-CNT NOT LESS 72000
                   READ KENFF
                       AT END CONTINUE
                       NOT AT END
                           ADD 1 TO KET-CNT
                           MOVE KE-REC TO KET-REC (KET-CNT)
                   END-READ
               END-PERFORM
               CLOSE KENFF
               MOVE 'Y' TO KEN-LOADED.
           MOVE 13 TO CP-RESP.
           MOVE ZERO TO W-MAX-YM W-HIT-IX.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I GREATER KET-CNT
               IF KET-REC (WS-I) (1:22) = CP-RIDFLD
                   IF FUNCTION NUMVAL (KET-REC (WS-I) (23:6))
                       GREATER W-MAX-YM
                       COMPUTE W-MAX-YM =
                           FUNCTION NUMVAL (KET-REC (WS-I) (23:6))
                       MOVE WS-I TO W-HIT-IX
                   END-IF
               END-IF
           END-PERFORM.
           IF W-HIT-IX GREATER ZERO
               MOVE KET-REC (W-HIT-IX) TO DATA-AREA (1:128)
               MOVE 0 TO CP-RESP.
       READ-RYO-R.
           IF RYO-LOADED = 'N'
               OPEN INPUT RYOFF
               PERFORM UNTIL ST-RY NOT = '00'
                            OR RYT-CNT NOT LESS 6000
                   READ RYOFF
                       AT END CONTINUE
                       NOT AT END
                           ADD 1 TO RYT-CNT
                           MOVE RY-REC TO RYT-REC (RYT-CNT)
                   END-READ
               END-PERFORM
               CLOSE RYOFF
               MOVE 'Y' TO RYO-LOADED.
           MOVE 13 TO CP-RESP.
           MOVE ZERO TO W-MAX-YM W-HIT-IX.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I GREATER RYT-CNT
               IF RYT-REC (WS-I) (1:22) = CP-RIDFLD
                   IF FUNCTION NUMVAL (RYT-REC (WS-I) (23:6))
                       GREATER W-MAX-YM
                       COMPUTE W-MAX-YM =
                           FUNCTION NUMVAL (RYT-REC (WS-I) (23:6))
                       MOVE WS-I TO W-HIT-IX
                   END-IF
               END-IF
           END-PERFORM.
           IF W-HIT-IX GREATER ZERO
               MOVE RYT-REC (W-HIT-IX) TO DATA-AREA (1:256)
               MOVE 0 TO CP-RESP.
      *    更新: 在庫表を書換え ジャーナルへ追記 (実データは不変)
       REWRITE-DS-R.
           IF KYK-LOADED = 'N'
               PERFORM KYK-LOAD-R
               MOVE 'Y' TO KYK-LOADED.
           IF UPD-OPEN = 'N'
               OPEN OUTPUT UPDJ
               MOVE 'Y' TO UPD-OPEN.
           MOVE 13 TO CP-RESP.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I GREATER KYT-CNT
               IF KYT-REC (WS-I) (1:22) = DATA-AREA (1:22)
                   MOVE DATA-AREA (1:320) TO KYT-REC (WS-I)
                   MOVE DATA-AREA (1:320) TO UP-REC
                   WRITE UP-REC
                   MOVE 0 TO CP-RESP
                   MOVE SPACES TO SO-REC
                   STRING 'UPD ' DATA-AREA (1:22)
                       DELIMITED BY SIZE INTO SO-REC
                   WRITE SO-REC
                   MOVE KYT-CNT TO WS-I
               END-IF
           END-PERFORM.
       KYK-LOAD-R.
           OPEN INPUT KYKF.
           PERFORM UNTIL ST-KY NOT = '00' OR KYT-CNT NOT LESS 6000
               READ KYKF
                   AT END CONTINUE
                   NOT AT END
                       ADD 1 TO KYT-CNT
                       MOVE KY-REC TO KYT-REC (KYT-CNT)
               END-READ
           END-PERFORM.
           CLOSE KYKF.
       ASKTIME-R.
           MOVE 1230000 TO EIBTIME.
           MOVE 0260701 TO EIBDATE.
       ABEND-R.
           MOVE SPACES TO SO-REC.
           STRING 'ABN ' CP-ABCODE DELIMITED BY SIZE INTO SO-REC.
           WRITE SO-REC.
           MOVE 'Y' TO END-FLG.
      *    ドライバ用: 次遷移の取出し / COMMAREA 授受
       GET-NEXT-R.
           MOVE SPACES TO DATA-AREA (1:212).
           MOVE NEXT-TRAN TO DATA-AREA (1:4).
           MOVE NEXT-PGM  TO DATA-AREA (5:8).
           MOVE END-FLG   TO DATA-AREA (13:1).
           MOVE SAVE-COMM TO DATA-AREA (14:200).
           MOVE SAVE-LEN  TO CP-DATA-LEN.
           MOVE SPACES TO NEXT-TRAN NEXT-PGM.
       PUT-COMM-R.
           CONTINUE.
