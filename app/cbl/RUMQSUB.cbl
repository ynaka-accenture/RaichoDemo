      *****************************************************************
      * RUMQSUB  簡易メッセージ授受 共通サブ   東西電力 電算部        *
      *---------------------------------------------------------------*
      * H10.03 初版 (MQSeries 導入時). MQI の煩雑な引数列を隠蔽し     *
      * 社内標準 IF (機能/バッファ/長さ/完了/理由) に統一する.        *
      * 各業務はこのサブ経由でのみ MQ を使うこと (電算部標準 5.1)     *
      * ※模擬環境では待ち行列をファイルで代替する                    *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RUMQSUB.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GTFF ASSIGN TO 'spool/gtf/MQ.trc'
               ORGANIZATION LINE SEQUENTIAL FILE STATUS IS ST-GT.
           SELECT QIN  ASSIGN TO 'app/data/portable/MQIN.dat'
               ORGANIZATION LINE SEQUENTIAL FILE STATUS IS ST-QI.
           SELECT QOUT ASSIGN TO 'app/data/portable/MQOUT.dat'
               ORGANIZATION LINE SEQUENTIAL FILE STATUS IS ST-QO.
       DATA DIVISION.
       FILE SECTION.
       FD  GTFF RECORD CONTAINS 132 CHARACTERS.
       01  GT-REC              PIC X(132).
       FD  QIN RECORD CONTAINS 100 CHARACTERS.
       01  QI-REC              PIC X(100).
       FD  QOUT RECORD CONTAINS 100 CHARACTERS.
       01  QO-REC              PIC X(100).
       WORKING-STORAGE SECTION.
       77  ST-QI               PIC XX VALUE '00'.
       77  ST-GT               PIC XX VALUE '00'.
       77  GTF-MODE            PIC X  VALUE 'U'.
       77  GTF-OPEN            PIC X  VALUE 'N'.
       01  GTF-SEQ             PIC 9(9) EXTERNAL.
       77  GTF-I               PIC 9(3) VALUE ZERO.
       77  GTF-RESP-ED         PIC 9(4) VALUE ZERO.
       77  ST-QO               PIC XX VALUE '00'.
       77  IN-OPEN             PIC X  VALUE 'N'.
       77  OUT-OPEN            PIC X  VALUE 'N'.
       77  W-TUBAN             PIC 9(5) VALUE ZERO.
       77  WS-I                PIC 9(3) VALUE ZERO.
       LINKAGE SECTION.
       01  MQ-FUNC             PIC X(4).
       01  MQ-BUFFER           PIC X(100).
       01  MQ-LEN              PIC S9(4) COMP.
       01  MQ-CC               PIC S9(4) COMP.
       01  MQ-REASON           PIC S9(4) COMP.
       PROCEDURE DIVISION USING MQ-FUNC MQ-BUFFER MQ-LEN
                                MQ-CC MQ-REASON.
       MAIN-RTN.
           IF GTF-MODE = 'U'
               ACCEPT GTF-MODE FROM ENVIRONMENT 'RACS_GTF'.
           IF GTF-MODE = '1'
               PERFORM GTF-KAKI-R THRU GTF-KAKI-EX.
           MOVE ZERO           TO MQ-CC MQ-REASON.
           IF MQ-FUNC = 'GET '
               GO TO GET-RTN.
           IF MQ-FUNC = 'PUT '
               GO TO PUT-RTN.
           IF MQ-FUNC = 'GETB'
      *        参照読み (BROWSE): 本模擬では GET と同義
               GO TO GET-RTN.
           IF MQ-FUNC = 'CMIT'
               GO TO OWARI-RTN.
           IF MQ-FUNC = 'CLOS'
               IF IN-OPEN = 'Y'
                   CLOSE QIN
                   MOVE 'N' TO IN-OPEN
               END-IF
               IF OUT-OPEN = 'Y'
                   CLOSE QOUT
                   MOVE 'N' TO OUT-OPEN
               END-IF
               GO TO OWARI-RTN.
           IF MQ-FUNC (1:1) = LOW-VALUE
               MOVE 2 TO MQ-CC
               MOVE 2067 TO MQ-REASON
               GO TO OWARI-RTN.
           IF MQ-FUNC = 'BACK'
               GO TO OWARI-RTN.
           IF MQ-FUNC = SPACES
               MOVE 2 TO MQ-CC
               MOVE 2067 TO MQ-REASON
               GO TO OWARI-RTN.
           MOVE 2              TO MQ-CC.
           MOVE 2085           TO MQ-REASON.
       OWARI-RTN.
           GOBACK.
      *----------------------------------------------------------------
      * ※模擬環境用フック: 以下の GTF 書出しは RaichoDemo の仮想
      *   メインフレーム (デバッグ相関) 専用. RACS_GTF 未設定時は
      *   一切動作しない. 実際の現場でも共通サブにはこの種の
      *   トレースフックが埋まっているものだが, 移行評価の対象
      *   ロジックには含めないこと
      *----------------------------------------------------------------
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
           STRING 'GTF ' GTF-SEQ ' MQ   ' MQ-FUNC
               ' BUF=' MQ-BUFFER (1:28)
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
       GET-RTN.
           IF IN-OPEN = 'N'
               OPEN INPUT QIN
               IF ST-QI NOT = '00'
                   MOVE 2 TO MQ-CC
                   MOVE 2085 TO MQ-REASON
                   GO TO OWARI-RTN
               END-IF
               MOVE 'Y' TO IN-OPEN.
           READ QIN
               AT END
                   MOVE 2 TO MQ-CC
                   MOVE 2033 TO MQ-REASON
                   GOBACK.
           MOVE QI-REC         TO MQ-BUFFER.
           MOVE 100            TO MQ-LEN.
           IF MQ-BUFFER (1:2) = SPACES
               MOVE 1 TO MQ-CC
               MOVE 2110 TO MQ-REASON
               GO TO OWARI-RTN.
           MOVE 1              TO WS-I.
       GET-020.
           IF WS-I GREATER 2
               GO TO GET-030.
           IF MQ-BUFFER (WS-I:1) LESS SPACE
               MOVE 1 TO MQ-CC
               MOVE 2110 TO MQ-REASON
               GO TO OWARI-RTN.
           ADD 1               TO WS-I.
           GO TO GET-020.
       GET-030.
           IF MQ-LEN LESS 1
               MOVE 2 TO MQ-CC
               MOVE 2010 TO MQ-REASON
               GO TO OWARI-RTN.
           IF MQ-LEN GREATER 100
               MOVE 2 TO MQ-CC
               MOVE 2010 TO MQ-REASON
               GO TO OWARI-RTN.
           ADD 1               TO W-TUBAN.
           GOBACK.
       PUT-RTN.
           IF OUT-OPEN = 'N'
               OPEN OUTPUT QOUT
               IF ST-QO NOT = '00'
                   MOVE 2 TO MQ-CC
                   MOVE 2085 TO MQ-REASON
                   GO TO OWARI-RTN
               END-IF
               MOVE 'Y' TO OUT-OPEN.
           IF MQ-BUFFER = SPACES
               MOVE 1 TO MQ-CC
               MOVE 2005 TO MQ-REASON
               GO TO OWARI-RTN.
           IF MQ-BUFFER (1:1) = LOW-VALUE
               MOVE 1 TO MQ-CC
               MOVE 2005 TO MQ-REASON
               GO TO OWARI-RTN.
           IF MQ-BUFFER (1:2) NOT = 'R1' AND
              MQ-BUFFER (1:2) NOT = 'D1' AND
              MQ-BUFFER (1:2) NOT = 'K1'
               MOVE 1 TO MQ-CC
               MOVE 2085 TO MQ-REASON
               GO TO OWARI-RTN.
           MOVE 1              TO WS-I.
       PUT-020.
           IF WS-I GREATER 28
               GO TO PUT-030.
           IF MQ-BUFFER (WS-I:1) LESS SPACE
               MOVE 1 TO MQ-CC
               MOVE 2005 TO MQ-REASON
               GO TO OWARI-RTN.
           ADD 1               TO WS-I.
           GO TO PUT-020.
       PUT-030.
           ADD 1               TO W-TUBAN.
           MOVE MQ-BUFFER      TO QO-REC.
           WRITE QO-REC.
           IF ST-QO NOT = '00'
               MOVE 2 TO MQ-CC
               MOVE 2195 TO MQ-REASON
               GO TO OWARI-RTN.
           GOBACK.
