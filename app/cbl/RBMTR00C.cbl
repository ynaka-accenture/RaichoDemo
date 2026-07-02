      *****************************************************************
      * RBMTR00C  計器マスタ抽出 (IMS -> 順次)  東西電力 電算部      *
      *---------------------------------------------------------------*
      * S58.09 初版  H04.02 現行様式                                  *
      * 計器設備DB (IMS/MTRDBD) が計器資産の正本である. 本ジョブは    *
      * 日次サイクル先頭で DB を全走査し 現行取付ツイン (取外日=0)    *
      * と合成して 計器マスタ順次 (MTRMAST) を切り出す.               *
      * 後続 (RBMTR01C 検定監視ほか) はこの抽出物を読む.              *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RBMTR00C.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MTROUT ASSIGN TO 'app/data/portable/MTRMAST.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-MO.
       DATA DIVISION.
       FILE SECTION.
       FD  MTROUT RECORD CONTAINS 128 CHARACTERS.
       01  MO-REC              PIC X(128).
       WORKING-STORAGE SECTION.
       77  ST-MO               PIC XX VALUE '00'.
       77  DLI-GU              PIC X(4) VALUE 'GU  '.
       77  DLI-GN              PIC X(4) VALUE 'GN  '.
       77  DLI-GNP             PIC X(4) VALUE 'GNP '.
       77  SSA-NASHI           PIC X(40) VALUE SPACE.
       77  OWARI-FLG           PIC X(1) VALUE 'N'.
       77  ROOT-ARI            PIC X(1) VALUE 'N'.
       77  GENKO-ARI           PIC 9(1) VALUE ZERO.
       77  WK-ROOT             PIC 9(7) VALUE ZERO.
       77  WK-TWIN             PIC 9(7) VALUE ZERO.
       77  WK-OUT              PIC 9(7) VALUE ZERO.
       77  WK-SEIGO-NG         PIC 9(7) VALUE ZERO.
       77  W-MAE-TORI          PIC 9(8) VALUE ZERO.
       77  WS-K                PIC 9(2) VALUE ZERO.
       01  DLI-IOAREA          PIC X(48) VALUE SPACE.
       COPY RCMTRSEG.
       01  GENKO-TWIN.
           05  GN-SPT          PIC X(22) VALUE SPACE.
           05  GN-SETTI        PIC 9(8)  VALUE ZERO.
       COPY RCMTRREC.
       LINKAGE SECTION.
       01  MTR-PCB.
       COPY RCIMSPCB.
       PROCEDURE DIVISION USING MTR-PCB.
       MAIN-RTN.
           OPEN OUTPUT MTROUT.
           IF ST-MO NOT = '00'
               DISPLAY 'RBMTR00C E001 OPEN NG ' ST-MO
               MOVE 16 TO RETURN-CODE
               STOP RUN.
      *    先頭ルートへ位置付け (無修飾 GU)
           CALL 'CBLTDLI' USING DLI-GU MTR-PCB DLI-IOAREA SSA-NASHI.
           PERFORM STATUS-KENSA THRU STATUS-KENSA-EX.
           IF OWARI-FLG = 'Y'
               GO TO SYUKEI-RTN.
       ROOT-LOOP.
           IF PCB-SEG-NAME NOT = 'KEIKISEG'
               DISPLAY 'RBMTR00C E003 KAISO KUZURE'
               ADD 1 TO WK-SEIGO-NG
               GO TO TUGI-ROOT.
           MOVE DLI-IOAREA (1:32) TO KEIKISEG-AREA.
           ADD 1               TO WK-ROOT.
           MOVE 'Y'            TO ROOT-ARI.
           PERFORM ROOT-KENSA  THRU ROOT-KENSA-EX.
           PERFORM TWIN-ATSUME THRU TWIN-ATSUME-EX.
           IF GENKO-ARI = ZERO
               DISPLAY 'RBMTR00C E004 GENKO NASI ' KS-KEIKI-NO
               ADD 1 TO WK-SEIGO-NG
               GO TO TUGI-ROOT.
           IF GENKO-ARI GREATER 1
               DISPLAY 'RBMTR00C E005 GENKO JUFUKU ' KS-KEIKI-NO
               ADD 1 TO WK-SEIGO-NG
               GO TO TUGI-ROOT.
           PERFORM SYUTU-HENSYU THRU SYUTU-HENSYU-EX.
       TUGI-ROOT.
      *    GNP 消化後 現在位置は本ルート最終ツイン. GN で次ルートへ
           IF OWARI-FLG = 'Y'
               GO TO SYUKEI-RTN.
           CALL 'CBLTDLI' USING DLI-GN MTR-PCB DLI-IOAREA SSA-NASHI.
           PERFORM STATUS-KENSA THRU STATUS-KENSA-EX.
           IF OWARI-FLG = 'Y'
               GO TO SYUKEI-RTN.
           GO TO ROOT-LOOP.
       SYUKEI-RTN.
           CLOSE MTROUT.
           DISPLAY 'RBMTR00C ROOT=' WK-ROOT ' TWIN=' WK-TWIN
                   ' OUT=' WK-OUT ' SEIGO-NG=' WK-SEIGO-NG.
           IF WK-SEIGO-NG GREATER ZERO
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE.
           STOP RUN.
      *----------------------------------------------------------------
      * ツイン収集: GNP で親内を全走査. 取外日=0 が現行
      *----------------------------------------------------------------
       TWIN-ATSUME             SECTION.
       TWA-010.
           MOVE ZERO           TO GENKO-ARI W-MAE-TORI.
       TWA-020.
           CALL 'CBLTDLI' USING DLI-GNP MTR-PCB DLI-IOAREA SSA-NASHI.
           IF PCB-STATUS = 'GE'
               GO TO TWIN-ATSUME-EX.
           IF PCB-STATUS NOT = SPACES
               DISPLAY 'RBMTR00C E006 GNP ' PCB-STATUS
               ADD 1 TO WK-SEIGO-NG
               GO TO TWIN-ATSUME-EX.
           MOVE DLI-IOAREA (1:48) TO TRTUSEG-AREA.
           ADD 1               TO WK-TWIN.
           PERFORM TWIN-KENSA  THRU TWIN-KENSA-EX.
           IF TS-TORIHAZUSI-BI = ZERO
               ADD 1 TO GENKO-ARI
               MOVE TS-SPT-NO      TO GN-SPT
               MOVE TS-TORITUKE-BI TO GN-SETTI.
           GO TO TWA-020.
       TWIN-ATSUME-EX.
           EXIT.
      *----------------------------------------------------------------
      * ルート整合検査
      *----------------------------------------------------------------
       ROOT-KENSA              SECTION.
       RTK-010.
           IF KS-KEIKI-NO (1:3) NOT = 'MTR'
               GO TO RTK-NG.
           IF KS-KEIKI-NO (4:7) NOT NUMERIC
               GO TO RTK-NG.
           IF KS-KENTEI-GENGO NOT = 'S' AND KS-KENTEI-GENGO NOT = 'H'
              AND KS-KENTEI-GENGO NOT = 'R' AND
              KS-KENTEI-GENGO NOT = ' '
               GO TO RTK-NG.
           IF KS-KENTEI-YY NOT NUMERIC
               GO TO RTK-NG.
           IF KS-KENTEI-MM LESS 01 OR KS-KENTEI-MM GREATER 12
               GO TO RTK-NG.
           IF KS-JORITU = ZERO
               GO TO RTK-NG.
           IF KS-JORITU GREATER 60.0
               GO TO RTK-NG.
           IF KS-KETA-SU LESS 4 OR KS-KETA-SU GREATER 7
               GO TO RTK-NG.
           IF KS-KISYU-CD (1:1) NOT = 'K' AND
              KS-KISYU-CD (1:1) NOT = 'S' AND
              KS-KISYU-CD (1:1) NOT = 'D'
               GO TO RTK-NG.
           IF KS-KISYU-CD (2:1) NOT NUMERIC
               GO TO RTK-NG.
           MOVE 3              TO WS-K.
       RTK-020.
           IF WS-K GREATER 6
               GO TO RTK-030.
           IF KS-KISYU-CD (WS-K:1) LESS SPACE
               GO TO RTK-NG.
           ADD 1               TO WS-K.
           GO TO RTK-020.
       RTK-030.
           IF KS-KOKAN-BI NOT NUMERIC
               GO TO RTK-NG.
           IF KS-KOKAN-BI NOT = ZERO
               IF KS-KOKAN-BI LESS 19500101
                   GO TO RTK-NG
               END-IF
           END-IF
           GO TO ROOT-KENSA-EX.
       RTK-NG.
           DISPLAY 'RBMTR00C E007 ROOT FUSEI ' KS-KEIKI-NO.
           ADD 1               TO WK-SEIGO-NG.
       ROOT-KENSA-EX.
           EXIT.
      *----------------------------------------------------------------
      * ツイン整合検査: 期間の連なり・地点形式
      *----------------------------------------------------------------
       TWIN-KENSA              SECTION.
       TWK-010.
           IF TS-KEIKI-NO NOT = KS-KEIKI-NO
               GO TO TWK-NG.
           IF TS-TORITUKE-BI NOT NUMERIC
               GO TO TWK-NG.
           IF TS-TORITUKE-BI LESS 19500101
               GO TO TWK-NG.
           IF TS-TORIHAZUSI-BI NOT NUMERIC
               GO TO TWK-NG.
           IF TS-TORIHAZUSI-BI NOT = ZERO
               IF TS-TORIHAZUSI-BI LESS TS-TORITUKE-BI
                   GO TO TWK-NG
               END-IF
           END-IF
           IF W-MAE-TORI NOT = ZERO
               IF TS-TORITUKE-BI LESS W-MAE-TORI
      *            取付日が並び順で逆行 (ツイン順序崩れ)
                   GO TO TWK-NG
               END-IF
           END-IF
           MOVE TS-TORITUKE-BI TO W-MAE-TORI.
           IF TS-SPT-NO (1:2) NOT = '03'
               GO TO TWK-NG.
           IF TS-SPT-NO (3:2) NOT NUMERIC
               GO TO TWK-NG.
           IF TS-SPT-NO (3:2) LESS '01'
               GO TO TWK-NG.
           IF TS-SPT-NO (3:2) GREATER '47'
               GO TO TWK-NG.
           GO TO TWIN-KENSA-EX.
       TWK-NG.
           DISPLAY 'RBMTR00C E008 TWIN FUSEI ' TS-KEIKI-NO.
           ADD 1               TO WK-SEIGO-NG.
       TWIN-KENSA-EX.
           EXIT.
      *----------------------------------------------------------------
      * 抽出編集: ルート属性 + 現行ツイン -> 128B
      *----------------------------------------------------------------
       SYUTU-HENSYU            SECTION.
       SYH-010.
           MOVE SPACES         TO MTR-REC.
           MOVE KS-KEIKI-NO    TO MTR-NO.
           MOVE GN-SPT         TO MTR-SPT-NO.
           MOVE KS-KENTEI-GENGO TO MTR-KENTEI-GENGO.
           MOVE KS-KENTEI-YY   TO MTR-KENTEI-YY.
           MOVE KS-KENTEI-MM   TO MTR-KENTEI-MM.
           MOVE KS-JORITU      TO MTR-JORITU.
           MOVE KS-KETA-SU     TO MTR-KETA-SU.
           MOVE KS-KOKAN-BI    TO MTR-KOKAN-BI.
           MOVE GN-SETTI       TO MTR-SETTI-BI.
           MOVE KS-KISYU-CD    TO MTR-KISYU-CD.
           MOVE MTR-REC        TO MO-REC.
           WRITE MO-REC.
           ADD 1               TO WK-OUT.
       SYUTU-HENSYU-EX.
           EXIT.
      *----------------------------------------------------------------
       STATUS-KENSA            SECTION.
       STK-010.
           IF PCB-STATUS = SPACES
               GO TO STATUS-KENSA-EX.
           IF PCB-STATUS = 'GB'
               MOVE 'Y' TO OWARI-FLG
               GO TO STATUS-KENSA-EX.
           IF PCB-STATUS = 'GE'
               MOVE 'Y' TO OWARI-FLG
               GO TO STATUS-KENSA-EX.
           DISPLAY 'RBMTR00C E002 DLI ' PCB-STATUS.
           ADD 1               TO WK-SEIGO-NG.
           MOVE 'Y'            TO OWARI-FLG.
       STATUS-KENSA-EX.
           EXIT.
