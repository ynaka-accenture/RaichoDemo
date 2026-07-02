       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RCHKDAT.
      * 生成データとコピー句のバイト整合検証 (品質ゲート用)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TANKA-F ASSIGN TO 'app/data/portable/TANKA.dat'
               ORGANIZATION IS SEQUENTIAL.
           SELECT KYK-F ASSIGN TO 'app/data/portable/KYKMAST.dat'
               ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  TANKA-F RECORD CONTAINS 80 CHARACTERS.
       COPY RCTANKA.
       FD  KYK-F RECORD CONTAINS 320 CHARACTERS.
       COPY RCKYKREC.
       WORKING-STORAGE SECTION.
       77  WK-ED1              PIC ZZZZ9.99.
       77  WK-ED2              PIC -Z9.99.
       77  WK-ED3              PIC ZZZ,ZZZ,ZZ9.
       77  WK-CNT              PIC 9(7) VALUE ZERO.
       77  WK-EOF              PIC X VALUE 'N'.
       PROCEDURE DIVISION.
           OPEN INPUT TANKA-F.
           READ TANKA-F.
           MOVE TAN-KIHON-TANKA TO WK-ED1.
           MOVE TAN-NENCHO-TANKA TO WK-ED2.
           DISPLAY 'TANKA1: SYU=' TAN-SYUBETU
                   ' KAISI=' TAN-TEKIYO-KAISI
                   ' KIHON=' WK-ED1 ' NENCHO=' WK-ED2
                   ' ZEIKBN=' TAN-ZEI-KBN.
           CLOSE TANKA-F.
           OPEN INPUT KYK-F.
           READ KYK-F.
           MOVE KYK-KIHON-KIN TO WK-ED3.
           DISPLAY 'KYK1: SPT=' KYK-SPT-NO ' JYU=' KYK-JYU-NO
                   ' SYU=' KYK-SYUBETU ' KIHON=' WK-ED3.
           DISPLAY 'KYK1: KAISI=' KYK-TEKIYO-KAISI
                   ' SYURYO=' KYK-TEKIYO-SYURYO
                   ' CHIKU(3:2)=' KYK-SPT-NO (3:2)
                   ' CHK(21:2)=' KYK-SPT-NO (21:2).
           PERFORM UNTIL WK-EOF = 'Y'
               READ KYK-F
                   AT END MOVE 'Y' TO WK-EOF
                   NOT AT END ADD 1 TO WK-CNT
               END-READ
           END-PERFORM.
           DISPLAY 'KYK-COUNT=' WK-CNT ' (+1st = 6000 expected)'.
           CLOSE KYK-F.
           STOP RUN.
