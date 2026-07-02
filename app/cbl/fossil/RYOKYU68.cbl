      *****************************************************************
      * RYOKYU68  料金計算 (S43 初版 / OS/VS COBOL)                   *
      *   ※本メンバは S60 の RBRYO00C 移行後 未使用.                 *
      *   ※現行コンパイラでは翻訳不能 (最終翻訳 1998-06-12)          *
      *   ※資産台帳 No.0007 により保存指定 (削除禁止)                *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RYOKYU68.
       REMARKS. DENRYOKU RYOKIN KEISAN (KYU HOSHIKI).
                SAKUSEI SHOWA 43 NEN. TANTO FUWA.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-360.
       OBJECT-COMPUTER. IBM-360 MEMORY SIZE 128000 CHARACTERS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KENF ASSIGN TO UT-2400-S-KENIN.
           SELECT RYOF ASSIGN TO UT-2400-S-RYOOUT.
       DATA DIVISION.
       FILE SECTION.
       FD  KENF
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS KEN-KYU-REC
           RECORDING MODE IS F.
       01  KEN-KYU-REC.
           05  KYU-KOKYAKU     PIC 9(8).
           05  KYU-SIYO        PIC 9(5).
           05  KYU-KBN         PIC X.
           05  FILLER          PIC X(66).
       FD  RYOF
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS RYO-KYU-REC
           RECORDING MODE IS F.
       01  RYO-KYU-REC         PIC X(80).
       WORKING-STORAGE SECTION.
       77  GOKEI               PIC 9(7) VALUE ZERO.
       77  KENSU               PIC 9(5) VALUE ZERO.
       PROCEDURE DIVISION.
       HAJIME.
           NOTE KORE WA KYU HOSHIKI NO RYOKIN KEISAN DE ARU.
                SHOWA 43 NEN 7 GATSU SAKUSEI.
           READY TRACE.
           OPEN INPUT KENF OUTPUT RYOF.
       YOMI.
           READ KENF AT END GO TO OWARI.
           EXAMINE KYU-KOKYAKU TALLYING LEADING ZEROS.
           TRANSFORM KYU-KBN FROM ' ' TO '1'.
           COMPUTE GOKEI = 500 + 18 * KYU-SIYO.
           MOVE GOKEI TO RYO-KYU-REC.
           WRITE RYO-KYU-REC.
           ADD 1 TO KENSU.
           GO TO YOMI.
       OWARI.
           RESET TRACE.
           CLOSE KENF RYOF.
           DISPLAY 'RYOKYU68 KENSU=' KENSU.
           STOP RUN.
