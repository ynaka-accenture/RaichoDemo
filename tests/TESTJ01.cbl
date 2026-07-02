       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTJ01.
      * J-01 実証: 通算日に運用上あり得ない巨大値を渡すと、
      * 年カウンタ 9(4) が黙って桁あふれし、アベンドせず不正な日付を返す
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WK-PARM.
           05  WK-FUNC         PIC X(2) VALUE 'NS'.
           05  WK-GENGO        PIC X(1) VALUE SPACE.
           05  WK-DATE-IN      PIC X(8) VALUE SPACES.
           05  WK-DATE-OUT     PIC X(8) VALUE SPACES.
           05  WK-SERIAL       PIC S9(7) COMP-3 VALUE 9999999.
           05  WK-RC           PIC 9(2) VALUE ZERO.
       PROCEDURE DIVISION.
           CALL 'RUTLDTC' USING WK-PARM.
           DISPLAY 'J01 SERIAL=9999999 OUT=' WK-DATE-OUT ' RC=' WK-RC.
           STOP RUN.
