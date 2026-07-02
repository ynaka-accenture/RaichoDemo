       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTIMS.
      * DLIBATCH 相当: PCB を渡して IMS バッチを起動する
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PCB.
       COPY RCIMSPCB.
       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 'MTRDBD  '     TO PCB-DBD-NAME.
           MOVE 'A   '         TO PCB-PROC-OPT.
           CALL 'RBMTR00C' USING WS-PCB.
           STOP RUN.
