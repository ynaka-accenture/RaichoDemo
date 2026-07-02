       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RCHKCPY.
      * コピー句全数の構文・サイズ検証 (品質ゲート用)
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY RCJYUREC.
       COPY RCKYKREC.
       COPY RCMTRREC.
       COPY RCKENREC.
       COPY RCKENIN.
       COPY RCRYOREC.
       COPY RCZENGIN.
       COPY RCDMDMSG.
       COPY RCTANKA.
       COPY RCSHUREC.
       COPY RCDATWK.
       COPY RCCOMKYK.
       PROCEDURE DIVISION.
           MOVE 48 TO KIN-D-SU.
           DISPLAY 'JYU=' LENGTH OF JYU-REC
                   ' KYK=' LENGTH OF KYK-REC
                   ' MTR=' LENGTH OF MTR-REC
                   ' KEN=' LENGTH OF KEN-REC.
           DISPLAY 'KIN=' LENGTH OF KIN-REC
                   ' KIND=' LENGTH OF KIN-D-REC
                   ' RYO=' LENGTH OF RYO-REC
                   ' ZG=' LENGTH OF ZG-REC.
           DISPLAY 'DMD=' LENGTH OF DMD-MSG
                   ' TAN=' LENGTH OF TAN-REC
                   ' SHU=' LENGTH OF SHU-REC
                   ' DTC=' LENGTH OF DTC-PARM
                   ' COM=' LENGTH OF COM-KYK-AREA.
           STOP RUN.
