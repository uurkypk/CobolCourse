       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGEB105.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE
              ASSIGN TO IDXFILE
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM
              RECORD KEY IS IDX-KEY
              FILE STATUS IS IDX-ST.
           SELECT INP-FILE
              ASSIGN TO INPFILE
              FILE STATUS IS INP-ST.
           SELECT OUT-FILE
              ASSIGN TO OUTFILE
              FILE STATUS IS OUT-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  IDX-FILE.
       01  IDX-REC.
           03 IDX-KEY.
              05 IDX-ID            PIC S9(5) COMP-3.
              05 IDX-DVZ           PIC S9(3) COMP.
           03 IDX-NAME             PIC X(30).
           03 IDX-DATE             PIC S9(07) COMP-3.
           03 IDX-BALANCE          PIC S9(15) COMP-3.
       FD  INP-FILE RECORDING MODE F.
       01  INP-REC.
           03 REC-KEY.
              05 REC-ID            PIC X(5).
              05 REC-DVZ           PIC X(3).
       FD  OUT-FILE RECORDING MODE F.
       01  OUT-REC.
           03 REC-ID-O             PIC X(5).
           03 FILLER               PIC X(10) VALUE SPACE.
           03 REC-DVZ-O            PIC X(3).
           03 FILLER               PIC X(10) VALUE SPACE.
           03 REC-NAME-O           PIC X(30).
           03 REC-DATE-O           PIC X(8).
           03 FILLER               PIC X(10) VALUE SPACE.
           03 REC-BALANCE-O        PIC $$$,$$$,$$99.99.
       WORKING-STORAGE SECTION.
       01  WS-INT-DATE             PIC 9(7).
       01  WS-GREG-DATE            PIC 9(8).
       01  WS-NEW-BALANCE          PIC 9(15).
       01  WS-DVZ                  PIC 9(3).
       01  FLAGS.
           03 INP-FILE-EOF         PIC X     VALUE SPACE.
           03 IDX-ST               PIC X(02) VALUE SPACES.
               88 IDX-ST-OK                  VALUE '00'.
               88 IDX-ST-DUP-KEY             VALUE '02'.
           03 INP-ST               PIC X(02) VALUE SPACES.
               88 INP-ST-OK                  VALUE '00'.
           03 OUT-ST               PIC X(02) VALUE SPACES.
               88 OUT-ST-OK                  VALUE '00'.
       01  ERR-MSG.
           03 IDX-ERROR.
               05 IDX-ERROR-ID     PIC X(5).
           03 FILLER               PIC X(16) VALUE ': DATA NOT FOUND'.
       01  HEADER-1.
           03 FILLER         PIC X(24) VALUE 'CHANGED DATA'.
           03 FILLER         PIC X(60) VALUE SPACES.
       01  HEADER-2.
           03 FILLER         PIC X(15) VALUE 'ID'.
           03 FILLER         PIC X(13) VALUE 'DOVIZ'.
           03 FILLER         PIC X(15) VALUE 'NAME'.
           03 FILLER         PIC X(15) VALUE 'LASTNAME'.
           03 FILLER         PIC X(18) VALUE 'BIRTHDATE'.
           03 FILLER         PIC X(15) VALUE 'BALANCE'.
       01  HEADER-3.
           03 FILLER         PIC X(15) VALUE '---------------'.
           03 FILLER         PIC X(15) VALUE '---------------'.
           03 FILLER         PIC X(15) VALUE '---------------'.
           03 FILLER         PIC X(15) VALUE '---------------'.
           03 FILLER         PIC X(15) VALUE '---------------'.
           03 FILLER         PIC X(15) VALUE '---------------'.
           03 FILLER         PIC X(15) VALUE '---------------'.
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM OPEN-FILES-PARA.
           PERFORM WRITE-HEADER-PARA.
           PERFORM PROCESS-PARA.
           PERFORM CLOSE-PARA.
           STOP RUN.

       OPEN-FILES-PARA.
           INITIALIZE IDX-ST INP-ST OUT-ST.
           OPEN INPUT IDX-FILE
           OPEN INPUT INP-FILE
           OPEN OUTPUT OUT-FILE
           IF IDX-ST-OK
              CONTINUE
           ELSE
              DISPLAY "FILE OPEN FAILED: " IDX-ST
              GO TO EXIT-PARA
           END-IF.
           IF INP-ST-OK
              CONTINUE
           ELSE
              DISPLAY "FILE OPEN FAILED: " IDX-ST
              GO TO EXIT-PARA
           END-IF.
           IF OUT-ST-OK
              CONTINUE
           ELSE
              DISPLAY "FILE OPEN FAILED: " IDX-ST
              GO TO EXIT-PARA
           END-IF.

       PROCESS-PARA.
           PERFORM UNTIL INP-FILE-EOF = 'Y'
               READ INP-FILE
                   AT END MOVE 'Y' TO INP-FILE-EOF
               END-READ
               PERFORM CONVERT-CHAR-TO-NUMBER
               READ IDX-FILE
               KEY IS IDX-KEY
               INVALID KEY
                    MOVE IDX-ID TO IDX-ERROR-ID
                    PERFORM WRITE-ERROR-PARA
               NOT INVALID KEY
                    PERFORM WRITE-DATA-PARA
               END-READ
           END-PERFORM.

       CONVERT-DATE-TO-GREG.
           COMPUTE WS-INT-DATE = FUNCTION INTEGER-OF-DAY(IDX-DATE).
           COMPUTE WS-GREG-DATE = FUNCTION DATE-OF-INTEGER(WS-INT-DATE).

       CONVERT-CHAR-TO-NUMBER.
           COMPUTE IDX-ID = FUNCTION NUMVAL-C(REC-ID).
           COMPUTE IDX-DVZ = FUNCTION NUMVAL(REC-DVZ).

       WRITE-HEADER-PARA.
           WRITE OUT-REC FROM HEADER-1.
           WRITE OUT-REC FROM HEADER-2.
           WRITE OUT-REC FROM HEADER-3.

       WRITE-DATA-PARA.
           MOVE SPACES TO OUT-REC.
           PERFORM CONVERT-DATE-TO-GREG.
           COMPUTE WS-NEW-BALANCE = IDX-DVZ + IDX-BALANCE.
           MOVE IDX-ID           TO  REC-ID-O.
           MOVE IDX-DVZ          TO  REC-DVZ-O.
           MOVE IDX-NAME         TO  REC-NAME-O.
           MOVE WS-GREG-DATE     TO  REC-DATE-O.
           MOVE WS-NEW-BALANCE   TO  REC-BALANCE-O.
           WRITE OUT-REC.

       WRITE-ERROR-PARA.
           MOVE SPACES TO OUT-REC.
           PERFORM CONVERT-CHAR-TO-NUMBER.
           WRITE OUT-REC FROM ERR-MSG.

       CLOSE-PARA.
           CLOSE IDX-FILE.
           CLOSE INP-FILE.
           CLOSE OUT-FILE.
       EXIT-PARA. EXIT PROGRAM.