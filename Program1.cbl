       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZIPCODES.
      *******************************
      * AUTHOR: JACKIE MARCANO	    *
      * Project: ZIP CODE FINDER    *
      *******************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ZIP-DATA
               ASSIGN TO "C:\COBOL\us_postal_codes.prn"
                ORGANIZATION IS LINE SEQUENTIAL
                ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ZIP-DATA.
      *ACCEPT DATA FROM ZIP-DATA FILE INTO AN ARRAY.*
       01 ZIP-DATA-ENTRY.
           05  ZIP-DATA-TABLE OCCURS 50000 INDEXED BY X1.
               10 ZIP-CODE-IN      PIC 9(5).
               10 PLACE-NAME-IN    PIC X(23).
               10 STATE-NAME-IN    PIC X(16).
               10 STATE-CODE-IN    PIC X(3).
               10 COUNTY-NAME-IN   PIC X(27).
               10 LAT-IN           PIC X(6).
               10 LON-IN           PIC X(8).

       WORKING-STORAGE SECTION.
      ***************************************************
       01 WS-ZIPCODE.
           05 ZIP-CODE-HOLD      PIC 9(5).
           05 PLACE-NAME-HOLD    PIC X(23).
           05 STATE-NAME-HOLD    PIC X(16).
           05 STATE-CODE-HOLD    PIC X(3).
           05 COUNTY-NAME-HOLD   PIC X(27).
           05 LAT-HOLD           PIC X(6).
           05 LON-HOLD           PIC X(8).
      ****************************************************
       01 WS-MAX-SEARCH-INDEX      PIC 9(5) VALUE 50000.
       01 SEARCH-TYPE-INPUT        PIC Z.
       01 ZIPCODE-INPUT            PIC 9(5).
       01 STATE-INPUT              PIC X(2).
       01 DUMMY-PAUSE              PIC X.



       PROCEDURE DIVISION.

           100-MAIN.
           OPEN INPUT ZIP-DATA
           READ ZIP-DATA
           PERFORM UNTIL SEARCH-TYPE-INPUT = 3
           DISPLAY "What would you like to search for?"
           DISPLAY "1- ZIP CODE SEARCH    2-STATE CODE SEARCH 3- EXIT"
           ACCEPT SEARCH-TYPE-INPUT
           EVALUATE SEARCH-TYPE-INPUT
               WHEN 1
                   PERFORM 200-ZIP-SEARCH
               WHEN 2
                   PERFORM 300-STATE-CODE-SEARCH
               WHEN 3
                   CLOSE ZIP-DATA
                   STOP RUN
           END-EVALUATE
           END-PERFORM
           STOP RUN.

           200-ZIP-SEARCH.
           DISPLAY "ENTER ZIPCODE FOR SEARCH."
           ACCEPT ZIPCODE-INPUT
          PERFORM VARYING X1 FROM 1 BY 1
            UNTIL X1 > WS-MAX-SEARCH-INDEX
          IF ZIPCODE-INPUT = ZIP-CODE-IN(X1) THEN
              MOVE ZIP-DATA-TABLE(X1) TO WS-ZIPCODE
              DISPLAY WS-ZIPCODE
          END-IF
          END-PERFORM
          ACCEPT DUMMY-PAUSE
          display " " with erase
          MOVE 0 TO SEARCH-TYPE-INPUT.

           300-STATE-CODE-SEARCH.
               DISPLAY "WHAT STATE ZIPCODES WOULD YOU LIKE TO SEE?(i.e. NY)"
               ACCEPT STATE-INPUT
               PERFORM 400-STATE-SEARCH.

           400-STATE-SEARCH.
          PERFORM VARYING X1 FROM 1 BY 1
            UNTIL X1 > WS-MAX-SEARCH-INDEX
               IF STATE-INPUT = STATE-CODE-IN(X1) THEN
                   MOVE ZIP-DATA-TABLE(X1) TO WS-ZIPCODE
                   DISPLAY WS-ZIPCODE
               END-IF
          END-PERFORM
          DISPLAY "READ FIRST " WS-MAX-SEARCH-INDEX " RECORDS."
          ACCEPT DUMMY-PAUSE
          display " " with erase
            MOVE 0 TO SEARCH-TYPE-INPUT.




       END PROGRAM ZIPCODES.