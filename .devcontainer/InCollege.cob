>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. InCollege.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. GNUCOBOL.
OBJECT-COMPUTER. GNUCOBOL.

INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT USER-INPUT ASSIGN TO 'InCollege-test1.txt'
        ORGANIZATION IS LINE SEQUENTIAL.
    SELECT PROGRAM-OUTPUT ASSIGN TO 'InCollege-testoutput1.txt'
        ORGANIZATION IS LINE SEQUENTIAL.
    SELECT USER-ACCOUNTS ASSIGN TO 'Accounts.dat'
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS ACCOUNT-FILE-STATUS.
    SELECT USER-PROFILES ASSIGN TO 'Profiles.dat'
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS PROFILE-FILE-STATUS.
    SELECT USER-CONNECTIONS ASSIGN TO 'Connections.dat'
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS CONNECTION-FILE-STATUS.
    *> NEW: Permanent connections file
    SELECT PERMANENT-CONNECTIONS ASSIGN TO 'PermanentConnections.dat'
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS PERM-CONN-FILE-STATUS.

    SELECT USER-JOBS ASSIGN TO 'Jobs.dat'
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS JOB-FILE-STATUS.

    *> NEW: Applications file (user -> job-id)
    SELECT USER-APPLICATIONS ASSIGN TO 'Applications.dat'
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS APP-FILE-STATUS.

    *> NEW: Messages file (Week 8)
    SELECT USER-MESSAGES ASSIGN TO 'Messages.dat'
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS MESSAGE-FILE-STATUS.

DATA DIVISION.
FILE SECTION.
FD  USER-INPUT.
01  INPUT-LINE                 PIC X(200).

FD  PROGRAM-OUTPUT.
01  OUTPUT-LINE                PIC X(200).

FD  USER-ACCOUNTS.
01  ACCOUNT-LINE-OUT     PIC X(33).

*> NEW: Profiles file (one profile per line, '|' between fields, '^' between entries, '~' inside entry)
FD  USER-PROFILES.
01  PROFILE-REC                PIC X(1500).

*> NEW: Connections file (pending requests)
FD  USER-CONNECTIONS.
01  CONNECTION-REC             PIC X(200).

FD  PERMANENT-CONNECTIONS.
01  PERM-CONNECTION-REC         PIC X(200).

FD  USER-JOBS.
01  JOB-REC.
    05  JOB-ID        PIC 9(4).
    05  JOB-TITLE     PIC X(50).
    05  JOB-DESC      PIC X(200).
    05  JOB-EMPLOYER  PIC X(50).
    05  JOB-LOCATION  PIC X(50).
    05  JOB-SALARY    PIC X(30).
    05  JOB-POSTER    PIC X(20).

*> NEW: Applications file
FD  USER-APPLICATIONS.
01  APPLICATION-REC               PIC X(100).

*> NEW: Messages file (Week 8)
FD  USER-MESSAGES.
01  MESSAGE-REC                   PIC X(300).

WORKING-STORAGE SECTION.

*> =====================
*> NEW: Permanent Connections structures (for accept/reject)
*> =====================
01  MAX-PERMANENT-CONNECTIONS   PIC 99 VALUE 25.
01  PERMANENT-CONNECTION-TABLE.
       05  PERMANENT-COUNT         PIC 99 VALUE 0.
       05  PERMANENT-ENTRY OCCURS 25 TIMES.
           10  PERM-USER1          PIC X(20).
           10  PERM-USER2          PIC X(20).
01  PERM-CONN-FILE-STATUS       PIC XX.
01  WS-ACCT-USER                PIC X(20).
01  WS-ACCT-PASS                PIC X(12).
01  WS-FILE-STATUS              PIC XX.
01  GRAD-YEAR-IS-VALID          PIC X VALUE 'N'.
01  USERNAME-IS-VALID           PIC X VALUE 'N'.
01  ACCOUNT-FILE-STATUS        PIC XX.
01  PROFILE-FILE-STATUS        PIC XX.
01  END-OF-FILE-FLAG           PIC X      VALUE 'N'.
    88  NO-MORE-DATA           VALUE 'Y'.

01  INPUT-BUFFER               PIC X(200) VALUE SPACES.
01  MESSAGE-BUFFER             PIC X(200) VALUE SPACES.

01  CREDENTIALS-VALID          PIC X      VALUE 'N'.
01  PASSWORD-VALID-FLAG        PIC X      VALUE 'N'.

01  MAXIMUM-ACCOUNTS           PIC 9      VALUE 5.
01  ACCOUNT-INFO.
    05  ACCOUNT-COUNT          PIC 9      VALUE 0.
    05  ACCOUNT-ENTRY OCCURS 5 TIMES.
       10  ACCT-USER           PIC X(20).
       10  ACCT-PASS           PIC X(12).

01  LOOP-INDEX                 PIC 99     VALUE 0.
01  SKILL-INDEX                PIC 99     VALUE 0.

01  CURRENT-USER               PIC X(20)  VALUE SPACES.
01  INPUT-USERNAME             PIC X(20)  VALUE SPACES.
01  INPUT-PASSWORD             PIC X(12)  VALUE SPACES.

01  PASS-LENGTH                PIC 99     VALUE 0.
01  CURRENT-CHAR               PIC X      VALUE SPACE.
01  CONTAINS-UPPERCASE         PIC X      VALUE 'N'.
01  CONTAINS-DIGIT             PIC X      VALUE 'N'.
01  CONTAINS-SPECIAL-CHAR      PIC X      VALUE 'N'.

01  NORMALIZED-INPUT           PIC X(200) VALUE SPACES.

01  AVAILABLE-SKILLS.
    05  SKILL-LIST OCCURS 5 TIMES PIC X(40) VALUE SPACES.

01  MAX-JOBS        PIC 99 VALUE 25.
01  JOB-TABLE.
    05  JOB-COUNT    PIC 99 VALUE 0.
    05  JOB-ENTRY OCCURS 25 TIMES.
        10  J-ID         PIC 9(4).
        10  J-TITLE      PIC X(50).
        10  J-DESC       PIC X(200).
        10  J-EMPLOYER   PIC X(50).
        10  J-LOCATION   PIC X(50).
        10  J-SALARY     PIC X(30).
        10  J-POSTER     PIC X(20).
01  JOB-FILE-STATUS PIC XX.

*> =====================
*> NEW: Applications (persistent record of user -> job-id)
*> =====================
01  MAX-APPLICATIONS             PIC 9(3) VALUE 200.
01  APPLICATION-TABLE.
    05  APPLICATION-COUNT        PIC 9(3) VALUE 0.
    05  APPLICATION-ENTRY OCCURS 200 TIMES.
        10  APP-ID               PIC 9(4).
        10  APP-USER             PIC X(20).
        10  APP-JOBID            PIC 9(4).
01  APP-FILE-STATUS              PIC XX.
01  SELECTED-JOB-ID              PIC 9(4) VALUE 0.
01  MENU-CHOICE                  PIC X(30).
01  HAS-APPLIED-FLAG             PIC X VALUE 'N'.

*> =====================
*> NEW: Profile structures (in-memory)
*> =====================
01  MAX-PROFILES               PIC 9 VALUE 5.
01  PROFILE-TABLE.
    05  PROFILE-COUNT          PIC 9 VALUE 0.
    05  PROFILE-ENTRY OCCURS 5 TIMES.
       10  P-USER              PIC X(20).
       10  P-FIRST             PIC X(20).
       10  P-LAST              PIC X(20).
       10  P-UNIV              PIC X(40).
       10  P-MAJOR             PIC X(40).
       10  P-GRAD              PIC X(4).
       10  P-ABOUT             PIC X(200).
       10  P-EXP-COUNT         PIC 9 VALUE 0.
       10  P-EXP OCCURS 3 TIMES.
           15  P-EXP-TITLE     PIC X(30).
           15  P-EXP-COMP      PIC X(30).
           15  P-EXP-DATES     PIC X(30).
           15  P-EXP-DESC      PIC X(100).
       10  P-EDU-COUNT         PIC 9 VALUE 0.
       10  P-EDU OCCURS 3 TIMES.
           15  P-EDU-DEG       PIC X(30).
           15  P-EDU-SCHOOL    PIC X(40).
           15  P-EDU-YEARS     PIC X(20).

*> Helpers for serialization/deserialization
01  SER-LINE                   PIC X(1500).
01  TOK-USER                   PIC X(20).
01  TOK-FIRST                  PIC X(20).
01  TOK-LAST                   PIC X(20).
01  TOK-UNIV                   PIC X(40).
01  TOK-MAJOR                  PIC X(40).
01  TOK-GRAD                   PIC X(4).
01  TOK-ABOUT                  PIC X(200).
01  TOK-EXPCNT                 PIC 9.
01  TOK-EDUCNT                 PIC 9.
01  EXP-BLOCK                  PIC X(300).
01  EDU-BLOCK                  PIC X(300).
01  EXP1                       PIC X(120).
01  EXP2                       PIC X(120).
01  EXP3                       PIC X(120).
01  EDU1                       PIC X(120).
01  EDU2                       PIC X(120).
01  EDU3                       PIC X(120).
01  SUBI                       PIC 9 VALUE 0.
*> Pointers for STRING appends in serialization
01  EXP-PTR                    PIC 9(4) VALUE 1.
01  EDU-PTR                    PIC 9(4) VALUE 1.

01  YEAR-NUM                   PIC 9(4) VALUE 0.
01  PROFILE-IDX                PIC 9 VALUE 0.

*> =====================
*> NEW: Connections structures (in-memory)
*> =====================
01  MAX-CONNECTIONS            PIC 99 VALUE 25.
01  CONNECTION-TABLE.
    05  CONNECTION-COUNT       PIC 99 VALUE 0.
    05  CONNECTION-ENTRY OCCURS 25 TIMES.
        10  CONN-SENDER        PIC X(20).
        10  CONN-RECEIVER      PIC X(20).

01  CONNECTION-FILE-STATUS     PIC XX.

*> =====================
*> NEW: Messages structures (Week 8)
*> =====================
01  MAX-MESSAGES                PIC 9(3) VALUE 100.
01  MESSAGE-TABLE.
    05  MESSAGE-COUNT           PIC 9(3) VALUE 0.
    05  MESSAGE-ENTRY OCCURS 100 TIMES.
        10  MSG-SENDER          PIC X(20).
        10  MSG-RECIPIENT       PIC X(20).
        10  MSG-CONTENT         PIC X(200).
        10  MSG-TIMESTAMP       PIC X(20).

01  MESSAGE-FILE-STATUS         PIC XX.
01  RECIPIENT-USERNAME          PIC X(20).
01  MESSAGE-CONTENT-INPUT       PIC X(200).
01  IS-CONNECTED-FLAG           PIC X VALUE 'N'.
01  MESSAGES-FOUND              PIC 9(3) VALUE 0.

PROCEDURE DIVISION.
000-MAIN.
    PERFORM 100-INITIALIZE-PROGRAM
    PERFORM 200-MAIN-LOOP UNTIL NO-MORE-DATA
    PERFORM 900-TERMINATE-PROGRAM
    STOP RUN.

100-INITIALIZE-PROGRAM.
    OPEN OUTPUT PROGRAM-OUTPUT.
    OPEN INPUT  USER-INPUT.

    OPEN INPUT USER-ACCOUNTS.
    IF ACCOUNT-FILE-STATUS = "00"
        PERFORM FOREVER
            READ USER-ACCOUNTS
                AT END EXIT PERFORM
            END-READ

            IF ACCOUNT-LINE-OUT NOT = SPACES
                MOVE SPACES TO WS-ACCT-USER WS-ACCT-PASS
                UNSTRING ACCOUNT-LINE-OUT DELIMITED BY "|"
                    INTO WS-ACCT-USER
                         WS-ACCT-PASS
                END-UNSTRING

                ADD 1 TO ACCOUNT-COUNT
                MOVE FUNCTION TRIM(WS-ACCT-USER) TO ACCT-USER(ACCOUNT-COUNT)
                MOVE FUNCTION TRIM(WS-ACCT-PASS) TO ACCT-PASS(ACCOUNT-COUNT)
            END-IF
        END-PERFORM
        CLOSE USER-ACCOUNTS
    END-IF.

    MOVE "Skill 1" TO SKILL-LIST(1).
    MOVE "Skill 2" TO SKILL-LIST(2).
    MOVE "Skill 3" TO SKILL-LIST(3).
    MOVE "Skill 4" TO SKILL-LIST(4).
    MOVE "Skill 5" TO SKILL-LIST(5).

    PERFORM 860-LOAD-PROFILES.
    PERFORM 950-LOAD-CONNECTIONS.
    PERFORM 975-LOAD-PERMANENT-CONNECTIONS.
    PERFORM 934-LOAD-JOBS.
    PERFORM 940-LOAD-APPLICATIONS.
    PERFORM 590-LOAD-MESSAGES.

    MOVE "Welcome to InCollege!" TO MESSAGE-BUFFER.
    PERFORM 700-DISPLAY-MESSAGE.


200-MAIN-LOOP.
    MOVE "Log In" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    MOVE "Create New Account" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    MOVE "Enter your choice:" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    PERFORM 600-GET-USER-INPUT
    IF NO-MORE-DATA
        EXIT PARAGRAPH
    END-IF

    MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-BUFFER))
      TO NORMALIZED-INPUT

    EVALUATE TRUE
       WHEN NORMALIZED-INPUT = "1"
           OR NORMALIZED-INPUT = "LOGIN"
           OR NORMALIZED-INPUT = "LOG IN"
               PERFORM 300-LOGIN-PROCESS

       WHEN NORMALIZED-INPUT = "2"
             OR NORMALIZED-INPUT = "CREATE NEW ACCOUNT"
             OR NORMALIZED-INPUT = "CREATE ACCOUNT"
             OR NORMALIZED-INPUT = "CREATE"
               PERFORM 400-CREATE-ACCOUNT-PROCESS

       WHEN OTHER
           CONTINUE
    END-EVALUATE.

300-LOGIN-PROCESS.
    MOVE "Please enter your username:" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    PERFORM 600-GET-USER-INPUT
    IF NO-MORE-DATA
        EXIT PARAGRAPH
    END-IF
    MOVE FUNCTION TRIM(INPUT-BUFFER) TO INPUT-USERNAME

    MOVE "Please enter your password:" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    PERFORM 600-GET-USER-INPUT
    IF NO-MORE-DATA
        EXIT PARAGraph
    END-IF
    MOVE FUNCTION TRIM(INPUT-BUFFER) TO INPUT-PASSWORD

    PERFORM 800-VERIFY-CREDENTIALS
    IF CREDENTIALS-VALID = 'Y'
        MOVE "You have successfully logged in." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        MOVE SPACES TO MESSAGE-BUFFER
        STRING "Welcome, "                      DELIMITED BY SIZE
               FUNCTION TRIM(INPUT-USERNAME)    DELIMITED BY SIZE
               "!"                              DELIMITED BY SIZE
            INTO MESSAGE-BUFFER
        END-STRING
        PERFORM 700-DISPLAY-MESSAGE
        MOVE FUNCTION TRIM(INPUT-USERNAME) TO CURRENT-USER
        PERFORM 500-POST-LOGIN-OPERATIONS
    ELSE
        MOVE "Incorrect username/password, please try again"
            TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
    END-IF.


400-CREATE-ACCOUNT-PROCESS.
    IF ACCOUNT-COUNT >= MAXIMUM-ACCOUNTS
        MOVE "All permitted accounts have been created, please come back later"
            TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        EXIT PARAGRAPH
    END-IF

    *> Loop for getting a valid username
    MOVE 'N' TO USERNAME-IS-VALID
    PERFORM UNTIL USERNAME-IS-VALID = 'Y'
        MOVE "Please enter your username:" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM 600-GET-USER-INPUT
        IF NO-MORE-DATA
            EXIT PARAGRAPH
        END-IF
        MOVE FUNCTION TRIM(INPUT-BUFFER) TO INPUT-USERNAME

        *> Check 1: Length
        COMPUTE PASS-LENGTH =
            FUNCTION LENGTH(FUNCTION TRIM(INPUT-USERNAME))
        IF PASS-LENGTH > 20
            MOVE "Username too long (max 20)." TO MESSAGE-BUFFER
            PERFORM 700-DISPLAY-MESSAGE
        ELSE
            *> Check 2: Uniqueness
            MOVE 'Y' TO USERNAME-IS-VALID *> Assume it's valid
            PERFORM VARYING LOOP-INDEX FROM 1 BY 1
              UNTIL LOOP-INDEX > ACCOUNT-COUNT
                IF INPUT-USERNAME = FUNCTION TRIM(ACCT-USER(LOOP-INDEX))
                    MOVE "Username already exists, please try again."
                      TO MESSAGE-BUFFER
                    PERFORM 700-DISPLAY-MESSAGE
                    MOVE 'N' TO USERNAME-IS-VALID *> Mark as invalid
                    EXIT PERFORM
                END-IF
            END-PERFORM
        END-IF
    END-PERFORM

    MOVE "Please enter your password:" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    PERFORM 600-GET-USER-INPUT
    IF NO-MORE-DATA
        EXIT PARAGRAPH
    END-IF
    MOVE FUNCTION TRIM(INPUT-BUFFER) TO INPUT-PASSWORD

    PERFORM 810-VALIDATE-PASSWORD
    IF PASSWORD-VALID-FLAG NOT = 'Y'
        MOVE "Invalid password. Password must be 8-12 characters and include at least one capital letter, one digit, and one special character."
            TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        EXIT PARAGRAPH
    END-IF

    ADD 1 TO ACCOUNT-COUNT
    MOVE INPUT-USERNAME TO ACCT-USER(ACCOUNT-COUNT)
    MOVE INPUT-PASSWORD TO ACCT-PASS(ACCOUNT-COUNT)
    MOVE "Account created successfully." TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE.

500-POST-LOGIN-OPERATIONS.
    PERFORM UNTIL NO-MORE-DATA
        MOVE "1. Create/Edit My Profile" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        MOVE "2. View My Profile" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        MOVE "3. Search for User" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        MOVE "4. Learn a New Skill" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        MOVE "5. View My Pending Connection Requests" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        MOVE "6. View My Network" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        MOVE "7. Job Search/Internship" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        MOVE "8. Messages" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        MOVE "9. Exit" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        MOVE "Enter your choice:" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE

        PERFORM 600-GET-USER-INPUT
        IF NO-MORE-DATA
            EXIT PARAGRAPH
        END-IF
        MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-BUFFER))
          TO NORMALIZED-INPUT

        EVALUATE TRUE
           WHEN NORMALIZED-INPUT = "1"
             OR NORMALIZED-INPUT = "CREATE/EDIT MY PROFILE"
             OR NORMALIZED-INPUT = "CREATE"
             OR NORMALIZED-INPUT = "EDIT"
               PERFORM 560-CREATE-OR-EDIT-PROFILE

           WHEN NORMALIZED-INPUT = "2"
             OR NORMALIZED-INPUT = "VIEW MY PROFILE"
               PERFORM 565-VIEW-MY-PROFILE

           WHEN NORMALIZED-INPUT = "3"
             OR NORMALIZED-INPUT = "SEARCH FOR USER"
               PERFORM 570-SEARCH-AND-DISPLAY-PROFILE

           WHEN NORMALIZED-INPUT = "4"
             OR NORMALIZED-INPUT = "LEARN A NEW SKILL"
               PERFORM 550-SKILLS-MODULE

           WHEN NORMALIZED-INPUT = "5"
             OR NORMALIZED-INPUT = "VIEW MY PENDING CONNECTION REQUESTS"
               PERFORM 920-VIEW-PENDING-REQUESTS

           WHEN NORMALIZED-INPUT = "6"
             OR NORMALIZED-INPUT = "VIEW MY NETWORK"
               PERFORM 580-VIEW-MY-NETWORK

           WHEN NORMALIZED-INPUT = "7"
             OR NORMALIZED-INPUT = "JOB SEARCH/INTERNSHIP"
             OR NORMALIZED-INPUT = "JOB"
               PERFORM 930-JOB-SEARCH-MENU

           WHEN NORMALIZED-INPUT = "8"
             OR NORMALIZED-INPUT = "MESSAGES"
               PERFORM 585-MESSAGES-MENU

           WHEN NORMALIZED-INPUT = "9"
             OR NORMALIZED-INPUT = "EXIT"
               EXIT PARAGRAPH

           WHEN OTHER
               MOVE "Invalid option. Please try again." TO MESSAGE-BUFFER
               PERFORM 700-DISPLAY-MESSAGE
        END-EVALUATE
    END-PERFORM.

570-SEARCH-AND-DISPLAY-PROFILE.
    MOVE "Enter the full name of the person you are looking for:" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    PERFORM 600-GET-USER-INPUT
    IF NO-MORE-DATA EXIT PARAGRAPH END-IF
    MOVE FUNCTION TRIM(INPUT-BUFFER) TO INPUT-USERNAME

    MOVE 0 TO PROFILE-IDX
    PERFORM VARYING LOOP-INDEX FROM 1 BY 1 UNTIL LOOP-INDEX > PROFILE-COUNT
        MOVE SPACES TO MESSAGE-BUFFER
        MOVE SPACES TO INPUT-BUFFER
        *> Build full name from stored profile
        MOVE SPACES TO MESSAGE-BUFFER
        STRING FUNCTION TRIM(P-FIRST(LOOP-INDEX)) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(P-LAST(LOOP-INDEX)) DELIMITED BY SIZE
          INTO SER-LINE
        END-STRING
        IF FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-USERNAME)) =
           FUNCTION UPPER-CASE(FUNCTION TRIM(SER-LINE))
            MOVE LOOP-INDEX TO PROFILE-IDX
            EXIT PERFORM
        END-IF
    END-PERFORM

    IF PROFILE-IDX = 0
        MOVE "No one by that name could be found." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        EXIT PARAGRAPH
    END-IF

    MOVE "--- Found User Profile ---" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    MOVE SPACES TO MESSAGE-BUFFER
    STRING "Name: " DELIMITED BY SIZE
           FUNCTION TRIM(P-FIRST(PROFILE-IDX)) DELIMITED BY SIZE
           " " DELIMITED BY SIZE
           FUNCTION TRIM(P-LAST(PROFILE-IDX)) DELIMITED BY SIZE
      INTO MESSAGE-BUFFER
    END-STRING
    PERFORM 700-DISPLAY-MESSAGE

    MOVE SPACES TO MESSAGE-BUFFER
    STRING "University: " DELIMITED BY SIZE
           FUNCTION TRIM(P-UNIV(PROFILE-IDX)) DELIMITED BY SIZE
      INTO MESSAGE-BUFFER
    END-STRING
    PERFORM 700-DISPLAY-MESSAGE

    MOVE SPACES TO MESSAGE-BUFFER
    STRING "Major: " DELIMITED BY SIZE
           FUNCTION TRIM(P-MAJOR(PROFILE-IDX)) DELIMITED BY SIZE
      INTO MESSAGE-BUFFER
    END-STRING
    PERFORM 700-DISPLAY-MESSAGE

    MOVE SPACES TO MESSAGE-BUFFER
    STRING "Graduation Year: " DELIMITED BY SIZE
           FUNCTION TRIM(P-GRAD(PROFILE-IDX)) DELIMITED BY SIZE
      INTO MESSAGE-BUFFER
    END-STRING
    PERFORM 700-DISPLAY-MESSAGE

    IF FUNCTION TRIM(P-ABOUT(PROFILE-IDX)) NOT = SPACES
        MOVE SPACES TO MESSAGE-BUFFER
        STRING "About Me: " DELIMITED BY SIZE
               FUNCTION TRIM(P-ABOUT(PROFILE-IDX)) DELIMITED BY SIZE
          INTO MESSAGE-BUFFER
        END-STRING
        PERFORM 700-DISPLAY-MESSAGE
    END-IF

    MOVE "Experience:" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    IF P-EXP-COUNT(PROFILE-IDX) > 0
        PERFORM VARYING SUBI FROM 1 BY 1 UNTIL SUBI > P-EXP-COUNT(PROFILE-IDX)
            MOVE SPACES TO MESSAGE-BUFFER
            STRING "Title: " DELIMITED BY SIZE
                   FUNCTION TRIM(P-EXP-TITLE(PROFILE-IDX SUBI)) DELIMITED BY SIZE
              INTO MESSAGE-BUFFER
            END-STRING
            PERFORM 700-DISPLAY-MESSAGE

            MOVE SPACES TO MESSAGE-BUFFER
            STRING "Company: " DELIMITED BY SIZE
                   FUNCTION TRIM(P-EXP-COMP(PROFILE-IDX SUBI)) DELIMITED BY SIZE
              INTO MESSAGE-BUFFER
            END-STRING
            PERFORM 700-DISPLAY-MESSAGE

            MOVE SPACES TO MESSAGE-BUFFER
            STRING "Dates: " DELIMITED BY SIZE
                   FUNCTION TRIM(P-EXP-DATES(PROFILE-IDX SUBI)) DELIMITED BY SIZE
              INTO MESSAGE-BUFFER
            END-STRING
            PERFORM 700-DISPLAY-MESSAGE

            IF FUNCTION TRIM(P-EXP-DESC(PROFILE-IDX SUBI)) NOT = SPACES
                MOVE SPACES TO MESSAGE-BUFFER
                STRING "Description: " DELIMITED BY SIZE
                       FUNCTION TRIM(P-EXP-DESC(PROFILE-IDX SUBI)) DELIMITED BY SIZE
                  INTO MESSAGE-BUFFER
                END-STRING
                PERFORM 700-DISPLAY-MESSAGE
            END-IF
        END-PERFORM
    ELSE
        MOVE "Experience: None" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
    END-IF

    MOVE "Education:" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    IF P-EDU-COUNT(PROFILE-IDX) > 0
        PERFORM VARYING SUBI FROM 1 BY 1 UNTIL SUBI > P-EDU-COUNT(PROFILE-IDX)
            MOVE SPACES TO MESSAGE-BUFFER
            STRING "Degree: " DELIMITED BY SIZE
                   FUNCTION TRIM(P-EDU-DEG(PROFILE-IDX SUBI)) DELIMITED BY SIZE
              INTO MESSAGE-BUFFER
            END-STRING
            PERFORM 700-DISPLAY-MESSAGE

            MOVE SPACES TO MESSAGE-BUFFER
            STRING "University: " DELIMITED BY SIZE
                   FUNCTION TRIM(P-EDU-SCHOOL(PROFILE-IDX SUBI)) DELIMITED BY SIZE
              INTO MESSAGE-BUFFER
            END-STRING
            PERFORM 700-DISPLAY-MESSAGE

            MOVE SPACES TO MESSAGE-BUFFER
            STRING "Years: " DELIMITED BY SIZE
                   FUNCTION TRIM(P-EDU-YEARS(PROFILE-IDX SUBI)) DELIMITED BY SIZE
              INTO MESSAGE-BUFFER
            END-STRING
            PERFORM 700-DISPLAY-MESSAGE
        END-PERFORM
    ELSE
        MOVE "Education: None" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
    END-IF

    MOVE "--------------------" TO MESSAGE-BUFFER
    MOVE "Send connection request to this user? (Y/N)" TO MESSAGE-BUFFER
PERFORM 700-DISPLAY-MESSAGE
PERFORM 600-GET-USER-INPUT
IF NO-MORE-DATA EXIT PARAGRAPH END-IF

MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-BUFFER)) TO NORMALIZED-INPUT
IF NORMALIZED-INPUT = "Y" OR NORMALIZED-INPUT = "YES"
    PERFORM 910-SEND-CONNECTION-REQUESTS
END-IF.

*> =====================
*> NEW: View pending connection requests
*> =====================
*> =====================
*> UPDATED: View pending connection requests with accept/reject
*> =====================
*> =====================
*> UPDATED: View pending connection requests with accept/reject
*> =====================
*> =====================
*> UPDATED: View pending connection requests with accept/reject
*> =====================
920-VIEW-PENDING-REQUESTS.
    MOVE 0 TO LOOP-INDEX
    MOVE 0 TO SUBI
    MOVE "--- Pending Connection Requests ---" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    *> First, count how many pending requests exist
    MOVE 0 TO SUBI
    PERFORM VARYING LOOP-INDEX FROM 1 BY 1
      UNTIL LOOP-INDEX > CONNECTION-COUNT
        IF FUNCTION UPPER-CASE(FUNCTION TRIM(CONN-RECEIVER(LOOP-INDEX))) =
           FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER))
            ADD 1 TO SUBI
        END-IF
    END-PERFORM

    IF SUBI = 0
        MOVE "You have no pending connection requests." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        EXIT PARAGRAPH
    END-IF

    *> Process each pending request
    PERFORM VARYING LOOP-INDEX FROM 1 BY 1
      UNTIL LOOP-INDEX > CONNECTION-COUNT
        IF FUNCTION UPPER-CASE(FUNCTION TRIM(CONN-RECEIVER(LOOP-INDEX))) =
           FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER))
            MOVE SPACES TO MESSAGE-BUFFER
            STRING "Request from: " DELIMITED BY SIZE
                   FUNCTION TRIM(CONN-SENDER(LOOP-INDEX)) DELIMITED BY SIZE
              INTO MESSAGE-BUFFER
            END-STRING
            PERFORM 700-DISPLAY-MESSAGE

            *> Store the request index for processing
            MOVE LOOP-INDEX TO PROFILE-IDX
            PERFORM 925-PROCESS-SINGLE-REQUEST
        END-IF
    END-PERFORM

    MOVE "--------------------" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE.

*> =====================
*> NEW: Process individual connection request
*> =====================
925-PROCESS-SINGLE-REQUEST.
    MOVE "1. Accept" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    MOVE "2. Reject" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    MOVE SPACES TO MESSAGE-BUFFER
    STRING "Enter your choice for " DELIMITED BY SIZE
           FUNCTION TRIM(CONN-SENDER(PROFILE-IDX)) DELIMITED BY SIZE
           ":" DELIMITED BY SIZE
      INTO MESSAGE-BUFFER
    END-STRING
    PERFORM 700-DISPLAY-MESSAGE

    PERFORM 600-GET-USER-INPUT
    IF NO-MORE-DATA EXIT PARAGRAPH END-IF

    MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-BUFFER))
      TO NORMALIZED-INPUT

    EVALUATE TRUE
        WHEN NORMALIZED-INPUT = "1" OR NORMALIZED-INPUT = "ACCEPT"
            PERFORM 926-ACCEPT-CONNECTION-REQUEST
        WHEN NORMALIZED-INPUT = "2" OR NORMALIZED-INPUT = "REJECT"
            PERFORM 927-REJECT-CONNECTION-REQUEST
        WHEN OTHER
            MOVE "Invalid choice. Request will remain pending."
              TO MESSAGE-BUFFER
            PERFORM 700-DISPLAY-MESSAGE
    END-EVALUATE.

*> =====================
*> NEW: Accept connection request
*> =====================
926-ACCEPT-CONNECTION-REQUEST.
    *> Add to permanent connections (both directions)
    IF PERMANENT-COUNT < MAX-PERMANENT-CONNECTIONS
        ADD 1 TO PERMANENT-COUNT
        MOVE FUNCTION TRIM(CONN-SENDER(PROFILE-IDX))
          TO PERM-USER1(PERMANENT-COUNT)
        MOVE FUNCTION TRIM(CURRENT-USER)
          TO PERM-USER2(PERMANENT-COUNT)

        *> Also add reverse connection
        IF PERMANENT-COUNT < MAX-PERMANENT-CONNECTIONS
            ADD 1 TO PERMANENT-COUNT
            MOVE FUNCTION TRIM(CURRENT-USER)
              TO PERM-USER1(PERMANENT-COUNT)
            MOVE FUNCTION TRIM(CONN-SENDER(PROFILE-IDX))
              TO PERM-USER2(PERMANENT-COUNT)
        END-IF

        *> Remove from pending requests
        PERFORM 928-REMOVE-PENDING-REQUEST

        MOVE SPACES TO MESSAGE-BUFFER
        STRING "Connection request from " DELIMITED BY SIZE
               FUNCTION TRIM(CONN-SENDER(PROFILE-IDX)) DELIMITED BY SIZE
               " accepted!" DELIMITED BY SIZE
          INTO MESSAGE-BUFFER
        END-STRING
        PERFORM 700-DISPLAY-MESSAGE

        *> Save changes
        PERFORM 970-SAVE-PERMANENT-CONNECTIONS
        PERFORM 960-SAVE-CONNECTIONS
    ELSE
        MOVE "Cannot accept: connection limit reached." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
    END-IF.

*> =====================
*> NEW: Reject connection request
*> =====================
927-REJECT-CONNECTION-REQUEST.
    PERFORM 928-REMOVE-PENDING-REQUEST
    MOVE SPACES TO MESSAGE-BUFFER
    STRING "Connection request from " DELIMITED BY SIZE
           FUNCTION TRIM(CONN-SENDER(PROFILE-IDX)) DELIMITED BY SIZE
           " rejected." DELIMITED BY SIZE
      INTO MESSAGE-BUFFER
    END-STRING
    PERFORM 700-DISPLAY-MESSAGE
    PERFORM 960-SAVE-CONNECTIONS.

*> =====================
*> NEW: Remove pending request from table
*> =====================
928-REMOVE-PENDING-REQUEST.
    *> Shift all subsequent entries up
    IF PROFILE-IDX < CONNECTION-COUNT
        PERFORM VARYING LOOP-INDEX FROM PROFILE-IDX BY 1
          UNTIL LOOP-INDEX >= CONNECTION-COUNT
            MOVE CONN-SENDER(LOOP-INDEX + 1)
              TO CONN-SENDER(LOOP-INDEX)
            MOVE CONN-RECEIVER(LOOP-INDEX + 1)
              TO CONN-RECEIVER(LOOP-INDEX)
        END-PERFORM
    END-IF
    SUBTRACT 1 FROM CONNECTION-COUNT.
*> =====================
*> NEW: Process individual connection request
*> =====================

*> =====================
*> NEW: Accept connection request
*> =====================
910-SEND-CONNECTION-REQUESTS.
    *> Check for self-request
    IF FUNCTION TRIM(CURRENT-USER) = FUNCTION TRIM(P-USER(PROFILE-IDX))
        MOVE "You cannot send a connection request to yourself." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        EXIT PARAGRAPH
    END-IF

    *> Check if already connected in permanent connections
    PERFORM VARYING LOOP-INDEX FROM 1 BY 1 UNTIL LOOP-INDEX > PERMANENT-COUNT
        IF (FUNCTION TRIM(PERM-USER1(LOOP-INDEX)) = FUNCTION TRIM(CURRENT-USER) AND
            FUNCTION TRIM(PERM-USER2(LOOP-INDEX)) = FUNCTION TRIM(P-USER(PROFILE-IDX)))
        OR
           (FUNCTION TRIM(PERM-USER1(LOOP-INDEX)) = FUNCTION TRIM(P-USER(PROFILE-IDX)) AND
            FUNCTION TRIM(PERM-USER2(LOOP-INDEX)) = FUNCTION TRIM(CURRENT-USER))
            MOVE "You are already connected with this user." TO MESSAGE-BUFFER
            PERFORM 700-DISPLAY-MESSAGE
            EXIT PARAGRAPH
        END-IF
    END-PERFORM

    *> Check for existing pending request
    PERFORM VARYING LOOP-INDEX FROM 1 BY 1 UNTIL LOOP-INDEX > CONNECTION-COUNT
        *> Case 1: You already sent a request to them
        IF FUNCTION TRIM(CONN-SENDER(LOOP-INDEX)) = FUNCTION TRIM(CURRENT-USER) AND
           FUNCTION TRIM(CONN-RECEIVER(LOOP-INDEX)) = FUNCTION TRIM(P-USER(PROFILE-IDX))
                MOVE "You have already sent a connection request to this user." TO MESSAGE-BUFFER
                PERFORM 700-DISPLAY-MESSAGE
                EXIT PARAGRAPH
        END-IF
        *> Case 2: They already sent a request to you
        IF FUNCTION TRIM(CONN-SENDER(LOOP-INDEX)) = FUNCTION TRIM(P-USER(PROFILE-IDX)) AND
           FUNCTION TRIM(CONN-RECEIVER(LOOP-INDEX)) = FUNCTION TRIM(CURRENT-USER)
                MOVE "This user has already sent you a connection request." TO MESSAGE-BUFFER
                PERFORM 700-DISPLAY-MESSAGE
                EXIT PARAGRAPH
        END-IF
    END-PERFORM

    *> If we get here, no request exists. Add a new one.
    IF CONNECTION-COUNT < MAX-CONNECTIONS
        ADD 1 TO CONNECTION-COUNT
        MOVE FUNCTION TRIM(CURRENT-USER) TO CONN-SENDER(CONNECTION-COUNT)
        MOVE FUNCTION TRIM(P-USER(PROFILE-IDX)) TO CONN-RECEIVER(CONNECTION-COUNT)
        MOVE "Connection request sent!" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM 960-SAVE-CONNECTIONS
    ELSE
        MOVE "Cannot send request: connection request limit reached." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
    END-IF.

550-SKILLS-MODULE.
    PERFORM UNTIL NO-MORE-DATA
        *> Skills menu format to match sample output
        MOVE "Learn a New Skill:" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE

        *> Display Skill 1 through Skill 5
        PERFORM VARYING SKILL-INDEX FROM 1 BY 1
          UNTIL SKILL-INDEX > 5
            MOVE SKILL-LIST(SKILL-INDEX) TO MESSAGE-BUFFER
            PERFORM 700-DISPLAY-MESSAGE
        END-PERFORM

        MOVE "Go Back" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        MOVE "Enter your choice:" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE

        PERFORM 600-GET-USER-INPUT
        IF NO-MORE-DATA
            EXIT PARAGRAPH
        END-IF
        MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-BUFFER))
          TO NORMALIZED-INPUT

        IF NORMALIZED-INPUT = "GO BACK"
           OR NORMALIZED-INPUT = "BACK"
           OR NORMALIZED-INPUT = "0"
            EXIT PARAGRAPH
        ELSE
            MOVE 0 TO LOOP-INDEX
            IF NORMALIZED-INPUT >= "1" AND NORMALIZED-INPUT <= "5"
                MOVE FUNCTION NUMVAL(NORMALIZED-INPUT) TO LOOP-INDEX
            ELSE
                PERFORM VARYING SKILL-INDEX FROM 1 BY 1
                  UNTIL SKILL-INDEX > 5
                   IF NORMALIZED-INPUT = FUNCTION UPPER-CASE(
                      FUNCTION TRIM(SKILL-LIST(SKILL-INDEX)))
                      MOVE SKILL-INDEX TO LOOP-INDEX
                      EXIT PERFORM
                   END-IF
                END-PERFORM
            END-IF

            IF LOOP-INDEX >= 1 AND LOOP-INDEX <= 5
                MOVE "This skill is under construction." TO MESSAGE-BUFFER
                PERFORM 700-DISPLAY-MESSAGE
            END-IF
        END-IF
    END-PERFORM.

*> =====================
*> NEW: Profile creation / editing
*> =====================
560-CREATE-OR-EDIT-PROFILE.
    PERFORM 820-FIND-OR-CREATE-PROFILE-INDEX
    IF PROFILE-IDX = 0
        MOVE "Unable to create profile at this time." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        EXIT PARAGRAPH
    END-IF

    MOVE "--- Create/Edit Profile ---" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    *> First Name (Required)
    MOVE "Enter First Name:" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    PERFORM 600-GET-USER-INPUT
    IF NO-MORE-DATA EXIT PARAGRAPH END-IF
    MOVE FUNCTION TRIM(INPUT-BUFFER) TO P-FIRST(PROFILE-IDX)

    *> Last Name (Required)
    MOVE "Enter Last Name:" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    PERFORM 600-GET-USER-INPUT
    IF NO-MORE-DATA EXIT PARAGRAPH END-IF
    MOVE FUNCTION TRIM(INPUT-BUFFER) TO P-LAST(PROFILE-IDX)

    *> University (Required)
    MOVE "Enter University/College Attended:" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    PERFORM 600-GET-USER-INPUT
    IF NO-MORE-DATA EXIT PARAGRAPH END-IF
    MOVE FUNCTION TRIM(INPUT-BUFFER) TO P-UNIV(PROFILE-IDX)

    *> Major (Required)
    MOVE "Enter Major:" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    PERFORM 600-GET-USER-INPUT
    IF NO-MORE-DATA EXIT PARAGRAPH END-IF
    MOVE FUNCTION TRIM(INPUT-BUFFER) TO P-MAJOR(PROFILE-IDX)

    *> Graduation Year (Required, validated)
      MOVE 'N' TO GRAD-YEAR-IS-VALID
    PERFORM UNTIL GRAD-YEAR-IS-VALID = 'Y'
        MOVE "Enter Graduation Year (YYYY):" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM 600-GET-USER-INPUT
        IF NO-MORE-DATA
            EXIT PARAGRAPH
        END-IF
        MOVE FUNCTION TRIM(INPUT-BUFFER) TO P-GRAD(PROFILE-IDX)

        IF FUNCTION LENGTH(FUNCTION TRIM(P-GRAD(PROFILE-IDX))) NOT = 4
            MOVE "Invalid graduation year length." TO MESSAGE-BUFFER
            PERFORM 700-DISPLAY-MESSAGE
        ELSE
            COMPUTE YEAR-NUM = FUNCTION NUMVAL(P-GRAD(PROFILE-IDX))
            IF YEAR-NUM < 1900 OR YEAR-NUM > 2100
                MOVE "Invalid graduation year range." TO MESSAGE-BUFFER
                PERFORM 700-DISPLAY-MESSAGE
            ELSE
                MOVE 'Y' TO GRAD-YEAR-IS-VALID
            END-IF
        END-IF
    END-PERFORM.

    *> About Me (Optional)
    MOVE "Enter About Me (optional, max 200 chars, enter blank line to skip):" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    PERFORM 600-GET-USER-INPUT
    IF NO-MORE-DATA EXIT PARAGRAPH END-IF
    IF FUNCTION TRIM(INPUT-BUFFER) = SPACES
        CONTINUE
    ELSE
        MOVE FUNCTION TRIM(INPUT-BUFFER) TO P-ABOUT(PROFILE-IDX)
    END-IF

    *> Experience (Optional, up to 3)
    MOVE 0 TO P-EXP-COUNT(PROFILE-IDX)
    PERFORM VARYING SUBI FROM 1 BY 1 UNTIL SUBI > 3
        MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM 600-GET-USER-INPUT
        IF NO-MORE-DATA EXIT PERFORM END-IF
        MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-BUFFER)) TO NORMALIZED-INPUT
        IF NORMALIZED-INPUT = "DONE"
            EXIT PERFORM
        END-IF
        ADD 1 TO P-EXP-COUNT(PROFILE-IDX)
        MOVE "Experience #" TO MESSAGE-BUFFER
        STRING "Experience #" DELIMITED BY SIZE
               FUNCTION TRIM(FUNCTION NUMVAL-C(SUBI)) DELIMITED BY SIZE
               " - Title:" DELIMITED BY SIZE INTO MESSAGE-BUFFER
        END-STRING
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM 600-GET-USER-INPUT
        IF NO-MORE-DATA EXIT PERFORM END-IF
        MOVE FUNCTION TRIM(INPUT-BUFFER) TO P-EXP-TITLE(PROFILE-IDX SUBI)

        MOVE "Experience #" TO MESSAGE-BUFFER
        STRING "Experience #" DELIMITED BY SIZE
               FUNCTION TRIM(FUNCTION NUMVAL-C(SUBI)) DELIMITED BY SIZE
               " - Company/Organization:" DELIMITED BY SIZE INTO MESSAGE-BUFFER
        END-STRING
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM 600-GET-USER-INPUT
        IF NO-MORE-DATA EXIT PERFORM END-IF
        MOVE FUNCTION TRIM(INPUT-BUFFER) TO P-EXP-COMP(PROFILE-IDX SUBI)

        MOVE "Experience #" TO MESSAGE-BUFFER
        STRING "Experience #" DELIMITED BY SIZE
               FUNCTION TRIM(FUNCTION NUMVAL-C(SUBI)) DELIMITED BY SIZE
               " - Dates (e.g., Summer 2024):" DELIMITED BY SIZE INTO MESSAGE-BUFFER
        END-STRING
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM 600-GET-USER-INPUT
        IF NO-MORE-DATA EXIT PERFORM END-IF
        MOVE FUNCTION TRIM(INPUT-BUFFER) TO P-EXP-DATES(PROFILE-IDX SUBI)

        MOVE "Experience #" TO MESSAGE-BUFFER
        STRING "Experience #" DELIMITED BY SIZE
               FUNCTION TRIM(FUNCTION NUMVAL-C(SUBI)) DELIMITED BY SIZE
               " - Description (optional, max 100 chars, blank to skip):" DELIMITED BY SIZE INTO MESSAGE-BUFFER
        END-STRING
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM 600-GET-USER-INPUT
        IF NO-MORE-DATA EXIT PERFORM END-IF
        IF FUNCTION TRIM(INPUT-BUFFER) NOT = SPACES
            MOVE FUNCTION TRIM(INPUT-BUFFER) TO P-EXP-DESC(PROFILE-IDX SUBI)
        END-IF
    END-PERFORM

    *> Education (Optional, up to 3)
    MOVE 0 TO P-EDU-COUNT(PROFILE-IDX)
    PERFORM VARYING SUBI FROM 1 BY 1 UNTIL SUBI > 3
        MOVE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM 600-GET-USER-INPUT
        IF NO-MORE-DATA EXIT PERFORM END-IF
        MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-BUFFER)) TO NORMALIZED-INPUT
        IF NORMALIZED-INPUT = "DONE"
            EXIT PERFORM
        END-IF
        ADD 1 TO P-EDU-COUNT(PROFILE-IDX)
        MOVE "Education #" TO MESSAGE-BUFFER
        STRING "Education #" DELIMITED BY SIZE
               FUNCTION TRIM(FUNCTION NUMVAL-C(SUBI)) DELIMITED BY SIZE
               " - Degree:" DELIMITED BY SIZE INTO MESSAGE-BUFFER
        END-STRING
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM 600-GET-USER-INPUT
        IF NO-MORE-DATA EXIT PERFORM END-IF
        MOVE FUNCTION TRIM(INPUT-BUFFER) TO P-EDU-DEG(PROFILE-IDX SUBI)

        MOVE "Education #" TO MESSAGE-BUFFER
        STRING "Education #" DELIMITED BY SIZE
               FUNCTION TRIM(FUNCTION NUMVAL-C(SUBI)) DELIMITED BY SIZE
               " - University/College:" DELIMITED BY SIZE INTO MESSAGE-BUFFER
        END-STRING
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM 600-GET-USER-INPUT
        IF NO-MORE-DATA EXIT PERFORM END-IF
        MOVE FUNCTION TRIM(INPUT-BUFFER) TO P-EDU-SCHOOL(PROFILE-IDX SUBI)

        MOVE "Education #" TO MESSAGE-BUFFER
        STRING "Education #" DELIMITED BY SIZE
               FUNCTION TRIM(FUNCTION NUMVAL-C(SUBI)) DELIMITED BY SIZE
               " - Years Attended (e.g., 2023-2025):" DELIMITED BY SIZE INTO MESSAGE-BUFFER
        END-STRING
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM 600-GET-USER-INPUT
        IF NO-MORE-DATA EXIT PERFORM END-IF
        MOVE FUNCTION TRIM(INPUT-BUFFER) TO P-EDU-YEARS(PROFILE-IDX SUBI)
    END-PERFORM

    MOVE "Profile saved successfully!" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    *> Save to disk immediately so it persists even if program ends early
    PERFORM 870-SAVE-PROFILES.

*> =====================
*> NEW: View profile
*> =====================
565-VIEW-MY-PROFILE.
    PERFORM 821-FIND-PROFILE-INDEX-ONLY
    IF PROFILE-IDX = 0
        MOVE "No profile found. Use 'Create/Edit My Profile' first." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        EXIT PARAGRAPH
    END-IF

    MOVE "--- Your Profile ---" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    MOVE SPACES TO MESSAGE-BUFFER
    STRING "Name: " DELIMITED BY SIZE
           FUNCTION TRIM(P-FIRST(PROFILE-IDX)) DELIMITED BY SIZE
           " " DELIMITED BY SIZE
           FUNCTION TRIM(P-LAST(PROFILE-IDX)) DELIMITED BY SIZE
      INTO MESSAGE-BUFFER
    END-STRING
    PERFORM 700-DISPLAY-MESSAGE

    MOVE SPACES TO MESSAGE-BUFFER
    STRING "University: " DELIMITED BY SIZE
           FUNCTION TRIM(P-UNIV(PROFILE-IDX)) DELIMITED BY SIZE
      INTO MESSAGE-BUFFER
    END-STRING
    PERFORM 700-DISPLAY-MESSAGE

    MOVE SPACES TO MESSAGE-BUFFER
    STRING "Major: " DELIMITED BY SIZE
           FUNCTION TRIM(P-MAJOR(PROFILE-IDX)) DELIMITED BY SIZE
      INTO MESSAGE-BUFFER
    END-STRING
    PERFORM 700-DISPLAY-MESSAGE

    MOVE SPACES TO MESSAGE-BUFFER
    STRING "Graduation Year: " DELIMITED BY SIZE
           FUNCTION TRIM(P-GRAD(PROFILE-IDX)) DELIMITED BY SIZE
      INTO MESSAGE-BUFFER
    END-STRING
    PERFORM 700-DISPLAY-MESSAGE

    IF FUNCTION TRIM(P-ABOUT(PROFILE-IDX)) NOT = SPACES
        MOVE SPACES TO MESSAGE-BUFFER
        STRING "About Me: " DELIMITED BY SIZE
               FUNCTION TRIM(P-ABOUT(PROFILE-IDX)) DELIMITED BY SIZE
          INTO MESSAGE-BUFFER
        END-STRING
        PERFORM 700-DISPLAY-MESSAGE
    END-IF

    IF P-EXP-COUNT(PROFILE-IDX) > 0
        MOVE "Experience:" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM VARYING SUBI FROM 1 BY 1 UNTIL SUBI > P-EXP-COUNT(PROFILE-IDX)
            MOVE SPACES TO MESSAGE-BUFFER
            STRING "Title: " DELIMITED BY SIZE
                   FUNCTION TRIM(P-EXP-TITLE(PROFILE-IDX SUBI)) DELIMITED BY SIZE
              INTO MESSAGE-BUFFER
            END-STRING
            PERFORM 700-DISPLAY-MESSAGE

            MOVE SPACES TO MESSAGE-BUFFER
            STRING "Company: " DELIMITED BY SIZE
                   FUNCTION TRIM(P-EXP-COMP(PROFILE-IDX SUBI)) DELIMITED BY SIZE
              INTO MESSAGE-BUFFER
            END-STRING
            PERFORM 700-DISPLAY-MESSAGE

            MOVE SPACES TO MESSAGE-BUFFER
            STRING "Dates: " DELIMITED BY SIZE
                   FUNCTION TRIM(P-EXP-DATES(PROFILE-IDX SUBI)) DELIMITED BY SIZE
              INTO MESSAGE-BUFFER
            END-STRING
            PERFORM 700-DISPLAY-MESSAGE

            IF FUNCTION TRIM(P-EXP-DESC(PROFILE-IDX SUBI)) NOT = SPACES
                MOVE SPACES TO MESSAGE-BUFFER
                STRING "Description: " DELIMITED BY SIZE
                       FUNCTION TRIM(P-EXP-DESC(PROFILE-IDX SUBI)) DELIMITED BY SIZE
                  INTO MESSAGE-BUFFER
                END-STRING
                PERFORM 700-DISPLAY-MESSAGE
            END-IF
        END-PERFORM
    END-IF

    IF P-EDU-COUNT(PROFILE-IDX) > 0
        MOVE "Education:" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM VARYING SUBI FROM 1 BY 1 UNTIL SUBI > P-EDU-COUNT(PROFILE-IDX)
            MOVE SPACES TO MESSAGE-BUFFER
            STRING "Degree: " DELIMITED BY SIZE
                   FUNCTION TRIM(P-EDU-DEG(PROFILE-IDX SUBI)) DELIMITED BY SIZE
              INTO MESSAGE-BUFFER
            END-STRING
            PERFORM 700-DISPLAY-MESSAGE

            MOVE SPACES TO MESSAGE-BUFFER
            STRING "University: " DELIMITED BY SIZE
                   FUNCTION TRIM(P-EDU-SCHOOL(PROFILE-IDX SUBI)) DELIMITED BY SIZE
              INTO MESSAGE-BUFFER
            END-STRING
            PERFORM 700-DISPLAY-MESSAGE

            MOVE SPACES TO MESSAGE-BUFFER
            STRING "Years: " DELIMITED BY SIZE
                   FUNCTION TRIM(P-EDU-YEARS(PROFILE-IDX SUBI)) DELIMITED BY SIZE
              INTO MESSAGE-BUFFER
            END-STRING
            PERFORM 700-DISPLAY-MESSAGE
        END-PERFORM
    END-IF

    MOVE "--------------------" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE.

600-GET-USER-INPUT.
    READ USER-INPUT
        AT END
            SET NO-MORE-DATA TO TRUE
        NOT AT END
            MOVE FUNCTION TRIM(INPUT-LINE) TO INPUT-BUFFER
    END-READ.

700-DISPLAY-MESSAGE.
    MOVE SPACES TO OUTPUT-LINE
    MOVE FUNCTION TRIM(MESSAGE-BUFFER) TO OUTPUT-LINE
    DISPLAY FUNCTION TRIM(MESSAGE-BUFFER)
    WRITE OUTPUT-LINE.


800-VERIFY-CREDENTIALS.
    MOVE 'N' TO CREDENTIALS-VALID
    PERFORM VARYING LOOP-INDEX FROM 1 BY 1
      UNTIL LOOP-INDEX > ACCOUNT-COUNT
        IF INPUT-USERNAME = FUNCTION TRIM(ACCT-USER(LOOP-INDEX))
           AND INPUT-PASSWORD = FUNCTION TRIM(ACCT-PASS(LOOP-INDEX))
           MOVE 'Y' TO CREDENTIALS-VALID
           EXIT PERFORM
        END-IF
    END-PERFORM.

810-VALIDATE-PASSWORD.
    MOVE 0 TO PASS-LENGTH
    MOVE 'N' TO CONTAINS-UPPERCASE
    MOVE 'N' TO CONTAINS-DIGIT
    MOVE 'N' TO CONTAINS-SPECIAL-CHAR

    COMPUTE PASS-LENGTH =
        FUNCTION LENGTH(FUNCTION TRIM(INPUT-PASSWORD))
    IF PASS-LENGTH < 8 OR PASS-LENGTH > 12
        MOVE 'N' TO PASSWORD-VALID-FLAG
        EXIT PARAGRAPH
    END-IF

    PERFORM VARYING LOOP-INDEX FROM 1 BY 1
      UNTIL LOOP-INDEX > PASS-LENGTH
        MOVE INPUT-PASSWORD(LOOP-INDEX:1) TO CURRENT-CHAR

        IF CURRENT-CHAR IS NUMERIC
            MOVE 'Y' TO CONTAINS-DIGIT
        END-IF
        IF CURRENT-CHAR IS ALPHABETIC AND
           CURRENT-CHAR = FUNCTION UPPER-CASE(CURRENT-CHAR)
            MOVE 'Y' TO CONTAINS-UPPERCASE
        END-IF
        IF CURRENT-CHAR NOT ALPHABETIC AND
           CURRENT-CHAR NOT NUMERIC AND
           CURRENT-CHAR NOT = SPACE
            MOVE 'Y' TO CONTAINS-SPECIAL-CHAR
        END-IF
    END-PERFORM

    IF CONTAINS-UPPERCASE = 'Y' AND
       CONTAINS-DIGIT = 'Y' AND
       CONTAINS-SPECIAL-CHAR = 'Y'
        MOVE 'Y' TO PASSWORD-VALID-FLAG
    ELSE
        MOVE 'N' TO PASSWORD-VALID-FLAG
    END-IF.

*> =====================
*> NEW: Profile index helpers
*> =====================
820-FIND-OR-CREATE-PROFILE-INDEX.
    MOVE 0 TO PROFILE-IDX
    PERFORM VARYING LOOP-INDEX FROM 1 BY 1 UNTIL LOOP-INDEX > PROFILE-COUNT
        IF CURRENT-USER = FUNCTION TRIM(P-USER(LOOP-INDEX))
            MOVE LOOP-INDEX TO PROFILE-IDX
            EXIT PERFORM
        END-IF
    END-PERFORM
    IF PROFILE-IDX = 0
        IF PROFILE-COUNT < MAX-PROFILES
            ADD 1 TO PROFILE-COUNT
            MOVE PROFILE-COUNT TO PROFILE-IDX
            MOVE FUNCTION TRIM(CURRENT-USER) TO P-USER(PROFILE-IDX)
            *> Reset counts on new profile
            MOVE 0 TO P-EXP-COUNT(PROFILE-IDX)
            MOVE 0 TO P-EDU-COUNT(PROFILE-IDX)
        ELSE
            MOVE 0 TO PROFILE-IDX
        END-IF
    END-IF.

821-FIND-PROFILE-INDEX-ONLY.
    MOVE 0 TO PROFILE-IDX
    PERFORM VARYING LOOP-INDEX FROM 1 BY 1 UNTIL LOOP-INDEX > PROFILE-COUNT
        IF CURRENT-USER = FUNCTION TRIM(P-USER(LOOP-INDEX))
            MOVE LOOP-INDEX TO PROFILE-IDX
            EXIT PERFORM
        END-IF
    END-PERFORM.

*> =====================
*> NEW: Persistence (load/save)
*> =====================
860-LOAD-PROFILES.
    OPEN INPUT USER-PROFILES

    IF PROFILE-FILE-STATUS = "00"
        PERFORM FOREVER
            READ USER-PROFILES AT END EXIT PERFORM END-READ
            IF FUNCTION TRIM(PROFILE-REC) NOT = SPACES
                IF PROFILE-COUNT < MAX-PROFILES
                    ADD 1 TO PROFILE-COUNT
                    PERFORM 830-DESERIALIZE-PROFILE
                END-IF
            END-IF
        END-PERFORM
    ELSE
        IF PROFILE-FILE-STATUS NOT = "35"
            DISPLAY "Error opening Profiles.dat for input: "
                    PROFILE-FILE-STATUS
        END-IF
    END-IF

    CLOSE USER-PROFILES.

870-SAVE-PROFILES.
    OPEN OUTPUT USER-PROFILES

    IF PROFILE-FILE-STATUS NOT = "00"
        DISPLAY "Error opening Profiles.dat for output: "
                PROFILE-FILE-STATUS
        GOBACK
    END-IF

    PERFORM VARYING LOOP-INDEX FROM 1 BY 1
        UNTIL PROFILE-COUNT = 0 OR LOOP-INDEX > PROFILE-COUNT
            PERFORM 835-SERIALIZE-PROFILE
            WRITE PROFILE-REC FROM SER-LINE
    END-PERFORM

    CLOSE USER-PROFILES.
900-TERMINATE-PROGRAM.
    OPEN OUTPUT USER-ACCOUNTS.
    PERFORM VARYING LOOP-INDEX FROM 1 BY 1
      UNTIL LOOP-INDEX > ACCOUNT-COUNT
        MOVE SPACES TO ACCOUNT-LINE-OUT
        STRING FUNCTION TRIM(ACCT-USER(LOOP-INDEX)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(ACCT-PASS(LOOP-INDEX)) DELIMITED BY SIZE
            INTO ACCOUNT-LINE-OUT
        END-STRING
        WRITE ACCOUNT-LINE-OUT
    END-PERFORM.
    CLOSE USER-ACCOUNTS.

    PERFORM 870-SAVE-PROFILES.
    PERFORM 960-SAVE-CONNECTIONS.
    PERFORM 970-SAVE-PERMANENT-CONNECTIONS.
    PERFORM 945-SAVE-APPLICATIONS.

    MOVE "--- END_OF_PROGRAM_EXECUTION ---" TO MESSAGE-BUFFER.
    PERFORM 700-DISPLAY-MESSAGE.

    CLOSE PROGRAM-OUTPUT.
    CLOSE USER-INPUT.
830-DESERIALIZE-PROFILE.
    *> PROFILE-REC: user|first|last|univ|major|grad|about|expCount|eduCount|expBlock|eduBlock
    MOVE SPACES TO TOK-USER TOK-FIRST TOK-LAST TOK-UNIV TOK-MAJOR TOK-GRAD TOK-ABOUT
    MOVE SPACES TO EXP-BLOCK EDU-BLOCK
    MOVE 0 TO TOK-EXPCNT TOK-EDUCNT

    UNSTRING PROFILE-REC DELIMITED BY ALL "|"
        INTO TOK-USER
             TOK-FIRST
             TOK-LAST
             TOK-UNIV
             TOK-MAJOR
             TOK-GRAD
             TOK-ABOUT
             TOK-EXPCNT
             TOK-EDUCNT
             EXP-BLOCK
             EDU-BLOCK
    END-UNSTRING

    MOVE FUNCTION TRIM(TOK-USER)  TO P-USER(PROFILE-COUNT)
    MOVE FUNCTION TRIM(TOK-FIRST) TO P-FIRST(PROFILE-COUNT)
    MOVE FUNCTION TRIM(TOK-LAST)  TO P-LAST(PROFILE-COUNT)
    MOVE FUNCTION TRIM(TOK-UNIV)  TO P-UNIV(PROFILE-COUNT)
    MOVE FUNCTION TRIM(TOK-MAJOR) TO P-MAJOR(PROFILE-COUNT)
    MOVE FUNCTION TRIM(TOK-GRAD)  TO P-GRAD(PROFILE-COUNT)
    MOVE FUNCTION TRIM(TOK-ABOUT) TO P-ABOUT(PROFILE-COUNT)

    MOVE TOK-EXPCNT TO P-EXP-COUNT(PROFILE-COUNT)
    MOVE TOK-EDUCNT TO P-EDU-COUNT(PROFILE-COUNT)

    MOVE SPACES TO EXP1 EXP2 EXP3
    UNSTRING EXP-BLOCK DELIMITED BY "^"
        INTO EXP1 EXP2 EXP3
    END-UNSTRING

    PERFORM VARYING SUBI FROM 1 BY 1 UNTIL SUBI > P-EXP-COUNT(PROFILE-COUNT)
        EVALUATE SUBI
           WHEN 1 MOVE EXP1 TO SER-LINE
           WHEN 2 MOVE EXP2 TO SER-LINE
           WHEN 3 MOVE EXP3 TO SER-LINE
        END-EVALUATE
        MOVE SPACES TO TOK-FIRST TOK-LAST TOK-UNIV TOK-MAJOR
        UNSTRING SER-LINE DELIMITED BY "~"
            INTO P-EXP-TITLE(PROFILE-COUNT SUBI)
                 P-EXP-COMP(PROFILE-COUNT SUBI)
                 P-EXP-DATES(PROFILE-COUNT SUBI)
                 P-EXP-DESC(PROFILE-COUNT SUBI)
        END-UNSTRING
    END-PERFORM

    MOVE SPACES TO EDU1 EDU2 EDU3
    UNSTRING EDU-BLOCK DELIMITED BY "^"
        INTO EDU1 EDU2 EDU3
    END-UNSTRING

    PERFORM VARYING SUBI FROM 1 BY 1 UNTIL SUBI > P-EDU-COUNT(PROFILE-COUNT)
        EVALUATE SUBI
           WHEN 1 MOVE EDU1 TO SER-LINE
           WHEN 2 MOVE EDU2 TO SER-LINE
           WHEN 3 MOVE EDU3 TO SER-LINE
        END-EVALUATE
        UNSTRING SER-LINE DELIMITED BY "~"
            INTO P-EDU-DEG(PROFILE-COUNT SUBI)
                 P-EDU-SCHOOL(PROFILE-COUNT SUBI)
                 P-EDU-YEARS(PROFILE-COUNT SUBI)
        END-UNSTRING
    END-PERFORM.

835-SERIALIZE-PROFILE.
    MOVE SPACES TO SER-LINE

    *> Build EXP-BLOCK safely using explicit pointers
    MOVE SPACES TO EXP-BLOCK
    MOVE 1 TO EXP-PTR
    IF P-EXP-COUNT(LOOP-INDEX) > 0
        PERFORM VARYING SUBI FROM 1 BY 1 UNTIL SUBI > P-EXP-COUNT(LOOP-INDEX)
            IF SUBI > 1
                STRING "^" DELIMITED BY SIZE
                  INTO EXP-BLOCK WITH POINTER EXP-PTR
                END-STRING
            END-IF
            STRING FUNCTION TRIM(P-EXP-TITLE(LOOP-INDEX SUBI)) DELIMITED BY SIZE
                   "~" DELIMITED BY SIZE
                   FUNCTION TRIM(P-EXP-COMP(LOOP-INDEX SUBI))  DELIMITED BY SIZE
                   "~" DELIMITED BY SIZE
                   FUNCTION TRIM(P-EXP-DATES(LOOP-INDEX SUBI)) DELIMITED BY SIZE
                   "~" DELIMITED BY SIZE
                   FUNCTION TRIM(P-EXP-DESC(LOOP-INDEX SUBI))  DELIMITED BY SIZE
              INTO EXP-BLOCK WITH POINTER EXP-PTR
            END-STRING
        END-PERFORM
    END-IF

    *> Build EDU-BLOCK safely using explicit pointers
    MOVE SPACES TO EDU-BLOCK
    MOVE 1 TO EDU-PTR
    IF P-EDU-COUNT(LOOP-INDEX) > 0
        PERFORM VARYING SUBI FROM 1 BY 1 UNTIL SUBI > P-EDU-COUNT(LOOP-INDEX)
            IF SUBI > 1
                STRING "^" DELIMITED BY SIZE
                  INTO EDU-BLOCK WITH POINTER EDU-PTR
                END-STRING
            END-IF
            STRING FUNCTION TRIM(P-EDU-DEG(LOOP-INDEX SUBI))    DELIMITED BY SIZE
                   "~" DELIMITED BY SIZE
                   FUNCTION TRIM(P-EDU-SCHOOL(LOOP-INDEX SUBI)) DELIMITED BY SIZE
                   "~" DELIMITED BY SIZE
                   FUNCTION TRIM(P-EDU-YEARS(LOOP-INDEX SUBI))  DELIMITED BY SIZE
              INTO EDU-BLOCK WITH POINTER EDU-PTR
            END-STRING
        END-PERFORM
    END-IF

    *> Assemble full line: user|first|last|univ|major|grad|about|expCnt|eduCnt|expBlock|eduBlock
    MOVE SPACES TO SER-LINE
    STRING FUNCTION TRIM(P-USER(LOOP-INDEX))  DELIMITED BY SIZE
           "|" DELIMITED BY SIZE
           FUNCTION TRIM(P-FIRST(LOOP-INDEX)) DELIMITED BY SIZE
           "|" DELIMITED BY SIZE
           FUNCTION TRIM(P-LAST(LOOP-INDEX))  DELIMITED BY SIZE
           "|" DELIMITED BY SIZE
           FUNCTION TRIM(P-UNIV(LOOP-INDEX))  DELIMITED BY SIZE
           "|" DELIMITED BY SIZE
           FUNCTION TRIM(P-MAJOR(LOOP-INDEX)) DELIMITED BY SIZE
           "|" DELIMITED BY SIZE
           FUNCTION TRIM(P-GRAD(LOOP-INDEX))  DELIMITED BY SIZE
           "|" DELIMITED BY SIZE
           FUNCTION TRIM(P-ABOUT(LOOP-INDEX)) DELIMITED BY SIZE
           "|" DELIMITED BY SIZE
           P-EXP-COUNT(LOOP-INDEX)            DELIMITED BY SIZE
           "|" DELIMITED BY SIZE
           P-EDU-COUNT(LOOP-INDEX)            DELIMITED BY SIZE
           "|" DELIMITED BY SIZE
           EXP-BLOCK                           DELIMITED BY SIZE
           "|" DELIMITED BY SIZE
           EDU-BLOCK                           DELIMITED BY SIZE
      INTO SER-LINE
    END-STRING.

*> =====================
*> 950 - LOAD CONNECTIONS
*> =====================
950-LOAD-CONNECTIONS.
    OPEN INPUT USER-CONNECTIONS

    IF CONNECTION-FILE-STATUS = "35"
        MOVE 0 TO CONNECTION-COUNT
        CLOSE USER-CONNECTIONS
        EXIT PARAGRAPH
    END-IF

    IF CONNECTION-FILE-STATUS NOT = "00"
        DISPLAY "ERROR: Unable to open Connections.dat (status " CONNECTION-FILE-STATUS ")"
        CLOSE USER-CONNECTIONS
        EXIT PARAGRAPH
    END-IF

    MOVE 0 TO CONNECTION-COUNT
    PERFORM FOREVER
        READ USER-CONNECTIONS
            AT END EXIT PERFORM
        END-READ
        IF FUNCTION TRIM(CONNECTION-REC) NOT = SPACES
            ADD 1 TO CONNECTION-COUNT
            UNSTRING CONNECTION-REC DELIMITED BY "|"
                INTO CONN-SENDER(CONNECTION-COUNT)
                     CONN-RECEIVER(CONNECTION-COUNT)
            END-UNSTRING
        END-IF
    END-PERFORM

    CLOSE USER-CONNECTIONS.


*> =====================
*> 960 - SAVE CONNECTIONS
*> =====================
960-SAVE-CONNECTIONS.
    OPEN OUTPUT USER-CONNECTIONS

    IF CONNECTION-FILE-STATUS NOT = "00"
        DISPLAY "ERROR: Could not open Connections.dat for OUTPUT (status "
                 CONNECTION-FILE-STATUS ")"
        EXIT PARAGRAPH
    END-IF

    PERFORM VARYING LOOP-INDEX FROM 1 BY 1
        UNTIL LOOP-INDEX > CONNECTION-COUNT
        MOVE SPACES TO CONNECTION-REC
        STRING FUNCTION TRIM(CONN-SENDER(LOOP-INDEX)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(CONN-RECEIVER(LOOP-INDEX)) DELIMITED BY SIZE
          INTO CONNECTION-REC
        END-STRING
        WRITE CONNECTION-REC
    END-PERFORM

    CLOSE USER-CONNECTIONS.

*> =====================
*> NEW: Save permanent connections
*> =====================
970-SAVE-PERMANENT-CONNECTIONS.
    OPEN OUTPUT PERMANENT-CONNECTIONS
    IF PERM-CONN-FILE-STATUS NOT = "00"
        DISPLAY "Error saving permanent connections: "
                PERM-CONN-FILE-STATUS
        EXIT PARAGRAPH
    END-IF

    PERFORM VARYING LOOP-INDEX FROM 1 BY 1
        UNTIL LOOP-INDEX > PERMANENT-COUNT
        MOVE SPACES TO PERM-CONNECTION-REC
        STRING FUNCTION TRIM(PERM-USER1(LOOP-INDEX)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(PERM-USER2(LOOP-INDEX)) DELIMITED BY SIZE
          INTO PERM-CONNECTION-REC
        END-STRING
        WRITE PERM-CONNECTION-REC
    END-PERFORM

    CLOSE PERMANENT-CONNECTIONS.

*> =====================
*> NEW: Load permanent connections at startup
*> =====================
975-LOAD-PERMANENT-CONNECTIONS.
    OPEN INPUT PERMANENT-CONNECTIONS
    IF PERM-CONN-FILE-STATUS = "35"
        *> File doesn't exist yet, that's OK
        CLOSE PERMANENT-CONNECTIONS
        EXIT PARAGRAPH
    END-IF

    IF PERM-CONN-FILE-STATUS NOT = "00"
        DISPLAY "Error loading permanent connections: "
                PERM-CONN-FILE-STATUS
        CLOSE PERMANENT-CONNECTIONS
        EXIT PARAGRAPH
    END-IF

    MOVE 0 TO PERMANENT-COUNT
    PERFORM FOREVER
        READ PERMANENT-CONNECTIONS
            AT END EXIT PERFORM
        END-READ
        IF FUNCTION TRIM(PERM-CONNECTION-REC) NOT = SPACES
            ADD 1 TO PERMANENT-COUNT
            UNSTRING PERM-CONNECTION-REC DELIMITED BY "|"
                INTO PERM-USER1(PERMANENT-COUNT)
                     PERM-USER2(PERMANENT-COUNT)
            END-UNSTRING
        END-IF
    END-PERFORM

    CLOSE PERMANENT-CONNECTIONS.

*> =====================
*> UPDATED: View My Network functionality to match sample format
*> =====================
*> =====================
*> UPDATED: View My Network functionality to match sample format
*> =====================
580-VIEW-MY-NETWORK.
    MOVE "--- Your Network ---" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    MOVE 0 TO LOOP-INDEX
    MOVE 0 TO SUBI

    *> Count connections for current user
    PERFORM VARYING LOOP-INDEX FROM 1 BY 1
      UNTIL LOOP-INDEX > PERMANENT-COUNT
        IF FUNCTION UPPER-CASE(FUNCTION TRIM(PERM-USER1(LOOP-INDEX))) =
           FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER))
            ADD 1 TO SUBI
        END-IF
    END-PERFORM

    IF SUBI = 0
        MOVE "You have no connections in your network yet."
          TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        EXIT PARAGRAPH
    END-IF

    *> Display all connections in the exact sample format
    PERFORM VARYING LOOP-INDEX FROM 1 BY 1
      UNTIL LOOP-INDEX > PERMANENT-COUNT
        IF FUNCTION UPPER-CASE(FUNCTION TRIM(PERM-USER1(LOOP-INDEX))) =
           FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER))
            *> Find profile info for this connection
            MOVE 0 TO PROFILE-IDX
            PERFORM VARYING SUBI FROM 1 BY 1 UNTIL SUBI > PROFILE-COUNT
                IF FUNCTION TRIM(PERM-USER2(LOOP-INDEX)) =
                   FUNCTION TRIM(P-USER(SUBI))
                    MOVE SUBI TO PROFILE-IDX
                    EXIT PERFORM
                END-IF
            END-PERFORM

            *> Display in exact sample format: "Connected with: Username (University: X, Major: Y)"
            MOVE SPACES TO MESSAGE-BUFFER
            IF PROFILE-IDX > 0
                *> If profile exists, show username, university and major
                STRING "Connected with: " DELIMITED BY SIZE
                       FUNCTION TRIM(PERM-USER2(LOOP-INDEX)) DELIMITED BY SIZE
                       " (University: " DELIMITED BY SIZE
                       FUNCTION TRIM(P-UNIV(PROFILE-IDX)) DELIMITED BY SIZE
                       ", Major: " DELIMITED BY SIZE
                       FUNCTION TRIM(P-MAJOR(PROFILE-IDX)) DELIMITED BY SIZE
                       ")" DELIMITED BY SIZE
                  INTO MESSAGE-BUFFER
                END-STRING
            ELSE
                *> If no profile, just show username
                STRING "Connected with: " DELIMITED BY SIZE
                       FUNCTION TRIM(PERM-USER2(LOOP-INDEX)) DELIMITED BY SIZE
                  INTO MESSAGE-BUFFER
                END-STRING
            END-IF

            PERFORM 700-DISPLAY-MESSAGE
        END-IF
    END-PERFORM

    MOVE "--------------------" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE.

*> =====================
*> JOB MENU (Week 7 additions integrated)
*> =====================
930-JOB-SEARCH-MENU.
    MOVE "--- Job Search/Internship Menu ---" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    MOVE "1. Post a Job/Internship" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    MOVE "2. Browse Jobs/Internships" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    MOVE "3. View My Applications" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    MOVE "4. Back to Main Menu" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    MOVE "Enter your choice:" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    PERFORM 600-GET-USER-INPUT
    IF NO-MORE-DATA EXIT PARAGRAPH END-IF
    MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-BUFFER)) TO NORMALIZED-INPUT

    EVALUATE TRUE
        WHEN NORMALIZED-INPUT = "1" OR NORMALIZED-INPUT = "POST"
            PERFORM 931-POST-JOB
        WHEN NORMALIZED-INPUT = "2" OR NORMALIZED-INPUT = "BROWSE"
            PERFORM 932-BROWSE-JOBS
        WHEN NORMALIZED-INPUT = "3" OR NORMALIZED-INPUT = "VIEW MY APPLICATIONS"
            PERFORM 937-VIEW-MY-APPLICATIONS
        WHEN NORMALIZED-INPUT = "4" OR NORMALIZED-INPUT = "BACK"
            EXIT PARAGRAPH
        WHEN OTHER
            MOVE "Invalid option." TO MESSAGE-BUFFER
            PERFORM 700-DISPLAY-MESSAGE
    END-EVALUATE.

931-POST-JOB.
    IF JOB-COUNT >= MAX-JOBS
        MOVE "Job limit reached. Cannot post more." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        EXIT PARAGRAPH
    END-IF

    ADD 1 TO JOB-COUNT
    MOVE JOB-COUNT TO J-ID(JOB-COUNT)
    MOVE FUNCTION TRIM(CURRENT-USER) TO J-POSTER(JOB-COUNT)

    MOVE "--- Post a New Job/Internship ---" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    MOVE "Enter Job Title:" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    PERFORM 600-GET-USER-INPUT
    MOVE FUNCTION TRIM(INPUT-BUFFER) TO J-TITLE(JOB-COUNT)

    MOVE "Enter Description (max 200 chars):" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    PERFORM 600-GET-USER-INPUT
    MOVE FUNCTION TRIM(INPUT-BUFFER) TO J-DESC(JOB-COUNT)

    MOVE "Enter Employer Name:" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    PERFORM 600-GET-USER-INPUT
    MOVE FUNCTION TRIM(INPUT-BUFFER) TO J-EMPLOYER(JOB-COUNT)

    MOVE "Enter Location:" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    PERFORM 600-GET-USER-INPUT
    MOVE FUNCTION TRIM(INPUT-BUFFER) TO J-LOCATION(JOB-COUNT)

    MOVE "Enter Salary (optional, enter 'NONE' to skip):" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    PERFORM 600-GET-USER-INPUT
    IF FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-BUFFER)) = "NONE"
        MOVE SPACES TO J-SALARY(JOB-COUNT)
    ELSE
        MOVE FUNCTION TRIM(INPUT-BUFFER) TO J-SALARY(JOB-COUNT)
    END-IF

    *> =====================
    *> VALIDATION: Check required fields and description length
    *> =====================
    IF FUNCTION TRIM(J-TITLE(JOB-COUNT)) = SPACES
        MOVE "Error: Job Title is required." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        SUBTRACT 1 FROM JOB-COUNT
        MOVE "----------------------------------" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM 930-JOB-SEARCH-MENU
        EXIT PARAGRAPH
    END-IF

    IF FUNCTION TRIM(J-DESC(JOB-COUNT)) = SPACES
        MOVE "Error: Description is required." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        SUBTRACT 1 FROM JOB-COUNT
        MOVE "----------------------------------" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM 930-JOB-SEARCH-MENU
        EXIT PARAGRAPH
    END-IF

    IF FUNCTION LENGTH(FUNCTION TRIM(J-DESC(JOB-COUNT))) > 200
        MOVE "Error: Description exceeds 200 characters." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        SUBTRACT 1 FROM JOB-COUNT
        MOVE "----------------------------------" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM 930-JOB-SEARCH-MENU
        EXIT PARAGRAPH
    END-IF

    IF FUNCTION TRIM(J-EMPLOYER(JOB-COUNT)) = SPACES
        MOVE "Error: Employer Name is required." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        SUBTRACT 1 FROM JOB-COUNT
        MOVE "----------------------------------" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM 930-JOB-SEARCH-MENU
        EXIT PARAGRAPH
    END-IF

    IF FUNCTION TRIM(J-LOCATION(JOB-COUNT)) = SPACES
        MOVE "Error: Location is required." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        SUBTRACT 1 FROM JOB-COUNT
        MOVE "----------------------------------" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        PERFORM 930-JOB-SEARCH-MENU
        EXIT PARAGRAPH
    END-IF

    *> All validations passed, save the job
    PERFORM 933-SAVE-JOBS

    MOVE "Job posted successfully!" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    MOVE "----------------------------------" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    *> Return to the Job Menu so user can pick again
    PERFORM 930-JOB-SEARCH-MENU.

*> =====================
*> 932 - Browse Jobs/Internships (Implemented)
*> =====================
932-BROWSE-JOBS.
    IF JOB-COUNT = 0
        MOVE "--- Available Job Listings ---" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        MOVE "(No jobs posted yet.)" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        MOVE "-----------------------------" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        EXIT PARAGRAPH
    END-IF

    PERFORM UNTIL NO-MORE-DATA
        MOVE "--- Available Job Listings ---" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE

        PERFORM VARYING LOOP-INDEX FROM 1 BY 1 UNTIL LOOP-INDEX > JOB-COUNT
            MOVE SPACES TO MESSAGE-BUFFER
            STRING LOOP-INDEX DELIMITED BY SIZE
                   ". " DELIMITED BY SIZE
                   FUNCTION TRIM(J-TITLE(LOOP-INDEX)) DELIMITED BY SIZE
                   " at " DELIMITED BY SIZE
                   FUNCTION TRIM(J-EMPLOYER(LOOP-INDEX)) DELIMITED BY SIZE
                   " (" DELIMITED BY SIZE
                   FUNCTION TRIM(J-LOCATION(LOOP-INDEX)) DELIMITED BY SIZE
                   ")" DELIMITED BY SIZE
              INTO MESSAGE-BUFFER
            END-STRING
            PERFORM 700-DISPLAY-MESSAGE
        END-PERFORM

        MOVE "-----------------------------" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        MOVE "Enter job number to view details, or 0 to go back:" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE

        PERFORM 600-GET-USER-INPUT
        IF NO-MORE-DATA EXIT PARAGRAPH END-IF

        MOVE FUNCTION TRIM(INPUT-BUFFER) TO MENU-CHOICE
        IF FUNCTION TRIM(MENU-CHOICE) = "0"
            EXIT PARAGRAPH
        END-IF

        *> Validate numeric and range
        MOVE 'Y' TO PASSWORD-VALID-FLAG
        IF FUNCTION TRIM(MENU-CHOICE) IS NUMERIC
            MOVE FUNCTION NUMVAL(FUNCTION TRIM(MENU-CHOICE)) TO SELECTED-JOB-ID
            IF SELECTED-JOB-ID < 1 OR SELECTED-JOB-ID > JOB-COUNT
                MOVE "Invalid job number." TO MESSAGE-BUFFER
                PERFORM 700-DISPLAY-MESSAGE
                MOVE 'N' TO PASSWORD-VALID-FLAG
            END-IF
        ELSE
            MOVE "Invalid selection." TO MESSAGE-BUFFER
            PERFORM 700-DISPLAY-MESSAGE
            MOVE 'N' TO PASSWORD-VALID-FLAG
        END-IF

        IF PASSWORD-VALID-FLAG = 'Y'
            PERFORM 935-SHOW-JOB-DETAILS
        END-IF
    END-PERFORM.

935-SHOW-JOB-DETAILS.
    MOVE "--- Job Details ---" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    MOVE SPACES TO MESSAGE-BUFFER
    STRING "Title: " DELIMITED BY SIZE
           FUNCTION TRIM(J-TITLE(SELECTED-JOB-ID)) DELIMITED BY SIZE
      INTO MESSAGE-BUFFER
    END-STRING
    PERFORM 700-DISPLAY-MESSAGE

    MOVE SPACES TO MESSAGE-BUFFER
    STRING "Description: " DELIMITED BY SIZE
           FUNCTION TRIM(J-DESC(SELECTED-JOB-ID)) DELIMITED BY SIZE
      INTO MESSAGE-BUFFER
    END-STRING
    PERFORM 700-DISPLAY-MESSAGE

    MOVE SPACES TO MESSAGE-BUFFER
    STRING "Employer: " DELIMITED BY SIZE
           FUNCTION TRIM(J-EMPLOYER(SELECTED-JOB-ID)) DELIMITED BY SIZE
      INTO MESSAGE-BUFFER
    END-STRING
    PERFORM 700-DISPLAY-MESSAGE

    MOVE SPACES TO MESSAGE-BUFFER
    STRING "Location: " DELIMITED BY SIZE
           FUNCTION TRIM(J-LOCATION(SELECTED-JOB-ID)) DELIMITED BY SIZE
      INTO MESSAGE-BUFFER
    END-STRING
    PERFORM 700-DISPLAY-MESSAGE

    IF FUNCTION TRIM(J-SALARY(SELECTED-JOB-ID)) NOT = SPACES
        MOVE SPACES TO MESSAGE-BUFFER
        STRING "Salary: " DELIMITED BY SIZE
               FUNCTION TRIM(J-SALARY(SELECTED-JOB-ID)) DELIMITED BY SIZE
          INTO MESSAGE-BUFFER
        END-STRING
        PERFORM 700-DISPLAY-MESSAGE
    END-IF

    MOVE "-------------------" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    MOVE "1. Apply for this Job" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    MOVE "2. Back to Job List" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    MOVE "Enter your choice:" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    PERFORM 600-GET-USER-INPUT
    IF NO-MORE-DATA EXIT PARAGRAPH END-IF
    MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-BUFFER)) TO NORMALIZED-INPUT

    EVALUATE TRUE
        WHEN NORMALIZED-INPUT = "1" OR NORMALIZED-INPUT = "APPLY" OR NORMALIZED-INPUT = "APPLY FOR THIS JOB"
            PERFORM 936-APPLY-TO-JOB
        WHEN NORMALIZED-INPUT = "2" OR NORMALIZED-INPUT = "BACK"
            CONTINUE
        WHEN OTHER
            MOVE "Invalid option." TO MESSAGE-BUFFER
            PERFORM 700-DISPLAY-MESSAGE
    END-EVALUATE.

936-APPLY-TO-JOB.
    MOVE 'N' TO HAS-APPLIED-FLAG
    PERFORM 938-ALREADY-APPLIED
    IF HAS-APPLIED-FLAG = 'Y'
        MOVE "You have already applied to this job." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        EXIT PARAGRAPH
    END-IF

    IF APPLICATION-COUNT >= MAX-APPLICATIONS
        MOVE "Application limit reached. Cannot record more applications." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        EXIT PARAGRAPH
    END-IF

    ADD 1 TO APPLICATION-COUNT
    MOVE APPLICATION-COUNT TO APP-ID(APPLICATION-COUNT)
    MOVE FUNCTION TRIM(CURRENT-USER) TO APP-USER(APPLICATION-COUNT)
    MOVE SELECTED-JOB-ID TO APP-JOBID(APPLICATION-COUNT)

    PERFORM 945-SAVE-APPLICATIONS

    MOVE SPACES TO MESSAGE-BUFFER
    STRING "Your application for " DELIMITED BY SIZE
           FUNCTION TRIM(J-TITLE(SELECTED-JOB-ID)) DELIMITED BY SIZE
           " at " DELIMITED BY SIZE
           FUNCTION TRIM(J-EMPLOYER(SELECTED-JOB-ID)) DELIMITED BY SIZE
           " has been submitted." DELIMITED BY SIZE
      INTO MESSAGE-BUFFER
    END-STRING
    PERFORM 700-DISPLAY-MESSAGE.

938-ALREADY-APPLIED.
    *> Sets HAS-APPLIED-FLAG = 'Y' if CURRENT-USER already applied to SELECTED-JOB-ID
    PERFORM VARYING LOOP-INDEX FROM 1 BY 1
      UNTIL LOOP-INDEX > APPLICATION-COUNT
        IF FUNCTION UPPER-CASE(FUNCTION TRIM(APP-USER(LOOP-INDEX))) =
           FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER))
           AND APP-JOBID(LOOP-INDEX) = SELECTED-JOB-ID
            MOVE 'Y' TO HAS-APPLIED-FLAG
            EXIT PERFORM
        END-IF
    END-PERFORM.

*> =====================
*> REPORT: View My Applications
*> =====================
937-VIEW-MY-APPLICATIONS.
    MOVE "--- Your Job Applications ---" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    MOVE SPACES TO MESSAGE-BUFFER
    STRING "Application Summary for " DELIMITED BY SIZE
           FUNCTION TRIM(CURRENT-USER) DELIMITED BY SIZE
      INTO MESSAGE-BUFFER
    END-STRING
    PERFORM 700-DISPLAY-MESSAGE

    MOVE "------------------------------" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    MOVE 0 TO SUBI
    PERFORM VARYING LOOP-INDEX FROM 1 BY 1 UNTIL LOOP-INDEX > APPLICATION-COUNT
        IF FUNCTION UPPER-CASE(FUNCTION TRIM(APP-USER(LOOP-INDEX))) =
           FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER))
            ADD 1 TO SUBI

            *> Defensive: ensure job id is still valid
            IF APP-JOBID(LOOP-INDEX) >= 1 AND APP-JOBID(LOOP-INDEX) <= JOB-COUNT
                MOVE SPACES TO MESSAGE-BUFFER
                STRING "Job Title: " DELIMITED BY SIZE
                       FUNCTION TRIM(J-TITLE(APP-JOBID(LOOP-INDEX))) DELIMITED BY SIZE
                  INTO MESSAGE-BUFFER
                END-STRING
                PERFORM 700-DISPLAY-MESSAGE

                MOVE SPACES TO MESSAGE-BUFFER
                STRING "Employer: " DELIMITED BY SIZE
                       FUNCTION TRIM(J-EMPLOYER(APP-JOBID(LOOP-INDEX))) DELIMITED BY SIZE
                  INTO MESSAGE-BUFFER
                END-STRING
                PERFORM 700-DISPLAY-MESSAGE

                MOVE SPACES TO MESSAGE-BUFFER
                STRING "Location: " DELIMITED BY SIZE
                       FUNCTION TRIM(J-LOCATION(APP-JOBID(LOOP-INDEX))) DELIMITED BY SIZE
                  INTO MESSAGE-BUFFER
                END-STRING
                PERFORM 700-DISPLAY-MESSAGE

                MOVE "---" TO MESSAGE-BUFFER
                PERFORM 700-DISPLAY-MESSAGE
            END-IF
        END-IF
    END-PERFORM

    IF SUBI = 0
        MOVE "You have not applied to any jobs yet." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
    END-IF

    MOVE "------------------------------" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    MOVE SPACES TO MESSAGE-BUFFER
    STRING "Total Applications: " DELIMITED BY SIZE
           SUBI DELIMITED BY SIZE
      INTO MESSAGE-BUFFER
    END-STRING
    PERFORM 700-DISPLAY-MESSAGE

    MOVE "------------------------------" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE.

933-SAVE-JOBS.
    OPEN OUTPUT USER-JOBS
    PERFORM VARYING LOOP-INDEX FROM 1 BY 1 UNTIL LOOP-INDEX > JOB-COUNT
        MOVE SPACES TO JOB-REC
        STRING J-ID(LOOP-INDEX) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(J-TITLE(LOOP-INDEX)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(J-DESC(LOOP-INDEX)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(J-EMPLOYER(LOOP-INDEX)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(J-LOCATION(LOOP-INDEX)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(J-SALARY(LOOP-INDEX)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(J-POSTER(LOOP-INDEX)) DELIMITED BY SIZE
          INTO JOB-REC
        END-STRING
        WRITE JOB-REC
    END-PERFORM
    CLOSE USER-JOBS.

934-LOAD-JOBS.
    OPEN INPUT USER-JOBS
    IF JOB-FILE-STATUS = "35"
        MOVE 0 TO JOB-COUNT
        CLOSE USER-JOBS
        EXIT PARAGRAPH
    END-IF
    MOVE 0 TO JOB-COUNT
    PERFORM FOREVER
        READ USER-JOBS AT END EXIT PERFORM END-READ
        IF FUNCTION TRIM(JOB-REC) NOT = SPACES
            ADD 1 TO JOB-COUNT
            UNSTRING JOB-REC DELIMITED BY "|"
                INTO J-ID(JOB-COUNT)
                     J-TITLE(JOB-COUNT)
                     J-DESC(JOB-COUNT)
                     J-EMPLOYER(JOB-COUNT)
                     J-LOCATION(JOB-COUNT)
                     J-SALARY(JOB-COUNT)
                     J-POSTER(JOB-COUNT)
            END-UNSTRING
        END-IF
    END-PERFORM
    CLOSE USER-JOBS.

*> =====================
*> APPLICATIONS persistence
*> =====================
940-LOAD-APPLICATIONS.
    OPEN INPUT USER-APPLICATIONS
    IF APP-FILE-STATUS = "35"
        MOVE 0 TO APPLICATION-COUNT
        CLOSE USER-APPLICATIONS
        EXIT PARAGRAPH
    END-IF

    IF APP-FILE-STATUS NOT = "00"
        DISPLAY "ERROR: Unable to open Applications.dat (status "
                 APP-FILE-STATUS ")"
        CLOSE USER-APPLICATIONS
        EXIT PARAGRAPH
    END-IF

    MOVE 0 TO APPLICATION-COUNT
    PERFORM FOREVER
        READ USER-APPLICATIONS
            AT END EXIT PERFORM
        END-READ
        IF FUNCTION TRIM(APPLICATION-REC) NOT = SPACES
            IF APPLICATION-COUNT < MAX-APPLICATIONS
                ADD 1 TO APPLICATION-COUNT
                UNSTRING APPLICATION-REC DELIMITED BY "|"
                    INTO APP-ID(APPLICATION-COUNT)
                         APP-USER(APPLICATION-COUNT)
                         APP-JOBID(APPLICATION-COUNT)
                END-UNSTRING
            END-IF
        END-IF
    END-PERFORM

    CLOSE USER-APPLICATIONS.

945-SAVE-APPLICATIONS.
    OPEN OUTPUT USER-APPLICATIONS
    IF APP-FILE-STATUS NOT = "00"
        DISPLAY "ERROR: Could not open Applications.dat for OUTPUT (status "
                 APP-FILE-STATUS ")"
        EXIT PARAGRAPH
    END-IF

    PERFORM VARYING LOOP-INDEX FROM 1 BY 1 UNTIL LOOP-INDEX > APPLICATION-COUNT
        MOVE SPACES TO APPLICATION-REC
        STRING APP-ID(LOOP-INDEX) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(APP-USER(LOOP-INDEX)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               APP-JOBID(LOOP-INDEX) DELIMITED BY SIZE
          INTO APPLICATION-REC
        END-STRING
        WRITE APPLICATION-REC
    END-PERFORM

    CLOSE USER-APPLICATIONS.

*> =====================
*> NEW: Messages Menu (Week 8)
*> =====================
585-MESSAGES-MENU.
    PERFORM UNTIL NO-MORE-DATA
        MOVE "--- Messages Menu ---" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        MOVE "1. Send a New Message" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        MOVE "2. View My Messages" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        MOVE "3. Back to Main Menu" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        MOVE "Enter your choice:" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE

        PERFORM 600-GET-USER-INPUT
        IF NO-MORE-DATA
            EXIT PARAGRAPH
        END-IF
        MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-BUFFER))
          TO NORMALIZED-INPUT

        EVALUATE TRUE
            WHEN NORMALIZED-INPUT = "1"
              OR NORMALIZED-INPUT = "SEND A NEW MESSAGE"
                PERFORM 586-SEND-NEW-MESSAGE

            WHEN NORMALIZED-INPUT = "2"
              OR NORMALIZED-INPUT = "VIEW MY MESSAGES"
                PERFORM 587-VIEW-MY-MESSAGES

            WHEN NORMALIZED-INPUT = "3"
              OR NORMALIZED-INPUT = "BACK TO MAIN MENU"
              OR NORMALIZED-INPUT = "BACK"
                EXIT PARAGRAPH

            WHEN OTHER
                CONTINUE
        END-EVALUATE
    END-PERFORM.

*> =====================
*> NEW: Send a New Message (Week 8)
*> =====================
586-SEND-NEW-MESSAGE.
    MOVE "Enter recipient's username (must be a connection):" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    PERFORM 600-GET-USER-INPUT
    IF NO-MORE-DATA EXIT PARAGRAPH END-IF
    MOVE FUNCTION TRIM(INPUT-BUFFER) TO RECIPIENT-USERNAME

    *> Validate recipient exists and is a connection
    PERFORM 587-VALIDATE-RECIPIENT
    IF IS-CONNECTED-FLAG NOT = 'Y'
        EXIT PARAGRAPH
    END-IF

    *> Get message content
    MOVE "Enter your message (max 200 chars):" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE
    PERFORM 600-GET-USER-INPUT
    IF NO-MORE-DATA EXIT PARAGRAPH END-IF
    MOVE FUNCTION TRIM(INPUT-BUFFER) TO MESSAGE-CONTENT-INPUT

    *> Save the message
    IF MESSAGE-COUNT < MAX-MESSAGES
        ADD 1 TO MESSAGE-COUNT
        MOVE FUNCTION TRIM(CURRENT-USER) TO MSG-SENDER(MESSAGE-COUNT)
        MOVE FUNCTION TRIM(RECIPIENT-USERNAME) TO MSG-RECIPIENT(MESSAGE-COUNT)
        MOVE FUNCTION TRIM(MESSAGE-CONTENT-INPUT) TO MSG-CONTENT(MESSAGE-COUNT)
        MOVE SPACES TO MSG-TIMESTAMP(MESSAGE-COUNT)

        *> Save to file
        PERFORM 595-SAVE-MESSAGES

        MOVE SPACES TO MESSAGE-BUFFER
        STRING "Message sent to " DELIMITED BY SIZE
               FUNCTION TRIM(RECIPIENT-USERNAME) DELIMITED BY SIZE
               " successfully!" DELIMITED BY SIZE
          INTO MESSAGE-BUFFER
        END-STRING
        PERFORM 700-DISPLAY-MESSAGE
        MOVE "---------------------" TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
    ELSE
        MOVE "Cannot send message: message limit reached." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
    END-IF.

587-VIEW-MY-MESSAGES.
    MOVE 0 TO LOOP-INDEX
    MOVE 0 TO MESSAGES-FOUND
    MOVE "--- My Messages ---" TO MESSAGE-BUFFER
    PERFORM 700-DISPLAY-MESSAGE

    *> Count messages for current user
    PERFORM VARYING LOOP-INDEX FROM 1 BY 1 UNTIL LOOP-INDEX > MESSAGE-COUNT
        IF FUNCTION TRIM(MSG-RECIPIENT(LOOP-INDEX)) = FUNCTION TRIM(CURRENT-USER)
            ADD 1 TO MESSAGES-FOUND
        END-IF
    END-PERFORM

    *> Display messages if any found
    IF MESSAGES-FOUND > 0
        PERFORM VARYING LOOP-INDEX FROM 1 BY 1 UNTIL LOOP-INDEX > MESSAGE-COUNT
            IF FUNCTION TRIM(MSG-RECIPIENT(LOOP-INDEX)) = FUNCTION TRIM(CURRENT-USER)
                MOVE SPACES TO MESSAGE-BUFFER
                STRING "From: " DELIMITED BY SIZE
                       FUNCTION TRIM(MSG-SENDER(LOOP-INDEX)) DELIMITED BY SIZE
                  INTO MESSAGE-BUFFER
                END-STRING
                PERFORM 700-DISPLAY-MESSAGE

                MOVE SPACES TO MESSAGE-BUFFER
                STRING "Message: " DELIMITED BY SIZE
                       FUNCTION TRIM(MSG-CONTENT(LOOP-INDEX)) DELIMITED BY SIZE
                  INTO MESSAGE-BUFFER
                END-STRING
                PERFORM 700-DISPLAY-MESSAGE

                *> Display timestamp if available
                IF FUNCTION TRIM(MSG-TIMESTAMP(LOOP-INDEX)) NOT = SPACES
                    MOVE SPACES TO MESSAGE-BUFFER
                    STRING "Time: " DELIMITED BY SIZE
                           FUNCTION TRIM(MSG-TIMESTAMP(LOOP-INDEX)) DELIMITED BY SIZE
                      INTO MESSAGE-BUFFER
                    END-STRING
                    PERFORM 700-DISPLAY-MESSAGE
                END-IF

                MOVE "---------------------" TO MESSAGE-BUFFER
                PERFORM 700-DISPLAY-MESSAGE
            END-IF
        END-PERFORM
    ELSE
        MOVE "You have no messages at this time." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
    END-IF.

*> =====================
*> NEW: Validate Recipient is Connected (Week 8)
*> =====================
587-VALIDATE-RECIPIENT.
    MOVE 'N' TO IS-CONNECTED-FLAG

    *> Check if recipient exists in accounts
    MOVE 0 TO LOOP-INDEX
    PERFORM VARYING LOOP-INDEX FROM 1 BY 1 UNTIL LOOP-INDEX > ACCOUNT-COUNT
        IF FUNCTION UPPER-CASE(FUNCTION TRIM(ACCT-USER(LOOP-INDEX))) =
           FUNCTION UPPER-CASE(FUNCTION TRIM(RECIPIENT-USERNAME))
            MOVE 'Y' TO IS-CONNECTED-FLAG
            EXIT PERFORM
        END-IF
    END-PERFORM

    IF IS-CONNECTED-FLAG = 'N'
        MOVE "You can only message users you are connected with." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
        EXIT PARAGRAPH
    END-IF

    *> Check if they are connected
    MOVE 'N' TO IS-CONNECTED-FLAG
    PERFORM VARYING LOOP-INDEX FROM 1 BY 1 UNTIL LOOP-INDEX > PERMANENT-COUNT
        IF (FUNCTION UPPER-CASE(FUNCTION TRIM(PERM-USER1(LOOP-INDEX))) =
            FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER)) AND
            FUNCTION UPPER-CASE(FUNCTION TRIM(PERM-USER2(LOOP-INDEX))) =
            FUNCTION UPPER-CASE(FUNCTION TRIM(RECIPIENT-USERNAME)))
        OR
           (FUNCTION UPPER-CASE(FUNCTION TRIM(PERM-USER1(LOOP-INDEX))) =
            FUNCTION UPPER-CASE(FUNCTION TRIM(RECIPIENT-USERNAME)) AND
            FUNCTION UPPER-CASE(FUNCTION TRIM(PERM-USER2(LOOP-INDEX))) =
            FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER)))
            MOVE 'Y' TO IS-CONNECTED-FLAG
            EXIT PERFORM
        END-IF
    END-PERFORM

    IF IS-CONNECTED-FLAG = 'N'
        MOVE "You can only message users you are connected with." TO MESSAGE-BUFFER
        PERFORM 700-DISPLAY-MESSAGE
    END-IF.

*> =====================
*> NEW: Load Messages from File (Week 8)
*> =====================
590-LOAD-MESSAGES.
    OPEN INPUT USER-MESSAGES
    IF MESSAGE-FILE-STATUS = "35"
        *> File doesn't exist yet, that's OK
        CLOSE USER-MESSAGES
        EXIT PARAGRAPH
    END-IF

    IF MESSAGE-FILE-STATUS NOT = "00"
        DISPLAY "Error loading messages: " MESSAGE-FILE-STATUS
        CLOSE USER-MESSAGES
        EXIT PARAGRAPH
    END-IF

    MOVE 0 TO MESSAGE-COUNT
    PERFORM FOREVER
        READ USER-MESSAGES
            AT END EXIT PERFORM
        END-READ
        IF FUNCTION TRIM(MESSAGE-REC) NOT = SPACES
            IF MESSAGE-COUNT < MAX-MESSAGES
                ADD 1 TO MESSAGE-COUNT
                UNSTRING MESSAGE-REC DELIMITED BY "|"
                    INTO MSG-SENDER(MESSAGE-COUNT)
                         MSG-RECIPIENT(MESSAGE-COUNT)
                         MSG-CONTENT(MESSAGE-COUNT)
                         MSG-TIMESTAMP(MESSAGE-COUNT)
                END-UNSTRING
            END-IF
        END-IF
    END-PERFORM

    CLOSE USER-MESSAGES.

*> =====================
*> NEW: Save Messages to File (Week 8)
*> =====================
595-SAVE-MESSAGES.
    OPEN OUTPUT USER-MESSAGES
    IF MESSAGE-FILE-STATUS NOT = "00"
        DISPLAY "ERROR: Could not open Messages.dat for OUTPUT (status "
                 MESSAGE-FILE-STATUS ")"
        EXIT PARAGRAPH
    END-IF

    PERFORM VARYING LOOP-INDEX FROM 1 BY 1 UNTIL LOOP-INDEX > MESSAGE-COUNT
        MOVE SPACES TO MESSAGE-REC
        STRING FUNCTION TRIM(MSG-SENDER(LOOP-INDEX)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(MSG-RECIPIENT(LOOP-INDEX)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(MSG-CONTENT(LOOP-INDEX)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(MSG-TIMESTAMP(LOOP-INDEX)) DELIMITED BY SIZE
          INTO MESSAGE-REC
        END-STRING
        WRITE MESSAGE-REC
    END-PERFORM

    CLOSE USER-MESSAGES.
