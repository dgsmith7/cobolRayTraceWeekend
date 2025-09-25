      *****************************************************************
      * RAY-TRACER - Simple PPM Image Generator                      *
      * Generates a gradient image in PPM format                     *
      * Based on "Ray Tracing in One Weekend" tutorial              *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RAY-TRACER.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Define output file for PPM image format
           SELECT OUTPUT-FILE ASSIGN TO "image.ppm"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
      * File descriptor for PPM output file
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD           PIC X(80).  *> 80-char output record
       
       WORKING-STORAGE SECTION.
      * Image dimensions and PPM format constants
       01  IMAGE-WIDTH             PIC 9(3) VALUE 256.  *> Image width in pixels
       01  IMAGE-HEIGHT            PIC 9(3) VALUE 256.  *> Image height in pixels
       01  MAX-COLOR-VALUE         PIC 9(3) VALUE 255.  *> Max RGB value for PPM
       
      * Loop iteration variables
       01  LOOP-COUNTERS.
           05  I                   PIC 9(3) VALUE 0.  *> X-axis pixel counter
           05  J                   PIC 9(3) VALUE 0.  *> Y-axis pixel counter
       
      * Floating-point color values (0.0 to 1.0)
       01  COLOR-VALUES.
           05  R                   PIC 9V9(6) COMP-3.  *> Red component
           05  G                   PIC 9V9(6) COMP-3.  *> Green component
           05  B                   PIC 9V9(6) COMP-3 VALUE 0.0.  *> Blue (always 0)
       
      * Integer RGB values for PPM output (0-255)
       01  RGB-INTEGERS.
           05  IR                  PIC 9(3).        *> Red integer value
           05  IG                  PIC 9(3).        *> Green integer value
           05  IB                  PIC 9(3) VALUE 0.        *> Blue integer value
       
      * Temporary calculation variables
       01  TEMP-CALCULATIONS.
           05  TEMP-R              PIC 9V9(6) COMP-3.  *> Temp red calculation
           05  TEMP-G              PIC 9V9(6) COMP-3.  *> Temp green calculation
           05  WIDTH-MINUS-1       PIC 9(3).           *> Width - 1 for division
           05  HEIGHT-MINUS-1      PIC 9(3).           *> Height - 1 for division
           05  COLOR-MULTIPLIER    PIC 9(3)V9(3) COMP-3 VALUE 255.999.  *> Scale factor
       
      * Output formatting variables
       01  OUTPUT-LINE             PIC X(20).  *> Formatted pixel RGB values
       01  HEADER-LINE             PIC X(20).  *> PPM header format line
       01  DIMENSION-LINE          PIC X(20).  *> Image dimensions line
       01  MAX-COLOR-LINE          PIC X(10).  *> Maximum color value line
       01  PROGRESS-LINE           PIC X(50).  *> Progress display formatting
       01  SCANLINES-REMAINING     PIC 9(3).   *> Countdown for progress
       
       PROCEDURE DIVISION.
      *****************************************************************
      * Main program execution flow                                  *
      *****************************************************************
       MAIN-PROGRAM.
           PERFORM OPEN-OUTPUT-FILE     *> Open PPM file for writing
           PERFORM INITIALIZE-VALUES    *> Set up calculation constants
           PERFORM OUTPUT-HEADER        *> Write PPM file header
           PERFORM RENDER-IMAGE         *> Generate pixel data with progress
           PERFORM CLOSE-OUTPUT-FILE    *> Close the output file
           DISPLAY "PPM file 'image.ppm' created successfully!"
           STOP RUN.
       
      * File handling procedures
       OPEN-OUTPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.     *> Open file for writing
       
       CLOSE-OUTPUT-FILE.
           CLOSE OUTPUT-FILE.           *> Close and finalize file
       
      * Initialize constants for gradient calculations
       INITIALIZE-VALUES.
           COMPUTE WIDTH-MINUS-1 = IMAGE-WIDTH - 1   *> For normalizing X coords
           COMPUTE HEIGHT-MINUS-1 = IMAGE-HEIGHT - 1. *> For normalizing Y coords
       
      * Write PPM file header (P3 format)
       OUTPUT-HEADER.
           MOVE "P3" TO OUTPUT-RECORD           *> PPM ASCII format identifier
           WRITE OUTPUT-RECORD
           
           STRING IMAGE-WIDTH " " IMAGE-HEIGHT   *> Image dimensions
                  DELIMITED BY SIZE INTO DIMENSION-LINE
           MOVE DIMENSION-LINE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           
           MOVE MAX-COLOR-VALUE TO MAX-COLOR-LINE  *> Maximum color value (255)
           MOVE MAX-COLOR-LINE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.
       
      * Main image rendering loop - generates gradient pattern
       RENDER-IMAGE.
      * Outer loop: iterate through each row (Y-axis)
           PERFORM VARYING J FROM 0 BY 1 UNTIL J >= IMAGE-HEIGHT
               PERFORM DISPLAY-PROGRESS     *> Show progress to terminal
      * Inner loop: iterate through each column (X-axis)
               PERFORM VARYING I FROM 0 BY 1 UNTIL I >= IMAGE-WIDTH
                   PERFORM CALCULATE-PIXEL-COLOR  *> Calculate RGB values
                   PERFORM OUTPUT-PIXEL           *> Write pixel to file
               END-PERFORM
           END-PERFORM
           PERFORM DISPLAY-COMPLETION.      *> Show completion message
       
      * Calculate RGB values for current pixel position
       CALCULATE-PIXEL-COLOR.
      * Normalize coordinates to 0.0-1.0 range
           COMPUTE TEMP-R = I / WIDTH-MINUS-1     *> Red increases left to right
           COMPUTE TEMP-G = J / HEIGHT-MINUS-1    *> Green increases top to bottom
           MOVE TEMP-R TO R
           MOVE TEMP-G TO G
           
      * Convert floating-point colors to integer RGB (0-255)
           COMPUTE IR = COLOR-MULTIPLIER * R      *> Red component (0-255)
           COMPUTE IG = COLOR-MULTIPLIER * G      *> Green component (0-255)
           COMPUTE IB = COLOR-MULTIPLIER * B.     *> Blue component (always 0)
       
      * Write pixel RGB values to PPM file
       OUTPUT-PIXEL.
           STRING IR " " IG " " IB          *> Format: "R G B"
                  DELIMITED BY SIZE INTO OUTPUT-LINE
           MOVE OUTPUT-LINE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.                *> Write pixel data to file
       
      * Display rendering progress to terminal
       DISPLAY-PROGRESS.
           COMPUTE SCANLINES-REMAINING = IMAGE-HEIGHT - J  *> Count down remaining rows
           STRING "Scanlines remaining: " SCANLINES-REMAINING " "
                  DELIMITED BY SIZE INTO PROGRESS-LINE
           DISPLAY PROGRESS-LINE.           *> Show progress on terminal
       
      * Display completion message
       DISPLAY-COMPLETION.
           DISPLAY "Done.                    ".  *> Clear progress line and show done
