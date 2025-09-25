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
       
      *****************************************************************
      * VEC3 DATA STRUCTURES - 3D Vector Support                     *
      *****************************************************************
      * Primary vector structures (equivalent to vec3 objects)
       01  VEC3-A.                             *> First vector
           05  VEC3-A-X            PIC S9V9(6) COMP-3.  *> X component
           05  VEC3-A-Y            PIC S9V9(6) COMP-3.  *> Y component  
           05  VEC3-A-Z            PIC S9V9(6) COMP-3.  *> Z component
           
       01  VEC3-B.                             *> Second vector
           05  VEC3-B-X            PIC S9V9(6) COMP-3.  *> X component
           05  VEC3-B-Y            PIC S9V9(6) COMP-3.  *> Y component
           05  VEC3-B-Z            PIC S9V9(6) COMP-3.  *> Z component
           
       01  VEC3-RESULT.                        *> Result vector
           05  VEC3-RESULT-X       PIC S9V9(6) COMP-3.  *> X component
           05  VEC3-RESULT-Y       PIC S9V9(6) COMP-3.  *> Y component
           05  VEC3-RESULT-Z       PIC S9V9(6) COMP-3.  *> Z component
           
       01  VEC3-TEMP.                          *> Temporary vector
           05  VEC3-TEMP-X         PIC S9V9(6) COMP-3.  *> X component
           05  VEC3-TEMP-Y         PIC S9V9(6) COMP-3.  *> Y component
           05  VEC3-TEMP-Z         PIC S9V9(6) COMP-3.  *> Z component
           
      * Vector calculation working variables
       01  VEC3-WORK-VARS.
           05  VEC3-SCALAR         PIC S9V9(6) COMP-3.  *> Scalar multiplier
           05  VEC3-LENGTH         PIC 9V9(6) COMP-3.   *> Vector length
           05  VEC3-LENGTH-SQR     PIC 9V9(6) COMP-3.   *> Length squared
           05  VEC3-DOT-PRODUCT    PIC S9V9(6) COMP-3.  *> Dot product result
           05  VEC3-TEMP-CALC      PIC S9V9(6) COMP-3.  *> General temp calc
           
       01  VEC3-OUTPUT-LINE        PIC X(40).  *> For vector display
       
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
           
      *****************************************************************
      * VEC3 PROCEDURES - 3D Vector Operations                       *
      *****************************************************************
      
      * Initialize vector to zero (default constructor equivalent)
       VEC3-INIT-ZERO.
           MOVE 0 TO VEC3-A-X
           MOVE 0 TO VEC3-A-Y  
           MOVE 0 TO VEC3-A-Z.
           
      * Initialize VEC3-A with specific values (parameterized constructor)
      * Call with values in VEC3-TEMP before calling this procedure
       VEC3-INIT-VALUES-A.
           MOVE VEC3-TEMP-X TO VEC3-A-X
           MOVE VEC3-TEMP-Y TO VEC3-A-Y
           MOVE VEC3-TEMP-Z TO VEC3-A-Z.
           
      * Initialize VEC3-B with specific values
       VEC3-INIT-VALUES-B.
           MOVE VEC3-TEMP-X TO VEC3-B-X
           MOVE VEC3-TEMP-Y TO VEC3-B-Y
           MOVE VEC3-TEMP-Z TO VEC3-B-Z.
           
      * Copy VEC3-A to VEC3-RESULT
       VEC3-COPY-A-TO-RESULT.
           MOVE VEC3-A-X TO VEC3-RESULT-X
           MOVE VEC3-A-Y TO VEC3-RESULT-Y
           MOVE VEC3-A-Z TO VEC3-RESULT-Z.
           
      * Accessor procedures (equivalent to x(), y(), z() methods)
      * Get X component of VEC3-A (result in VEC3-TEMP-CALC)
       VEC3-GET-X-A.
           MOVE VEC3-A-X TO VEC3-TEMP-CALC.
           
      * Get Y component of VEC3-A  
       VEC3-GET-Y-A.
           MOVE VEC3-A-Y TO VEC3-TEMP-CALC.
           
      * Get Z component of VEC3-A
       VEC3-GET-Z-A.
           MOVE VEC3-A-Z TO VEC3-TEMP-CALC.
           
      * Array-style access for VEC3-A (index in VEC3-SCALAR, result in VEC3-TEMP-CALC)
       VEC3-GET-ELEMENT-A.
           EVALUATE VEC3-SCALAR
               WHEN 0 MOVE VEC3-A-X TO VEC3-TEMP-CALC
               WHEN 1 MOVE VEC3-A-Y TO VEC3-TEMP-CALC  
               WHEN 2 MOVE VEC3-A-Z TO VEC3-TEMP-CALC
               WHEN OTHER MOVE 0 TO VEC3-TEMP-CALC
           END-EVALUATE.
           
      * Arithmetic Operations
      * Vector addition: VEC3-RESULT = VEC3-A + VEC3-B
       VEC3-ADD.
           ADD VEC3-A-X TO VEC3-B-X GIVING VEC3-RESULT-X
           ADD VEC3-A-Y TO VEC3-B-Y GIVING VEC3-RESULT-Y
           ADD VEC3-A-Z TO VEC3-B-Z GIVING VEC3-RESULT-Z.
           
      * Vector subtraction: VEC3-RESULT = VEC3-A - VEC3-B  
       VEC3-SUBTRACT.
           SUBTRACT VEC3-B-X FROM VEC3-A-X GIVING VEC3-RESULT-X
           SUBTRACT VEC3-B-Y FROM VEC3-A-Y GIVING VEC3-RESULT-Y
           SUBTRACT VEC3-B-Z FROM VEC3-A-Z GIVING VEC3-RESULT-Z.
           
      * Vector negation: VEC3-RESULT = -VEC3-A
       VEC3-NEGATE-A.
           COMPUTE VEC3-RESULT-X = -VEC3-A-X
           COMPUTE VEC3-RESULT-Y = -VEC3-A-Y
           COMPUTE VEC3-RESULT-Z = -VEC3-A-Z.
           
      * Scalar multiplication: VEC3-RESULT = VEC3-SCALAR * VEC3-A
       VEC3-MULTIPLY-SCALAR-A.
           COMPUTE VEC3-RESULT-X = VEC3-SCALAR * VEC3-A-X
           COMPUTE VEC3-RESULT-Y = VEC3-SCALAR * VEC3-A-Y
           COMPUTE VEC3-RESULT-Z = VEC3-SCALAR * VEC3-A-Z.
           
      * Component-wise multiplication: VEC3-RESULT = VEC3-A * VEC3-B
       VEC3-MULTIPLY-VECTORS.
           COMPUTE VEC3-RESULT-X = VEC3-A-X * VEC3-B-X
           COMPUTE VEC3-RESULT-Y = VEC3-A-Y * VEC3-B-Y
           COMPUTE VEC3-RESULT-Z = VEC3-A-Z * VEC3-B-Z.
           
      * Scalar division: VEC3-RESULT = VEC3-A / VEC3-SCALAR
       VEC3-DIVIDE-SCALAR-A.
           COMPUTE VEC3-RESULT-X = VEC3-A-X / VEC3-SCALAR
           COMPUTE VEC3-RESULT-Y = VEC3-A-Y / VEC3-SCALAR
           COMPUTE VEC3-RESULT-Z = VEC3-A-Z / VEC3-SCALAR.
           
      * In-place operations (equivalent to +=, *=, /= operators)
      * VEC3-A += VEC3-B
       VEC3-ADD-TO-A.
           ADD VEC3-B-X TO VEC3-A-X
           ADD VEC3-B-Y TO VEC3-A-Y
           ADD VEC3-B-Z TO VEC3-A-Z.
           
      * VEC3-A *= VEC3-SCALAR
       VEC3-MULTIPLY-A-BY-SCALAR.
           COMPUTE VEC3-A-X = VEC3-A-X * VEC3-SCALAR
           COMPUTE VEC3-A-Y = VEC3-A-Y * VEC3-SCALAR
           COMPUTE VEC3-A-Z = VEC3-A-Z * VEC3-SCALAR.
           
      * VEC3-A /= VEC3-SCALAR  
       VEC3-DIVIDE-A-BY-SCALAR.
           COMPUTE VEC3-A-X = VEC3-A-X / VEC3-SCALAR
           COMPUTE VEC3-A-Y = VEC3-A-Y / VEC3-SCALAR
           COMPUTE VEC3-A-Z = VEC3-A-Z / VEC3-SCALAR.
           
      * Vector utility functions
      * Calculate length squared of VEC3-A (result in VEC3-LENGTH-SQR)
       VEC3-LENGTH-SQUARED-A.
           COMPUTE VEC3-LENGTH-SQR = (VEC3-A-X * VEC3-A-X) + 
                                     (VEC3-A-Y * VEC3-A-Y) + 
                                     (VEC3-A-Z * VEC3-A-Z).
                                     
      * Calculate length of VEC3-A (result in VEC3-LENGTH)
       VEC3-LENGTH-A.
           PERFORM VEC3-LENGTH-SQUARED-A
           COMPUTE VEC3-LENGTH = VEC3-LENGTH-SQR ** 0.5.
           
      * Dot product: VEC3-DOT-PRODUCT = VEC3-A • VEC3-B
       VEC3-DOT-PRODUCT.
           COMPUTE VEC3-DOT-PRODUCT = (VEC3-A-X * VEC3-B-X) +
                                      (VEC3-A-Y * VEC3-B-Y) +
                                      (VEC3-A-Z * VEC3-B-Z).
                                      
      * Cross product: VEC3-RESULT = VEC3-A × VEC3-B
       VEC3-CROSS-PRODUCT.
           COMPUTE VEC3-RESULT-X = (VEC3-A-Y * VEC3-B-Z) - (VEC3-A-Z * VEC3-B-Y)
           COMPUTE VEC3-RESULT-Y = (VEC3-A-Z * VEC3-B-X) - (VEC3-A-X * VEC3-B-Z)
           COMPUTE VEC3-RESULT-Z = (VEC3-A-X * VEC3-B-Y) - (VEC3-A-Y * VEC3-B-X).
           
      * Unit vector: VEC3-RESULT = VEC3-A / |VEC3-A|
       VEC3-UNIT-VECTOR-A.
           PERFORM VEC3-LENGTH-A
           MOVE VEC3-LENGTH TO VEC3-SCALAR
           PERFORM VEC3-MULTIPLY-SCALAR-A
      * Note: This puts unit vector in VEC3-RESULT, but actually divides by length
           COMPUTE VEC3-RESULT-X = VEC3-A-X / VEC3-LENGTH
           COMPUTE VEC3-RESULT-Y = VEC3-A-Y / VEC3-LENGTH
           COMPUTE VEC3-RESULT-Z = VEC3-A-Z / VEC3-LENGTH.
           
      * Output/Display procedures (equivalent to ostream operator)
      * Display VEC3-A components to terminal
       VEC3-DISPLAY-A.
           STRING VEC3-A-X " " VEC3-A-Y " " VEC3-A-Z
                  DELIMITED BY SIZE INTO VEC3-OUTPUT-LINE
           DISPLAY VEC3-OUTPUT-LINE.
           
      * Display VEC3-RESULT components to terminal  
       VEC3-DISPLAY-RESULT.
           STRING VEC3-RESULT-X " " VEC3-RESULT-Y " " VEC3-RESULT-Z
                  DELIMITED BY SIZE INTO VEC3-OUTPUT-LINE
           DISPLAY VEC3-OUTPUT-LINE.
           
      * Write VEC3-A components to file
       VEC3-WRITE-A-TO-FILE.
           STRING VEC3-A-X " " VEC3-A-Y " " VEC3-A-Z
                  DELIMITED BY SIZE INTO VEC3-OUTPUT-LINE
           MOVE VEC3-OUTPUT-LINE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.
