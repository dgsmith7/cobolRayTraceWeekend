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
      * Display formatting variables (COMP-3 cannot be used in STRING)
       01  VEC3-DISPLAY-VARS.                  *> For STRING operations
           05  VEC3-DISPLAY-X      PIC -(6)9.9(6).  *> X component in display format
           05  VEC3-DISPLAY-Y      PIC -(6)9.9(6).  *> Y component in display format
           05  VEC3-DISPLAY-Z      PIC -(6)9.9(6).  *> Z component in display format

      *****************************************************************
      * COLOR DATA STRUCTURES - Color Support (using color = vec3)   *
      *****************************************************************
      * Color structures (equivalent to color alias for vec3)
       01  PIXEL-COLOR.                        *> Current pixel color
           05  PIXEL-COLOR-R       PIC 9V9(6) COMP-3.  *> Red component (0.0-1.0)
           05  PIXEL-COLOR-G       PIC 9V9(6) COMP-3.  *> Green component (0.0-1.0)
           05  PIXEL-COLOR-B       PIC 9V9(6) COMP-3.  *> Blue component (0.0-1.0)
           
      * Color output working variables
       01  COLOR-WORK-VARS.
           05  COLOR-R-BYTE        PIC 9(3).            *> Red byte value (0-255)
           05  COLOR-G-BYTE        PIC 9(3).            *> Green byte value (0-255)  
           05  COLOR-B-BYTE        PIC 9(3).            *> Blue byte value (0-255)
           05  COLOR-OUTPUT-LINE   PIC X(20).           *> Formatted color output
       
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
      * Create pixel color equivalent to: 
      * auto pixel_color = color(double(i)/(image_width-1), double(j)/(image_height-1), 0);
           COMPUTE VEC3-TEMP-X = I / WIDTH-MINUS-1     *> Red component (0.0-1.0)
           COMPUTE VEC3-TEMP-Y = J / HEIGHT-MINUS-1    *> Green component (0.0-1.0)  
           MOVE 0 TO VEC3-TEMP-Z                       *> Blue component (always 0)
           PERFORM COLOR-INIT-RGB.                    *> Initialize PIXEL-COLOR with these values
       
      * Write pixel color to PPM file using write_color function
       OUTPUT-PIXEL.
      * Equivalent to: write_color(std::cout, pixel_color);
           PERFORM WRITE-COLOR-TO-FILE.
       
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
           
      *****************************************************************
      * VEC3 ACCESSOR METHODS - Component Access                      *
      *****************************************************************
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
      * Equivalent to: vec3[index] where index 0=X, 1=Y, 2=Z
       VEC3-GET-ELEMENT-A.
           EVALUATE VEC3-SCALAR
               WHEN 0 MOVE VEC3-A-X TO VEC3-TEMP-CALC  *> Return X component
               WHEN 1 MOVE VEC3-A-Y TO VEC3-TEMP-CALC  *> Return Y component
               WHEN 2 MOVE VEC3-A-Z TO VEC3-TEMP-CALC  *> Return Z component
               WHEN OTHER MOVE 0 TO VEC3-TEMP-CALC     *> Invalid index
           END-EVALUATE.
           
      *****************************************************************
      * VEC3 ARITHMETIC OPERATIONS - Vector Math                     *
      *****************************************************************
      * Vector addition: VEC3-RESULT = VEC3-A + VEC3-B (operator+ equivalent)
       VEC3-ADD.
           ADD VEC3-A-X TO VEC3-B-X GIVING VEC3-RESULT-X  *> Result X = A.X + B.X
           ADD VEC3-A-Y TO VEC3-B-Y GIVING VEC3-RESULT-Y  *> Result Y = A.Y + B.Y
           ADD VEC3-A-Z TO VEC3-B-Z GIVING VEC3-RESULT-Z. *> Result Z = A.Z + B.Z
           
      * Vector subtraction: VEC3-RESULT = VEC3-A - VEC3-B (operator- equivalent)
       VEC3-SUBTRACT.
           SUBTRACT VEC3-B-X FROM VEC3-A-X GIVING VEC3-RESULT-X  *> Result X = A.X - B.X
           SUBTRACT VEC3-B-Y FROM VEC3-A-Y GIVING VEC3-RESULT-Y  *> Result Y = A.Y - B.Y
           SUBTRACT VEC3-B-Z FROM VEC3-A-Z GIVING VEC3-RESULT-Z. *> Result Z = A.Z - B.Z
           
      * Vector negation: VEC3-RESULT = -VEC3-A (unary operator- equivalent)
       VEC3-NEGATE-A.
           COMPUTE VEC3-RESULT-X = -VEC3-A-X  *> Negate X component
           COMPUTE VEC3-RESULT-Y = -VEC3-A-Y  *> Negate Y component
           COMPUTE VEC3-RESULT-Z = -VEC3-A-Z. *> Negate Z component
           
      * Scalar multiplication: VEC3-RESULT = VEC3-SCALAR * VEC3-A (t * vec equivalent)
       VEC3-MULTIPLY-SCALAR-A.
           COMPUTE VEC3-RESULT-X = VEC3-SCALAR * VEC3-A-X  *> Scale X component
           COMPUTE VEC3-RESULT-Y = VEC3-SCALAR * VEC3-A-Y  *> Scale Y component
           COMPUTE VEC3-RESULT-Z = VEC3-SCALAR * VEC3-A-Z. *> Scale Z component
           
      * Component-wise multiplication: VEC3-RESULT = VEC3-A * VEC3-B (Hadamard product)
       VEC3-MULTIPLY-VECTORS.
           COMPUTE VEC3-RESULT-X = VEC3-A-X * VEC3-B-X  *> Multiply X components
           COMPUTE VEC3-RESULT-Y = VEC3-A-Y * VEC3-B-Y  *> Multiply Y components
           COMPUTE VEC3-RESULT-Z = VEC3-A-Z * VEC3-B-Z. *> Multiply Z components
           
      * Scalar division: VEC3-RESULT = VEC3-A / VEC3-SCALAR (vec / t equivalent)
       VEC3-DIVIDE-SCALAR-A.
           COMPUTE VEC3-RESULT-X = VEC3-A-X / VEC3-SCALAR  *> Divide X component
           COMPUTE VEC3-RESULT-Y = VEC3-A-Y / VEC3-SCALAR  *> Divide Y component
           COMPUTE VEC3-RESULT-Z = VEC3-A-Z / VEC3-SCALAR. *> Divide Z component
           
      *****************************************************************
      * VEC3 IN-PLACE OPERATIONS - Modify Vector In Place            *
      *****************************************************************
      * VEC3-A += VEC3-B (operator+= equivalent)
       VEC3-ADD-TO-A.
           ADD VEC3-B-X TO VEC3-A-X  *> A.X += B.X
           ADD VEC3-B-Y TO VEC3-A-Y  *> A.Y += B.Y
           ADD VEC3-B-Z TO VEC3-A-Z. *> A.Z += B.Z
           
      * VEC3-A *= VEC3-SCALAR (operator*= equivalent)
       VEC3-MULTIPLY-A-BY-SCALAR.
           COMPUTE VEC3-A-X = VEC3-A-X * VEC3-SCALAR  *> A.X *= scalar
           COMPUTE VEC3-A-Y = VEC3-A-Y * VEC3-SCALAR  *> A.Y *= scalar
           COMPUTE VEC3-A-Z = VEC3-A-Z * VEC3-SCALAR. *> A.Z *= scalar
           
      * VEC3-A /= VEC3-SCALAR (operator/= equivalent)
       VEC3-DIVIDE-A-BY-SCALAR.
           COMPUTE VEC3-A-X = VEC3-A-X / VEC3-SCALAR  *> A.X /= scalar
           COMPUTE VEC3-A-Y = VEC3-A-Y / VEC3-SCALAR  *> A.Y /= scalar
           COMPUTE VEC3-A-Z = VEC3-A-Z / VEC3-SCALAR. *> A.Z /= scalar
           
      *****************************************************************
      * VEC3 UTILITY FUNCTIONS - Advanced Vector Operations          *
      *****************************************************************
      * Calculate length squared of VEC3-A: |A|² = A.X² + A.Y² + A.Z²
      * (Result stored in VEC3-LENGTH-SQR - more efficient than length)
       VEC3-LENGTH-SQUARED-A.
           COMPUTE VEC3-LENGTH-SQR = (VEC3-A-X * VEC3-A-X) + 
                                     (VEC3-A-Y * VEC3-A-Y) + 
                                     (VEC3-A-Z * VEC3-A-Z).
                                     
      * Calculate length (magnitude) of VEC3-A: |A| = √(A.X² + A.Y² + A.Z²)
      * (Result stored in VEC3-LENGTH)
       VEC3-LENGTH-A.
           PERFORM VEC3-LENGTH-SQUARED-A      *> First get length squared
           COMPUTE VEC3-LENGTH = VEC3-LENGTH-SQR ** 0.5.  *> Then take square root
           
      * Dot product: VEC3-DOT-PRODUCT = VEC3-A • VEC3-B = A.X*B.X + A.Y*B.Y + A.Z*B.Z
      * (Measures how parallel two vectors are)
       VEC3-CALCULATE-DOT-PRODUCT.
           COMPUTE VEC3-DOT-PRODUCT = (VEC3-A-X * VEC3-B-X) +
                                      (VEC3-A-Y * VEC3-B-Y) +
                                      (VEC3-A-Z * VEC3-B-Z).
                                      
      * Cross product: VEC3-RESULT = VEC3-A × VEC3-B
      * Creates a vector perpendicular to both A and B
      * Formula: (A.Y*B.Z - A.Z*B.Y, A.Z*B.X - A.X*B.Z, A.X*B.Y - A.Y*B.X)
       VEC3-CROSS-PRODUCT.
           COMPUTE VEC3-RESULT-X = (VEC3-A-Y * VEC3-B-Z) - 
                                   (VEC3-A-Z * VEC3-B-Y)  *> X = A.Y*B.Z - A.Z*B.Y
           COMPUTE VEC3-RESULT-Y = (VEC3-A-Z * VEC3-B-X) - 
                                   (VEC3-A-X * VEC3-B-Z)  *> Y = A.Z*B.X - A.X*B.Z
           COMPUTE VEC3-RESULT-Z = (VEC3-A-X * VEC3-B-Y) - 
                                   (VEC3-A-Y * VEC3-B-X). *> Z = A.X*B.Y - A.Y*B.X
           
      * Unit vector: VEC3-RESULT = VEC3-A / |VEC3-A| (normalize to length 1)
      * Creates a vector pointing in same direction as A but with length 1
       VEC3-UNIT-VECTOR-A.
           PERFORM VEC3-LENGTH-A               *> Calculate vector length
           MOVE VEC3-LENGTH TO VEC3-SCALAR
           PERFORM VEC3-MULTIPLY-SCALAR-A
      * Note: This puts unit vector in VEC3-RESULT, but actually divides by length
           COMPUTE VEC3-RESULT-X = VEC3-A-X / VEC3-LENGTH  *> Normalize X
           COMPUTE VEC3-RESULT-Y = VEC3-A-Y / VEC3-LENGTH  *> Normalize Y
           COMPUTE VEC3-RESULT-Z = VEC3-A-Z / VEC3-LENGTH. *> Normalize Z
           
      *****************************************************************
      * VEC3 OUTPUT PROCEDURES - Display and File Operations         *
      *****************************************************************
      * Display VEC3-A components to terminal (equivalent to cout << vec3)
      * Note: Must convert COMP-3 to DISPLAY format before STRING operation
       VEC3-DISPLAY-A.
           MOVE VEC3-A-X TO VEC3-DISPLAY-X     *> Convert X to display format
           MOVE VEC3-A-Y TO VEC3-DISPLAY-Y     *> Convert Y to display format
           MOVE VEC3-A-Z TO VEC3-DISPLAY-Z     *> Convert Z to display format
           STRING VEC3-DISPLAY-X " " VEC3-DISPLAY-Y " " VEC3-DISPLAY-Z
                  DELIMITED BY SIZE INTO VEC3-OUTPUT-LINE
           DISPLAY VEC3-OUTPUT-LINE.            *> Output: "X.XXXXXX Y.YYYYYY Z.ZZZZZZ"
           
      * Display VEC3-RESULT components to terminal
       VEC3-DISPLAY-RESULT.
           MOVE VEC3-RESULT-X TO VEC3-DISPLAY-X *> Convert X to display format
           MOVE VEC3-RESULT-Y TO VEC3-DISPLAY-Y *> Convert Y to display format
           MOVE VEC3-RESULT-Z TO VEC3-DISPLAY-Z *> Convert Z to display format
           STRING VEC3-DISPLAY-X " " VEC3-DISPLAY-Y " " VEC3-DISPLAY-Z
                  DELIMITED BY SIZE INTO VEC3-OUTPUT-LINE
           DISPLAY VEC3-OUTPUT-LINE.            *> Output formatted vector
           
      * Write VEC3-A components to file (for file output)
       VEC3-WRITE-A-TO-FILE.
           MOVE VEC3-A-X TO VEC3-DISPLAY-X     *> Convert to display format
           MOVE VEC3-A-Y TO VEC3-DISPLAY-Y     *> Convert to display format
           MOVE VEC3-A-Z TO VEC3-DISPLAY-Z     *> Convert to display format
           STRING VEC3-DISPLAY-X " " VEC3-DISPLAY-Y " " VEC3-DISPLAY-Z
                  DELIMITED BY SIZE INTO VEC3-OUTPUT-LINE
           MOVE VEC3-OUTPUT-LINE TO OUTPUT-RECORD  *> Prepare for file write
           WRITE OUTPUT-RECORD.                 *> Write vector to file
           
      *****************************************************************
      * COLOR PROCEDURES - Color Output Support                      *
      *****************************************************************
      
      * Write color to output stream (equivalent to write_color function)
      * This is the main color output function from the C++ tutorial
      * Input: PIXEL-COLOR contains color components (0.0-1.0 range)
      * Output: Writes RGB bytes (0-255 range) to PPM file
       WRITE-COLOR-TO-FILE.
      * Get RGB components from pixel color (equivalent to pixel_color.x(), y(), z())
           MOVE PIXEL-COLOR-R TO TEMP-R        *> Extract red component
           MOVE PIXEL-COLOR-G TO TEMP-G        *> Extract green component
           MOVE PIXEL-COLOR-B TO B             *> Extract blue component
           
      * Translate [0,1] component values to byte range [0,255]
      * Uses same 255.999 multiplier as C++ version for proper rounding
           COMPUTE COLOR-R-BYTE = 255.999 * TEMP-R    *> rbyte = int(255.999 * r)
           COMPUTE COLOR-G-BYTE = 255.999 * TEMP-G    *> gbyte = int(255.999 * g)  
           COMPUTE COLOR-B-BYTE = 255.999 * B         *> bbyte = int(255.999 * b)
           
      * Write out the pixel color components in PPM format
           STRING COLOR-R-BYTE " " COLOR-G-BYTE " " COLOR-B-BYTE
                  DELIMITED BY SIZE INTO COLOR-OUTPUT-LINE
           MOVE COLOR-OUTPUT-LINE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.                 *> Output: "255 128 64" (example)
           
      * Write color to terminal (for debugging/display purposes)
      * Same as WRITE-COLOR-TO-FILE but outputs to terminal instead of file
       WRITE-COLOR-TO-TERMINAL.
      * Get RGB components and convert to bytes
           MOVE PIXEL-COLOR-R TO TEMP-R        *> Extract red component
           MOVE PIXEL-COLOR-G TO TEMP-G        *> Extract green component
           MOVE PIXEL-COLOR-B TO B             *> Extract blue component
           
           COMPUTE COLOR-R-BYTE = 255.999 * TEMP-R  *> Convert to byte range
           COMPUTE COLOR-G-BYTE = 255.999 * TEMP-G  *> Convert to byte range
           COMPUTE COLOR-B-BYTE = 255.999 * B       *> Convert to byte range
           
      * Display color components to terminal
           STRING COLOR-R-BYTE " " COLOR-G-BYTE " " COLOR-B-BYTE
                  DELIMITED BY SIZE INTO COLOR-OUTPUT-LINE
           DISPLAY COLOR-OUTPUT-LINE.           *> Show RGB values on screen
           
      * Initialize color from vec3 (copy VEC3-A to PIXEL-COLOR)
      * Equivalent to: color pixel_color = some_vec3;
       COLOR-FROM-VEC3-A.
           MOVE VEC3-A-X TO PIXEL-COLOR-R      *> Copy X -> Red component
           MOVE VEC3-A-Y TO PIXEL-COLOR-G      *> Copy Y -> Green component
           MOVE VEC3-A-Z TO PIXEL-COLOR-B.     *> Copy Z -> Blue component
           
      * Initialize color with specific RGB values
      * Equivalent to: color pixel_color(r, g, b);
      * Input: Set values in VEC3-TEMP before calling (X=R, Y=G, Z=B)
       COLOR-INIT-RGB.
           MOVE VEC3-TEMP-X TO PIXEL-COLOR-R   *> Set red component
           MOVE VEC3-TEMP-Y TO PIXEL-COLOR-G   *> Set green component
           MOVE VEC3-TEMP-Z TO PIXEL-COLOR-B.  *> Set blue component
