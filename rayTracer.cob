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
       01  IMAGE-WIDTH             PIC 9(3) VALUE 400.  *> Image width in pixels (updated from tutorial)
       01  IMAGE-HEIGHT            PIC 9(3).            *> Image height (calculated from aspect ratio)
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
       
      *****************************************************************
      * RAY DATA STRUCTURES - 3D Ray Support (ray = origin + t*dir)  *
      *****************************************************************
      * Ray structure (equivalent to ray class with origin and direction)
       01  RAY-DATA.                           *> Primary ray structure
           05  RAY-ORIGIN.                     *> Ray origin point (point3)
               10  RAY-ORIGIN-X    PIC S9V9(6) COMP-3.  *> Origin X coordinate
               10  RAY-ORIGIN-Y    PIC S9V9(6) COMP-3.  *> Origin Y coordinate
               10  RAY-ORIGIN-Z    PIC S9V9(6) COMP-3.  *> Origin Z coordinate
           05  RAY-DIRECTION.                  *> Ray direction vector (vec3)
               10  RAY-DIR-X       PIC S9V9(6) COMP-3.  *> Direction X component
               10  RAY-DIR-Y       PIC S9V9(6) COMP-3.  *> Direction Y component
               10  RAY-DIR-Z       PIC S9V9(6) COMP-3.  *> Direction Z component
               
      * Secondary ray for calculations (equivalent to multiple ray objects)
       01  RAY-TEMP.                          *> Temporary ray structure
           05  RAY-TEMP-ORIGIN.               *> Temporary origin point
               10  RAY-TEMP-ORIG-X PIC S9V9(6) COMP-3.  *> Temp origin X
               10  RAY-TEMP-ORIG-Y PIC S9V9(6) COMP-3.  *> Temp origin Y
               10  RAY-TEMP-ORIG-Z PIC S9V9(6) COMP-3.  *> Temp origin Z
           05  RAY-TEMP-DIRECTION.            *> Temporary direction vector
               10  RAY-TEMP-DIR-X  PIC S9V9(6) COMP-3.  *> Temp direction X
               10  RAY-TEMP-DIR-Y  PIC S9V9(6) COMP-3.  *> Temp direction Y
               10  RAY-TEMP-DIR-Z  PIC S9V9(6) COMP-3.  *> Temp direction Z
               
      * Ray calculation working variables
       01  RAY-WORK-VARS.
           05  RAY-PARAMETER-T     PIC S9V9(6) COMP-3.  *> Ray parameter t
           05  RAY-POINT-X         PIC S9V9(6) COMP-3.  *> Calculated point X
           05  RAY-POINT-Y         PIC S9V9(6) COMP-3.  *> Calculated point Y
           05  RAY-POINT-Z         PIC S9V9(6) COMP-3.  *> Calculated point Z
           
      *****************************************************************
      * CAMERA DATA STRUCTURES - 3D Camera and Viewport Support       *
      *****************************************************************
      * Camera parameters (equivalent to camera setup variables)
       01  CAMERA-PARAMS.
           05  ASPECT-RATIO        PIC 9V9(6) COMP-3 VALUE 1.777777.  *> 16/9 aspect ratio
           05  FOCAL-LENGTH        PIC 9V9(6) COMP-3 VALUE 1.0.      *> Camera focal length
           05  VIEWPORT-HEIGHT     PIC 9V9(6) COMP-3 VALUE 2.0.      *> Viewport height
           05  VIEWPORT-WIDTH      PIC 9V9(6) COMP-3.                *> Calculated viewport width
           
      * Camera center point (equivalent to camera_center)
       01  CAMERA-CENTER.
           05  CAMERA-CENTER-X     PIC S9V9(6) COMP-3 VALUE 0.0.     *> Camera X position
           05  CAMERA-CENTER-Y     PIC S9V9(6) COMP-3 VALUE 0.0.     *> Camera Y position
           05  CAMERA-CENTER-Z     PIC S9V9(6) COMP-3 VALUE 0.0.     *> Camera Z position
           
      * Viewport vectors (equivalent to viewport_u, viewport_v)
       01  VIEWPORT-VECTORS.
           05  VIEWPORT-U-X        PIC S9V9(6) COMP-3.               *> U vector X component
           05  VIEWPORT-U-Y        PIC S9V9(6) COMP-3 VALUE 0.0.     *> U vector Y component
           05  VIEWPORT-U-Z        PIC S9V9(6) COMP-3 VALUE 0.0.     *> U vector Z component
           05  VIEWPORT-V-X        PIC S9V9(6) COMP-3 VALUE 0.0.     *> V vector X component
           05  VIEWPORT-V-Y        PIC S9V9(6) COMP-3.               *> V vector Y component
           05  VIEWPORT-V-Z        PIC S9V9(6) COMP-3 VALUE 0.0.     *> V vector Z component
               
      * Pixel delta vectors (equivalent to pixel_delta_u, pixel_delta_v)
       01  PIXEL-DELTAS.
           05  PIXEL-DELTA-U-X     PIC S9V9(6) COMP-3.               *> Delta U X component
           05  PIXEL-DELTA-U-Y     PIC S9V9(6) COMP-3.               *> Delta U Y component
           05  PIXEL-DELTA-U-Z     PIC S9V9(6) COMP-3.               *> Delta U Z component
           05  PIXEL-DELTA-V-X     PIC S9V9(6) COMP-3.               *> Delta V X component
           05  PIXEL-DELTA-V-Y     PIC S9V9(6) COMP-3.               *> Delta V Y component
           05  PIXEL-DELTA-V-Z     PIC S9V9(6) COMP-3.               *> Delta V Z component
               
      * Viewport positioning (equivalent to viewport_upper_left, pixel00_loc)
       01  VIEWPORT-POSITIONS.
           05  VIEWPORT-UL-X       PIC S9V9(6) COMP-3.               *> Upper left X
           05  VIEWPORT-UL-Y       PIC S9V9(6) COMP-3.               *> Upper left Y
           05  VIEWPORT-UL-Z       PIC S9V9(6) COMP-3.               *> Upper left Z
           05  PIXEL00-X           PIC S9V9(6) COMP-3.               *> Pixel (0,0) X
           05  PIXEL00-Y           PIC S9V9(6) COMP-3.               *> Pixel (0,0) Y
           05  PIXEL00-Z           PIC S9V9(6) COMP-3.               *> Pixel (0,0) Z
               
      * Current pixel calculations
       01  PIXEL-CALCULATIONS.
           05  PIXEL-CENTER-X      PIC S9V9(6) COMP-3.               *> Pixel center X
           05  PIXEL-CENTER-Y      PIC S9V9(6) COMP-3.               *> Pixel center Y
           05  PIXEL-CENTER-Z      PIC S9V9(6) COMP-3.               *> Pixel center Z
           05  RAY-DIR-CALC-X      PIC S9V9(6) COMP-3.               *> Ray direction X
           05  RAY-DIR-CALC-Y      PIC S9V9(6) COMP-3.               *> Ray direction Y
           05  RAY-DIR-CALC-Z      PIC S9V9(6) COMP-3.               *> Ray direction Z
           
      *****************************************************************
      * SPHERE INTERSECTION DATA - Ray-Sphere Collision Detection      *
      *****************************************************************
      * Sphere parameters for intersection testing
       01  SPHERE-DATA.
           05  SPHERE-CENTER-X     PIC S9V9(6) COMP-3.               *> Sphere center X
           05  SPHERE-CENTER-Y     PIC S9V9(6) COMP-3.               *> Sphere center Y
           05  SPHERE-CENTER-Z     PIC S9V9(6) COMP-3.               *> Sphere center Z
           05  SPHERE-RADIUS       PIC 9V9(6) COMP-3.                *> Sphere radius
           
      * Ray-sphere intersection working variables
       01  SPHERE-INTERSECTION-VARS.
           05  SPHERE-OC-X         PIC S9V9(6) COMP-3.               *> Origin to center X
           05  SPHERE-OC-Y         PIC S9V9(6) COMP-3.               *> Origin to center Y
           05  SPHERE-OC-Z         PIC S9V9(6) COMP-3.               *> Origin to center Z
           05  SPHERE-A            PIC 9V9(6) COMP-3.                *> Quadratic coefficient a
           05  SPHERE-B            PIC S9V9(6) COMP-3.               *> Quadratic coefficient b
           05  SPHERE-C            PIC 9V9(6) COMP-3.                *> Quadratic coefficient c
           05  SPHERE-DISCRIMINANT PIC S9V9(6) COMP-3.               *> b²-4ac discriminant
           05  SPHERE-HIT-FLAG     PIC 9 VALUE 0.                    *> 1=hit, 0=miss
           05  SPHERE-HIT-T        PIC S9V9(6) COMP-3.               *> Hit distance (t parameter)
       
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
       
      * Initialize camera system and image parameters
       INITIALIZE-VALUES.
      * Calculate image height from aspect ratio (16:9)
      * Ensure height is at least 1 pixel
           COMPUTE IMAGE-HEIGHT = IMAGE-WIDTH / ASPECT-RATIO
           IF IMAGE-HEIGHT < 1
               MOVE 1 TO IMAGE-HEIGHT
           END-IF
           
      * Calculate viewport width from height and image aspect ratio
           COMPUTE VIEWPORT-WIDTH = VIEWPORT-HEIGHT * 
                                   (IMAGE-WIDTH / IMAGE-HEIGHT)
                                   
      * Set up viewport vectors
      * viewport_u = vec3(viewport_width, 0, 0)
           MOVE VIEWPORT-WIDTH TO VIEWPORT-U-X
           MOVE 0 TO VIEWPORT-U-Y
           MOVE 0 TO VIEWPORT-U-Z
           
      * viewport_v = vec3(0, -viewport_height, 0) 
           MOVE 0 TO VIEWPORT-V-X
           COMPUTE VIEWPORT-V-Y = -VIEWPORT-HEIGHT  *> Negative for screen coordinates
           MOVE 0 TO VIEWPORT-V-Z
           
      * Calculate pixel delta vectors (spacing between pixels)
      * pixel_delta_u = viewport_u / image_width
           COMPUTE PIXEL-DELTA-U-X = VIEWPORT-U-X / IMAGE-WIDTH
           COMPUTE PIXEL-DELTA-U-Y = VIEWPORT-U-Y / IMAGE-WIDTH  
           COMPUTE PIXEL-DELTA-U-Z = VIEWPORT-U-Z / IMAGE-WIDTH
           
      * pixel_delta_v = viewport_v / image_height
           COMPUTE PIXEL-DELTA-V-X = VIEWPORT-V-X / IMAGE-HEIGHT
           COMPUTE PIXEL-DELTA-V-Y = VIEWPORT-V-Y / IMAGE-HEIGHT
           COMPUTE PIXEL-DELTA-V-Z = VIEWPORT-V-Z / IMAGE-HEIGHT
           
      * Calculate viewport upper left corner
      * viewport_upper_left = camera_center - vec3(0,0,focal_length) - viewport_u/2 - viewport_v/2
           COMPUTE VIEWPORT-UL-X = CAMERA-CENTER-X - 0 - 
                  (VIEWPORT-U-X / 2) - (VIEWPORT-V-X / 2)
           COMPUTE VIEWPORT-UL-Y = CAMERA-CENTER-Y - 0 - 
                  (VIEWPORT-U-Y / 2) - (VIEWPORT-V-Y / 2)
           COMPUTE VIEWPORT-UL-Z = CAMERA-CENTER-Z - FOCAL-LENGTH - 
                  (VIEWPORT-U-Z / 2) - (VIEWPORT-V-Z / 2)
           
      * Calculate pixel (0,0) location 
      * pixel00_loc = viewport_upper_left + 0.5 * (pixel_delta_u + pixel_delta_v)
           COMPUTE PIXEL00-X = VIEWPORT-UL-X + 
                  0.5 * (PIXEL-DELTA-U-X + PIXEL-DELTA-V-X)
           COMPUTE PIXEL00-Y = VIEWPORT-UL-Y + 
                  0.5 * (PIXEL-DELTA-U-Y + PIXEL-DELTA-V-Y)
           COMPUTE PIXEL00-Z = VIEWPORT-UL-Z + 
                  0.5 * (PIXEL-DELTA-U-Z + PIXEL-DELTA-V-Z)
           
      * Legacy calculations for compatibility
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
       
      * Calculate RGB values for current pixel using ray tracing
       CALCULATE-PIXEL-COLOR.
      * Calculate pixel center in 3D space
      * pixel_center = pixel00_loc + (i * pixel_delta_u) + (j * pixel_delta_v)
           COMPUTE PIXEL-CENTER-X = PIXEL00-X + 
                  (I * PIXEL-DELTA-U-X) + (J * PIXEL-DELTA-V-X)
           COMPUTE PIXEL-CENTER-Y = PIXEL00-Y + 
                  (I * PIXEL-DELTA-U-Y) + (J * PIXEL-DELTA-V-Y)
           COMPUTE PIXEL-CENTER-Z = PIXEL00-Z + 
                  (I * PIXEL-DELTA-U-Z) + (J * PIXEL-DELTA-V-Z)
           
      * Calculate ray direction from camera center to pixel center
      * ray_direction = pixel_center - camera_center
           COMPUTE RAY-DIR-CALC-X = PIXEL-CENTER-X - CAMERA-CENTER-X
           COMPUTE RAY-DIR-CALC-Y = PIXEL-CENTER-Y - CAMERA-CENTER-Y
           COMPUTE RAY-DIR-CALC-Z = PIXEL-CENTER-Z - CAMERA-CENTER-Z
           
      * Set up ray with camera center as origin and calculated direction
      * ray r(camera_center, ray_direction)
           MOVE CAMERA-CENTER-X TO VEC3-A-X    *> Origin = camera center
           MOVE CAMERA-CENTER-Y TO VEC3-A-Y
           MOVE CAMERA-CENTER-Z TO VEC3-A-Z
           MOVE RAY-DIR-CALC-X TO VEC3-B-X      *> Direction = calculated direction
           MOVE RAY-DIR-CALC-Y TO VEC3-B-Y
           MOVE RAY-DIR-CALC-Z TO VEC3-B-Z
           PERFORM RAY-CONSTRUCT-WITH-PARAMS    *> Create the ray
           
      * Get pixel color using ray_color function
      * color pixel_color = ray_color(r)
           PERFORM RAY-COLOR-FUNCTION           *> Calculate color for this ray
           
      * Result is now in PIXEL-COLOR, ready for output
           EXIT.
       
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
           
      *****************************************************************
      * RAY CLASS IMPLEMENTATION - 3D Ray Operations                 *
      *****************************************************************
      * Ray constructor procedures (equivalent to ray constructors)  *
      *****************************************************************
      
      *> Default ray constructor - creates ray at origin with no direction
      *> C++ equivalent: ray()
       RAY-CONSTRUCT-DEFAULT.
           MOVE ZEROS TO RAY-ORIGIN-X RAY-ORIGIN-Y RAY-ORIGIN-Z
           MOVE ZEROS TO RAY-DIR-X RAY-DIR-Y RAY-DIR-Z.
           
      *> Ray constructor with origin and direction parameters
      *> C++ equivalent: ray(const point3& origin, const vec3& direction)
      *> Input: VEC3-A contains origin, VEC3-B contains direction
       RAY-CONSTRUCT-WITH-PARAMS.
           MOVE VEC3-A-X TO RAY-ORIGIN-X
           MOVE VEC3-A-Y TO RAY-ORIGIN-Y
           MOVE VEC3-A-Z TO RAY-ORIGIN-Z
           MOVE VEC3-B-X TO RAY-DIR-X
           MOVE VEC3-B-Y TO RAY-DIR-Y
           MOVE VEC3-B-Z TO RAY-DIR-Z.
           
      *****************************************************************
      * Ray accessor methods (equivalent to public member functions) *
      *****************************************************************
      
      *> Get ray origin - returns origin point in VEC3-RESULT
      *> C++ equivalent: const point3& origin() const
       RAY-GET-ORIGIN.
           MOVE RAY-ORIGIN-X TO VEC3-RESULT-X
           MOVE RAY-ORIGIN-Y TO VEC3-RESULT-Y
           MOVE RAY-ORIGIN-Z TO VEC3-RESULT-Z.
           
      *> Get ray direction - returns direction vector in VEC3-RESULT
      *> C++ equivalent: const vec3& direction() const
       RAY-GET-DIRECTION.
           MOVE RAY-DIR-X TO VEC3-RESULT-X
           MOVE RAY-DIR-Y TO VEC3-RESULT-Y
           MOVE RAY-DIR-Z TO VEC3-RESULT-Z.
           
      *****************************************************************
      * Ray calculation methods                                       *
      *****************************************************************
      
      *> Calculate point along ray at parameter t
      *> Formula: point = origin + t * direction
      *> C++ equivalent: point3 at(double t) const
      *> Input: RAY-PARAMETER-T contains the parameter t
      *> Output: Result stored in RAY-POINT-X, RAY-POINT-Y, RAY-POINT-Z
       RAY-AT-PARAMETER.
      *    Calculate t * direction for each component, then add origin
           COMPUTE RAY-POINT-X = RAY-ORIGIN-X + 
                                (RAY-PARAMETER-T * RAY-DIR-X)
           COMPUTE RAY-POINT-Y = RAY-ORIGIN-Y + 
                                (RAY-PARAMETER-T * RAY-DIR-Y)
           COMPUTE RAY-POINT-Z = RAY-ORIGIN-Z + 
                                (RAY-PARAMETER-T * RAY-DIR-Z).
           
      *> Calculate point along ray and store result in VEC3-RESULT
      *> Same as RAY-AT-PARAMETER but returns result in standard location
      *> Input: RAY-PARAMETER-T contains the parameter t
      *> Output: Result stored in VEC3-RESULT-X, VEC3-RESULT-Y, VEC3-RESULT-Z
       RAY-AT-PARAMETER-TO-VEC3.
           PERFORM RAY-AT-PARAMETER
           MOVE RAY-POINT-X TO VEC3-RESULT-X
           MOVE RAY-POINT-Y TO VEC3-RESULT-Y
           MOVE RAY-POINT-Z TO VEC3-RESULT-Z.
           
      *****************************************************************
      * SPHERE INTERSECTION FUNCTION - Ray-Sphere Collision Detection  *
      *****************************************************************
      
      *> Test if ray intersects sphere and return hit distance
      *> C++ equivalent: double hit_sphere(const point3& center, double radius, const ray& r)
      *> Input: SPHERE-CENTER-X/Y/Z contains sphere center
      *>        SPHERE-RADIUS contains sphere radius
      *>        RAY-DATA contains the ray to test
      *> Output: SPHERE-HIT-T = hit distance if hit (>0), -1.0 if miss
       HIT-SPHERE.
      * Calculate vector from ray origin to sphere center
      * vec3 oc = center - r.origin()
           COMPUTE SPHERE-OC-X = SPHERE-CENTER-X - RAY-ORIGIN-X
           COMPUTE SPHERE-OC-Y = SPHERE-CENTER-Y - RAY-ORIGIN-Y
           COMPUTE SPHERE-OC-Z = SPHERE-CENTER-Z - RAY-ORIGIN-Z
           
      * Calculate quadratic equation coefficients
      * auto a = dot(r.direction(), r.direction())
           COMPUTE SPHERE-A = (RAY-DIR-X * RAY-DIR-X) +
                              (RAY-DIR-Y * RAY-DIR-Y) +
                              (RAY-DIR-Z * RAY-DIR-Z)
                              
      * auto b = -2.0 * dot(r.direction(), oc)
           COMPUTE SPHERE-B = -2.0 * ((RAY-DIR-X * SPHERE-OC-X) +
                                      (RAY-DIR-Y * SPHERE-OC-Y) +
                                      (RAY-DIR-Z * SPHERE-OC-Z))
                                      
      * auto c = dot(oc, oc) - radius*radius
           COMPUTE SPHERE-C = (SPHERE-OC-X * SPHERE-OC-X) +
                              (SPHERE-OC-Y * SPHERE-OC-Y) +
                              (SPHERE-OC-Z * SPHERE-OC-Z) -
                              (SPHERE-RADIUS * SPHERE-RADIUS)
                              
      * Calculate discriminant: b*b - 4*a*c
           COMPUTE SPHERE-DISCRIMINANT = (SPHERE-B * SPHERE-B) -
                                         (4 * SPHERE-A * SPHERE-C)
                                         
      * Return hit distance or -1.0 if no hit
           IF SPHERE-DISCRIMINANT < 0
               MOVE -1.0 TO SPHERE-HIT-T            *> Miss - no intersection
           ELSE
      * Calculate nearest intersection: (-b - sqrt(discriminant)) / (2*a)
               COMPUTE SPHERE-HIT-T = (-SPHERE-B - 
                                      (SPHERE-DISCRIMINANT ** 0.5)) / 
                                      (2.0 * SPHERE-A)
           END-IF
           EXIT.
           
      *****************************************************************
      * RAY COLOR FUNCTION - Ray Tracing Color Calculation           *
      *****************************************************************
      
      *> Calculate color for a given ray (equivalent to ray_color function)
      *> C++ equivalent: color ray_color(const ray& r)
      *> Input: RAY-DATA contains the ray to process
      *> Output: PIXEL-COLOR contains the calculated color
      *> Renders sphere with surface normal coloring at (0,0,-1) with radius 0.5
       RAY-COLOR-FUNCTION.
      * Test for sphere intersection at point3(0,0,-1) with radius 0.5
      * auto t = hit_sphere(point3(0,0,-1), 0.5, r);
           MOVE 0.0 TO SPHERE-CENTER-X           *> Sphere at origin X
           MOVE 0.0 TO SPHERE-CENTER-Y           *> Sphere at origin Y
           MOVE -1.0 TO SPHERE-CENTER-Z          *> Sphere at Z = -1 (in front of camera)
           MOVE 0.5 TO SPHERE-RADIUS             *> Sphere radius = 0.5
           PERFORM HIT-SPHERE                    *> Get hit distance
           
      * If ray hits sphere (t > 0.0), calculate surface normal and color
           IF SPHERE-HIT-T > 0.0
      * Calculate hit point: r.at(t)
               MOVE SPHERE-HIT-T TO RAY-PARAMETER-T
               PERFORM RAY-AT-PARAMETER          *> Result in RAY-POINT-X/Y/Z
               
      * Calculate surface normal: unit_vector(hit_point - sphere_center)
      * vec3 N = unit_vector(r.at(t) - vec3(0,0,-1))
               COMPUTE VEC3-A-X = RAY-POINT-X - 0.0     *> hit_point - center
               COMPUTE VEC3-A-Y = RAY-POINT-Y - 0.0
               COMPUTE VEC3-A-Z = RAY-POINT-Z - (-1.0)
               PERFORM VEC3-UNIT-VECTOR-A        *> Normalize (result in VEC3-RESULT)
               
      * Color based on surface normal: 0.5*color(N.x()+1, N.y()+1, N.z()+1)
      * Map normal from [-1,1] to color range [0,1] and scale by 0.5 for softer look
               COMPUTE PIXEL-COLOR-R = 0.5 * (VEC3-RESULT-X + 1.0)
               COMPUTE PIXEL-COLOR-G = 0.5 * (VEC3-RESULT-Y + 1.0)
               COMPUTE PIXEL-COLOR-B = 0.5 * (VEC3-RESULT-Z + 1.0)
           ELSE
      * Otherwise render sky gradient (existing code)
      * Get the ray direction and normalize it to unit vector
      * vec3 unit_direction = unit_vector(r.direction());
               MOVE RAY-DIR-X TO VEC3-A-X        *> Copy ray direction to VEC3-A
               MOVE RAY-DIR-Y TO VEC3-A-Y
               MOVE RAY-DIR-Z TO VEC3-A-Z
               PERFORM VEC3-UNIT-VECTOR-A        *> Calculate unit vector (result in VEC3-RESULT)
               
      * Calculate interpolation parameter based on Y component
      * auto a = 0.5*(unit_direction.y() + 1.0);
      * This maps Y from [-1,1] to a from [0,1]
               COMPUTE VEC3-SCALAR = 0.5 * (VEC3-RESULT-Y + 1.0)
               
      * Linear interpolation between white and light blue
      * return (1.0-a)*color(1.0, 1.0, 1.0) + a*color(0.5, 0.7, 1.0);
      * White color when a=0 (Y=-1, looking down)
      * Blue color when a=1 (Y=+1, looking up)
      
      * Calculate (1.0-a) for white component weight
               COMPUTE VEC3-TEMP-CALC = 1.0 - VEC3-SCALAR
               
      * Calculate final color components using linear interpolation
      * Red:   (1-a)*1.0 + a*0.5 = (1-a) + 0.5*a
               COMPUTE PIXEL-COLOR-R = VEC3-TEMP-CALC * 1.0 + 
                                       VEC3-SCALAR * 0.5
               
      * Green: (1-a)*1.0 + a*0.7 = (1-a) + 0.7*a  
               COMPUTE PIXEL-COLOR-G = VEC3-TEMP-CALC * 1.0 + 
                                       VEC3-SCALAR * 0.7
               
      * Blue:  (1-a)*1.0 + a*1.0 = (1-a) + a = 1.0 (always full blue)
               COMPUTE PIXEL-COLOR-B = VEC3-TEMP-CALC * 1.0 + 
                                       VEC3-SCALAR * 1.0
           END-IF
           
      * Result: Creates sphere with surface normal shading on sky gradient background
      * - Sphere hit: Color based on surface normal direction (creates 3D shading effect)
      * - Sky background: White to blue gradient based on ray direction
           EXIT.
