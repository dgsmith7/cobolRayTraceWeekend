      *>****************************************************************
      *> RAY-TRACER - Simple PPM Image Generator                       *
      *> Generates a gradient image in PPM format                      *
      *> Based on "Ray Tracing in One Weekend" tutorial                *
      *>****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RAY-TRACER.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *> Define output file for PPM image format
           SELECT OUTPUT-FILE ASSIGN TO "image.ppm"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.*> File descriptor for PPM output file
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD           PIC X(80).  *> 80-char output record
       
       WORKING-STORAGE SECTION. *> Image dimensions and PPM format constants  
       01  IMAGE-WIDTH             PIC 9(3) VALUE 400.  *> Image width in pixels
      *> (updated from tutorial)
       01  IMAGE-HEIGHT            PIC 9(3).            *> Image height (calculated
      *> from aspect ratio)
       01  MAX-COLOR-VALUE         PIC 9(3) VALUE 255.  *> Max RGB value for PPM
       
      *> Loop iteration variables
       01  LOOP-COUNTERS.
           05  I                   PIC 9(3) VALUE 0.  *> X-axis pixel counter
           05  J                   PIC 9(3) VALUE 0.  *> Y-axis pixel counter
       
      *> Floating-point color values (0.0 to 1.0)
       01  COLOR-VALUES.
           05  R                   PIC 9V9(6) COMP-3.  *> Red component
           05  G                   PIC 9V9(6) COMP-3.  *> Green component
           05  B                   PIC 9V9(6) COMP-3 VALUE 0.0.  *> Blue (always 0)
       
      *> Integer RGB values for PPM output (0-255)
       01  RGB-INTEGERS.
           05  IR                  PIC 9(3).        *> Red integer value
           05  IG                  PIC 9(3).        *> Green integer value
           05  IB                  PIC 9(3) VALUE 0.        *> Blue integer value
       
      *> Temporary calculation variables
       01  TEMP-CALCULATIONS.
           05  TEMP-R              PIC 9V9(6) COMP-3.  *> Temp red calculation
           05  TEMP-G              PIC 9V9(6) COMP-3.  *> Temp green calculation
           05  WIDTH-MINUS-1       PIC 9(3).           *> Width - 1 for division
           05  HEIGHT-MINUS-1      PIC 9(3).           *> Height - 1 for division
           05  COLOR-MULTIPLIER    PIC 9(3)V9(3) COMP-3 VALUE 255.999.  *> Scale factor
       
      *> Output formatting variables
       01  OUTPUT-LINE             PIC X(20).  *> Formatted pixel RGB values
       01  HEADER-LINE             PIC X(20).  *> PPM header format line
       01  DIMENSION-LINE          PIC X(20).  *> Image dimensions line
       01  MAX-COLOR-LINE          PIC X(10).  *> Maximum color value line
       01  PROGRESS-LINE           PIC X(50).  *> Progress display formatting
       01  SCANLINES-REMAINING     PIC 9(3).   *> Countdown for progress
       
      *>****************************************************************
      *> VEC3 DATA STRUCTURES - 3D Vector Support                      *
      *>****************************************************************
     *> Primary vector structures (equivalent to vec3 objects)
       01  VEC3-A.                             *> First vector
           05  VEC3-A-X            PIC S9(3)V9(6) COMP-3.  *> X component (-999.999999 to +999.999999)
           05  VEC3-A-Y            PIC S9(3)V9(6) COMP-3.  *> Y component (-999.999999 to +999.999999)  
           05  VEC3-A-Z            PIC S9(3)V9(6) COMP-3.  *> Z component (-999.999999 to +999.999999)
           
       01  VEC3-B.                             *> Second vector
           05  VEC3-B-X            PIC S9(3)V9(6) COMP-3.  *> X component
           05  VEC3-B-Y            PIC S9(3)V9(6) COMP-3.  *> Y component
           05  VEC3-B-Z            PIC S9(3)V9(6) COMP-3.  *> Z component
           
       01  VEC3-RESULT.                        *> Result vector
           05  VEC3-RESULT-X       PIC S9(3)V9(6) COMP-3.  *> X component
           05  VEC3-RESULT-Y       PIC S9(3)V9(6) COMP-3.  *> Y component
           05  VEC3-RESULT-Z       PIC S9(3)V9(6) COMP-3.  *> Z component
           
       01  VEC3-TEMP.                          *> Temporary vector
           05  VEC3-TEMP-X         PIC S9(3)V9(6) COMP-3.  *> X component
           05  VEC3-TEMP-Y         PIC S9(3)V9(6) COMP-3.  *> Y component
           05  VEC3-TEMP-Z         PIC S9(3)V9(6) COMP-3.  *> Z component
           
      *> Vector calculation working variables
       01  VEC3-WORK-VARS.
           05  VEC3-SCALAR         PIC S9(3)V9(6) COMP-3.  *> Scalar multiplier (-999.999999 to +999.999999)
           05  VEC3-LENGTH         PIC 9(3)V9(6) COMP-3.   *> Vector length
           05  VEC3-LENGTH-SQR     PIC 9(6)V9(6) COMP-3.   *> Length squared (larger for squared values)
           05  VEC3-DOT-PRODUCT    PIC S9(6)V9(6) COMP-3.  *> Dot product result (can be large)
           05  VEC3-TEMP-CALC      PIC S9(3)V9(6) COMP-3.  *> General temp calc         
       01  VEC3-OUTPUT-LINE        PIC X(40).  *> For vector display
      *> Display formatting variables (COMP-3 cannot be used in STRING)
       01  VEC3-DISPLAY-VARS.                  *> For STRING operations
           05  VEC3-DISPLAY-X      PIC -(6)9.9(6).  *> X component in display format
           05  VEC3-DISPLAY-Y      PIC -(6)9.9(6).  *> Y component in display format
           05  VEC3-DISPLAY-Z      PIC -(6)9.9(6).  *> Z component in display format

      *>****************************************************************
      *> COLOR DATA STRUCTURES - Color Support (using color = vec3)    *
      *>****************************************************************
      *> Color structures (equivalent to color alias for vec3)
       01  PIXEL-COLOR.                        *> Current pixel color
           05  PIXEL-COLOR-R       PIC 9V9(6) COMP-3.  *> Red component (0.0-1.0)
           05  PIXEL-COLOR-G       PIC 9V9(6) COMP-3.  *> Green component (0.0-1.0)
           05  PIXEL-COLOR-B       PIC 9V9(6) COMP-3.  *> Blue component (0.0-1.0)
           
      *> Color output working variables
       01  COLOR-WORK-VARS.
           05  COLOR-R-BYTE        PIC 9(3).            *> Red byte value (0-255)
           05  COLOR-G-BYTE        PIC 9(3).            *> Green byte value (0-255)  
           05  COLOR-B-BYTE        PIC 9(3).            *> Blue byte value (0-255)
           05  COLOR-OUTPUT-LINE   PIC X(20).           *> Formatted color output
       
      *>****************************************************************
      *> RAY DATA STRUCTURES - 3D Ray Support (ray = origin + t*dir)   *
      *>****************************************************************
      *> Ray structure (equivalent to ray class with origin and direction)
       01  RAY-DATA.                           *> Primary ray structure
           05  RAY-ORIGIN.                     *> Ray origin point (point3)
               10  RAY-ORIGIN-X    PIC S9V9(6) COMP-3.  *> Origin X coordinate
               10  RAY-ORIGIN-Y    PIC S9V9(6) COMP-3.  *> Origin Y coordinate
               10  RAY-ORIGIN-Z    PIC S9V9(6) COMP-3.  *> Origin Z coordinate
           05  RAY-DIRECTION.                  *> Ray direction vector (vec3)
               10  RAY-DIR-X       PIC S9V9(6) COMP-3.  *> Direction X component
               10  RAY-DIR-Y       PIC S9V9(6) COMP-3.  *> Direction Y component
               10  RAY-DIR-Z       PIC S9V9(6) COMP-3.  *> Direction Z component
               
      *> Secondary ray for calculations (equivalent to multiple ray objects)
       01  RAY-TEMP.                          *> Temporary ray structure
           05  RAY-TEMP-ORIGIN.               *> Temporary origin point
               10  RAY-TEMP-ORIG-X PIC S9V9(6) COMP-3.  *> Temp origin X
               10  RAY-TEMP-ORIG-Y PIC S9V9(6) COMP-3.  *> Temp origin Y
               10  RAY-TEMP-ORIG-Z PIC S9V9(6) COMP-3.  *> Temp origin Z
           05  RAY-TEMP-DIRECTION.            *> Temporary direction vector
               10  RAY-TEMP-DIR-X  PIC S9V9(6) COMP-3.  *> Temp direction X
               10  RAY-TEMP-DIR-Y  PIC S9V9(6) COMP-3.  *> Temp direction Y
               10  RAY-TEMP-DIR-Z  PIC S9V9(6) COMP-3.  *> Temp direction Z
               
      *> Ray calculation working variables
       01  RAY-WORK-VARS.
           05  RAY-PARAMETER-T     PIC S9V9(6) COMP-3.  *> Ray parameter t
           05  RAY-POINT-X         PIC S9V9(6) COMP-3.  *> Calculated point X
           05  RAY-POINT-Y         PIC S9V9(6) COMP-3.  *> Calculated point Y
           05  RAY-POINT-Z         PIC S9V9(6) COMP-3.  *> Calculated point Z
           
      *>****************************************************************
      *> CAMERA DATA STRUCTURES - 3D Camera and Viewport Support       *
      *>****************************************************************
      *> Camera parameters (equivalent to camera setup variables)
       01  CAMERA-PARAMS.
           05  ASPECT-RATIO        PIC 9V9(6) COMP-3 VALUE 1.777777.  *> 16/9 aspect ratio
           05  FOCAL-LENGTH        PIC 9V9(6) COMP-3 VALUE 1.0.      *> Camera focal length
           05  VIEWPORT-HEIGHT     PIC 9V9(6) COMP-3 VALUE 2.0.      *> Viewport height
           05  VIEWPORT-WIDTH      PIC 9V9(6) COMP-3.                *> Calculated viewport width
           
      *> Camera center point (equivalent to camera_center)
       01  CAMERA-CENTER.
           05  CAMERA-CENTER-X     PIC S9V9(6) COMP-3 VALUE 0.0.     *> Camera X position
           05  CAMERA-CENTER-Y     PIC S9V9(6) COMP-3 VALUE 0.0.     *> Camera Y position
           05  CAMERA-CENTER-Z     PIC S9V9(6) COMP-3 VALUE 0.0.     *> Camera Z position
           
      *> Viewport vectors (equivalent to viewport_u, viewport_v)
       01  VIEWPORT-VECTORS.
           05  VIEWPORT-U-X        PIC S9V9(6) COMP-3.               *> U vector X component
           05  VIEWPORT-U-Y        PIC S9V9(6) COMP-3 VALUE 0.0.     *> U vector Y component
           05  VIEWPORT-U-Z        PIC S9V9(6) COMP-3 VALUE 0.0.     *> U vector Z component
           05  VIEWPORT-V-X        PIC S9V9(6) COMP-3 VALUE 0.0.     *> V vector X component
           05  VIEWPORT-V-Y        PIC S9V9(6) COMP-3.               *> V vector Y component
           05  VIEWPORT-V-Z        PIC S9V9(6) COMP-3 VALUE 0.0.     *> V vector Z component
               
      *> Pixel delta vectors (equivalent to pixel_delta_u, pixel_delta_v)
       01  PIXEL-DELTAS.
           05  PIXEL-DELTA-U-X     PIC S9V9(6) COMP-3.               *> Delta U X component
           05  PIXEL-DELTA-U-Y     PIC S9V9(6) COMP-3.               *> Delta U Y component
           05  PIXEL-DELTA-U-Z     PIC S9V9(6) COMP-3.               *> Delta U Z component
           05  PIXEL-DELTA-V-X     PIC S9V9(6) COMP-3.               *> Delta V X component
           05  PIXEL-DELTA-V-Y     PIC S9V9(6) COMP-3.               *> Delta V Y component
           05  PIXEL-DELTA-V-Z     PIC S9V9(6) COMP-3.               *> Delta V Z component
               
      *> Viewport positioning (equivalent to viewport_upper_left, pixel00_loc)
       01  VIEWPORT-POSITIONS.
           05  VIEWPORT-UL-X       PIC S9V9(6) COMP-3.               *> Upper left X
           05  VIEWPORT-UL-Y       PIC S9V9(6) COMP-3.               *> Upper left Y
           05  VIEWPORT-UL-Z       PIC S9V9(6) COMP-3.               *> Upper left Z
           05  PIXEL00-X           PIC S9V9(6) COMP-3.               *> Pixel (0,0) X
           05  PIXEL00-Y           PIC S9V9(6) COMP-3.               *> Pixel (0,0) Y
           05  PIXEL00-Z           PIC S9V9(6) COMP-3.               *> Pixel (0,0) Z
               
      *> Current pixel calculations
       01  PIXEL-CALCULATIONS.
           05  PIXEL-CENTER-X      PIC S9V9(6) COMP-3.               *> Pixel center X
           05  PIXEL-CENTER-Y      PIC S9V9(6) COMP-3.               *> Pixel center Y
           05  PIXEL-CENTER-Z      PIC S9V9(6) COMP-3.               *> Pixel center Z
           05  RAY-DIR-CALC-X      PIC S9V9(6) COMP-3.               *> Ray direction X
           05  RAY-DIR-CALC-Y      PIC S9V9(6) COMP-3.               *> Ray direction Y
           05  RAY-DIR-CALC-Z      PIC S9V9(6) COMP-3.               *> Ray direction Z
           
      *>****************************************************************
      *> SPHERE INTERSECTION DATA - Ray-Sphere Collision Detection     *
      *>****************************************************************
      *> Sphere parameters for intersection testing
       01  SPHERE-DATA.
           05  SPHERE-CENTER-X     PIC S9(3)V9(6) COMP-3.           *> Sphere center X
           05  SPHERE-CENTER-Y     PIC S9(3)V9(6) COMP-3.           *> Sphere center Y
           05  SPHERE-CENTER-Z     PIC S9(3)V9(6) COMP-3.           *> Sphere center Z
           05  SPHERE-RADIUS       PIC 9(3)V9(6) COMP-3.            *> Sphere radius
           
      *> Ray-sphere intersection working variables  
       01  SPHERE-INTERSECTION-VARS.
           05  SPHERE-OC-X         PIC S9(3)V9(6) COMP-3.           *> Origin to center X
           05  SPHERE-OC-Y         PIC S9(3)V9(6) COMP-3.           *> Origin to center Y
           05  SPHERE-OC-Z         PIC S9(3)V9(6) COMP-3.           *> Origin to center Z
           05  SPHERE-A            PIC 9(6)V9(6) COMP-3.            *> Length squared of ray direction
           05  SPHERE-H            PIC S9(3)V9(6) COMP-3.           *> Dot product of direction and oc
           05  SPHERE-C            PIC S9(6)V9(6) COMP-3.           *> oc.length_squared - radius²
           05  SPHERE-DISCRIMINANT PIC S9(6)V9(6) COMP-3.           *> h²-ac discriminant
           05  SPHERE-HIT-FLAG     PIC 9 VALUE 0.                    *> 1=hit, 0=miss
           05  SPHERE-HIT-T        PIC S9V9(6) COMP-3.               *> Hit distance (t parameter)
           
      *>****************************************************************
      *> HITTABLE ABSTRACT CLASS - Polymorphic Object System           *
      *>****************************************************************
      *> Hit record structure (equivalent to hit_record class)
      *> Contains intersection information when ray hits an object
       01  HIT-RECORD.
           05  HIT-POINT.                          *> Intersection point (point3 p)
               10  HIT-POINT-X     PIC S9(3)V9(6) COMP-3.  *> Hit point X coordinate (larger)
               10  HIT-POINT-Y     PIC S9(3)V9(6) COMP-3.  *> Hit point Y coordinate (larger)
               10  HIT-POINT-Z     PIC S9(3)V9(6) COMP-3.  *> Hit point Z coordinate (larger)
           05  HIT-NORMAL.                         *> Surface normal (vec3 normal)
               10  HIT-NORMAL-X    PIC S9V9(6) COMP-3.     *> Normal X component (can stay smaller)
               10  HIT-NORMAL-Y    PIC S9V9(6) COMP-3.     *> Normal Y component (can stay smaller)
               10  HIT-NORMAL-Z    PIC S9V9(6) COMP-3.     *> Normal Z component (can stay smaller)
           05  HIT-T               PIC S9(6)V9(6) COMP-3.   *> Ray parameter t (distance) - larger
           05  HIT-FRONT-FACE      PIC 9 VALUE 0.           *> 1=ray hits front face, 0=back face
           05  HIT-OCCURRED        PIC 9 VALUE 0.           *> 1=hit detected, 0=no hit           
      *> Hittable object structure (equivalent to abstract hittable class)
      *> Uses discriminated union pattern for polymorphism
       01  HITTABLE-OBJECT.
           05  HITTABLE-TYPE       PIC X(10).            *> Object type discriminator
           05  HITTABLE-DATA       PIC X(50).            *> Variant data (union-like)
            05  SPHERE-OBJ REDEFINES HITTABLE-DATA.      *> Sphere object data
               10  SPHERE-OBJ-CENTER-X PIC S9(3)V9(6) COMP-3. *> Allow larger values
               10  SPHERE-OBJ-CENTER-Y PIC S9(3)V9(6) COMP-3. *> Allow -100.5
               10  SPHERE-OBJ-CENTER-Z PIC S9(3)V9(6) COMP-3. *> Allow negative Z
               10  SPHERE-OBJ-RADIUS   PIC 9(3)V9(6) COMP-3.  *> Allow radius up to 999.999999
               10  FILLER              PIC X(10).             *> Adjust padding             
   *> Ray intersection parameters (equivalent to function parameters)
       01  HITTABLE-HIT-PARAMS.
           05  HIT-RAY-TMIN        PIC S9(3)V9(6) COMP-3.  *> Allow larger range
           05  HIT-RAY-TMAX        PIC S9(6)V9(6) COMP-3.  *> Allow much larger range for infinity
           05  HIT-RESULT          PIC 9 VALUE 0.          *> Return value: 1=hit, 0=miss           
      *>****************************************************************
      *> SPHERE CLASS - Concrete Hittable Implementation               *
      *>****************************************************************
     *> Sphere working variables for enhanced intersection calculations
       01  SPHERE-WORK-VARS.
           05  SPHERE-SQRTD        PIC S9(3)V9(6) COMP-3.  *> Square root of discriminant
           05  SPHERE-ROOT1        PIC S9(6)V9(6) COMP-3.  *> First intersection root - larger
           05  SPHERE-ROOT2        PIC S9(6)V9(6) COMP-3.  *> Second intersection root - larger  
           05  SPHERE-FINAL-ROOT   PIC S9(6)V9(6) COMP-3.  *> Selected valid root - larger           
      *> Working variables for set_face_normal method
       01  SET-FACE-NORMAL-VARS.
           05  OUTWARD-NORMAL-X    PIC S9V9(6) COMP-3.  *> Outward normal X component
           05  OUTWARD-NORMAL-Y    PIC S9V9(6) COMP-3.  *> Outward normal Y component
           05  OUTWARD-NORMAL-Z    PIC S9V9(6) COMP-3.  *> Outward normal Z component
           05  DOT-PRODUCT-RESULT  PIC S9V9(6) COMP-3.  *> Dot product for front face test
           
      *>****************************************************************
      *> HITTABLE LIST CLASS - Collection of Hittable Objects          *
      *>****************************************************************
      *> HittableList structure (equivalent to hittable_list class)
      *> Manages collection of hittable objects with closest hit detection
       01  HITTABLE-LIST.
           05  HITTABLE-COUNT      PIC 9(3) VALUE 0.           *> Number of active objects
           05  HITTABLE-MAX        PIC 9(3) VALUE 100.         *> Maximum capacity
           05  HITTABLE-OBJECTS OCCURS 100 TIMES.             *> Array of objects
               10  HITTABLE-ACTIVE PIC 9 VALUE 0.             *> 1=active, 0=empty slot
               10  HITTABLE-TYPE   PIC X(10).                 *> Object type discriminator
               10  HITTABLE-DATA   PIC X(50).                 *> Object-specific data (union)
                    10  SPHERE-OBJ REDEFINES HITTABLE-DATA.        *> Sphere object data
                   15  SPHERE-OBJ-CENTER-X PIC S9(3)V9(6) COMP-3.
                   15  SPHERE-OBJ-CENTER-Y PIC S9(3)V9(6) COMP-3.
                   15  SPHERE-OBJ-CENTER-Z PIC S9(3)V9(6) COMP-3.
                   15  SPHERE-OBJ-RADIUS   PIC 9(3)V9(6) COMP-3.
                   15  FILLER              PIC X(10).     *> Adjust padding                 
        *> Working variables for HittableList operations
       01  HITTABLE-LIST-VARS.
           05  LIST-INDEX          PIC 9(3).                   *> Loop counter for list iteration
           05  NEXT-SLOT           PIC 9(3).                   *> Next available slot index
           05  SLOT-FOUND          PIC 9 VALUE 0.              *> 1=found empty slot, 0=list full
           05  CLOSEST-T           PIC S9(6)V9(6) COMP-3.      *> Closest intersection distance (larger)
           05  HIT-ANYTHING        PIC 9 VALUE 0.              *> 1=found any hit, 0=no hits
           05  TEMP-TMIN           PIC S9(3)V9(6) COMP-3.      *> Temporary storage for ray tmin           
      *> Temporary hit record for list processing (equivalent to temp_rec)
       01  TEMP-HIT-RECORD.
           05  TEMP-HIT-POINT.
               10  TEMP-HIT-POINT-X PIC S9(3)V9(6) COMP-3.  *> Larger for distant objects
               10  TEMP-HIT-POINT-Y PIC S9(3)V9(6) COMP-3.  *> Larger for distant objects
               10  TEMP-HIT-POINT-Z PIC S9(3)V9(6) COMP-3.  *> Larger for distant objects
           05  TEMP-HIT-NORMAL.
               10  TEMP-HIT-NORMAL-X PIC S9V9(6) COMP-3.    *> Normals can stay smaller
               10  TEMP-HIT-NORMAL-Y PIC S9V9(6) COMP-3.    *> Normals can stay smaller
               10  TEMP-HIT-NORMAL-Z PIC S9V9(6) COMP-3.    *> Normals can stay smaller
           05  TEMP-HIT-T          PIC S9(6)V9(6) COMP-3.   *> Distance - larger range
           05  TEMP-HIT-FRONT-FACE PIC 9 VALUE 0.
           05  TEMP-HIT-OCCURRED   PIC 9 VALUE 0.
           
      *>****************************************************************
      *> MATHEMATICAL CONSTANTS - Ray Tracing Utilities (rtweekend.h)  *
      *>****************************************************************
      *> Mathematical constants (equivalent to rtweekend.h constants)
       01  MATH-CONSTANTS.
           05  PI-VALUE      PIC 9V9(15) COMP-3 VALUE 3.141592653589793.
           05  INFINITY-VALUE      PIC 9(6) COMP-3 VALUE 999999.
           05 DEGREES-TO-RAD PIC 9V9(15) COMP-3 VALUE 0.017453292519943.
           
      *> Utility function working variables
       01  UTILITY-WORK-VARS.
           05  DEGREES-INPUT       PIC S9V9(6) COMP-3.                         *> Input degrees value
           05  RADIANS-OUTPUT      PIC S9V9(6) COMP-3.                         *> Output radians value
           05  TEMP-CALC           PIC S9V9(12) COMP-3.                        *> Temporary calculation
           
       PROCEDURE DIVISION.
      *>****************************************************************
      *> Main program execution flow                                  *
      *>****************************************************************
       MAIN-PROGRAM.
           PERFORM OPEN-OUTPUT-FILE     *> Open PPM file for writing
           PERFORM INITIALIZE-VALUES    *> Set up calculation constants
           PERFORM OUTPUT-HEADER        *> Write PPM file header
           PERFORM RENDER-IMAGE         *> Generate pixel data with progress
           PERFORM CLOSE-OUTPUT-FILE    *> Close the output file
           DISPLAY "PPM file 'image.ppm' created successfully!"
           STOP RUN.
       
      *> File handling procedures
       OPEN-OUTPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.     *> Open file for writing
       
       CLOSE-OUTPUT-FILE.
           CLOSE OUTPUT-FILE.           *> Close and finalize file
       
      *> Initialize camera system and image parameters
       INITIALIZE-VALUES.
      *> Calculate image height from aspect ratio (16:9)
      *> Ensure height is at least 1 pixel
           COMPUTE IMAGE-HEIGHT = IMAGE-WIDTH / ASPECT-RATIO
           IF IMAGE-HEIGHT < 1
               MOVE 1 TO IMAGE-HEIGHT
           END-IF
           
      *>****************************************************************
      *> WORLD SETUP - Create scene with multiple spheres             *
      *>****************************************************************
      *> World setup (equivalent to C++ world creation)
      *> hittable_list world;
           PERFORM HITTABLE-LIST-CONSTRUCTOR       *> Initialize empty world
           
      *> world.add(make_shared<sphere>(point3(0,0,-1), 0.5));
           MOVE 0.0 TO VEC3-A-X                    *> First sphere center
           MOVE 0.0 TO VEC3-A-Y
           MOVE -1.0 TO VEC3-A-Z
           MOVE 0.5 TO VEC3-SCALAR                 *> First sphere radius
           PERFORM HITTABLE-LIST-ADD-SPHERE        *> Add first sphere to world
           
      *> world.add(make_shared<sphere>(point3(0,-100.5,-1), 100));
           MOVE 0.0 TO VEC3-A-X                    *> Ground sphere center
           MOVE -100.5 TO VEC3-A-Y                 *> Large ground sphere below
           MOVE -1.0 TO VEC3-A-Z
           MOVE 100.0 TO VEC3-SCALAR               *> Large ground sphere radius
           PERFORM HITTABLE-LIST-ADD-SPHERE        *> Add ground sphere to world
           
           DISPLAY "World created with " HITTABLE-COUNT OF HITTABLE-LIST " objects"      
      *>****************************************************************
      *> CAMERA SETUP - Camera and viewport calculations               *
      *>****************************************************************
           
      *>> Calculate viewport width from height and image aspect ratio
           COMPUTE VIEWPORT-WIDTH = VIEWPORT-HEIGHT * 
                                   (IMAGE-WIDTH / IMAGE-HEIGHT)
                                   
      *> Set up viewport vectors
      *> viewport_u = vec3(viewport_width, 0, 0)
           MOVE VIEWPORT-WIDTH TO VIEWPORT-U-X
           MOVE 0 TO VIEWPORT-U-Y
           MOVE 0 TO VIEWPORT-U-Z
           
      *> viewport_v = vec3(0, -viewport_height, 0) 
           MOVE 0 TO VIEWPORT-V-X
           COMPUTE VIEWPORT-V-Y = -VIEWPORT-HEIGHT  *> Negative for screen coordinates
           MOVE 0 TO VIEWPORT-V-Z
           
      *> Calculate pixel delta vectors (spacing between pixels)
      *> pixel_delta_u = viewport_u / image_width
           COMPUTE PIXEL-DELTA-U-X = VIEWPORT-U-X / IMAGE-WIDTH
           COMPUTE PIXEL-DELTA-U-Y = VIEWPORT-U-Y / IMAGE-WIDTH  
           COMPUTE PIXEL-DELTA-U-Z = VIEWPORT-U-Z / IMAGE-WIDTH
           
      *> pixel_delta_v = viewport_v / image_height
           COMPUTE PIXEL-DELTA-V-X = VIEWPORT-V-X / IMAGE-HEIGHT
           COMPUTE PIXEL-DELTA-V-Y = VIEWPORT-V-Y / IMAGE-HEIGHT
           COMPUTE PIXEL-DELTA-V-Z = VIEWPORT-V-Z / IMAGE-HEIGHT
           
      *> Calculate viewport upper left corner
      *> viewport_upper_left = camera_center - vec3(0,0,focal_length) - viewport_u/2 - viewport_v/2
           COMPUTE VIEWPORT-UL-X = CAMERA-CENTER-X - 
                  (VIEWPORT-U-X / 2)
           COMPUTE VIEWPORT-UL-Y = CAMERA-CENTER-Y - 
                  (VIEWPORT-V-Y / 2)
           COMPUTE VIEWPORT-UL-Z = CAMERA-CENTER-Z - FOCAL-LENGTH
           
      *> Calculate pixel (0,0) location 
      *> pixel00_loc = viewport_upper_left + 0.5 * (pixel_delta_u + pixel_delta_v)
           COMPUTE PIXEL00-X = VIEWPORT-UL-X + 
                  0.5 * (PIXEL-DELTA-U-X + PIXEL-DELTA-V-X)
           COMPUTE PIXEL00-Y = VIEWPORT-UL-Y + 
                  0.5 * (PIXEL-DELTA-U-Y + PIXEL-DELTA-V-Y)
           COMPUTE PIXEL00-Z = VIEWPORT-UL-Z + 
                  0.5 * (PIXEL-DELTA-U-Z + PIXEL-DELTA-V-Z)
           
      *> Legacy calculations for compatibility
           COMPUTE WIDTH-MINUS-1 = IMAGE-WIDTH - 1   *> For normalizing X coords
           COMPUTE HEIGHT-MINUS-1 = IMAGE-HEIGHT - 1. *> For normalizing Y coords
           EXIT.
       
      *> Write PPM file header (P3 format)
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
       
      *> Main image rendering loop - generates gradient pattern
       RENDER-IMAGE.
      *> Outer loop: iterate through each row (Y-axis)
           PERFORM VARYING J FROM 0 BY 1 UNTIL J >= IMAGE-HEIGHT
               PERFORM DISPLAY-PROGRESS     *> Show progress to terminal
      *> Inner loop: iterate through each column (X-axis)
               PERFORM VARYING I FROM 0 BY 1 UNTIL I >= IMAGE-WIDTH
                   PERFORM CALCULATE-PIXEL-COLOR  *> Calculate RGB values
                   PERFORM OUTPUT-PIXEL           *> Write pixel to file
               END-PERFORM
           END-PERFORM
           PERFORM DISPLAY-COMPLETION.      *> Show completion message
       
      *> Calculate RGB values for current pixel using ray tracing
       CALCULATE-PIXEL-COLOR.
      *> Calculate pixel center in 3D space
      *> pixel_center = pixel00_loc + (i * pixel_delta_u) + (j * pixel_delta_v)
           COMPUTE PIXEL-CENTER-X = PIXEL00-X + 
                  (I * PIXEL-DELTA-U-X) + (J * PIXEL-DELTA-V-X)
           COMPUTE PIXEL-CENTER-Y = PIXEL00-Y + 
                  (I * PIXEL-DELTA-U-Y) + (J * PIXEL-DELTA-V-Y)
           COMPUTE PIXEL-CENTER-Z = PIXEL00-Z + 
                  (I * PIXEL-DELTA-U-Z) + (J * PIXEL-DELTA-V-Z)
           
      *> Calculate ray direction from camera center to pixel center
      *> ray_direction = pixel_center - camera_center
           COMPUTE RAY-DIR-CALC-X = PIXEL-CENTER-X - CAMERA-CENTER-X
           COMPUTE RAY-DIR-CALC-Y = PIXEL-CENTER-Y - CAMERA-CENTER-Y
           COMPUTE RAY-DIR-CALC-Z = PIXEL-CENTER-Z - CAMERA-CENTER-Z
           
      *> Set up ray with camera center as origin and calculated direction
      *> ray r(camera_center, ray_direction)
           MOVE CAMERA-CENTER-X TO VEC3-A-X    *> Origin = camera center
           MOVE CAMERA-CENTER-Y TO VEC3-A-Y
           MOVE CAMERA-CENTER-Z TO VEC3-A-Z
           MOVE RAY-DIR-CALC-X TO VEC3-B-X      *> Direction = calculated direction
           MOVE RAY-DIR-CALC-Y TO VEC3-B-Y
           MOVE RAY-DIR-CALC-Z TO VEC3-B-Z
           PERFORM RAY-CONSTRUCT-WITH-PARAMS    *> Create the ray
           
      *> Get pixel color using ray_color function
      *> color pixel_color = ray_color(r)
           PERFORM RAY-COLOR-FUNCTION           *> Calculate color for this ray
           
      *> Result is now in PIXEL-COLOR, ready for output
           EXIT.
       
      *> Write pixel color to PPM file using write_color function
       OUTPUT-PIXEL.
      *> Equivalent to: write_color(std::cout, pixel_color);
           PERFORM WRITE-COLOR-TO-FILE.
       
      *> Display rendering progress to terminal
       DISPLAY-PROGRESS.
           COMPUTE SCANLINES-REMAINING = IMAGE-HEIGHT - J  *> Count down remaining rows
           STRING "Scanlines remaining: " SCANLINES-REMAINING " "
                  DELIMITED BY SIZE INTO PROGRESS-LINE
           DISPLAY PROGRESS-LINE.           *> Show progress on terminal
           EXIT.
       
      *> Display completion message
       DISPLAY-COMPLETION.
           DISPLAY "Done.                    ".  *> Clear progress line and show done
           
      *>****************************************************************
      *> VEC3 PROCEDURES - 3D Vector Operations                       *
      *>****************************************************************
      
      *> Initialize vector to zero (default constructor equivalent)
       VEC3-INIT-ZERO.
           MOVE 0 TO VEC3-A-X
           MOVE 0 TO VEC3-A-Y  
           MOVE 0 TO VEC3-A-Z.
           
      *> Initialize VEC3-A with specific values (parameterized constructor)
      *> Call with values in VEC3-TEMP before calling this procedure
       VEC3-INIT-VALUES-A.
           MOVE VEC3-TEMP-X TO VEC3-A-X
           MOVE VEC3-TEMP-Y TO VEC3-A-Y
           MOVE VEC3-TEMP-Z TO VEC3-A-Z.
           
      *> Initialize VEC3-B with specific values
       VEC3-INIT-VALUES-B.
           MOVE VEC3-TEMP-X TO VEC3-B-X
           MOVE VEC3-TEMP-Y TO VEC3-B-Y
           MOVE VEC3-TEMP-Z TO VEC3-B-Z.

      *> Copy VEC3-A to VEC3-RESULT
       VEC3-COPY-A-TO-RESULT.
           MOVE VEC3-A-X TO VEC3-RESULT-X
           MOVE VEC3-A-Y TO VEC3-RESULT-Y
           MOVE VEC3-A-Z TO VEC3-RESULT-Z.
           
      *>****************************************************************
      *> VEC3 ACCESSOR METHODS - Component Access                      *
      *>****************************************************************
      *> Accessor procedures (equivalent to x(), y(), z() methods)
      *> Get X component of VEC3-A (result in VEC3-TEMP-CALC)
       VEC3-GET-X-A.
           MOVE VEC3-A-X TO VEC3-TEMP-CALC.
           
      *> Get Y component of VEC3-A  
       VEC3-GET-Y-A.
           MOVE VEC3-A-Y TO VEC3-TEMP-CALC.
           
      *> Get Z component of VEC3-A
       VEC3-GET-Z-A.
           MOVE VEC3-A-Z TO VEC3-TEMP-CALC.
           
      *> Array-style access for VEC3-A (index in VEC3-SCALAR, result in VEC3-TEMP-CALC)
      *> Equivalent to: vec3[index] where index 0=X, 1=Y, 2=Z
       VEC3-GET-ELEMENT-A.
           EVALUATE VEC3-SCALAR
               WHEN 0 MOVE VEC3-A-X TO VEC3-TEMP-CALC  *> Return X component
               WHEN 1 MOVE VEC3-A-Y TO VEC3-TEMP-CALC  *> Return Y component
               WHEN 2 MOVE VEC3-A-Z TO VEC3-TEMP-CALC  *> Return Z component
               WHEN OTHER MOVE 0 TO VEC3-TEMP-CALC     *> Invalid index
           END-EVALUATE.
           
      *>****************************************************************
      *> VEC3 ARITHMETIC OPERATIONS - Vector Math                     *
      *>****************************************************************
      *> Vector addition: VEC3-RESULT = VEC3-A + VEC3-B (operator+ equivalent)
       VEC3-ADD.
           ADD VEC3-A-X TO VEC3-B-X GIVING VEC3-RESULT-X  *> Result X = A.X + B.X
           ADD VEC3-A-Y TO VEC3-B-Y GIVING VEC3-RESULT-Y  *> Result Y = A.Y + B.Y
           ADD VEC3-A-Z TO VEC3-B-Z GIVING VEC3-RESULT-Z. *> Result Z = A.Z + B.Z
           
      *> Vector subtraction: VEC3-RESULT = VEC3-A - VEC3-B (operator- equivalent)
       VEC3-SUBTRACT.
           SUBTRACT VEC3-B-X FROM VEC3-A-X GIVING VEC3-RESULT-X  *> Result X = A.X - B.X
           SUBTRACT VEC3-B-Y FROM VEC3-A-Y GIVING VEC3-RESULT-Y  *> Result Y = A.Y - B.Y
           SUBTRACT VEC3-B-Z FROM VEC3-A-Z GIVING VEC3-RESULT-Z. *> Result Z = A.Z - B.Z
           
      *> Vector negation: VEC3-RESULT = -VEC3-A (unary operator- equivalent)
       VEC3-NEGATE-A.
           COMPUTE VEC3-RESULT-X = -VEC3-A-X  *> Negate X component
           COMPUTE VEC3-RESULT-Y = -VEC3-A-Y  *> Negate Y component
           COMPUTE VEC3-RESULT-Z = -VEC3-A-Z. *> Negate Z component
           
      *> Scalar multiplication: VEC3-RESULT = VEC3-SCALAR * VEC3-A (t * vec equivalent)
       VEC3-MULTIPLY-SCALAR-A.
           COMPUTE VEC3-RESULT-X = VEC3-SCALAR * VEC3-A-X  *> Scale X component
           COMPUTE VEC3-RESULT-Y = VEC3-SCALAR * VEC3-A-Y  *> Scale Y component
           COMPUTE VEC3-RESULT-Z = VEC3-SCALAR * VEC3-A-Z. *> Scale Z component
           
      *> Component-wise multiplication: VEC3-RESULT = VEC3-A * VEC3-B (Hadamard product)
       VEC3-MULTIPLY-VECTORS.
           COMPUTE VEC3-RESULT-X = VEC3-A-X * VEC3-B-X  *> Multiply X components
           COMPUTE VEC3-RESULT-Y = VEC3-A-Y * VEC3-B-Y  *> Multiply Y components
           COMPUTE VEC3-RESULT-Z = VEC3-A-Z * VEC3-B-Z. *> Multiply Z components
           
      *> Scalar division: VEC3-RESULT = VEC3-A / VEC3-SCALAR (vec / t equivalent)
       VEC3-DIVIDE-SCALAR-A.
           COMPUTE VEC3-RESULT-X = VEC3-A-X / VEC3-SCALAR  *> Divide X component
           COMPUTE VEC3-RESULT-Y = VEC3-A-Y / VEC3-SCALAR  *> Divide Y component
           COMPUTE VEC3-RESULT-Z = VEC3-A-Z / VEC3-SCALAR. *> Divide Z component
           
      *>****************************************************************
      *> VEC3 IN-PLACE OPERATIONS - Modify Vector In Place            *
      *>****************************************************************
      *> VEC3-A += VEC3-B (operator+= equivalent)
       VEC3-ADD-TO-A.
           ADD VEC3-B-X TO VEC3-A-X  *> A.X += B.X
           ADD VEC3-B-Y TO VEC3-A-Y  *> A.Y += B.Y
           ADD VEC3-B-Z TO VEC3-A-Z. *> A.Z += B.Z
           
      *> VEC3-A *= VEC3-SCALAR (operator*= equivalent)
       VEC3-MULTIPLY-A-BY-SCALAR.
           COMPUTE VEC3-A-X = VEC3-A-X * VEC3-SCALAR  *> A.X *= scalar
           COMPUTE VEC3-A-Y = VEC3-A-Y * VEC3-SCALAR  *> A.Y *= scalar
           COMPUTE VEC3-A-Z = VEC3-A-Z * VEC3-SCALAR. *> A.Z *= scalar
           
      *> VEC3-A /= VEC3-SCALAR (operator/= equivalent)
       VEC3-DIVIDE-A-BY-SCALAR.
           COMPUTE VEC3-A-X = VEC3-A-X / VEC3-SCALAR  *> A.X /= scalar
           COMPUTE VEC3-A-Y = VEC3-A-Y / VEC3-SCALAR  *> A.Y /= scalar
           COMPUTE VEC3-A-Z = VEC3-A-Z / VEC3-SCALAR. *> A.Z /= scalar
           
      *>****************************************************************
      *> VEC3 UTILITY FUNCTIONS - Advanced Vector Operations          *
      *>****************************************************************
      *> Calculate length squared of VEC3-A: |A|² = A.X² + A.Y² + A.Z²
      *> (Result stored in VEC3-LENGTH-SQR - more efficient than length)
       VEC3-LENGTH-SQUARED-A.
           COMPUTE VEC3-LENGTH-SQR = (VEC3-A-X * VEC3-A-X) + 
                                     (VEC3-A-Y * VEC3-A-Y) + 
                                     (VEC3-A-Z * VEC3-A-Z).
                                     
      *> Calculate length (magnitude) of VEC3-A: |A| = √(A.X² + A.Y² + A.Z²)
      *> (Result stored in VEC3-LENGTH)
       VEC3-LENGTH-A.
           PERFORM VEC3-LENGTH-SQUARED-A      *> First get length squared
           COMPUTE VEC3-LENGTH = VEC3-LENGTH-SQR ** 0.5.  *> Then take square root
           
      *> Dot product: VEC3-DOT-PRODUCT = VEC3-A • VEC3-B = A.X*B.X + A.Y*B.Y + A.Z*B.Z
      *> (Measures how parallel two vectors are)
       VEC3-CALCULATE-DOT-PRODUCT.
           COMPUTE VEC3-DOT-PRODUCT = (VEC3-A-X * VEC3-B-X) +
                                      (VEC3-A-Y * VEC3-B-Y) +
                                      (VEC3-A-Z * VEC3-B-Z).
                                      
      *> Cross product: VEC3-RESULT = VEC3-A × VEC3-B
      *> Creates a vector perpendicular to both A and B
      *> Formula: (A.Y*B.Z - A.Z*B.Y, A.Z*B.X - A.X*B.Z, A.X*B.Y - A.Y*B.X)
       VEC3-CROSS-PRODUCT.
           COMPUTE VEC3-RESULT-X = (VEC3-A-Y * VEC3-B-Z) - 
                                   (VEC3-A-Z * VEC3-B-Y)  *> X = A.Y*B.Z - A.Z*B.Y
           COMPUTE VEC3-RESULT-Y = (VEC3-A-Z * VEC3-B-X) - 
                                   (VEC3-A-X * VEC3-B-Z)  *> Y = A.Z*B.X - A.X*B.Z
           COMPUTE VEC3-RESULT-Z = (VEC3-A-X * VEC3-B-Y) - 
                                   (VEC3-A-Y * VEC3-B-X). *> Z = A.X*B.Y - A.Y*B.X
           
      *> Unit vector: VEC3-RESULT = VEC3-A / |VEC3-A| (normalize to length 1)
      *> Creates a vector pointing in same direction as A but with length 1
       VEC3-UNIT-VECTOR-A.
           PERFORM VEC3-LENGTH-A               *> Calculate vector length
           MOVE VEC3-LENGTH TO VEC3-SCALAR
           PERFORM VEC3-MULTIPLY-SCALAR-A
      *> Note: This puts unit vector in VEC3-RESULT, but actually divides by length
           COMPUTE VEC3-RESULT-X = VEC3-A-X / VEC3-LENGTH  *> Normalize X
           COMPUTE VEC3-RESULT-Y = VEC3-A-Y / VEC3-LENGTH  *> Normalize Y
           COMPUTE VEC3-RESULT-Z = VEC3-A-Z / VEC3-LENGTH. *> Normalize Z
           
      *>****************************************************************
      *> VEC3 OUTPUT PROCEDURES - Display and File Operations         *
      *>****************************************************************
      *> Display VEC3-A components to terminal (equivalent to cout << vec3)
      *> Note: Must convert COMP-3 to DISPLAY format before STRING operation
       VEC3-DISPLAY-A.
           MOVE VEC3-A-X TO VEC3-DISPLAY-X     *> Convert X to display format
           MOVE VEC3-A-Y TO VEC3-DISPLAY-Y     *> Convert Y to display format
           MOVE VEC3-A-Z TO VEC3-DISPLAY-Z     *> Convert Z to display format
           STRING VEC3-DISPLAY-X " " VEC3-DISPLAY-Y " " VEC3-DISPLAY-Z
                  DELIMITED BY SIZE INTO VEC3-OUTPUT-LINE
           DISPLAY VEC3-OUTPUT-LINE.            *> Output: "X.XXXXXX Y.YYYYYY Z.ZZZZZZ"
           
      *> Display VEC3-RESULT components to terminal
       VEC3-DISPLAY-RESULT.
           MOVE VEC3-RESULT-X TO VEC3-DISPLAY-X *> Convert X to display format
           MOVE VEC3-RESULT-Y TO VEC3-DISPLAY-Y *> Convert Y to display format
           MOVE VEC3-RESULT-Z TO VEC3-DISPLAY-Z *> Convert Z to display format
           STRING VEC3-DISPLAY-X " " VEC3-DISPLAY-Y " " VEC3-DISPLAY-Z
                  DELIMITED BY SIZE INTO VEC3-OUTPUT-LINE
           DISPLAY VEC3-OUTPUT-LINE.            *> Output formatted vector
           
      *> Write VEC3-A components to file (for file output)
       VEC3-WRITE-A-TO-FILE.
           MOVE VEC3-A-X TO VEC3-DISPLAY-X     *> Convert to display format
           MOVE VEC3-A-Y TO VEC3-DISPLAY-Y     *> Convert to display format
           MOVE VEC3-A-Z TO VEC3-DISPLAY-Z     *> Convert to display format
           STRING VEC3-DISPLAY-X " " VEC3-DISPLAY-Y " " VEC3-DISPLAY-Z
                  DELIMITED BY SIZE INTO VEC3-OUTPUT-LINE
           MOVE VEC3-OUTPUT-LINE TO OUTPUT-RECORD  *> Prepare for file write
           WRITE OUTPUT-RECORD.                 *> Write vector to file
           
      *>****************************************************************
      *> COLOR PROCEDURES - Color Output Support                      *
      *>****************************************************************
      
      *> Write color to output stream (equivalent to write_color function)
      *> This is the main color output function from the C++ tutorial
      *> Input: PIXEL-COLOR contains color components (0.0-1.0 range)
      *> Output: Writes RGB bytes (0-255 range) to PPM file
       WRITE-COLOR-TO-FILE.
      *> Get RGB components from pixel color (equivalent to pixel_color.x(), y(), z())
           MOVE PIXEL-COLOR-R TO TEMP-R        *> Extract red component
           MOVE PIXEL-COLOR-G TO TEMP-G        *> Extract green component
           MOVE PIXEL-COLOR-B TO B             *> Extract blue component
           
      *> Translate [0,1] component values to byte range [0,255]
      *> Uses same 255.999 multiplier as C++ version for proper rounding
           COMPUTE COLOR-R-BYTE = 255.999 * TEMP-R    *> rbyte = int(255.999 * r)
           COMPUTE COLOR-G-BYTE = 255.999 * TEMP-G    *> gbyte = int(255.999 * g)  
           COMPUTE COLOR-B-BYTE = 255.999 * B         *> bbyte = int(255.999 * b)
           
      *> Write out the pixel color components in PPM format
           STRING COLOR-R-BYTE " " COLOR-G-BYTE " " COLOR-B-BYTE
                  DELIMITED BY SIZE INTO COLOR-OUTPUT-LINE
           MOVE COLOR-OUTPUT-LINE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.                 *> Output: "255 128 64" (example)
           
      *> Write color to terminal (for debugging/display purposes)
      *> Same as WRITE-COLOR-TO-FILE but outputs to terminal instead of file
       WRITE-COLOR-TO-TERMINAL.
      *> Get RGB components and convert to bytes
           MOVE PIXEL-COLOR-R TO TEMP-R        *> Extract red component
           MOVE PIXEL-COLOR-G TO TEMP-G        *> Extract green component
           MOVE PIXEL-COLOR-B TO B             *> Extract blue component
           
           COMPUTE COLOR-R-BYTE = 255.999 * TEMP-R  *> Convert to byte range
           COMPUTE COLOR-G-BYTE = 255.999 * TEMP-G  *> Convert to byte range
           COMPUTE COLOR-B-BYTE = 255.999 * B       *> Convert to byte range
           
      *> Display color components to terminal
           STRING COLOR-R-BYTE " " COLOR-G-BYTE " " COLOR-B-BYTE
                  DELIMITED BY SIZE INTO COLOR-OUTPUT-LINE
           DISPLAY COLOR-OUTPUT-LINE.       *> Show RGB values on screen
           
      *> Initialize color from vec3 (copy VEC3-A to PIXEL-COLOR)
      *> Equivalent to: color pixel_color = some_vec3;
       COLOR-FROM-VEC3-A.
           MOVE VEC3-A-X TO PIXEL-COLOR-R      *> Copy X -> Red component
           MOVE VEC3-A-Y TO PIXEL-COLOR-G      *> Copy Y -> Green component
           MOVE VEC3-A-Z TO PIXEL-COLOR-B.     *> Copy Z -> Blue component
           
      *> Initialize color with specific RGB values
      *> Equivalent to: color pixel_color(r, g, b);
      *> Input: Set values in VEC3-TEMP before calling (X=R, Y=G, Z=B)
       COLOR-INIT-RGB.
           MOVE VEC3-TEMP-X TO PIXEL-COLOR-R   *> Set red component
           MOVE VEC3-TEMP-Y TO PIXEL-COLOR-G   *> Set green component
           MOVE VEC3-TEMP-Z TO PIXEL-COLOR-B.  *> Set blue component
           
      *>****************************************************************
      *> RAY CLASS IMPLEMENTATION - 3D Ray Operations                 *
      *>****************************************************************
      *> Ray constructor procedures (equivalent to ray constructors)  *
      *>****************************************************************
      
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
           
      *>****************************************************************
      *> Ray accessor methods (equivalent to public member functions) *
      *>****************************************************************
      
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
           
      *>****************************************************************
      *> Ray calculation methods                                       *
      *>****************************************************************
      
      *> Calculate point along ray at parameter t
      *> Formula: point = origin + t * direction
      *> C++ equivalent: point3 at(double t) const
      *> Input: RAY-PARAMETER-T contains the parameter t
      *> Output: Result stored in RAY-POINT-X, RAY-POINT-Y, RAY-POINT-Z
       RAY-AT-PARAMETER.
      *>    Calculate t * direction for each component, then add origin
           COMPUTE RAY-POINT-X = RAY-ORIGIN-X + 
                                (RAY-PARAMETER-T * RAY-DIR-X)
           COMPUTE RAY-POINT-Y = RAY-ORIGIN-Y + 
                                (RAY-PARAMETER-T * RAY-DIR-Y)
           COMPUTE RAY-POINT-Z = RAY-ORIGIN-Z + 
                                (RAY-PARAMETER-T * RAY-DIR-Z)
           EXIT.
           
      *> Calculate point along ray and store result in VEC3-RESULT
      *> Same as RAY-AT-PARAMETER but returns result in standard location
      *> Input: RAY-PARAMETER-T contains the parameter t
      *> Output: Result stored in VEC3-RESULT-X, VEC3-RESULT-Y, VEC3-RESULT-Z
       RAY-AT-PARAMETER-TO-VEC3.
           PERFORM RAY-AT-PARAMETER
           MOVE RAY-POINT-X TO VEC3-RESULT-X
           MOVE RAY-POINT-Y TO VEC3-RESULT-Y
           MOVE RAY-POINT-Z TO VEC3-RESULT-Z.
           
      *>****************************************************************
      *> SPHERE INTERSECTION FUNCTION - Ray-Sphere Collision Detection  *
      *>****************************************************************
      
      *> Test if ray intersects sphere and return hit distance (optimized version)
      *> C++ equivalent: double hit_sphere(const point3& center, double radius, const ray& r)
      *> Input: SPHERE-CENTER-X/Y/Z contains sphere center
      *>        SPHERE-RADIUS contains sphere radius
      *>        RAY-DATA contains the ray to test
      *> Output: SPHERE-HIT-T = hit distance if hit (>0), -1.0 if miss
       HIT-SPHERE.
      *> Calculate vector from ray origin to sphere center
      *> vec3 oc = center - r.origin()
           COMPUTE SPHERE-OC-X = SPHERE-CENTER-X - RAY-ORIGIN-X
           COMPUTE SPHERE-OC-Y = SPHERE-CENTER-Y - RAY-ORIGIN-Y
           COMPUTE SPHERE-OC-Z = SPHERE-CENTER-Z - RAY-ORIGIN-Z
           
      *> Calculate length_squared of ray direction
      *> auto a = r.direction().length_squared()
           COMPUTE SPHERE-A = (RAY-DIR-X * RAY-DIR-X) +
                              (RAY-DIR-Y * RAY-DIR-Y) +
                              (RAY-DIR-Z * RAY-DIR-Z)
                              
      *> Calculate dot product of ray direction and oc vector
      *> auto h = dot(r.direction(), oc)
           COMPUTE SPHERE-H = (RAY-DIR-X * SPHERE-OC-X) +
                              (RAY-DIR-Y * SPHERE-OC-Y) +
                              (RAY-DIR-Z * SPHERE-OC-Z)
                              
      *> Calculate oc.length_squared() - radius*radius
      *> auto c = oc.length_squared() - radius*radius
           COMPUTE SPHERE-C = (SPHERE-OC-X * SPHERE-OC-X) +
                              (SPHERE-OC-Y * SPHERE-OC-Y) +
                              (SPHERE-OC-Z * SPHERE-OC-Z) -
                              (SPHERE-RADIUS * SPHERE-RADIUS)
                              
      *> Calculate discriminant: h*h - a*c
           COMPUTE SPHERE-DISCRIMINANT = (SPHERE-H * SPHERE-H) -
                                         (SPHERE-A * SPHERE-C)
                                         
      *> Return hit distance or -1.0 if no hit
           IF SPHERE-DISCRIMINANT < 0
               MOVE -1.0 TO SPHERE-HIT-T            *> Miss - no intersection
           ELSE
      *> Calculate nearest intersection: (h - sqrt(discriminant)) / a
               COMPUTE SPHERE-HIT-T = (SPHERE-H - 
                                      (SPHERE-DISCRIMINANT ** 0.5)) / 
                                      SPHERE-A
           END-IF
           EXIT.
           
      *>****************************************************************
      *> HITTABLE HIT METHOD - Virtual Method Dispatch                 *
      *>****************************************************************
      *> Abstract hit method dispatch (equivalent to virtual bool hit(...))
      *> C++ equivalent: virtual bool hit(const ray& r, double ray_tmin, double ray_tmax, hit_record& rec)
      *> Input: HITTABLE-OBJECT contains the object to test
      *>        RAY-DATA contains the ray to test
      *>        HIT-RAY-TMIN/TMAX contains valid t range
      *> Output: HIT-RECORD contains intersection info if hit
      *>         HIT-RESULT = 1 if hit detected, 0 if miss
       HITTABLE-HIT.
      *> Dispatch to appropriate hit method based on object type
      *> Uses EVALUATE for virtual method dispatch pattern
           EVALUATE HITTABLE-TYPE OF HITTABLE-OBJECT
               WHEN 'SPHERE'
                   PERFORM SPHERE-HIT-ENHANCED      *> Use enhanced sphere implementation
               WHEN 'PLANE'
      *> Future: Add plane intersection here
                   MOVE 0 TO HIT-RESULT         *> Not implemented yet
               WHEN 'TRIANGLE' 
      *> Future: Add triangle intersection here
                   MOVE 0 TO HIT-RESULT         *> Not implemented yet
               WHEN OTHER
                   MOVE 0 TO HIT-RESULT         *> Unknown object type
           END-EVALUATE
           EXIT.

      *>****************************************************************
      *> SPHERE CLASS METHODS - Concrete Hittable Implementation       *
      *>****************************************************************
      *> Sphere constructor (equivalent to sphere(const point3& center, double radius))
      *> C++ equivalent: sphere(const point3& center, double radius) : center(center), radius(std::fmax(0,radius))
      *> Input: VEC3-A-X/Y/Z contains center coordinates
      *>        VEC3-SCALAR contains radius value  
      *> Output: HITTABLE-OBJECT configured as sphere
       SPHERE-CONSTRUCTOR.
      *> Set object type to sphere
           MOVE 'SPHERE' TO HITTABLE-TYPE OF HITTABLE-OBJECT
           
      *> Copy center coordinates (equivalent to center(center))
           MOVE VEC3-A-X TO SPHERE-OBJ-CENTER-X OF HITTABLE-OBJECT
           MOVE VEC3-A-Y TO SPHERE-OBJ-CENTER-Y OF HITTABLE-OBJECT
           MOVE VEC3-A-Z TO SPHERE-OBJ-CENTER-Z OF HITTABLE-OBJECT
           
      *> Set radius with validation (equivalent to radius(std::fmax(0,radius)))
           IF VEC3-SCALAR >= 0
               MOVE VEC3-SCALAR TO SPHERE-OBJ-RADIUS OF HITTABLE-OBJECT
           ELSE
               MOVE 0.0 TO SPHERE-OBJ-RADIUS OF HITTABLE-OBJECT    *> Ensure radius is non-negative
           END-IF
           EXIT.
           
      *> Enhanced sphere hit method with proper root finding
      *> C++ equivalent: bool hit(const ray& r, double ray_tmin, double ray_tmax, hit_record& rec) const override
      *> This replaces HITTABLE-HIT-SPHERE with full C++ algorithm implementation
       SPHERE-HIT-ENHANCED.
      *> Copy sphere data from hittable object to working variables
           MOVE SPHERE-OBJ-CENTER-X OF HITTABLE-OBJECT 
                TO SPHERE-CENTER-X OF SPHERE-DATA.
           MOVE SPHERE-OBJ-CENTER-Y OF HITTABLE-OBJECT 
                TO SPHERE-CENTER-Y OF SPHERE-DATA.
           MOVE SPHERE-OBJ-CENTER-Z OF HITTABLE-OBJECT 
                TO SPHERE-CENTER-Z OF SPHERE-DATA.
           MOVE SPHERE-OBJ-RADIUS OF HITTABLE-OBJECT 
                TO SPHERE-RADIUS OF SPHERE-DATA.

      *> Calculate vector from ray origin to sphere center
      *> vec3 oc = center - r.origin()
           COMPUTE SPHERE-OC-X = SPHERE-CENTER-X OF SPHERE-DATA - 
                                 RAY-ORIGIN-X.
           COMPUTE SPHERE-OC-Y = SPHERE-CENTER-Y OF SPHERE-DATA - 
                                 RAY-ORIGIN-Y.
           COMPUTE SPHERE-OC-Z = SPHERE-CENTER-Z OF SPHERE-DATA - 
                                 RAY-ORIGIN-Z.
           
      *> Calculate intersection equation coefficients  
      *> auto a = r.direction().length_squared()
           COMPUTE SPHERE-A = (RAY-DIR-X * RAY-DIR-X) +
                              (RAY-DIR-Y * RAY-DIR-Y) +
                              (RAY-DIR-Z * RAY-DIR-Z)
                              
      *> auto h = dot(r.direction(), oc)
           COMPUTE SPHERE-H = (RAY-DIR-X * SPHERE-OC-X) +
                              (RAY-DIR-Y * SPHERE-OC-Y) +
                              (RAY-DIR-Z * SPHERE-OC-Z)
                              
      *> auto c = oc.length_squared() - radius*radius
           COMPUTE SPHERE-C = (SPHERE-OC-X * SPHERE-OC-X) +
                              (SPHERE-OC-Y * SPHERE-OC-Y) +
                              (SPHERE-OC-Z * SPHERE-OC-Z) -
                              (SPHERE-RADIUS * SPHERE-RADIUS)
                              
      *> auto discriminant = h*h - a*c
           COMPUTE SPHERE-DISCRIMINANT = (SPHERE-H * SPHERE-H) -
                                         (SPHERE-A * SPHERE-C)
                                         
      *> if (discriminant < 0) return false
           IF SPHERE-DISCRIMINANT < 0
               MOVE 0 TO HIT-RESULT             *> No intersection
               MOVE 0 TO HIT-OCCURRED
           ELSE
      *> auto sqrtd = std::sqrt(discriminant)
               COMPUTE SPHERE-SQRTD = SPHERE-DISCRIMINANT ** 0.5
      *> Find the nearest root that lies in the acceptable range
      *> auto root = (h - sqrtd) / a;  // Try nearest intersection first
               COMPUTE SPHERE-ROOT1 = 
                   (SPHERE-H - SPHERE-SQRTD) / SPHERE-A
      *> if (root <= ray_tmin || ray_tmax <= root)
               IF SPHERE-ROOT1 <= HIT-RAY-TMIN OR 
                  SPHERE-ROOT1 >= HIT-RAY-TMAX
      *> root = (h + sqrtd) / a;  // Try farther intersection
                   COMPUTE SPHERE-ROOT2 = 
                       (SPHERE-H + SPHERE-SQRTD) / SPHERE-A
      *> if (root <= ray_tmin || ray_tmax <= root) return false
                   IF SPHERE-ROOT2 <= HIT-RAY-TMIN OR
                      SPHERE-ROOT2 >= HIT-RAY-TMAX
                       MOVE 0 TO HIT-RESULT     *> No valid intersection
                       MOVE 0 TO HIT-OCCURRED
                   ELSE
                       MOVE SPHERE-ROOT2 TO SPHERE-FINAL-ROOT  *> Use farther root
                       PERFORM SPHERE-POPULATE-HIT-RECORD
                   END-IF
               ELSE
                   MOVE SPHERE-ROOT1 TO SPHERE-FINAL-ROOT      *> Use nearer root
                   PERFORM SPHERE-POPULATE-HIT-RECORD
               END-IF
           END-IF
           EXIT.
      *> Populate hit record with intersection details using set_face_normal
      *> C++ equivalent: rec.t = root; rec.p = r.at(rec.t); 
      *>                 vec3 outward_normal = (rec.p - center) / radius;
      *>                 rec.set_face_normal(r, outward_normal); return true;
       SPHERE-POPULATE-HIT-RECORD.
      *> rec.t = root
           MOVE SPHERE-FINAL-ROOT TO HIT-T
           
      *> rec.p = r.at(rec.t)  // Calculate hit point
           MOVE SPHERE-FINAL-ROOT TO RAY-PARAMETER-T
           PERFORM RAY-AT-PARAMETER             *> Result in RAY-POINT-X/Y/Z
           MOVE RAY-POINT-X TO HIT-POINT-X
           MOVE RAY-POINT-Y TO HIT-POINT-Y
           MOVE RAY-POINT-Z TO HIT-POINT-Z
           
      *> vec3 outward_normal = (rec.p - center) / radius
           COMPUTE OUTWARD-NORMAL-X = 
               (HIT-POINT-X - SPHERE-CENTER-X OF SPHERE-DATA) / 
               SPHERE-RADIUS OF SPHERE-DATA.
           COMPUTE OUTWARD-NORMAL-Y = 
               (HIT-POINT-Y - SPHERE-CENTER-Y OF SPHERE-DATA) / 
               SPHERE-RADIUS OF SPHERE-DATA.
           COMPUTE OUTWARD-NORMAL-Z = 
               (HIT-POINT-Z - SPHERE-CENTER-Z OF SPHERE-DATA) / 
               SPHERE-RADIUS OF SPHERE-DATA.
           
      *> rec.set_face_normal(r, outward_normal)
           PERFORM SET-FACE-NORMAL.              *> Sets HIT-NORMAL and HIT-FRONT-FACE
           
      *> return true
           MOVE 1 TO HIT-RESULT                 *> Mark successful hit
           MOVE 1 TO HIT-OCCURRED
           EXIT.
           
      *>****************************************************************
      *> HIT RECORD METHODS - Normal Orientation and Face Detection    *
      *>****************************************************************
      *> Set face normal method (equivalent to set_face_normal method)
      *> C++ equivalent: void set_face_normal(const ray& r, const vec3& outward_normal)
      *> Input: RAY-DATA contains the ray
      *>        OUTWARD-NORMAL-X/Y/Z contains unit outward normal
      *> Output: HIT-NORMAL set to proper orientation
      *>         HIT-FRONT-FACE set to 1 if front face, 0 if back face
       SET-FACE-NORMAL.
      *> Calculate dot product of ray direction and outward normal
      *> front_face = dot(r.direction(), outward_normal) < 0
           COMPUTE DOT-PRODUCT-RESULT = (RAY-DIR-X * OUTWARD-NORMAL-X) +
                                        (RAY-DIR-Y * OUTWARD-NORMAL-Y) +
                                        (RAY-DIR-Z * OUTWARD-NORMAL-Z)
                                        
      *> Determine if ray hits front face or back face
           IF DOT-PRODUCT-RESULT < 0
               MOVE 1 TO HIT-FRONT-FACE         *> Ray hits front face
      *> normal = front_face ? outward_normal : -outward_normal
               MOVE OUTWARD-NORMAL-X TO HIT-NORMAL-X
               MOVE OUTWARD-NORMAL-Y TO HIT-NORMAL-Y
               MOVE OUTWARD-NORMAL-Z TO HIT-NORMAL-Z

           ELSE
               MOVE 0 TO HIT-FRONT-FACE         *> Ray hits back face  
      *> normal = -outward_normal (flip the normal)
               COMPUTE HIT-NORMAL-X = -OUTWARD-NORMAL-X
               COMPUTE HIT-NORMAL-Y = -OUTWARD-NORMAL-Y
               COMPUTE HIT-NORMAL-Z = -OUTWARD-NORMAL-Z
           END-IF
           EXIT.
           
      *>****************************************************************
      *> HITTABLE LIST CLASS METHODS - Object Collection Management    *
      *>****************************************************************
      *> HittableList constructor (equivalent to hittable_list())
      *> C++ equivalent: hittable_list() {} - initializes empty list
       HITTABLE-LIST-CONSTRUCTOR.
      *> Initialize empty list
           MOVE 0 TO HITTABLE-COUNT             *> Start with no objects
           
      *> Clear all object slots
           PERFORM VARYING LIST-INDEX FROM 1 BY 1
                   UNTIL LIST-INDEX > HITTABLE-MAX
               MOVE 0 TO HITTABLE-ACTIVE(LIST-INDEX)
               MOVE SPACES TO HITTABLE-TYPE OF 
               HITTABLE-OBJECTS(LIST-INDEX)
           END-PERFORM
           EXIT.
           
      *> Clear method (equivalent to clear())
      *> C++ equivalent: void clear() { objects.clear(); }
       HITTABLE-LIST-CLEAR.
      *> Reset count and mark all slots as inactive
           MOVE 0 TO HITTABLE-COUNT
           
           PERFORM VARYING LIST-INDEX FROM 1 BY 1
                   UNTIL LIST-INDEX > HITTABLE-MAX
               MOVE 0 TO HITTABLE-ACTIVE(LIST-INDEX)
           END-PERFORM
           EXIT.
           
      *> Add method (equivalent to add(shared_ptr<hittable> object))
      *> C++ equivalent: void add(shared_ptr<hittable> object) { objects.push_back(object); }
      *> Input: HITTABLE-OBJECT contains the object to add
      *> Output: Object added to list if space available
       HITTABLE-LIST-ADD.
      *> Find next available slot
           PERFORM HITTABLE-LIST-FIND-SLOT
           
           IF SLOT-FOUND = 1
      *> Copy object to list slot
               MOVE 1 TO HITTABLE-ACTIVE(NEXT-SLOT)
               MOVE HITTABLE-TYPE OF HITTABLE-OBJECT 
                    TO HITTABLE-TYPE OF HITTABLE-OBJECTS(NEXT-SLOT)
               MOVE HITTABLE-DATA OF HITTABLE-OBJECT 
                    TO HITTABLE-DATA OF HITTABLE-OBJECTS(NEXT-SLOT)
               ADD 1 TO HITTABLE-COUNT
           ELSE
               DISPLAY "WARNING: HittableList full (" HITTABLE-MAX ")" 
               DISPLAY "Cannot add more objects to scene"
           END-IF
           EXIT.
           
      *> Find next available slot in list
       HITTABLE-LIST-FIND-SLOT.
           MOVE 0 TO SLOT-FOUND
           
           PERFORM VARYING LIST-INDEX FROM 1 BY 1
                   UNTIL LIST-INDEX > HITTABLE-MAX OR SLOT-FOUND = 1
               IF HITTABLE-ACTIVE(LIST-INDEX) = 0
                   MOVE LIST-INDEX TO NEXT-SLOT
                   MOVE 1 TO SLOT-FOUND
               END-IF
           END-PERFORM
           EXIT.
           
      *> List hit method (equivalent to bool hit(...) const override)
      *> C++ equivalent: bool hit(const ray& r, double ray_tmin, double ray_tmax, hit_record& rec) const override
      *> Input: RAY-DATA contains ray, HIT-RAY-TMIN/TMAX contains range
      *> Output: HIT-RECORD contains closest intersection, HIT-RESULT = 1 if any hit found
       HITTABLE-LIST-HIT.
      *> Initialize search for closest hit
           MOVE 0 TO HIT-ANYTHING               *> bool hit_anything = false
           MOVE HIT-RAY-TMAX TO CLOSEST-T       *> auto closest_so_far = ray_tmax
           MOVE HIT-RAY-TMIN TO TEMP-TMIN       *> Save original tmin
           
      *> Iterate through all objects in list
      *> for (const auto& object : objects)
           PERFORM VARYING LIST-INDEX FROM 1 BY 1
                   UNTIL LIST-INDEX > HITTABLE-COUNT
               IF HITTABLE-ACTIVE(LIST-INDEX) = 1
      *> Copy object from list to working area for hit test
                   MOVE HITTABLE-TYPE OF HITTABLE-OBJECTS(LIST-INDEX) 
                        TO HITTABLE-TYPE OF HITTABLE-OBJECT
                   MOVE HITTABLE-DATA OF HITTABLE-OBJECTS(LIST-INDEX) 
                        TO HITTABLE-DATA OF HITTABLE-OBJECT
                   
      *> Test intersection with current object using closer range
      *> if (object->hit(r, ray_tmin, closest_so_far, temp_rec))
                   MOVE TEMP-TMIN TO HIT-RAY-TMIN
                   MOVE CLOSEST-T TO HIT-RAY-TMAX   *> Only accept closer hits
                   PERFORM HITTABLE-HIT             *> Test intersection
      



                   *> Debug: Show intersection result (only for first few pixels)
                   *> (debug output removed for cleaner output)
  



      *> If hit found and it's closer than previous closest
                   IF HIT-RESULT = 1 AND HIT-OCCURRED = 1
                       MOVE 1 TO HIT-ANYTHING       *> hit_anything = true
                       MOVE HIT-T TO CLOSEST-T      *> closest_so_far = temp_rec.t
      *> rec = temp_rec (save this as the closest hit so far)
                       MOVE HIT-POINT-X TO TEMP-HIT-POINT-X
                       MOVE HIT-POINT-Y TO TEMP-HIT-POINT-Y
                       MOVE HIT-POINT-Z TO TEMP-HIT-POINT-Z
                       MOVE HIT-NORMAL-X TO TEMP-HIT-NORMAL-X
                       MOVE HIT-NORMAL-Y TO TEMP-HIT-NORMAL-Y
                       MOVE HIT-NORMAL-Z TO TEMP-HIT-NORMAL-Z
                       MOVE HIT-T TO TEMP-HIT-T
                       MOVE HIT-FRONT-FACE TO TEMP-HIT-FRONT-FACE
                       MOVE HIT-OCCURRED TO TEMP-HIT-OCCURRED
                   END-IF
               END-IF
           END-PERFORM
           
      *> Return closest hit found (if any)
      *> return hit_anything
           IF HIT-ANYTHING = 1
      *> Copy temp record back to final hit record
               MOVE TEMP-HIT-POINT-X TO HIT-POINT-X
               MOVE TEMP-HIT-POINT-Y TO HIT-POINT-Y
               MOVE TEMP-HIT-POINT-Z TO HIT-POINT-Z
               MOVE TEMP-HIT-NORMAL-X TO HIT-NORMAL-X
               MOVE TEMP-HIT-NORMAL-Y TO HIT-NORMAL-Y
               MOVE TEMP-HIT-NORMAL-Z TO HIT-NORMAL-Z
               MOVE TEMP-HIT-T TO HIT-T
               MOVE TEMP-HIT-FRONT-FACE TO HIT-FRONT-FACE
               MOVE TEMP-HIT-OCCURRED TO HIT-OCCURRED
               MOVE 1 TO HIT-RESULT             *> Return true - hit found
           ELSE
               MOVE 0 TO HIT-RESULT             *> Return false - no hits
               MOVE 0 TO HIT-OCCURRED
           END-IF
           EXIT.
           
      *> Convenience method to add sphere to list
      *> C++ equivalent: list.add(make_shared<sphere>(center, radius))
      *> Input: VEC3-A-X/Y/Z contains center, VEC3-SCALAR contains radius
       HITTABLE-LIST-ADD-SPHERE.
      *> Create sphere object using constructor
           PERFORM SPHERE-CONSTRUCTOR           *> Creates sphere in HITTABLE-OBJECT
           
      *> Add sphere to list
           PERFORM HITTABLE-LIST-ADD           *> Adds HITTABLE-OBJECT to list
           EXIT.
           
      *>****************************************************************
      *> MATHEMATICAL UTILITY FUNCTIONS - rtweekend.h Equivalents      *
      *>****************************************************************
      *> Degrees to radians conversion (equivalent to degrees_to_radians function)
      *> C++ equivalent: inline double degrees_to_radians(double degrees) { return degrees * pi / 180.0; }
      *> Input: DEGREES-INPUT contains angle in degrees
      *> Output: RADIANS-OUTPUT contains angle in radians
       DEGREES-TO-RADIANS.
      *> Convert degrees to radians using pi/180 conversion factor
           COMPUTE RADIANS-OUTPUT = DEGREES-INPUT * DEGREES-TO-RAD
           EXIT.
           
      *> Alternative degrees to radians using VEC3 scalar input
      *> Input: VEC3-SCALAR contains degrees
      *> Output: VEC3-SCALAR contains radians
       DEGREES-TO-RADIANS-VEC3.
      *> Convert using VEC3 working variables for convenience
           COMPUTE VEC3-SCALAR = VEC3-SCALAR * DEGREES-TO-RAD
           EXIT.
           
      *> Clamp function (equivalent to C++ std::clamp)
      *> Input: VEC3-SCALAR contains value to clamp
      *>        VEC3-A-X contains minimum value
      *>        VEC3-A-Y contains maximum value  
      *> Output: VEC3-RESULT contains clamped value
       CLAMP-VALUE.
           IF VEC3-SCALAR < VEC3-A-X
               MOVE VEC3-A-X TO VEC3-RESULT         *> Below minimum - use min
           ELSE
               IF VEC3-SCALAR > VEC3-A-Y
                   MOVE VEC3-A-Y TO VEC3-RESULT     *> Above maximum - use max
               ELSE
                   MOVE VEC3-SCALAR TO VEC3-RESULT  *> Within range - use original
               END-IF
           END-IF
           EXIT.
           
      *> Random number generator (equivalent to C++ random_double)
      *> Output: VEC3-SCALAR contains random value between 0 and 1
       RANDOM-DOUBLE.
      *> Use COBOL RANDOM function to generate value between 0 and 1
           COMPUTE VEC3-SCALAR = FUNCTION RANDOM
           EXIT.
           
      *> Random number in range (equivalent to C++ random_double(min, max))
      *> Input: VEC3-A-X contains minimum value
      *>        VEC3-A-Y contains maximum value
      *> Output: VEC3-SCALAR contains random value in range [min, max)
       RANDOM-DOUBLE-RANGE.
      *> Generate random value in specified range
           COMPUTE VEC3-SCALAR = VEC3-A-X + (FUNCTION RANDOM * (VEC3-A-Y - VEC3-A-X))
           EXIT.
           
      *>****************************************************************
      *> RAY COLOR FUNCTION - Ray Tracing Color Calculation           *
      *>****************************************************************
      
      *> Calculate color for a given ray using world objects (equivalent to ray_color function)
      *> C++ equivalent: color ray_color(const ray& r, const hittable& world)
      *> Input: RAY-DATA contains the ray to process
      *>        HITTABLE-LIST contains the world objects
      *> Output: PIXEL-COLOR contains the calculated color
       RAY-COLOR-FUNCTION.
      *> Test ray against world objects using hittable list
      *> hit_record rec;
      *> if (world.hit(r, 0, infinity, rec))
           MOVE 0.001 TO HIT-RAY-TMIN              *> Small epsilon instead of 0
           MOVE INFINITY-VALUE TO HIT-RAY-TMAX     *> Use infinity constant
           PERFORM HITTABLE-LIST-HIT               *> Test against world
           
      *> If ray hits any object in world, use surface normal for coloring
           IF HIT-RESULT = 1 AND HIT-OCCURRED = 1
      *> return 0.5 * (rec.normal + color(1,1,1))
      *> Add (1,1,1) to normal to shift from [-1,1] to [0,2], then scale by 0.5 to get [0,1]
               COMPUTE PIXEL-COLOR-R = 0.5 * (HIT-NORMAL-X + 1.0)
               COMPUTE PIXEL-COLOR-G = 0.5 * (HIT-NORMAL-Y + 1.0)
               COMPUTE PIXEL-COLOR-B = 0.5 * (HIT-NORMAL-Z + 1.0)
           ELSE
      *> Otherwise render sky gradient (existing code)
      *> Get the ray direction and normalize it to unit vector
      *> vec3 unit_direction = unit_vector(r.direction());
               MOVE RAY-DIR-X TO VEC3-A-X        *> Copy ray direction to VEC3-A
               MOVE RAY-DIR-Y TO VEC3-A-Y
               MOVE RAY-DIR-Z TO VEC3-A-Z
               PERFORM VEC3-UNIT-VECTOR-A        *> Calculate unit vector (result in VEC3-RESULT)
               
      *> Calculate interpolation parameter based on Y component
      *> auto a = 0.5*(unit_direction.y() + 1.0);
      *> This maps Y from [-1,1] to a from [0,1]
               COMPUTE VEC3-SCALAR = 0.5 * (VEC3-RESULT-Y + 1.0)
               
      *> Linear interpolation between white and light blue
      *> return (1.0-a)*color(1.0, 1.0, 1.0) + a*color(0.5, 0.7, 1.0);
      *> White color when a=0 (Y=-1, looking down)
      *> Blue color when a=1 (Y=+1, looking up)
      
      *> Calculate (1.0-a) for white component weight
               COMPUTE VEC3-TEMP-CALC = 1.0 - VEC3-SCALAR
               
      *> Calculate final color components using linear interpolation
      *> Red:   (1-a)*1.0 + a*0.5 = (1-a) + 0.5*a
               COMPUTE PIXEL-COLOR-R = VEC3-TEMP-CALC * 1.0 + 
                                       VEC3-SCALAR * 0.5
               
      *> Green: (1-a)*1.0 + a*0.7 = (1-a) + 0.7*a  
               COMPUTE PIXEL-COLOR-G = VEC3-TEMP-CALC * 1.0 + 
                                       VEC3-SCALAR * 0.7
               
      *> Blue:  (1-a)*1.0 + a*1.0 = (1-a) + a = 1.0 (always full blue)
               COMPUTE PIXEL-COLOR-B = VEC3-TEMP-CALC * 1.0 + 
                                       VEC3-SCALAR * 1.0
           END-IF
           
      *> Result: Creates sphere with surface normal shading on sky gradient background
      *> - Sphere hit: Color based on surface normal direction (creates 3D shading effect)
      *> - Sky background: White to blue gradient based on ray direction
           EXIT.
