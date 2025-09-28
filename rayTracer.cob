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
       
       WORKING-STORAGE SECTION. 
      *> PPM format constants  
       01  MAX-COLOR-VALUE         PIC 9(3) VALUE 255.  *> Max RGB value for PPM
       
      *> Loop iteration variables
       01  LOOP-COUNTERS.
           05  I                   PIC 9(3) VALUE 0.  *> X-axis pixel counter
           05  J                   PIC 9(3) VALUE 0.  *> Y-axis pixel counter
           05  SAMPLE              PIC 9(3) VALUE 0.  *> Sample counter for antialiasing
           
      *> Sample offset working variables (for random sampling)
       01  SAMPLE-WORK-VARS.
           05  SAMPLE-OFFSET-X     PIC S9V9(6) COMP-3. *> Random X offset [-0.5, +0.5]
           05  SAMPLE-OFFSET-Y     PIC S9V9(6) COMP-3. *> Random Y offset [-0.5, +0.5]
       
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
           05  PIXEL-COLOR-R       PIC 999V9(6) COMP-3.  *> Red component (accumulates samples, then scaled)
           05  PIXEL-COLOR-G       PIC 999V9(6) COMP-3.  *> Green component (accumulates samples, then scaled)
           05  PIXEL-COLOR-B       PIC 999V9(6) COMP-3.  *> Blue component (accumulates samples, then scaled)
           
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
      *> CAMERA CLASS - Ray Generation and Rendering                   *
      *>****************************************************************
      *> COBOL implementation of C++ camera class with public parameters
      *> and methods: initialize(), render(), get_ray(), ray_color()
      *> 
      *> Public Camera Parameters (equivalent to C++ public variables)
      *> These can be modified before calling render()
       01  CAMERA-PUBLIC-PARAMS.
           05  ASPECT-RATIO        PIC 9V9(6) COMP-3 VALUE 1.0.      *> Ratio of image width over height
           05  IMAGE-WIDTH         PIC 9(3) VALUE 100.                *> Rendered image width in pixel count
           05  SAMPLES-PER-PIXEL   PIC 9(3) VALUE 10.                 *> Count of random samples for each pixel
           
      *> Private Camera Variables (equivalent to C++ private variables)
      *> These are calculated internally by initialize()
       01  CAMERA-PRIVATE-VARS.
           05  IMAGE-HEIGHT        PIC 9(3).                          *> Rendered image height (calculated)
           05  PIXEL-SAMPLES-SCALE PIC 9V9(6) COMP-3.                *> Color scale factor for sum of pixel samples
           05  CENTER-X            PIC S9V9(6) COMP-3.                *> Camera center X (calculated) 
           05  CENTER-Y            PIC S9V9(6) COMP-3.                *> Camera center Y (calculated)
           05  CENTER-Z            PIC S9V9(6) COMP-3.                *> Camera center Z (calculated)
           05  PIXEL00-LOC-X       PIC S9V9(6) COMP-3.                *> Location of pixel 0,0 X
           05  PIXEL00-LOC-Y       PIC S9V9(6) COMP-3.                *> Location of pixel 0,0 Y  
           05  PIXEL00-LOC-Z       PIC S9V9(6) COMP-3.                *> Location of pixel 0,0 Z
           05  PIXEL-DELTA-U-X     PIC S9V9(6) COMP-3.                *> Offset to pixel to the right X
           05  PIXEL-DELTA-U-Y     PIC S9V9(6) COMP-3.                *> Offset to pixel to the right Y
           05  PIXEL-DELTA-U-Z     PIC S9V9(6) COMP-3.                *> Offset to pixel to the right Z
           05  PIXEL-DELTA-V-X     PIC S9V9(6) COMP-3.                *> Offset to pixel below X
           05  PIXEL-DELTA-V-Y     PIC S9V9(6) COMP-3.                *> Offset to pixel below Y
           05  PIXEL-DELTA-V-Z     PIC S9V9(6) COMP-3.                *> Offset to pixel below Z
           
      *> Legacy support structures (for backward compatibility)
       01  CAMERA-PARAMS.
           05  FOCAL-LENGTH        PIC 9V9(6) COMP-3 VALUE 1.0.      *> Camera focal length
           05  VIEWPORT-HEIGHT     PIC 9V9(6) COMP-3 VALUE 2.0.      *> Viewport height
           05  VIEWPORT-WIDTH      PIC 9V9(6) COMP-3.                *> Calculated viewport width
           
      *> Legacy camera center (for backward compatibility)
       01  CAMERA-CENTER.
           05  CAMERA-CENTER-X     PIC S9V9(6) COMP-3 VALUE 0.0.     *> Camera X position
           05  CAMERA-CENTER-Y     PIC S9V9(6) COMP-3 VALUE 0.0.     *> Camera Y position
           05  CAMERA-CENTER-Z     PIC S9V9(6) COMP-3 VALUE 0.0.     *> Camera Z position
           
      *> Working variables for viewport calculations
       01  VIEWPORT-WORK-VARS.
           05  VIEWPORT-U-X        PIC S9V9(6) COMP-3.               *> U vector X component
           05  VIEWPORT-U-Y        PIC S9V9(6) COMP-3 VALUE 0.0.     *> U vector Y component
           05  VIEWPORT-U-Z        PIC S9V9(6) COMP-3 VALUE 0.0.     *> U vector Z component
           05  VIEWPORT-V-X        PIC S9V9(6) COMP-3 VALUE 0.0.     *> V vector X component
           05  VIEWPORT-V-Y        PIC S9V9(6) COMP-3.               *> V vector Y component
           05  VIEWPORT-V-Z        PIC S9V9(6) COMP-3 VALUE 0.0.     *> V vector Z component
           05  VIEWPORT-UL-X       PIC S9V9(6) COMP-3.               *> Upper left X
           05  VIEWPORT-UL-Y       PIC S9V9(6) COMP-3.               *> Upper left Y
           05  VIEWPORT-UL-Z       PIC S9V9(6) COMP-3.               *> Upper left Z
               
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
           05  HIT-RAY-T.                              *> Ray t interval parameter
               10  HIT-RAY-T-MIN   PIC S9(6)V9(6) COMP-3.  *> Minimum t value
               10  HIT-RAY-T-MAX   PIC S9(6)V9(6) COMP-3.  *> Maximum t value
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
           
      *>****************************************************************
      *> INTERVAL CLASS - Range/Interval Mathematical Operations       *
      *>****************************************************************
      *> Interval data structure (equivalent to interval class)
      *> Represents a mathematical interval [min, max] with range operations
       01  INTERVAL-DATA.                        *> Primary interval structure
           05  INTERVAL-MIN        PIC S9(6)V9(6) COMP-3.  *> Minimum bound of interval
           05  INTERVAL-MAX        PIC S9(6)V9(6) COMP-3.  *> Maximum bound of interval
           
      *> Secondary interval for calculations (equivalent to multiple interval objects)
       01  INTERVAL-TEMP.                        *> Temporary interval structure
           05  INTERVAL-TEMP-MIN   PIC S9(6)V9(6) COMP-3.  *> Temp minimum bound
           05  INTERVAL-TEMP-MAX   PIC S9(6)V9(6) COMP-3.  *> Temp maximum bound
           
      *> Interval calculation working variables
       01  INTERVAL-WORK-VARS.
           05  INTERVAL-SIZE       PIC S9(6)V9(6) COMP-3.  *> Size of interval (max - min)
           05  INTERVAL-TEST-VALUE PIC S9(6)V9(6) COMP-3.  *> Value to test against interval
           05  INTERVAL-CONTAINS   PIC 9 VALUE 0.          *> 1=value contained, 0=not contained
           05  INTERVAL-SURROUNDS  PIC 9 VALUE 0.          *> 1=value surrounded, 0=not surrounded
           
      *> Predefined interval constants (equivalent to static const intervals)
       01  INTERVAL-CONSTANTS.
           05  EMPTY-INTERVAL.                             *> Empty interval (+inf, -inf)
               10  EMPTY-MIN       PIC S9(6)V9(6) COMP-3 VALUE +999999.
               10  EMPTY-MAX       PIC S9(6)V9(6) COMP-3 VALUE -999999.
           05  UNIVERSE-INTERVAL.                          *> Universe interval (-inf, +inf)
               10  UNIVERSE-MIN    PIC S9(6)V9(6) COMP-3 VALUE -999999.
               10  UNIVERSE-MAX    PIC S9(6)V9(6) COMP-3 VALUE +999999.
           05  INTENSITY-INTERVAL.                         *> Color intensity interval [0.000, 0.999]
               10  INTENSITY-MIN   PIC S9(6)V9(6) COMP-3 VALUE 0.000.
               10  INTENSITY-MAX   PIC S9(6)V9(6) COMP-3 VALUE 0.999.
           
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
      *> Create camera and set up world (equivalent to C++ main)
           PERFORM CAMERA-SET-DEFAULTS      *> Set camera public parameters
           PERFORM WORLD-SETUP              *> Create world with spheres
           PERFORM CAMERA-RENDER            *> Camera handles everything else
           DISPLAY "PPM file 'image.ppm' created successfully!"
           STOP RUN.
       
      *> File handling procedures
       OPEN-OUTPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.     *> Open file for writing
       
      *> Camera set defaults - equivalent to setting public parameters in C++
      *> C++ equivalent: camera cam; cam.aspect_ratio = 16.0/9.0; cam.image_width = 400; cam.samples_per_pixel = 100;
       CAMERA-SET-DEFAULTS.
      *> Set public camera parameters (can be overridden by user)
           MOVE 1.777777 TO ASPECT-RATIO        *> 16/9 aspect ratio (16.0/9.0)  
           MOVE 400 TO IMAGE-WIDTH              *> Rendered image width 
           MOVE 100 TO SAMPLES-PER-PIXEL        *> DEBUG: Back to 100 samples to see what's wrong
           EXIT.
           
       CLOSE-OUTPUT-FILE.
           CLOSE OUTPUT-FILE.           *> Close and finalize file
       
      *> World setup - separate from camera (equivalent to C++ main world creation)
      *> C++ equivalent: hittable_list world; world.add(...);
       WORLD-SETUP.
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
           EXIT.
           
      *>****************************************************************
      *> CAMERA CLASS METHODS - Public and Private Methods             *
      *>****************************************************************
      
      *> Camera initialize() method - prepares camera for rendering
      *> C++ equivalent: void initialize() (private method)
      *> Called automatically at start of render()
       CAMERA-INITIALIZE.
      *> Calculate image height from aspect ratio
      *> image_height = int(image_width / aspect_ratio);
      *> image_height = (image_height < 1) ? 1 : image_height;
           COMPUTE IMAGE-HEIGHT = IMAGE-WIDTH / ASPECT-RATIO
           IF IMAGE-HEIGHT < 1
               MOVE 1 TO IMAGE-HEIGHT
           END-IF
           
      *> pixel_samples_scale = 1.0 / samples_per_pixel;
           COMPUTE PIXEL-SAMPLES-SCALE = 1.0 / SAMPLES-PER-PIXEL
           
      *> Set camera center
      *> center = point3(0, 0, 0);
           MOVE 0.0 TO CENTER-X
           MOVE 0.0 TO CENTER-Y  
           MOVE 0.0 TO CENTER-Z
           
      *> Determine viewport dimensions
      *> auto focal_length = 1.0;
      *> auto viewport_height = 2.0;
      *> auto viewport_width = viewport_height * (double(image_width)/image_height);
           MOVE 1.0 TO FOCAL-LENGTH
           MOVE 2.0 TO VIEWPORT-HEIGHT
           COMPUTE VIEWPORT-WIDTH = VIEWPORT-HEIGHT * 
                                   (IMAGE-WIDTH / IMAGE-HEIGHT)
           
      *> Calculate the vectors across the horizontal and down the vertical viewport edges
      *> auto viewport_u = vec3(viewport_width, 0, 0);
      *> auto viewport_v = vec3(0, -viewport_height, 0);
           MOVE VIEWPORT-WIDTH TO VIEWPORT-U-X
           MOVE 0 TO VIEWPORT-U-Y
           MOVE 0 TO VIEWPORT-U-Z
           
           MOVE 0 TO VIEWPORT-V-X
           COMPUTE VIEWPORT-V-Y = -VIEWPORT-HEIGHT  *> Negative for screen coordinates
           MOVE 0 TO VIEWPORT-V-Z
           
      *> Calculate the horizontal and vertical delta vectors from pixel to pixel
      *> pixel_delta_u = viewport_u / image_width;
      *> pixel_delta_v = viewport_v / image_height;
           COMPUTE PIXEL-DELTA-U-X OF CAMERA-PRIVATE-VARS = VIEWPORT-U-X / IMAGE-WIDTH
           COMPUTE PIXEL-DELTA-U-Y OF CAMERA-PRIVATE-VARS = VIEWPORT-U-Y / IMAGE-WIDTH  
           COMPUTE PIXEL-DELTA-U-Z OF CAMERA-PRIVATE-VARS = VIEWPORT-U-Z / IMAGE-WIDTH
           
           COMPUTE PIXEL-DELTA-V-X OF CAMERA-PRIVATE-VARS = VIEWPORT-V-X / IMAGE-HEIGHT
           COMPUTE PIXEL-DELTA-V-Y OF CAMERA-PRIVATE-VARS = VIEWPORT-V-Y / IMAGE-HEIGHT
           COMPUTE PIXEL-DELTA-V-Z OF CAMERA-PRIVATE-VARS = VIEWPORT-V-Z / IMAGE-HEIGHT
           
      *> Calculate the location of the upper left pixel
      *> auto viewport_upper_left = center - vec3(0,0,focal_length) - viewport_u/2 - viewport_v/2;
      *> pixel00_loc = viewport_upper_left + 0.5 * (pixel_delta_u + pixel_delta_v);
           COMPUTE VIEWPORT-UL-X = CENTER-X - 0 - (VIEWPORT-U-X / 2)
           COMPUTE VIEWPORT-UL-Y = CENTER-Y - 0 - (VIEWPORT-V-Y / 2)  
           COMPUTE VIEWPORT-UL-Z = CENTER-Z - FOCAL-LENGTH - 0
           
           COMPUTE PIXEL00-LOC-X = VIEWPORT-UL-X + 
                  0.5 * (PIXEL-DELTA-U-X OF CAMERA-PRIVATE-VARS + PIXEL-DELTA-V-X OF CAMERA-PRIVATE-VARS)
           COMPUTE PIXEL00-LOC-Y = VIEWPORT-UL-Y + 
                  0.5 * (PIXEL-DELTA-U-Y OF CAMERA-PRIVATE-VARS + PIXEL-DELTA-V-Y OF CAMERA-PRIVATE-VARS)
           COMPUTE PIXEL00-LOC-Z = VIEWPORT-UL-Z + 
                  0.5 * (PIXEL-DELTA-U-Z OF CAMERA-PRIVATE-VARS + PIXEL-DELTA-V-Z OF CAMERA-PRIVATE-VARS)
           
      *> Copy to legacy variables for compatibility
           MOVE CENTER-X TO CAMERA-CENTER-X
           MOVE CENTER-Y TO CAMERA-CENTER-Y
           MOVE CENTER-Z TO CAMERA-CENTER-Z
           
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
       
      *> Camera render() method - main public interface
      *> C++ equivalent: void render(const hittable& world)
      *> Handles file I/O, initialization, and rendering with antialiasing
       CAMERA-RENDER.
      *> Initialize camera (called automatically at start of render)
           PERFORM CAMERA-INITIALIZE
           
      *> Open output file and write header
           PERFORM OPEN-OUTPUT-FILE
           PERFORM OUTPUT-HEADER
           
      *> Main image rendering loop - generates ray traced image with antialiasing
      *> Outer loop: iterate through each row (Y-axis)
           PERFORM VARYING J FROM 0 BY 1 UNTIL J >= IMAGE-HEIGHT
               PERFORM DISPLAY-PROGRESS     *> Show progress to terminal
      *> Inner loop: iterate through each column (X-axis)
               PERFORM VARYING I FROM 0 BY 1 UNTIL I >= IMAGE-WIDTH
      *> Initialize pixel color accumulator: color pixel_color(0,0,0);
                   MOVE 0.0 TO PIXEL-COLOR-R
                   MOVE 0.0 TO PIXEL-COLOR-G  
                   MOVE 0.0 TO PIXEL-COLOR-B
      *> Antialiasing sample loop: for (int sample = 0; sample < samples_per_pixel; sample++)
                   PERFORM VARYING SAMPLE FROM 0 BY 1 UNTIL SAMPLE >= SAMPLES-PER-PIXEL
                       PERFORM CAMERA-GET-RAY           *> ray r = get_ray(i, j);
                       PERFORM CAMERA-RAY-COLOR         *> pixel_color += ray_color(r, world);
      *> Add ray color to pixel color accumulator
                       COMPUTE PIXEL-COLOR-R = PIXEL-COLOR-R + VEC3-RESULT-X
                       COMPUTE PIXEL-COLOR-G = PIXEL-COLOR-G + VEC3-RESULT-Y
                       COMPUTE PIXEL-COLOR-B = PIXEL-COLOR-B + VEC3-RESULT-Z
                   END-PERFORM
      *> C++ equivalent: write_color(std::cout, pixel_samples_scale * pixel_color);
      *> Apply pixel samples scaling: pixel_samples_scale * pixel_color
                   COMPUTE PIXEL-COLOR-R = PIXEL-SAMPLES-SCALE * PIXEL-COLOR-R
                   COMPUTE PIXEL-COLOR-G = PIXEL-SAMPLES-SCALE * PIXEL-COLOR-G
                   COMPUTE PIXEL-COLOR-B = PIXEL-SAMPLES-SCALE * PIXEL-COLOR-B
                   PERFORM OUTPUT-PIXEL             *> write_color(std::cout, averaged_pixel_color);
               END-PERFORM
           END-PERFORM
           PERFORM DISPLAY-COMPLETION.      *> Show completion message
           
      *> Close output file 
           PERFORM CLOSE-OUTPUT-FILE
           EXIT.
       
      *> Camera get_ray() method - generates ray for given pixel with random sampling
      *> C++ equivalent: ray get_ray(int i, int j) const (private method)
      *> Input: I, J contain pixel coordinates      *> Output: RAY-DATA contains the generated ray
       CAMERA-GET-RAY.
      *> Construct a camera ray originating from the origin and directed at randomly sampled
      *> point around the pixel location i, j.
      *> auto offset = sample_square();
           PERFORM SAMPLE-SQUARE
           
      *> auto pixel_sample = pixel00_loc + ((i + offset.x()) * pixel_delta_u) + ((j + offset.y()) * pixel_delta_v);
           COMPUTE PIXEL-CENTER-X = PIXEL00-LOC-X + 
                  ((I + SAMPLE-OFFSET-X) * PIXEL-DELTA-U-X OF CAMERA-PRIVATE-VARS) + 
                  ((J + SAMPLE-OFFSET-Y) * PIXEL-DELTA-V-X OF CAMERA-PRIVATE-VARS)
           COMPUTE PIXEL-CENTER-Y = PIXEL00-LOC-Y + 
                  ((I + SAMPLE-OFFSET-X) * PIXEL-DELTA-U-Y OF CAMERA-PRIVATE-VARS) + 
                  ((J + SAMPLE-OFFSET-Y) * PIXEL-DELTA-V-Y OF CAMERA-PRIVATE-VARS)
           COMPUTE PIXEL-CENTER-Z = PIXEL00-LOC-Z + 
                  ((I + SAMPLE-OFFSET-X) * PIXEL-DELTA-U-Z OF CAMERA-PRIVATE-VARS) + 
                  ((J + SAMPLE-OFFSET-Y) * PIXEL-DELTA-V-Z OF CAMERA-PRIVATE-VARS)
           
      *> auto ray_origin = center;
      *> auto ray_direction = pixel_sample - ray_origin;
           COMPUTE RAY-DIR-CALC-X = PIXEL-CENTER-X - CENTER-X
           COMPUTE RAY-DIR-CALC-Y = PIXEL-CENTER-Y - CENTER-Y
           COMPUTE RAY-DIR-CALC-Z = PIXEL-CENTER-Z - CENTER-Z
           
      *> return ray(ray_origin, ray_direction);
           MOVE CENTER-X TO VEC3-A-X    *> Origin = camera center
           MOVE CENTER-Y TO VEC3-A-Y
           MOVE CENTER-Z TO VEC3-A-Z
           MOVE RAY-DIR-CALC-X TO VEC3-B-X      *> Direction = calculated direction
           MOVE RAY-DIR-CALC-Y TO VEC3-B-Y
           MOVE RAY-DIR-CALC-Z TO VEC3-B-Z
           PERFORM RAY-CONSTRUCT-WITH-PARAMS    *> Create the ray
           EXIT.
           
      *> Sample square method - returns random offset in [-0.5, +0.5] square
      *> C++ equivalent: vec3 sample_square() const (private method)
      *> Output: SAMPLE-OFFSET-X, SAMPLE-OFFSET-Y contain random offsets
       SAMPLE-SQUARE.
      *> Returns the vector to a random point in the [-0.5,-0.5]-[+0.5,+0.5] unit square.
      *> return vec3(random_double() - 0.5, random_double() - 0.5, 0);
           PERFORM RANDOM-DOUBLE                *> Generate first random number
           COMPUTE SAMPLE-OFFSET-X = VEC3-SCALAR - 0.5  *> X offset: random() - 0.5
           
           PERFORM RANDOM-DOUBLE                *> Generate second random number  
           COMPUTE SAMPLE-OFFSET-Y = VEC3-SCALAR - 0.5  *> Y offset: random() - 0.5
           EXIT.
           
      *> Camera ray_color() method - calculates color for a ray
      *> C++ equivalent: color ray_color(const ray& r, const hittable& world) const (private method)
      *> Input: RAY-DATA contains the ray to process
      *> Output: VEC3-RESULT contains the calculated color (for accumulation in render loop)
       CAMERA-RAY-COLOR.
      *> Test ray against world objects using hittable list
      *> hit_record rec;
      *> if (world.hit(r, interval(0, infinity), rec))
           MOVE 0.0 TO HIT-RAY-T-MIN               *> Use 0 as per C++ pseudocode
           MOVE INFINITY-VALUE TO HIT-RAY-T-MAX    *> Use infinity constant
           PERFORM HITTABLE-LIST-HIT               *> Test against world
           
      *> If ray hits any object in world, use surface normal for coloring
           IF HIT-RESULT = 1 AND HIT-OCCURRED = 1
      *> return 0.5 * (rec.normal + color(1,1,1))
      *> Add (1,1,1) to normal to shift from [-1,1] to [0,2], then scale by 0.5 to get [0,1]
               COMPUTE VEC3-RESULT-X = 0.5 * (HIT-NORMAL-X + 1.0)
               COMPUTE VEC3-RESULT-Y = 0.5 * (HIT-NORMAL-Y + 1.0)
               COMPUTE VEC3-RESULT-Z = 0.5 * (HIT-NORMAL-Z + 1.0)
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
               COMPUTE VEC3-RESULT-X = VEC3-TEMP-CALC * 1.0 + 
                                       VEC3-SCALAR * 0.5
               
      *> Green: (1-a)*1.0 + a*0.7 = (1-a) + 0.7*a  
               COMPUTE VEC3-RESULT-Y = VEC3-TEMP-CALC * 1.0 + 
                                       VEC3-SCALAR * 0.7
               
      *> Blue:  (1-a)*1.0 + a*1.0 = (1-a) + a = 1.0 (always full blue)
               COMPUTE VEC3-RESULT-Z = VEC3-TEMP-CALC * 1.0 + 
                                       VEC3-SCALAR * 1.0
           END-IF
           
      *> Result: Creates sphere with surface normal shading on sky gradient background
      *> - Sphere hit: Color based on surface normal direction (creates 3D shading effect)
      *> - Sky background: White to blue gradient based on ray direction
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
      *> C++ equivalent: write_color(std::ostream& out, const color& pixel_color)
       WRITE-COLOR-TO-FILE.
      *> Input: PIXEL-COLOR contains pre-averaged color (already scaled by pixel_samples_scale)
      *> C++ equivalent: write_color(std::cout, const color& pixel_color)
      
      *> Set up intensity interval for clamping [0.000, 0.999]
      *> C++ equivalent: static const interval intensity(0.000, 0.999);
           MOVE INTENSITY-MIN TO INTERVAL-MIN  *> Set interval min = 0.000
           MOVE INTENSITY-MAX TO INTERVAL-MAX  *> Set interval max = 0.999
           
      *> Get RGB components and clamp each to intensity interval
      *> C++ equivalent: auto r = pixel_color.x(); int rbyte = int(256 * intensity.clamp(r));
           MOVE PIXEL-COLOR-R TO INTERVAL-TEST-VALUE  *> r = pixel_color.x()
           PERFORM INTERVAL-CLAMP                     *> intensity.clamp(r)
           COMPUTE COLOR-R-BYTE = 256 * INTERVAL-TEST-VALUE  *> rbyte = int(256 * clamped_r)
           
           MOVE PIXEL-COLOR-G TO INTERVAL-TEST-VALUE  *> g = pixel_color.y()
           PERFORM INTERVAL-CLAMP                     *> intensity.clamp(g)
           COMPUTE COLOR-G-BYTE = 256 * INTERVAL-TEST-VALUE  *> gbyte = int(256 * clamped_g)
           
           MOVE PIXEL-COLOR-B TO INTERVAL-TEST-VALUE  *> b = pixel_color.z()
           PERFORM INTERVAL-CLAMP                     *> intensity.clamp(b)
           COMPUTE COLOR-B-BYTE = 256 * INTERVAL-TEST-VALUE  *> bbyte = int(256 * clamped_b)
           
      *> Write out the pixel color components in PPM format
      *> C++ equivalent: out << rbyte << ' ' << gbyte << ' ' << bbyte << '\n';
           STRING COLOR-R-BYTE " " COLOR-G-BYTE " " COLOR-B-BYTE
                  DELIMITED BY SIZE INTO COLOR-OUTPUT-LINE
           MOVE COLOR-OUTPUT-LINE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.                 *> Output: "255 128 64" (example)
           
      *> Write color to terminal (for debugging/display purposes)
      *> Same as WRITE-COLOR-TO-FILE but outputs to terminal instead of file
       WRITE-COLOR-TO-TERMINAL.
      *> Input: PIXEL-COLOR contains pre-averaged color (already scaled by pixel_samples_scale)
      *> C++ equivalent: write_color(std::cout, const color& pixel_color)
      
      *> Set up intensity interval for clamping [0.000, 0.999]
           MOVE INTENSITY-MIN TO INTERVAL-MIN  *> Set interval min = 0.000
           MOVE INTENSITY-MAX TO INTERVAL-MAX  *> Set interval max = 0.999
           
      *> Get RGB components and clamp each to intensity interval
           MOVE PIXEL-COLOR-R TO INTERVAL-TEST-VALUE  *> r = pixel_color.x()
           PERFORM INTERVAL-CLAMP                     *> intensity.clamp(r)
           COMPUTE COLOR-R-BYTE = 256 * INTERVAL-TEST-VALUE  *> rbyte = int(256 * clamped_r)
           
           MOVE PIXEL-COLOR-G TO INTERVAL-TEST-VALUE  *> g = pixel_color.y()
           PERFORM INTERVAL-CLAMP                     *> intensity.clamp(g)
           COMPUTE COLOR-G-BYTE = 256 * INTERVAL-TEST-VALUE  *> gbyte = int(256 * clamped_g)
           
           MOVE PIXEL-COLOR-B TO INTERVAL-TEST-VALUE  *> b = pixel_color.z()
           PERFORM INTERVAL-CLAMP                     *> intensity.clamp(b)
           COMPUTE COLOR-B-BYTE = 256 * INTERVAL-TEST-VALUE  *> bbyte = int(256 * clamped_b)
           
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
      *> HITTABLE HIT METHOD - Virtual Method Dispatch                 *
      *>****************************************************************
      *> Abstract hit method dispatch (equivalent to virtual bool hit(...))
      *> C++ equivalent: virtual bool hit(const ray& r, interval ray_t, hit_record& rec)
      *> Input: HITTABLE-OBJECT contains the object to test
      *>        RAY-DATA contains the ray to test
      *>        HIT-RAY-T contains valid t interval range
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

      *> Set up ray_t interval for surrounds testing
           MOVE HIT-RAY-T-MIN TO INTERVAL-MIN
           MOVE HIT-RAY-T-MAX TO INTERVAL-MAX

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
      *> if (!ray_t.surrounds(root))
               MOVE SPHERE-ROOT1 TO INTERVAL-TEST-VALUE
               PERFORM INTERVAL-TEST-SURROUNDS
               IF INTERVAL-SURROUNDS = 0
      *> root = (h + sqrtd) / a;  // Try farther intersection
                   COMPUTE SPHERE-ROOT2 = 
                       (SPHERE-H + SPHERE-SQRTD) / SPHERE-A
      *> if (!ray_t.surrounds(root)) return false
                   MOVE SPHERE-ROOT2 TO INTERVAL-TEST-VALUE
                   PERFORM INTERVAL-TEST-SURROUNDS
                   IF INTERVAL-SURROUNDS = 0
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
      *> C++ equivalent: bool hit(const ray& r, interval ray_t, hit_record& rec) const override
      *> Input: RAY-DATA contains ray, HIT-RAY-T contains t interval range
      *> Output: HIT-RECORD contains closest intersection, HIT-RESULT = 1 if any hit found
       HITTABLE-LIST-HIT.
      *> Initialize search for closest hit
           MOVE 0 TO HIT-ANYTHING               *> bool hit_anything = false
           MOVE HIT-RAY-T-MAX TO CLOSEST-T      *> auto closest_so_far = ray_t.max
           MOVE HIT-RAY-T-MIN TO TEMP-TMIN      *> Save original tmin
           
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
      *> if (object->hit(r, ray_t.min, closest_so_far, temp_rec))
                   MOVE TEMP-TMIN TO HIT-RAY-T-MIN
                   MOVE CLOSEST-T TO HIT-RAY-T-MAX  *> Only accept closer hits
                   PERFORM HITTABLE-HIT             *> Test intersection
      



                   *> Debug: Show intersection result (only for first few pixels)
                   *> (debug output removed for cleaner output)
  



      *> If hit found and it's closer than previous closest
                   IF HIT-RESULT = 1 AND HIT-OCCURRED = 1
                       MOVE 1 TO HIT-ANYTHING       *> hit_anything = true
                       MOVE HIT-T TO CLOSEST-T      *> closest_so_far = temp_rec.t
      *> rec = temp_rec (copy hit record immediately - C++ style)
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
      *> INTERVAL CLASS PROCEDURES - Range/Interval Operations         *
      *>****************************************************************
      
      *> Default interval constructor - creates empty interval (equivalent to interval())
      *> C++ equivalent: interval() : min(+infinity), max(-infinity)
      *> Creates an empty interval where min > max
       INTERVAL-CONSTRUCT-DEFAULT.
           MOVE +999999.0 TO INTERVAL-MIN          *> Set to +infinity (empty interval)
           MOVE -999999.0 TO INTERVAL-MAX          *> Set to -infinity (empty interval)
           EXIT.
           
      *> Interval constructor with min and max parameters
      *> C++ equivalent: interval(double min, double max) : min(min), max(max)
      *> Input: INTERVAL-TEMP-MIN contains minimum bound
      *>        INTERVAL-TEMP-MAX contains maximum bound
      *> Output: INTERVAL-DATA contains initialized interval
       INTERVAL-CONSTRUCT-WITH-BOUNDS.
           MOVE INTERVAL-TEMP-MIN TO INTERVAL-MIN  *> Set minimum bound
           MOVE INTERVAL-TEMP-MAX TO INTERVAL-MAX  *> Set maximum bound
           EXIT.
           
      *> Initialize interval from predefined empty constant
      *> C++ equivalent: interval empty = interval(+infinity, -infinity)
       INTERVAL-INIT-EMPTY.
           MOVE EMPTY-MIN TO INTERVAL-MIN          *> Copy empty interval min
           MOVE EMPTY-MAX TO INTERVAL-MAX          *> Copy empty interval max
           EXIT.
           
      *> Initialize interval from predefined universe constant  
      *> C++ equivalent: interval universe = interval(-infinity, +infinity)
       INTERVAL-INIT-UNIVERSE.
           MOVE UNIVERSE-MIN TO INTERVAL-MIN       *> Copy universe interval min
           MOVE UNIVERSE-MAX TO INTERVAL-MAX       *> Copy universe interval max
           EXIT.
           
      *> Calculate size of interval (equivalent to size() method)
      *> C++ equivalent: double size() const { return max - min; }
      *> Input: INTERVAL-DATA contains the interval
      *> Output: INTERVAL-SIZE contains the size (max - min)
       INTERVAL-CALCULATE-SIZE.
           COMPUTE INTERVAL-SIZE = INTERVAL-MAX - INTERVAL-MIN  *> Size = max - min
           EXIT.
           
      *> Test if interval contains a value (equivalent to contains() method)
      *> C++ equivalent: bool contains(double x) const { return min <= x && x <= max; }
      *> Input: INTERVAL-DATA contains the interval
      *>        INTERVAL-TEST-VALUE contains value to test
      *> Output: INTERVAL-CONTAINS = 1 if contained, 0 if not contained
       INTERVAL-TEST-CONTAINS.
      *> Test if min <= x <= max (inclusive bounds)
           IF INTERVAL-MIN <= INTERVAL-TEST-VALUE AND 
              INTERVAL-TEST-VALUE <= INTERVAL-MAX
               MOVE 1 TO INTERVAL-CONTAINS          *> Value is contained in interval
           ELSE
               MOVE 0 TO INTERVAL-CONTAINS          *> Value is outside interval
           END-IF
           EXIT.
           
      *> Test if interval surrounds a value (equivalent to surrounds() method)
      *> C++ equivalent: bool surrounds(double x) const { return min < x && x < max; }
      *> Input: INTERVAL-DATA contains the interval
      *>        INTERVAL-TEST-VALUE contains value to test
      *> Output: INTERVAL-SURROUNDS = 1 if surrounded, 0 if not surrounded
       INTERVAL-TEST-SURROUNDS.
      *> Test if min < x < max (exclusive bounds - stricter than contains)
           IF INTERVAL-MIN < INTERVAL-TEST-VALUE AND 
              INTERVAL-TEST-VALUE < INTERVAL-MAX
               MOVE 1 TO INTERVAL-SURROUNDS         *> Value is surrounded by interval
           ELSE
               MOVE 0 TO INTERVAL-SURROUNDS         *> Value is not surrounded
           END-IF
           EXIT.
           
      *> Convenience method to test contains using VEC3 scalar input
      *> Input: VEC3-SCALAR contains value to test
      *> Output: INTERVAL-CONTAINS = 1 if contained, 0 if not contained
       INTERVAL-CONTAINS-VEC3-SCALAR.
           MOVE VEC3-SCALAR TO INTERVAL-TEST-VALUE  *> Copy test value
           PERFORM INTERVAL-TEST-CONTAINS           *> Test containment
           EXIT.
           
      *> Convenience method to test surrounds using VEC3 scalar input
      *> Input: VEC3-SCALAR contains value to test
      *> Output: INTERVAL-SURROUNDS = 1 if surrounded, 0 if not surrounded
       INTERVAL-SURROUNDS-VEC3-SCALAR.
           MOVE VEC3-SCALAR TO INTERVAL-TEST-VALUE  *> Copy test value
           PERFORM INTERVAL-TEST-SURROUNDS          *> Test surrounding
           EXIT.
           
      *> Clamp a value to interval bounds (equivalent to clamp() method)
      *> C++ equivalent: double clamp(double x) const { if (x < min) return min; if (x > max) return max; return x; }
      *> Input: INTERVAL-DATA contains the interval
      *>        INTERVAL-TEST-VALUE contains value to clamp
      *> Output: INTERVAL-TEST-VALUE contains clamped value (modified in place)
       INTERVAL-CLAMP.
      *> Clamp value to interval bounds [min, max]
           IF INTERVAL-TEST-VALUE < INTERVAL-MIN
               MOVE INTERVAL-MIN TO INTERVAL-TEST-VALUE     *> Below minimum - clamp to min
           ELSE
               IF INTERVAL-TEST-VALUE > INTERVAL-MAX
                   MOVE INTERVAL-MAX TO INTERVAL-TEST-VALUE *> Above maximum - clamp to max
               END-IF
               *> If within bounds, value remains unchanged
           END-IF
           EXIT.
           
      *> Convenience method to clamp using VEC3 scalar input/output
      *> Input: VEC3-SCALAR contains value to clamp
      *> Output: VEC3-SCALAR contains clamped value (modified in place)
       INTERVAL-CLAMP-VEC3-SCALAR.
           MOVE VEC3-SCALAR TO INTERVAL-TEST-VALUE  *> Copy input value
           PERFORM INTERVAL-CLAMP                   *> Clamp to interval bounds
           MOVE INTERVAL-TEST-VALUE TO VEC3-SCALAR  *> Copy result back
           EXIT.

      *> Display interval bounds to terminal (for debugging)
      *> C++ equivalent: std::cout << "[" << min << ", " << max << "]"
       INTERVAL-DISPLAY.
           DISPLAY "[" INTERVAL-MIN ", " INTERVAL-MAX "]"
           EXIT.
           

