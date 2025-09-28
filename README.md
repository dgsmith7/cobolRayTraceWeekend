# cobolRayTraceWeekend

A Cobol implementation of the project covered in Ray Tracing in One Weekend by Peter Shirley.
https://raytracing.github.io/books/RayTracingInOneWeekend.html

## Overview

I am using gnuCobol on a macbook pro M3 with pro chip.

This will be an interesting project because Cobol is not an object-oriented language, so I will have to simulate classes and inheritance with data structures and procedural code. There is no inheritnce, no abstract classes, etc., all of which are used in this project. So it will be interesting to see one can implement these concepts in Cobol.

What else I have observed that is a departure from my usual JS/TS and Python coding is that Cobol is very versbose. I would normally divide this code base into many files to keep things organized, but in Cobol I am keeping everything in one file to avoid the overhead of multiple compilation units.

Additionally, COBOL is a compiled language, so there is a compilation step before running the code. Also, Cobol is strongly typed, so all variables must be declared with their types before use.

I am also harnessing Github Copilot with multiple AI models as an assistant.

All of the ppm image files generated with the code, in general, are named to reference the images in the book. View resulting "ppm" images in Preview on Mac. GIMP should work on Linux (and probably on Windows). I am othewise unsure about viewing ppm files on Windows.

I have worked with the AI assistant to annotate the code with educational notes about Cobol where appropriate, so it is woth looking over the code for those notes.

## Result:

We completed chapter 8. I started to work on chapter 9, which uses a recursive algorithm to model diffuse materials. Since COBOL does not support recursion, we tried several workarounds with poor results. Finally, I had a discussion with Claude Sonnet 4 about moving forward and how we might implement the algorithm:

Me: "Before you do that, lets consider that there is much more to implment in the guide so whatever solution you are coming up with will need to work moving forward with the guide. If you think that we are at the limits of what we can really do with COBOL, then just say so. But we really cant deviate too far from the guide's c++ pseudocode."

Claude Sonnet 4: "You're absolutely right to ask this question, and I need to be honest with you about COBOL's limitations here.

The fundamental issue: The "Ray Tracing in One Weekend" tutorial is built around recursive algorithms that are core to modern ray tracing. The C++ pseudocode relies heavily on:

Deep recursion (10-50 levels for ray bouncing)
Complex material systems with virtual dispatch
Dynamic memory allocation for object lists
Advanced mathematical operations and random number generation
COBOL's limitations:

No practical recursion support (causes stack overflows)
No object-oriented features (no virtual dispatch, inheritance)
Fixed memory allocation (can't dynamically build object lists)
Limited floating-point precision and mathematical operations
No function pointers or closures for material behavior
What we've discovered:

Surface normal shading: ✅ Works perfectly
Antialiasing: ✅ Works perfectly
Simple ray-sphere intersection: ✅ Works perfectly
Lambertian scattering with recursion: ❌ Fundamentally incompatible with COBOL
The honest assessment: We're at the practical limits of what COBOL can do for this tutorial. The guide assumes modern language features that simply don't exist in COBOL. To continue following the guide faithfully, we would need:

Materials classes (Chapter 9.2+)
Metal materials with reflection (Chapter 10)
Dielectric materials with refraction (Chapter 11)
Positionable cameras (Chapter 12)
Defocus blur (Chapter 13)
All of these depend on the recursive ray bouncing we can't implement properly in COBOL.

My recommendation: We have a beautiful, working COBOL ray tracer that implements the core concepts through Chapter 8 (antialiasing). This is actually quite an achievement! But to go further would require such significant deviations from the guide that it wouldn't really be following the tutorial anymore."
