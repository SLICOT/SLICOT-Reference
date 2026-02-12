CMAKE Example 
-------------

This example shows the general integration of SLICOT into a CMake-enabled project. 
The workflow requires, that SLICOT was installed on the system before. 

By adding 
```cmake
FIND_PACKAGE(SLICOT REQUIRED)
TARGET_LINK_LIBRARIES(your_target SLICOT::slicot)
```
or
```cmake
FIND_PACKAGE(SLICOT64 REQUIRED)
TARGET_LINK_LIBRARIES(your_target SLICOT::slicot)
```
SLICOT can be used in your project. 


