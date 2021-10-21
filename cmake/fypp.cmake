# Register custom commands for processing source files with fypp (.fpp -> .f90)
#
# Args:
#     oldfiles [in]: List of files to preprocess (must have .fpp suffix)
#     newfiles [out]: List of preprocessed files (will have .F90 suffix).
#
#     All files have full path
#
# Source:
#     https://github.com/dftbplus/mpifx/blob/master/cmake/MpiFxUtils.cmake
#
function(fypp_preprocess oldfiles newfiles)

  set(_newfiles)
  foreach(oldfile IN LISTS oldfiles)
    string(REGEX REPLACE "\\.fpp" ".F90" newfile ${oldfile})
    add_custom_command(
      OUTPUT ${newfile}
      COMMAND ${FYPP} ${FYPP_FLAGS} ${oldfile} ${newfile}
      MAIN_DEPENDENCY ${oldfile})
    list(APPEND _newfiles ${newfile})
  endforeach()
  set(${newfiles} ${_newfiles} PARENT_SCOPE)

endfunction()
