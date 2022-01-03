set(BINNAME "soulshared_test")

file(GLOB_RECURSE test_sources LIST_DIRECTORIES false *.f90 *.h *.F90)

include_directories (${TEST_SOURCE_DIR}/src)

set(SOURCES ${TEST_SOURCES})

add_executable(${BINNAME} ${test_sources})

target_include_directories(${BINNAME} PUBLIC ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})

target_link_libraries(${BINNAME} PUBLIC ${CMAKE_PROJECT_NAME}_lib)

add_test(NAME ${BINNAME} COMMAND ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${BINNAME})

add_subdirectory(arrays)
add_subdirectory(config)
add_subdirectory(masks)
add_subdirectory(structures)
add_subdirectory(grid)