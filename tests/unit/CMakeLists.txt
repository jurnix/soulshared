set(BINARY ${CMAKE_PROJECT_NAME}_test)

file(GLOB_RECURSE test_sources LIST_DIRECTORIES false *.f90 *.h *.F90)

include_directories (${TEST_SOURCE_DIR}/src)

set(SOURCES ${TEST_SOURCES})

add_executable(${BINARY} ${test_sources})

target_include_directories(${BINARY} PUBLIC ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})

target_link_libraries(${BINARY} PUBLIC ${CMAKE_PROJECT_NAME}_lib)

add_test(NAME ${BINARY} COMMAND ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${BINARY})
