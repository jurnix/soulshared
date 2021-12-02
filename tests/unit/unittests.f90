!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : unittest
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> It runs all tests 
!------------------------------------------------------------------------------

program unittest

  use datetime_test, only: testSuiteDatetime
  use tst_test, only: testSuiteTst
  use strings_test, only: testSuiteStrings
  use dict_test, only: testSuiteDict
  use orderedDict_test, only: testSuiteOrderedDict
  use arrayUtils_test, only: testSuiteArrayUtils
  use perfTimer_test, only: testSuitePerfTimer
  use file_test, only: testSuiteFile
  use configParser_test, only: testSuiteConfigParser
  use configFile_test, only: testSuiteConfigFile
  use typeOptions_test, only: testSuiteTypeOptions
  use cfgOptions_test, only: testSuiteCfgOptions
  use oopExtends_test, only: testSuiteOopExtends
  use linkedList_test, only: testSuiteLinkedList
  use set_test, only: testSuiteSet

  ! array
  use shr_arrayRealDim_test, only: testSuiteArrayRealDim
  use arrayRsp_test, only: testSuiteArrayRsp
  use shr_arrayGridFullRsp_test, only: testSuiteArrayGridFullRsp
  use shr_arrayContainerAllocatable_test, only: testSuiteArrayContainerAllocatable

  ! grid related
  use grid_test, only: testSuiteGrid
  use gridPartition_test, only: testSuiteGridPartition
  use gridcell_test, only: testSuiteGridCell
  use coord_test, only: testSuiteCoord
  use shr_gGridAxes_test, only: testSuitegGridAxes
  use shr_gGridAxesBounds_test, only: testSuitegGridAxesBounds
  use shr_gGridAxesCell_test, only: testSuitegGridAxesCell
  use shr_gAxisMapping_test, only: testSuitegAxisMapping
  use shr_gAxisCellIndex_test, only: testSuitegAxisCellIndex
  use shr_gridcellsMapping_test, only: testSuiteGridcellsMapping
  use shr_gridIndicesMapping_test, only: testSuiteGridIndicesMapping
  use shr_gridMask_test, only: testSuiteGridMask


  implicit none

  ! declare each test
  type(testSuiteDatetime) :: tsDatetime
  type(testSuiteTst) :: tsTst
  type(testSuiteStrings) :: tsStrings
  type(testSuiteDict) :: tsDict
  type(testSuiteOrderedDict) :: tsOrderedDict
  type(testSuiteArrayUtils) :: tsArrayUtils
  type(testSuitePerfTimer) :: tsPerfTimer
  type(testSuiteFile) :: tsFile
  type(testSuiteConfigParser) :: tsConfigParser
  type(testSuiteConfigFile) :: tsConfigFile
  type(testSuiteTypeOptions) :: tsTypeOptions
  type(testSuiteCfgOptions) :: tsCfgOptions
  type(testSuiteOopExtends) :: tsOOPExtends
  type(testSuiteLinkedList) :: tsLinkedList
  type(testSuiteSet) :: tsSet
  type(testSuiteArrayRealDim) :: tsArrayRealDim
  type(testSuiteArrayRsp) :: tsArrayRsp
  type(testSuiteArrayGridFullRsp) :: tsArrayGridFullRsp
  type(testSuiteArrayContainerAllocatable) :: tsArrayContainerAllocatable
  type(testSuiteGrid) :: tsGrid
  type(testSuiteGridPartition) :: tsGridPartition
  type(testSuiteGridCell) :: tsGridcell
  type(testSuiteCoord) :: tsCoord
  type(testSuitegGridAxesBounds) :: tsgGridAxesBounds
  type(testSuitegGridAxes) :: tsgGridAxes
  type(testSuitegGridAxesCell) :: tsgGridAxesCell
  type(testSuitegAxisMapping) :: tsgAxisMapping
  type(testSuitegAxisCellIndex) :: tsgAxisCellIndex
  type(testSuiteGridcellsMapping) :: tsGridcellsMapping
  type(testSuiteGridIndicesMapping) :: tsGridIndicesMapping
  type(testSuiteGridMask) :: tsGridMask

  ! declare tests
  call tsDatetime % init("Datetime test", 330)
  call tsTst % init("Ternary search tree test", 30)
  call tsStrings % init("Strings test", 70)
  call tsDict % init("Dict test", 10)
  call tsOrderedDict % init("Ordered dict test", 10)
  call tsArrayUtils % init("Array utils test", 20)
  call tsPerfTimer % init("Performance timer test", 20)
  call tsFile % init("File test", 10)
  call tsOOPExtends % init("OOP Extends test", 10)
  call tsConfigParser % init("(CFG) Config parser test", 30)
  call tsConfigFile % init("(CFG) Config file test", 20)
  call tsTypeOptions % init("(CFG) Type options test", 40)
  call tsCfgOptions % init("(CFG) Cfg options test", 20)
  call tsLinkedList % init("Linked list test", 10)
  call tsSet % init("Set test", 10)
  call tsArrayRealDim % init("ArrayRealDim test", 30)
  call tsArrayRsp % init("ArrayRsp test", 20)
  call tsArrayGridFullRsp % init("ArrayGridFullRsp test", 20)
  call tsArrayContainerAllocatable % init("ArrayContainerAllocatable test", 20)

  call tsGrid % init("Grid test", 40)
  call tsGridPartition % init("Grid partition test", 20)
  call tsGridcell % init("Gridcell test", 20)
  call tsCoord % init("Coordinate test", 10)
  call tsgGridAxesBounds % init("gGridAxesBounds test", 20)
  call tsgGridAxesCell % init("gGridAxesCell test", 20)
  call tsgGridAxes % init("gGridAxes test", 20)
  call tsgAxisMapping % init("gAxisMapping test", 20)
  call tsgAxisCellIndex % init("gAxisCellIndex test", 20)
  call tsGridcellsMapping % init("gridCellsMapping test", 10)
  call tsGridindicesMapping % init("gridIndicesMapping test", 10)
  call tsGridMask % init("gridMask test", 10)
  

  ! run all tests 
  call tsDatetime % run()
  call tsTst % run() 
  call tsStrings % run() 
  call tsDict % run() 
  call tsOrderedDict % run() 
  call tsArrayUtils % run() 
  call tsPerfTimer % run() 
  call tsFile % run() 
  call tsConfigParser % run() 
  call tsConfigFile % run() 
  call tsTypeOptions % run() 
  call tsCfgOptions % run() 
  call tsOOPExtends % run() 
  call tsLinkedList % run() 
  call tsSet % run() 
  call tsArrayRealDim % run() 
  call tsArrayContainerAllocatable % run() 
  call tsArrayRsp % run() 
  call tsArrayGridFullRsp % run() 
  call tsGrid % run() 
  call tsGridPartition % run() 
  call tsGridCell % run() 
  call tsCoord % run() 
  call tsgGridAxesBounds % run() 
  call tsgGridAxesCell % run() 
  call tsgGridAxes % run()
  call tsgAxisMapping % run()
  call tsgAxisCellIndex % run()
  call tsGridcellsMapping % run()
  call tsGridIndicesMapping % run()
  call tsGridMask % run()


  ! show results
  call tsDatetime % report()
  call tsTst % report() 
  call tsStrings % report() 
  call tsDict % report() 
  call tsOrderedDict % report() 
  call tsArrayUtils % report() 
  call tsPerfTimer % report() 
  call tsFile % report()
  call tsConfigParser % report() 
  call tsConfigFile % report() 
  call tsTypeOptions % report() 
  call tsCfgOptions % report() 
  call tsOOPExtends % report()
  call tsLinkedList % report() 
  call tsSet % report() 
  call tsArrayRealDim % report() 
  call tsArrayContainerAllocatable % report() 
  call tsArrayRsp % report() 
  call tsArrayGridFullRsp % report() 
  call tsGrid % report() 
  call tsGridPartition % report() 
  call tsGridcell % report() 
  call tsCoord % report() 
  call tsgGridAxesBounds % report()
  call tsgGridAxesCell % report()
  call tsgGridAxes % report()
  call tsgAxisMapping % report()
  call tsgAxisCellIndex % report()
  call tsGridcellsMapping % report() 
  call tsGridIndicesMapping % report() 
  call tsGridMask % report() 

  ! crash if any of the tests fail
  if (.not. tsDatetime % isSuccessful() .or. &
      .not. tsTst % isSuccessful() .or. &
      .not. tsStrings % isSuccessful() .or. & 
      .not. tsDict % isSuccessful() .or. &
      .not. tsOrderedDict % isSuccessful() .or. &
      .not. tsArrayUtils % isSuccessful() .or. &
      .not. tsPerfTimer % isSuccessful() .or. &
      .not. tsFile % isSuccessful() .or. &
      .not. tsConfigParser % isSuccessful() .or. & 
      .not. tsConfigFile % isSuccessful() .or. & 
      .not. tsTypeOptions % isSuccessful() .or. & 
      .not. tsCfgOptions % isSuccessful() .or. & 
      .not. tsOOPExtends % isSuccessful() .or. &
      .not. tsLinkedList % isSuccessful() .or. &
      .not. tsSet % isSuccessful() .or. &
      .not. tsArrayRealDim % isSuccessful() .or. &
      .not. tsArrayContainerAllocatable % isSuccessful() .or. &
      .not. tsArrayRsp % isSuccessful() .or. &
      .not. tsArrayGridFullRsp % isSuccessful() .or. &
      .not. tsGrid % isSuccessful() .or. &
      .not. tsGridPartition % isSuccessful() .or. &
      .not. tsGridcell % isSuccessful() .or. &
      .not. tsCoord % isSuccessful() .or. &
      .not. tsgGridAxesBounds % isSuccessful() .or. &
      .not. tsgGridAxesCell % isSuccessful() .or. &
      .not. tsgGridAxes % isSuccessful() .or. & 
      .not. tsgAxisMapping % isSuccessful() .or. & 
      .not. tsgAxisCellIndex % isSuccessful() .or. & 
      .not. tsGridcellsMapping % isSuccessful() .or. & 
      .not. tsGridIndicesMapping % isSuccessful() .or. & 
      .not. tsGridMask % isSuccessful() & 
      ) stop 1

end program unittest
